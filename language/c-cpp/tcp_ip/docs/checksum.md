# チェックサム

→ [index.md](index.md) | [アーキテクチャ](architecture.md)

---

## RFC 1071 インターネットチェックサムの仕組み

チェックサムは「16bit ビッグエンディアン語の和の 1 の補数」です。
`src/utils.c` に実装されています。

### 計算フロー

```
入力データ (任意長のバイト列)
  │
  ▼
checksum_partial(data, len, init)  → uint32_t
  ┌────────────────────────────────────────────────────┐
  │ while (len > 1):                                   │
  │     sum += (buf[i] << 8) | buf[i+1]   ← big-endian│
  │     i += 2                                         │
  │ if (len == 1):                                     │
  │     sum += buf[i] << 8   ← 奇数バイトは上位扱い   │
  │ return sum   (32bit, キャリーあり)                 │
  └────────────────────────────────────────────────────┘
  │
  ▼
checksum_finish(partial)  → uint16_t  ← ホストバイトオーダーで返す
  ┌────────────────────────────────────────────────────┐
  │ // 32bit → 16bit に折り畳む (キャリーを加算)       │
  │ while (partial >> 16):                             │
  │     partial = (partial & 0xffff) + (partial >> 16) │
  │ return ~partial   (1 の補数)                       │
  └────────────────────────────────────────────────────┘
```

複数バッファにまたがる場合は `checksum_partial` を連鎖して呼び出します:

```c
uint32_t csum = checksum_partial(buf1, len1, 0);      // 1 バッファ目
csum          = checksum_partial(buf2, len2, csum);   // 2 バッファ目に積み上げ
uint16_t result = checksum_finish(csum);
```

---

## `htons()` が必要な理由

`checksum_finish()` は **ホストバイトオーダー** (x86 では little-endian) で値を返します。
一方、チェックサムフィールドはネットワークバイトオーダー (big-endian) で格納する必要があります。

### バグのあるコード (修正前)

```c
hdr->checksum = checksum_finish(csum);
// x86 では little-endian でメモリに書き込まれる
// 例: ~fold = 0x1234 → メモリ上のバイト列: [0x34, 0x12]
```

検証時に `checksum_partial` がこのフィールドを big-endian として読み返すと:

```
読み取り値: (0x34 << 8) | 0x12 = 0x3412  ← 本来の 0x1234 とは異なる値
```

この結果、合計が `0xFFFF` にならず検証が常に失敗します。

### 正しいコード (修正後)

```c
hdr->checksum = htons(checksum_finish(csum));
// htons(): ホスト → ネットワークバイトオーダー変換
// 0x1234 → メモリ上のバイト列: [0x12, 0x34]  ← big-endian
```

検証時: `(0x12 << 8) | 0x34 = 0x1234` → 正しく読み取れる → 合計 = `0xFFFF` → 検証 PASS

### 影響を受けたファイルと修正箇所

| ファイル | 関数 | 修正内容 |
|---------|------|---------|
| `src/tcp.c` | `tcp_send_segment` | `checksum_finish(csum)` → `htons(checksum_finish(csum))` |
| `src/icmp.c` | `icmp_send_echo_reply` | `checksum(buf, total_len)` → `htons(checksum(buf, total_len))` |
| `src/ip.c` | `ip_checksum_fill` | `checksum(hdr, hdr_len)` → `htons(checksum(hdr, hdr_len))` |
| `fuzz/fuzz_tcp.c` | `LLVMFuzzerTestOneInput` | 同上 |
| `fuzz/fuzz_icmp.c` | `LLVMFuzzerTestOneInput` | 同上 |

---

## プロトコル別のチェックサム計算範囲

### IP ヘッダチェックサム (`src/ip.c`)

対象範囲: **IP ヘッダのみ** (ペイロードを含まない)

```c
void ip_checksum_fill(struct ip_hdr *hdr) {
    size_t hdr_len = (size_t)(hdr->version_ihl & 0x0f) * 4;  // 通常 20 バイト
    hdr->checksum = 0;
    hdr->checksum = htons(checksum(hdr, hdr_len));
}
```

検証: `ip_checksum_valid(hdr)` → `checksum(hdr, hdr_len) == 0`

### ICMP チェックサム (`src/icmp.c`)

対象範囲: **ICMP ヘッダ + データ全体**

```c
hdr->checksum = 0;
hdr->checksum = htons(checksum(buf, total_len));
```

検証: `checksum(payload, len) == 0`

### TCP チェックサム (`src/tcp.c`)

対象範囲: **IP 擬似ヘッダ (12 バイト) + TCP セグメント全体**

IP 擬似ヘッダ (RFC 793) を含めることで、IP アドレスの偽装を検出できます。

```c
struct ip_pseudo_hdr pseudo;
pseudo.src_ip   = conn->local_ip;
pseudo.dst_ip   = conn->remote_ip;
pseudo.zero     = 0;
pseudo.protocol = IP_PROTO_TCP;          // = 6
pseudo.tcp_len  = htons((uint16_t)seg_len);

hdr->checksum = 0;
uint32_t csum = checksum_partial(&pseudo, sizeof(pseudo), 0);  // 擬似ヘッダ
csum          = checksum_partial(seg, seg_len, csum);           // TCP セグメント
hdr->checksum = htons(checksum_finish(csum));
```

検証 (`tcp_recv` 内):

```c
uint32_t csum = checksum_partial(&pseudo, sizeof(pseudo), 0);
csum          = checksum_partial(segment, seg_len, csum);
if (checksum_finish(csum) != 0) {
    // チェックサム不正 → ドロップ
}
```

---

## 検証の数学的根拠

正しいチェックサムを含む全データに対してチェックサムを計算すると `0` になります。

```
データ部の和: S (折り畳み後)
チェックサム: C = ~S (1 の補数) を big-endian で格納

検証時の和:
  S + C = S + ~S = 0xFFFF  (1 の補数の性質)

checksum_finish(0xFFFF):
  ~0xFFFF = 0x0000  → 検証 PASS
```
