import hashlib
import hmac
import struct
from typing import Union, Optional
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.backends import default_backend

def ansi_x942_kdf(
    shared_secret: bytes, 
    key_length: int, 
    algorithm_id: bytes, 
    party_u_info: bytes = b"", 
    party_v_info: bytes = b"", 
    supp_pub_info: bytes = b""
) -> bytes:
    """
    ANS X9.42-2001 Key Derivation Function
    
    金融業界で使用される離散対数暗号における鍵導出関数。
    共有秘密から暗号化やMAC用の鍵を安全に導出する。
    
    アルゴリズム概要:
    1. OtherInfo = AlgId || PartyUInfo || PartyVInfo || SuppPubInfo を構成
    2. 必要な鍵長に応じてハッシュ回数 reps = ceil(key_length / hash_length) を計算
    3. 各ブロック i について: Ki = Hash(shared_secret || OtherInfo || counter_i)
    4. 最終鍵 = K1 || K2 || ... || Kreps の左端 key_length バイト
    
    処理レベル: derived_key = ansi_x942_kdf(shared_secret, key_length, algorithm_id, ...)
    
    Args:
        shared_secret: DH鍵交換等で得られた共有秘密値
        key_length: 導出したい鍵の長さ（バイト）
        algorithm_id: 使用するアルゴリズムの識別子
        party_u_info: パーティUの識別情報（オプション）
        party_v_info: パーティVの識別情報（オプション）
        supp_pub_info: 補助的な公開情報（オプション）
    
    Returns:
        指定された長さの導出鍵
        
    Note:
        RFC 2631 Section 2.1.2の仕様に準拠
        カウンタは shared_secret の後に配置される（SP 800-56Aとは異なる）
    """
    hash_func = hashlib.sha256
    hash_len = hash_func().digest_size
    
    # OtherInfo = AlgId || PartyUInfo || PartyVInfo || SuppPubInfo
    other_info = algorithm_id + party_u_info + party_v_info + supp_pub_info
    
    # 必要なハッシュの回数を計算
    reps = (key_length + hash_len - 1) // hash_len
    
    derived_key = b""
    for i in range(1, reps + 1):
        # Zi || OtherInfo || counter
        hash_input = shared_secret + other_info + struct.pack(">I", i)
        hash_output = hash_func(hash_input).digest()
        derived_key += hash_output
    
    return derived_key[:key_length]

def ansi_x963_kdf(
    shared_secret: bytes, 
    key_length: int, 
    shared_info: bytes = b""
) -> bytes:
    """
    ANS X9.63-2001 Key Derivation Function
    
    楕円曲線暗号における鍵導出関数。ECDH（楕円曲線Diffie-Hellman）から
    得られた共有秘密を暗号学的に安全な鍵に変換する。
    
    アルゴリズム概要:
    1. 必要な鍵長に応じてハッシュ回数 reps = ceil(key_length / hash_length) を計算
    2. 各ブロック i について: Ki = Hash(shared_secret || SharedInfo || counter_i)
    3. 最終鍵 = K1 || K2 || ... || Kreps の左端 key_length バイト
    
    処理レベル: derived_key = ansi_x963_kdf(shared_secret, key_length, shared_info)
    
    Args:
        shared_secret: ECDH等で得られた共有秘密値
        key_length: 導出したい鍵の長さ（バイト）
        shared_info: 両当事者が共有する追加情報（コンテキスト情報）
    
    Returns:
        指定された長さの導出鍵
        
    Note:
        RFC 3278やSEC 1で参照される標準的なECC KDF
        Bitcoin等の暗号通貨でも類似の方式が使用される
    """
    hash_func = hashlib.sha256
    hash_len = hash_func().digest_size
    
    # 必要なハッシュの回数を計算
    reps = (key_length + hash_len - 1) // hash_len
    
    derived_key = b""
    for i in range(1, reps + 1):
        # Z || SharedInfo || Counter
        hash_input = shared_secret + shared_info + struct.pack(">I", i)
        hash_output = hash_func(hash_input).digest()
        derived_key += hash_output
    
    return derived_key[:key_length]

def ssh_kdf(
    shared_secret: bytes, 
    hash_value: bytes, 
    session_id: bytes, 
    key_type: int, 
    key_length: int
) -> bytes:
    """
    SSH Key Derivation Function
    
    SSH（Secure Shell）プロトコルで使用される鍵導出関数。
    DH鍵交換の結果から、暗号化、MAC、初期化ベクトル用の鍵を導出する。
    
    アルゴリズム概要:
    1. K1 = Hash(K || H || X || session_id) を計算（初期ブロック）
    2. Ki = Hash(K || H || K1 || K2 || ... || Ki-1) を計算（累積方式）
    3. 最終鍵 = K1 || K2 || ... || Kn の左端 key_length バイト
    
    処理レベル: derived_key = ssh_kdf(shared_secret, hash_value, session_id, key_type, key_length)
    
    Args:
        shared_secret: DH鍵交換で得られた共有秘密 K
        hash_value: 鍵交換のハッシュ値 H
        session_id: SSH接続の一意識別子（通常は最初のH値）
        key_type: 鍵タイプ識別子（A=65:IV C→S, B=66:IV S→C, C=67:暗号鍵C→S, etc.）
        key_length: 導出したい鍵の長さ（バイト）
    
    Returns:
        指定された長さの導出鍵
        
    Note:
        RFC 4253 Section 7.2で定義
        各鍵タイプ（A-F）で異なる用途の鍵を生成
        フィードバック方式で長い鍵も安全に生成可能
    """
    hash_func = hashlib.sha256
    hash_len = hash_func().digest_size
    
    # 必要なハッシュの回数を計算
    reps = (key_length + hash_len - 1) // hash_len
    
    # 最初のキー K1 = HASH(K || H || X || session_id)
    first_input = shared_secret + hash_value + bytes([key_type]) + session_id
    k1 = hash_func(first_input).digest()
    
    if reps == 1:
        return k1[:key_length]
    
    # 追加のキーを生成（累積方式）
    derived_key = k1
    for i in range(2, reps + 1):
        # Ki = HASH(K || H || K1 || K2 || ... || K(i-1))
        hash_input = shared_secret + hash_value + derived_key
        ki = hash_func(hash_input).digest()
        derived_key += ki
    
    return derived_key[:key_length]

def srtp_kdf(
    master_key: bytes, 
    master_salt: bytes, 
    index: int, 
    label: int, 
    key_length: int, 
    kdr: int = 0
) -> bytes:
    """
    SRTP Key Derivation Function
    
    SRTP（Secure Real-time Transport Protocol）で使用される鍵導出関数。
    単一のマスター鍵から、暗号化鍵、認証鍵、ソルト値を導出する。
    
    アルゴリズム概要:
    1. key_id = label || (index DIV kdr) を計算
    2. AES入力 = (key_id XOR master_salt) || 0x0000 を構成
    3. AES-CTRモードでカウンタを増加させながら必要な鍵長まで生成
    4. 複数ブロックが必要な場合はAES入力にカウンタを加算
    
    処理レベル: derived_key = srtp_kdf(master_key, master_salt, index, label, key_length, kdr)
    
    Args:
        master_key: 128/192/256ビットのマスター鍵
        master_salt: マスターソルト（通常112ビット/14バイト）
        index: RTP/RTCPパケットのインデックス値
        label: 鍵タイプ識別子（0x00:暗号鍵, 0x01:認証鍵, 0x02:ソルト値）
        key_length: 導出したい鍵の長さ（バイト）
        kdr: Key Derivation Rate（鍵更新頻度、デフォルト0=更新なし）
    
    Returns:
        指定された長さの導出鍵
        
    Note:
        RFC 3711 Section 4.3で定義
        AES-128/192/256をサポート
        リアルタイム通信でのパフォーマンスを重視した設計
    """
    # key_id = label || (index DIV kdr)
    if kdr == 0:
        key_div_r = 0
    else:
        key_div_r = index // (2 ** kdr)
    
    # 48-bit index DIV kdr を6バイトとして表現
    key_id = bytes([label]) + struct.pack(">Q", key_div_r)[2:]  # 48-bit value (6 bytes)
    
    # key_id と master_salt を右揃えでXOR（長さを合わせる）
    if len(key_id) < len(master_salt):
        key_id = b'\x00' * (len(master_salt) - len(key_id)) + key_id
    elif len(master_salt) < len(key_id):
        master_salt = b'\x00' * (len(key_id) - len(master_salt)) + master_salt
    
    # input = (key_id XOR master_salt) || 0x0000
    xor_result = bytes(a ^ b for a, b in zip(key_id, master_salt))
    aes_input = xor_result + b'\x00\x00'  # 16バイトのAES入力
    
    # 16バイトに調整
    if len(aes_input) < 16:
        aes_input = aes_input + b'\x00' * (16 - len(aes_input))
    elif len(aes_input) > 16:
        aes_input = aes_input[:16]
    
    # AESで鍵導出
    cipher = Cipher(algorithms.AES(master_key), modes.ECB(), backend=default_backend())
    encryptor = cipher.encryptor()
    
    # 必要なブロック数を計算
    reps = (key_length + 15) // 16  # AESブロックサイズは16バイト
    
    derived_key = b""
    for i in range(reps):
        # input + i のカウンタを最後の4バイトに追加
        current_input = aes_input[:-4] + struct.pack(">I", 
            struct.unpack(">I", aes_input[-4:])[0] + i)
        
        block_output = encryptor.update(current_input)
        derived_key += block_output
    
    encryptor.finalize()
    return derived_key[:key_length]

def snmp_kdf(password: bytes, engine_id: bytes) -> bytes:
    """
    SNMP Key Derivation Function / Key Localization Function
    
    SNMP v3のUser-based Security Modelで使用される鍵導出関数。
    ユーザーパスワードから各SNMP Engineに固有の共有鍵を生成する。
    
    アルゴリズム概要:
    1. パスワードを1048576バイトまで繰り返し拡張
    2. 拡張パスワードをSHA-1でハッシュ化（derived_password）
    3. derived_password || engine_id || derived_password をSHA-1でハッシュ
    4. 結果が各Engine固有の共有鍵となる
    
    処理レベル: shared_key = snmp_kdf(password, engine_id)
    
    Args:
        password: ユーザーパスワード（任意長）
        engine_id: 各SNMP Engineの一意識別子
    
    Returns:
        20バイトの共有鍵（SHA-1出力）
        
    Note:
        RFC 2574 Section 2.6で定義
        各SNMPエンジンで異なる鍵を生成することでセキュリティを向上
        パスワードの繰り返し拡張により辞書攻撃を困難にする
    """
    # パスワードを1048576バイトまで拡張
    password_len = len(password)
    repetitions = (1048576 + password_len - 1) // password_len
    expanded_password = (password * repetitions)[:1048576]
    
    # 拡張されたパスワードをハッシュ
    derived_password = hashlib.sha1(expanded_password).digest()
    
    # engine_idと組み合わせて最終鍵を生成
    shared_key = hashlib.sha1(derived_password + engine_id + derived_password).digest()
    
    return shared_key

def tpm_kdf(auth: bytes, nonce_even: bytes, nonce_odd: bytes) -> bytes:
    """
    TPM Key Derivation Function
    
    TPM（Trusted Platform Module）v1.2で使用される鍵導出関数。
    認証値と二つのナンスから、TPMとアプリケーション間の
    セッション保護用の鍵を導出する。
    
    アルゴリズム概要:
    1. SKEY = HMAC-SHA1(Auth, Nonce_even || Nonce_odd)
    2. SKEYは通信の完全性保護用のHMAC鍵として使用
    3. 必要に応じてSKEYから更なる鍵を導出可能
    
    処理レベル: skey = tpm_kdf(auth, nonce_even, nonce_odd)
    
    Args:
        auth: TPMとアプリケーション間で共有される認証値
        nonce_even: TPMが生成する乱数値
        nonce_odd: アプリケーションが生成する乱数値
    
    Returns:
        20バイトのセッション鍵（SHA-1ベースHMAC出力）
        
    Note:
        TPM Main Specification Part 3で定義
        SP 800-108のフィードバックモード（IV=空）の特殊ケース
        TPMの限られた計算資源を考慮したシンプルな設計
    """
    # SKEY = HMAC(Auth, Nonce_even || Nonce_odd)
    combined_nonces = nonce_even + nonce_odd
    skey = hmac.new(auth, combined_nonces, hashlib.sha1).digest()
    
    return skey

def ikev1_skeyid_psk(pre_shared_key: bytes, ni: bytes, nr: bytes) -> bytes:
    """
    IKEv1 SKEYID generation with pre-shared key
    
    IKE（Internet Key Exchange）version 1での事前共有鍵を使用した
    SKEYID（Session Key ID）生成。IPSecのセキュリティアソシエーション確立で使用。
    
    アルゴリズム概要:
    1. SKEYID = HMAC(pre-shared-key, Ni || Nr)
    2. SKEYIDから更にSKEYID_d, SKEYID_a, SKEYID_eを導出
    3. 認証方式により異なるSKEYID生成方法を使用
    
    処理レベル: skeyid = ikev1_skeyid_psk(pre_shared_key, ni, nr)
    
    Args:
        pre_shared_key: 事前に共有された秘密鍵
        ni: イニシエータが生成したナンス値
        nr: レスポンダが生成したナンス値
    
    Returns:
        20バイトのSKEYID（HMAC-SHA1出力）
        
    Note:
        RFC 2409で定義
        IKEv1は現在廃止予定（IKEv2使用推奨）
        デジタル署名認証時は異なる計算式を使用
    """
    # SKEYID = HMAC(pre-shared-key, Ni || Nr)
    combined_nonces = ni + nr
    skeyid = hmac.new(pre_shared_key, combined_nonces, hashlib.sha1).digest()
    
    return skeyid

def ikev2_skeyseed(ni: bytes, nr: bytes, dh_shared_secret: bytes) -> bytes:
    """
    IKEv2 SKEYSEED generation
    
    IKE version 2でのSKEYSEED生成。IKEv1の改良版で、
    より安全な鍵導出とSP 800-56Cとの整合性を実現。
    
    アルゴリズム概要:
    1. SKEYSEED = HMAC(Ni || Nr, g^ir) - Extract段階
    2. SKEYSEEDから7つの鍵を導出 - Expand段階
    3. Extract-then-Expandパラダイムに準拠
    
    処理レベル: skeyseed = ikev2_skeyseed(ni, nr, dh_shared_secret)
    
    Args:
        ni: イニシエータが生成したナンス値
        nr: レスポンダが生成したナンス値
        dh_shared_secret: Diffie-Hellman鍵交換で得られた共有秘密
    
    Returns:
        32バイトのSKEYSEED（HMAC-SHA256出力）
        
    Note:
        RFC 4306で定義、RFC 7296で更新
        SP 800-56C Extract-then-Expand KDFに準拠
        IKEv1よりも強固なセキュリティを提供
    """
    # SKEYSEED = HMAC(Ni || Nr, g^ir)
    combined_nonces = ni + nr
    skeyseed = hmac.new(combined_nonces, dh_shared_secret, hashlib.sha256).digest()
    
    return skeyseed

def tls_prf_sha256(secret: bytes, label: bytes, seed: bytes, length: int) -> bytes:
    """
    TLS 1.2 PRF (P_SHA256)
    
    TLS（Transport Layer Security）version 1.2で使用される疑似乱数関数。
    マスター秘密から暗号化鍵、MAC鍵、初期化ベクトルを導出する。
    
    アルゴリズム概要:
    1. P_hash関数をHMAC-SHA256で実装
    2. A(0) = seed, A(i) = HMAC(secret, A(i-1))
    3. 出力 = HMAC(secret, A(1) + seed) || HMAC(secret, A(2) + seed) || ...
    4. 必要な長さまで繰り返し、最終的に切り詰める
    
    処理レベル: derived_key = tls_prf_sha256(secret, label, seed, length)
    
    Args:
        secret: マスター秘密またはその他の秘密値
        label: 鍵導出の目的を示すラベル（例："key expansion"）
        seed: クライアント・サーバーランダム値の結合等
        length: 導出したい鍵材料の長さ（バイト）
    
    Returns:
        指定された長さの鍵材料
        
    Note:
        RFC 5246 Section 5で定義
        TLS 1.0/1.1のMD5+SHA1 PRFを改良してSHA256単体に変更
        TLS 1.3では更にHKDFベースの設計に進化
    """
    def p_hash(secret: bytes, seed: bytes, length: int) -> bytes:
        """P_hash function using HMAC-SHA256"""
        result = b""
        a = seed  # A(0) = seed
        
        while len(result) < length:
            a = hmac.new(secret, a, hashlib.sha256).digest()  # A(i) = HMAC(secret, A(i-1))
            result += hmac.new(secret, a + seed, hashlib.sha256).digest()
        
        return result[:length]
    
    return p_hash(secret, label + seed, length)

def demonstrate_nist_kdfs() -> None:
    """
    NIST SP 800-135 Rev.1のKDF使用例
    
    各KDFの動作確認とパラメータの影響を実証する。
    実際のプロトコル実装時の参考として使用可能。
    """
    print("=== NIST SP 800-135 Rev.1 KDF実装例 ===\n")
    
    # 共通のテストデータ
    shared_secret = b"shared_secret_example_1234567890"
    key_length = 32
    
    # ANS X9.42 KDF
    print("1. ANS X9.42-2001 KDF:")
    algorithm_id = b"aes256-cbc"
    x942_key = ansi_x942_kdf(shared_secret, key_length, algorithm_id)
    print(f"   shared_secret: {shared_secret.hex()}")
    print(f"   algorithm_id: {algorithm_id}")
    print(f"   derived_key:  {x942_key.hex()}\n")
    
    # ANS X9.63 KDF
    print("2. ANS X9.63-2001 KDF:")
    shared_info = b"context_info"
    x963_key = ansi_x963_kdf(shared_secret, key_length, shared_info)
    print(f"   shared_secret: {shared_secret.hex()}")
    print(f"   shared_info:   {shared_info}")
    print(f"   derived_key:   {x963_key.hex()}\n")
    
    # SSH KDF
    print("3. SSH KDF:")
    hash_value = hashlib.sha256(b"ssh_exchange_hash").digest()
    session_id = hashlib.sha256(b"ssh_session_identifier").digest()
    key_type = ord('A')  # 'A' for initial IV client to server
    ssh_key = ssh_kdf(shared_secret, hash_value, session_id, key_type, key_length)
    print(f"   shared_secret: {shared_secret.hex()}")
    print(f"   key_type:      {chr(key_type)} ({key_type})")
    print(f"   derived_key:   {ssh_key.hex()}\n")
    
    # SRTP KDF
    print("4. SRTP KDF:")
    master_key = b"master_key_16bit" * 2  # 32バイト
    master_salt = b"master_salt_14bt"     # 14バイト
    index = 12345
    label = 0x00  # encryption key
    srtp_key = srtp_kdf(master_key[:16], master_salt, index, label, key_length)
    print(f"   master_key:  {master_key[:16].hex()}")
    print(f"   master_salt: {master_salt.hex()}")
    print(f"   index:       {index}")
    print(f"   label:       0x{label:02x}")
    print(f"   derived_key: {srtp_key.hex()}\n")
    
    # SNMP KDF
    print("5. SNMP KDF:")
    password = b"snmp_password"
    engine_id = b"engine_id_example"
    snmp_key = snmp_kdf(password, engine_id)
    print(f"   password:    {password}")
    print(f"   engine_id:   {engine_id}")
    print(f"   shared_key:  {snmp_key.hex()}\n")
    
    # TPM KDF
    print("6. TPM KDF:")
    auth = b"authorization_value_16b"
    nonce_even = b"nonce_even_8"
    nonce_odd = b"nonce_odd_8b"
    tpm_key = tpm_kdf(auth, nonce_even, nonce_odd)
    print(f"   auth:        {auth}")
    print(f"   nonce_even:  {nonce_even}")
    print(f"   nonce_odd:   {nonce_odd}")
    print(f"   skey:        {tpm_key.hex()}\n")
    
    # IKEv1 SKEYID
    print("7. IKEv1 SKEYID (Pre-shared key):")
    psk = b"pre_shared_key_example"
    ni = b"initiator_nonce_16b"
    nr = b"responder_nonce_16b"
    skeyid = ikev1_skeyid_psk(psk, ni, nr)
    print(f"   psk:         {psk}")
    print(f"   ni:          {ni}")
    print(f"   nr:          {nr}")
    print(f"   skeyid:      {skeyid.hex()}\n")
    
    # IKEv2 SKEYSEED
    print("8. IKEv2 SKEYSEED:")
    ni = b"initiator_nonce_v2"
    nr = b"responder_nonce_v2"
    dh_secret = b"diffie_hellman_shared_secret_example"
    skeyseed = ikev2_skeyseed(ni, nr, dh_secret)
    print(f"   ni:          {ni}")
    print(f"   nr:          {nr}")
    print(f"   dh_secret:   {dh_secret}")
    print(f"   skeyseed:    {skeyseed.hex()}\n")
    
    # TLS PRF
    print("9. TLS 1.2 PRF (P_SHA256):")
    secret = b"master_secret_example"
    label = b"key expansion"
    seed = b"client_random" + b"server_random"
    tls_key = tls_prf_sha256(secret, label, seed, key_length)
    print(f"   secret:      {secret}")
    print(f"   label:       {label}")
    print(f"   seed:        {seed}")
    print(f"   derived_key: {tls_key.hex()}")

if __name__ == "__main__":
    demonstrate_nist_kdfs()
