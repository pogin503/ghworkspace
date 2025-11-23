import hashlib
import hmac
import os
from typing import Optional, Union
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
from cryptography.hazmat.primitives.kdf.scrypt import Scrypt
from cryptography.hazmat.primitives.kdf.hkdf import HKDF
from cryptography.hazmat.primitives import hashes

def pbkdf2_derive(
    password: bytes, 
    salt: bytes, 
    iterations: int = 100000, 
    key_length: int = 32
) -> bytes:
    """
    PBKDF2による鍵導出（純粋関数）
    
    PBKDF2（Password-Based Key Derivation Function 2）は最も広く使用されている
    パスワードベースの鍵導出関数。パスワードから暗号学的に安全な鍵を生成する。
    
    アルゴリズム概要:
    1. PRF（疑似乱数関数）としてHMACを使用
    2. パスワード + ソルト + 反復回数でセキュリティを確保
    3. Ti = F(password, salt, iterations, i) = U1 ⊕ U2 ⊕ ... ⊕ Uc
    4. 各Ujは前の結果にPRFを適用して計算
    
    セキュリティ特徴:
    - ソルトにより辞書攻撃・レインボーテーブル攻撃を防止
    - 反復により総当たり攻撃のコストを増大
    - 計算時間とメモリ使用量のトレードオフを調整可能
    
    処理レベル: derived_key = pbkdf2_derive(password, salt, iterations, key_length)
    
    Args:
        password: 入力パスワード（バイト列）
        salt: ソルト値（ランダムなバイト列、最低8バイト推奨）
        iterations: 反復回数（デフォルト100,000、推奨600,000以上）
        key_length: 出力鍵の長さ（バイト、デフォルト32）
    
    Returns:
        指定された長さの導出鍵
        
    Note:
        RFC 2898, PKCS #5で標準化
        NIST SP 800-132で承認されたパスワードベース鍵導出関数
        最低100,000回の反復を推奨（2023年時点では600,000回以上）
        FIPS 140-3環境では実装ガイダンス要確認
    """
    # PBKDF2インスタンス作成
    kdf = PBKDF2HMAC(
        algorithm=hashes.SHA256(),
        length=key_length,
        salt=salt,
        iterations=iterations
    )
    
    # 鍵導出
    derived_key = kdf.derive(password)
    return derived_key

def scrypt_derive(
    password: bytes, 
    salt: bytes, 
    n: int = 2**14, 
    r: int = 8, 
    p: int = 1, 
    key_length: int = 32
) -> bytes:
    """
    scryptによる鍵導出（純粋関数）
    
    scryptはメモリハード関数として設計された鍵導出関数。
    PBKDF2よりもASIC（専用チップ）による攻撃に対して耐性が高い。
    
    アルゴリズム概要:
    1. PBKDF2でメモリブロックBを生成
    2. ROMixでメモリ集約的な処理を実行
    3. Salsa20/8暗号を使用した高速混合関数
    4. N個のブロックを生成・保存し、ランダムアクセスで混合
    
    パラメータ説明:
    - N: CPUコスト（2の冪乗、メモリ使用量に比例）
    - r: ブロックサイズ（並列性とメモリ使用量に影響）
    - p: 並列度（CPUコア数に応じて調整）
    
    セキュリティ特徴:
    - メモリ使用量 = 128 * N * r バイト
    - ASIC攻撃コストがメモリコストに比例
    - GPU攻撃も効果的でない（メモリ帯域幅がボトルネック）
    
    処理レベル: derived_key = scrypt_derive(password, salt, n, r, p, key_length)
    
    Args:
        password: 入力パスワード（バイト列）
        salt: ソルト値（ランダムなバイト列、最低8バイト推奨）
        n: CPUコスト（2の冪乗、デフォルト16384）
        r: ブロックサイズ（デフォルト8）
        p: 並列度（デフォルト1）
        key_length: 出力鍵の長さ（バイト、デフォルト32）
    
    Returns:
        指定された長さの導出鍵
        
    Note:
        RFC 7914で標準化
        Litecoin等の暗号通貨で採用
        推奨パラメータ: N=32768, r=8, p=1（約32MB使用）
    """
    # scryptインスタンス作成
    kdf = Scrypt(
        length=key_length,
        salt=salt,
        n=n,
        r=r,
        p=p
    )
    
    # 鍵導出
    derived_key = kdf.derive(password)
    return derived_key

def hkdf_derive(
    input_key: bytes, 
    salt: Optional[bytes] = None, 
    info: Optional[bytes] = None, 
    key_length: int = 32
) -> bytes:
    """
    HKDFによる鍵導出（純粋関数）
    
    HKDF（HMAC-based Key Derivation Function）はExtract-then-Expandパラダイムを
    実装した鍵導出関数。既存の鍵材料から複数の暗号学的に独立した鍵を生成する。
    
    アルゴリズム概要:
    1. Extract段階: PRK = HKDF-Extract(salt, IKM)
       - 入力鍵材料（IKM）から疑似乱数鍵（PRK）を抽出
       - エントロピーの均等分散と強化
    2. Expand段階: OKM = HKDF-Expand(PRK, info, L)
       - PRKから任意長の出力鍵材料（OKM）を展開
       - コンテキスト情報（info）で異なる用途の鍵を区別
    
    用途と特徴:
    - 鍵交換プロトコル（TLS 1.3、Signal Protocol等）
    - 単一の共有秘密から複数の専用鍵を導出
    - パスワードベースではない（高エントロピー前提）
    - 計算コストが低く、高速処理が可能
    
    処理レベル: derived_key = hkdf_derive(input_key, salt, info, key_length)
    
    Args:
        input_key: 入力鍵材料（高エントロピーなバイト列）
        salt: ソルト値（オプション、Noneの場合は零バイト列使用）
        info: コンテキスト情報（オプション、用途別の鍵生成に使用）
        key_length: 出力鍵の長さ（バイト、デフォルト32）
    
    Returns:
        指定された長さの導出鍵
        
    Note:
        RFC 5869で標準化
        TLS 1.3、Noise Protocol、Signal Protocolで採用
        暗号学的分析により安全性が証明済み
    """
    # HKDFインスタンス作成
    kdf = HKDF(
        algorithm=hashes.SHA256(),
        length=key_length,
        salt=salt,
        info=info
    )
    
    # 鍵導出
    derived_key = kdf.derive(input_key)
    return derived_key

def pbkdf2_example() -> bytes:
    """PBKDF2の使用例（固定値でのデモンストレーション）"""
    password = b"my_password"
    salt = b"fixed_salt_16byt"  # 固定ソルト（実環境では必ずランダム使用）
    
    derived_key = pbkdf2_derive(password, salt, iterations=100000, key_length=32)
    
    print(f"PBKDF2 - 元パスワード: {password}")
    print(f"PBKDF2 - ソルト: {salt.hex()}")
    print(f"PBKDF2 - 導出鍵: {derived_key.hex()}")
    return derived_key

def scrypt_example() -> bytes:
    """scryptの使用例（固定値でのデモンストレーション）"""
    password = b"my_password"
    salt = b"fixed_salt_16byt"  # 固定ソルト（実環境では必ずランダム使用）
    
    n = 2**14  # 16384
    derived_key = scrypt_derive(password, salt, n=n, r=8, p=1, key_length=32)
    
    print(f"\nscrypt - 元パスワード: {password}")
    print(f"scrypt - ソルト: {salt.hex()}")
    print(f"scrypt - メモリ使用量: {128 * n * 8 // 1024 // 1024} MB")
    print(f"scrypt - 導出鍵: {derived_key.hex()}")
    return derived_key

def hkdf_example() -> bytes:
    """HKDFの使用例（固定値でのデモンストレーション）"""
    # 固定の入力鍵材料（実環境では鍵交換等から取得）
    input_key = bytes.fromhex("0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20")
    salt = b"fixed_salt_16byt"
    info = b"application context"
    
    derived_key = hkdf_derive(input_key, salt=salt, info=info, key_length=32)
    
    print(f"\nHKDF - 入力鍵材料: {input_key.hex()}")
    print(f"HKDF - ソルト: {salt.hex()}")
    print(f"HKDF - コンテキスト情報: {info}")
    print(f"HKDF - 導出鍵: {derived_key.hex()}")
    return derived_key

def simple_kdf_function(
    password: bytes, 
    salt: bytes, 
    iterations: int, 
    key_length: int
) -> bytes:
    """
    シンプルなKDF関数の例
    
    Python標準ライブラリのhashlib.pbkdf2_hmacを使用した
    最小限のPBKDF2実装例。理解しやすさを重視。
    
    アルゴリズム概要:
    1. PBKDF2-HMAC-SHA256を使用
    2. パスワード、ソルト、反復回数、出力長を指定
    3. 内部的にHMAC-SHA256を反復適用
    
    処理レベル: derived_key = simple_kdf_function(password, salt, iterations, key_length)
    
    Args:
        password: 入力パスワード（バイト列）
        salt: ソルト値（ランダムなバイト列）
        iterations: 反復回数（セキュリティレベルを決定）
        key_length: 出力鍵の長さ（バイト）
    
    Returns:
        指定された長さの導出鍵
        
    Note:
        プロダクション環境では適切なライブラリ使用を推奨
        反復回数は最低100,000回、推奨600,000回以上
    """
    # PBKDF2の簡単な実装
    derived_key = hashlib.pbkdf2_hmac(
        'sha256',       # ハッシュ関数
        password,       # パスワード
        salt,          # ソルト
        iterations,    # 反復回数
        key_length     # 出力鍵長
    )
    return derived_key

def demonstrate_kdf_usage() -> None:
    """
    KDFの使用デモンストレーション
    
    主要な鍵導出関数の動作原理とパラメータの影響を実証する。
    セキュリティ要件に応じた適切なKDF選択の指針を提供。
    
    デモ内容:
    1. 各KDFの基本動作確認
    2. パラメータ変更による出力の違い
    3. セキュリティ強度の比較
    4. 実用的な使用例の紹介
    
    KDF選択の指針:
    - パスワード保存: PBKDF2, scrypt, Argon2
    - 鍵交換後の鍵展開: HKDF
    - 高セキュリティ要求: Argon2 > scrypt > PBKDF2
    - レガシー互換性: PBKDF2
    """
    print("=== KDF処理レベルデモンストレーション ===")
    
    # 各KDFの実行例（固定値使用）
    pbkdf2_key = pbkdf2_example()
    scrypt_key = scrypt_example()
    hkdf_key = hkdf_example()
    
    # 純粋関数の使用例
    print("\n=== 純粋関数の使用例 ===")
    password = b"test_password"
    salt = b"test_salt_16byte"
    
    # PBKDF2の可変パラメータ例
    pbkdf2_key1 = pbkdf2_derive(password, salt, iterations=10000, key_length=32)
    pbkdf2_key2 = pbkdf2_derive(password, salt, iterations=100000, key_length=32)
    print(f"PBKDF2 (10K反復):  {pbkdf2_key1.hex()}")
    print(f"PBKDF2 (100K反復): {pbkdf2_key2.hex()}")
    
    # scryptの可変パラメータ例
    scrypt_key1 = scrypt_derive(password, salt, n=2**12, r=8, p=1, key_length=32)  # 軽量
    scrypt_key2 = scrypt_derive(password, salt, n=2**14, r=8, p=1, key_length=32)  # 標準
    print(f"scrypt (N=4096):   {scrypt_key1.hex()}")
    print(f"scrypt (N=16384):  {scrypt_key2.hex()}")
    
    # HKDFの可変パラメータ例
    input_key = bytes.fromhex("0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20")
    hkdf_key1 = hkdf_derive(input_key, salt=salt, info=b"encryption", key_length=32)
    hkdf_key2 = hkdf_derive(input_key, salt=salt, info=b"authentication", key_length=32)
    print(f"HKDF (暗号化用):   {hkdf_key1.hex()}")
    print(f"HKDF (認証用):     {hkdf_key2.hex()}")
    
    # シンプルな関数呼び出し例（標準ライブラリ使用）
    print("\n=== 標準ライブラリ使用例 ===")
    simple_key = simple_kdf_function(password, salt, iterations=10000, key_length=32)
    print(f"simple_kdf_function の結果: {simple_key.hex()}")
    
    # パラメータの影響を確認
    print("\n=== パラメータの影響確認 ===")
    
    # 反復回数を変更
    key1 = simple_kdf_function(password, salt, 1000, 32)
    key2 = simple_kdf_function(password, salt, 10000, 32)
    
    print(f"反復1000回:  {key1.hex()}")
    print(f"反復10000回: {key2.hex()}")
    print(f"鍵が異なる: {key1 != key2}")
    
    # ソルトを変更
    salt2 = b"different_salt16"
    key3 = simple_kdf_function(password, salt2, 10000, 32)
    
    print(f"元のソルト: {simple_key.hex()}")
    print(f"別のソルト: {key3.hex()}")
    print(f"鍵が異なる: {simple_key != key3}")
    
    # セキュリティ強度の比較
    print("\n=== セキュリティ強度比較 ===")
    print("PBKDF2:")
    print("  - 時間複雑度: O(iterations)")
    print("  - 空間複雑度: O(1)")
    print("  - ASIC耐性: 低")
    print("  - 適用例: パスワード保存、レガシーシステム")
    
    print("scrypt:")
    print("  - 時間複雑度: O(N * r * p)")
    print("  - 空間複雑度: O(N * r)")
    print("  - ASIC耐性: 中")
    print("  - 適用例: 暗号通貨、高セキュリティ要求")
    
    print("HKDF:")
    print("  - 時間複雑度: O(output_length)")
    print("  - 空間複雑度: O(1)")
    print("  - ASIC耐性: N/A（高エントロピー前提）")
    print("  - 適用例: 鍵交換、プロトコル設計")
    
    print("\n=== 実環境での注意事項 ===")
    print("⚠️  固定ソルトは教育目的のみ。実環境では必ずランダムソルトを使用")
    print("⚠️  反復回数は2025年時点で最低600,000回を推奨")
    print("⚠️  セキュリティクリティカルな用途では最新のArgon2使用を検討")

# 実環境での正しい使用例（コメントアウト版）
def production_example_commented():
    """
    実環境での正しいKDF使用例（参考）
    
    実際のプロダクション環境では以下のようにランダムソルトと
    高セキュリティパラメータを使用する：
    
    import secrets
    
    # ランダムソルト生成
    salt = secrets.token_bytes(16)
    
    # 高セキュリティパラメータでの鍵導出
    password = b"user_password"
    key = pbkdf2_derive(password, salt, iterations=600000, key_length=32)
    
    # ソルトと鍵を安全に保存
    # store_securely(salt, key)
    """
    pass
    print("  - ASIC耐性: N/A（高エントロピー前提）")
    print("  - 適用例: 鍵交換、プロトコル設計")

if __name__ == "__main__":
    demonstrate_kdf_usage()
