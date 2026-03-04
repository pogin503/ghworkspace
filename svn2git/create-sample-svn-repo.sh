#!/bin/bash
# create-sample-svn-repo.sh

# スクリプトのディレクトリを取得
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 作業ディレクトリの設定（スクリプトと同じ場所に作成）
WORK_DIR="$SCRIPT_DIR/work"
REPO_NAME="sample-repo"
REPO_PATH="$WORK_DIR/$REPO_NAME"
TRUNK_CHECKOUT="$WORK_DIR/trunk"

# クリーンアップ
rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

echo "=== SVNサンプルリポジトリを作成します ==="
echo "作業ディレクトリ: $WORK_DIR"
echo ""

# 1. SVNリポジトリの作成
svnadmin create "$REPO_PATH"
echo "✓ リポジトリ作成: $REPO_PATH"

# SVNの認証設定（パスワード不要にする）
cat > "$REPO_PATH/conf/svnserve.conf" << 'EOF'
[general]
anon-access = write
auth-access = write
EOF

# 2. 基本構造の作成
svn mkdir -m "Initial structure" --username admin \
  file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches \
  file://"$REPO_PATH"/tags

echo "✓ trunk/branches/tags 構造を作成"

# 3. trunkでの開発（複数ユーザー）
svn checkout file://"$REPO_PATH"/trunk "$TRUNK_CHECKOUT"
cd "$TRUNK_CHECKOUT"

# 初期コミット（ユーザー: alice）
cat > README.md << 'EOF'
# Sample Project

これはSVN→Git移行テスト用のサンプルプロジェクトです。
EOF

cat > main.py << 'EOF'
def main():
    print("Hello, World!")

if __name__ == "__main__":
    main()
EOF

svn add README.md main.py
svn commit -m "Initial commit: Add README and main.py" --username alice
echo "✓ trunk: 初期コミット完了 (alice)"

# trunk での追加開発（ユーザー: bob）
echo "version = '1.0.0'" > version.py
svn add version.py
svn commit -m "Add version file" --username bob
echo "✓ trunk: バージョンファイル追加 (bob)"

# trunk での追加開発（ユーザー: charlie）
cat >> main.py << 'EOF'

def greet(name):
    return f"Hello, {name}!"
EOF
svn commit -m "Add greet function" --username charlie
echo "✓ trunk: greet関数追加 (charlie)"

cd "$WORK_DIR"

# 4. 古いブランチの作成（2020年のプロジェクト）
svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/proj1 \
  -m "Create proj1 branch (2020 project)" --username alice

svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/proj2 \
  -m "Create proj2 branch (2020 project)" --username bob

echo "✓ 古いブランチ作成: proj1 (alice), proj2 (bob)"

# proj1での開発（ユーザー: david）
svn checkout file://"$REPO_PATH"/branches/proj1 "$WORK_DIR/proj1"
cd "$WORK_DIR/proj1"
echo "# Project 1 specific changes" >> README.md
svn commit -m "proj1: Update README" --username david
echo "✓ proj1: README更新 (david)"
cd "$WORK_DIR"
rm -rf "$WORK_DIR/proj1"

# 5. old_branch ディレクトリの作成とブランチ移動
svn mkdir -m "Create old_branch directory for archiving" --username admin \
  file://"$REPO_PATH"/branches/old_branch

svn move file://"$REPO_PATH"/branches/proj1 \
  file://"$REPO_PATH"/branches/old_branch/proj1 \
  -m "Move proj1 to old_branch (archived)" --username admin

svn move file://"$REPO_PATH"/branches/proj2 \
  file://"$REPO_PATH"/branches/old_branch/proj2 \
  -m "Move proj2 to old_branch (archived)" --username admin

echo "✓ proj1, proj2 を old_branch へ移動 (admin)"

# 2020年の追加プロジェクト
svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/old_branch/proj_2020_1 \
  -m "Create proj_2020_1 (archived project)" --username alice

svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/old_branch/proj_2020_2 \
  -m "Create proj_2020_2 (archived project)" --username bob

echo "✓ 古いプロジェクト作成: proj_2020_1 (alice), proj_2020_2 (bob)"

# 6. 現行のアクティブブランチ作成
cd "$TRUNK_CHECKOUT"
svn update

# trunkでさらに開発（ユーザー: alice）
cat >> main.py << 'EOF'

def calculate(a, b):
    return a + b
EOF
svn commit -m "Add calculate function" --username alice
echo "✓ trunk: calculate関数追加 (alice)"

# 現行ブランチの作成
cd "$WORK_DIR"
svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/proj3 \
  -m "Create proj3 branch (active)" --username charlie

svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/proj4 \
  -m "Create proj4 branch (active)" --username david

echo "✓ 現行ブランチ作成: proj3 (charlie), proj4 (david)"

# proj3での開発（ユーザー: charlie）
svn checkout file://"$REPO_PATH"/branches/proj3 "$WORK_DIR/proj3"
cd "$WORK_DIR/proj3"
cat > feature3.py << 'EOF'
def feature3():
    print("Feature 3")
EOF
svn add feature3.py
svn commit -m "proj3: Add feature3" --username charlie

echo "# Project 3" > PROJECT3.md
svn add PROJECT3.md
svn commit -m "proj3: Add project documentation" --username charlie
echo "✓ proj3: 機能追加 (charlie)"
cd "$WORK_DIR"
rm -rf "$WORK_DIR/proj3"

# proj4での開発（ユーザー: david）
svn checkout file://"$REPO_PATH"/branches/proj4 "$WORK_DIR/proj4"
cd "$WORK_DIR/proj4"
cat > feature4.py << 'EOF'
def feature4():
    print("Feature 4")
EOF
svn add feature4.py
svn commit -m "proj4: Add feature4" --username david
echo "✓ proj4: 機能追加 (david)"
cd "$WORK_DIR"
rm -rf "$WORK_DIR/proj4"

# 7. タグの作成
svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/tags/v1.0.0 \
  -m "Release v1.0.0" --username admin

svn copy file://"$REPO_PATH"/branches/proj3 \
  file://"$REPO_PATH"/tags/proj3-v1.0 \
  -m "Release proj3 v1.0" --username charlie

echo "✓ タグ作成: v1.0.0 (admin), proj3-v1.0 (charlie)"

# 8. trunkでさらに開発（ユーザー: bob）
cd "$TRUNK_CHECKOUT"
svn update
echo "version = '1.1.0'" > version.py
svn commit -m "Bump version to 1.1.0" --username bob
echo "✓ trunk: バージョンアップ (bob)"

# クリーンアップ
cd "$WORK_DIR"
rm -rf "$TRUNK_CHECKOUT"

# authorsファイルの作成
cat > "$WORK_DIR/authors.txt" << 'EOF'
admin = Admin User <admin@example.com>
alice = Alice Developer <alice@example.com>
bob = Bob Engineer <bob@example.com>
charlie = Charlie Programmer <charlie@example.com>
david = David Coder <david@example.com>
EOF

echo ""
echo "=== サンプルSVNリポジトリ作成完了 ==="
echo "リポジトリパス: file://$REPO_PATH"
echo ""
echo "✓ authorsファイル作成: $WORK_DIR/authors.txt"
echo ""
echo "コミットユーザー一覧:"
svn log -q file://"$REPO_PATH" | grep "^r" | awk '{print $3}' | sort -u
echo ""
echo "構造確認:"
svn list -R file://"$REPO_PATH" | head -30
echo ""
echo "=== 使用方法 ==="
echo "1. リポジトリ構造の確認:"
echo "   svn list -R file://$REPO_PATH"
echo ""
echo "2. コミット履歴とユーザーの確認:"
echo "   svn log file://$REPO_PATH"
echo ""
echo "3. Git移行テスト:"
echo "   cd $SCRIPT_DIR"
echo "   git svn clone file://$REPO_PATH --authors-file=$WORK_DIR/authors.txt -T trunk -b branches -t tags"