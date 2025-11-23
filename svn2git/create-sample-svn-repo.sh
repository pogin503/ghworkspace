#!/bin/bash
# create-sample-svn-repo.sh

# スクリプトのディレクトリを取得
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 作業ディレクトリの設定（スクリプトと同じ場所に作成）
WORK_DIR="$SCRIPT_DIR/work"
REPO_NAME="sample-repo"
REPO_PATH="$WORK_DIR/$REPO_NAME"
CHECKOUT_PATH="$WORK_DIR/trunk"

# クリーンアップ
rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

echo "=== SVNサンプルリポジトリを作成します ==="
echo "作業ディレクトリ: $WORK_DIR"
echo ""

# 1. SVNリポジトリの作成
svnadmin create "$REPO_PATH"
echo "✓ リポジトリ作成: $REPO_PATH"

# 2. 基本構造の作成
svn mkdir -m "Initial structure" \
  file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches \
  file://"$REPO_PATH"/tags

echo "✓ trunk/branches/tags 構造を作成"

# 3. trunkでの開発
svn checkout file://"$REPO_PATH"/trunk "$CHECKOUT_PATH"
cd "$CHECKOUT_PATH"

# 初期コミット
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
svn commit -m "Initial commit: Add README and main.py"
echo "✓ trunk: 初期コミット完了"

# trunk での追加開発
echo "version = '1.0.0'" > version.py
svn add version.py
svn commit -m "Add version file"

cat >> main.py << 'EOF'

def greet(name):
    return f"Hello, {name}!"
EOF
svn commit -m "Add greet function"

echo "✓ trunk: 追加コミット完了"

cd "$WORK_DIR"

# 4. 古いブランチの作成（2020年のプロジェクト）
svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/proj1 \
  -m "Create proj1 branch (2020 project)"

svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/proj2 \
  -m "Create proj2 branch (2020 project)"

echo "✓ 古いブランチ作成: proj1, proj2"

# proj1での開発
svn checkout file://"$REPO_PATH"/branches/proj1 "proj1"
cd "proj1"
echo "# Project 1 specific changes" >> README.md
svn commit -m "proj1: Update README"
cd "$WORK_DIR"
rm -rf "proj1"

# 5. old_branch ディレクトリの作成とブランチ移動
svn mkdir -m "Create old_branch directory for archiving" \
  file://"$REPO_PATH"/branches/old_branch

svn move file://"$REPO_PATH"/branches/proj1 \
  file://"$REPO_PATH"/branches/old_branch/proj1 \
  -m "Move proj1 to old_branch (archived)"

svn move file://"$REPO_PATH"/branches/proj2 \
  file://"$REPO_PATH"/branches/old_branch/proj2 \
  -m "Move proj2 to old_branch (archived)"

echo "✓ proj1, proj2 を old_branch へ移動"

# 2020年の追加プロジェクト
svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/old_branch/proj_2020_1 \
  -m "Create proj_2020_1 (archived project)"

svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/old_branch/proj_2020_2 \
  -m "Create proj_2020_2 (archived project)"

echo "✓ 古いプロジェクト作成: proj_2020_1, proj_2020_2"

# 6. 現行のアクティブブランチ作成
cd "$CHECKOUT_PATH"
svn update

# trunkでさらに開発
cat >> main.py << 'EOF'

def calculate(a, b):
    return a + b
EOF
svn commit -m "Add calculate function"

# 現行ブランチの作成
cd "$WORK_DIR"
svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/proj3 \
  -m "Create proj3 branch (active)"

svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/branches/proj4 \
  -m "Create proj4 branch (active)"

echo "✓ 現行ブランチ作成: proj3, proj4"

# proj3での開発
svn checkout file://"$REPO_PATH"/branches/proj3 "proj3"
cd "proj3"
cat > feature3.py << 'EOF'
def feature3():
    print("Feature 3")
EOF
svn add feature3.py
svn commit -m "proj3: Add feature3"

echo "# Project 3" > PROJECT3.md
svn add PROJECT3.md
svn commit -m "proj3: Add project documentation"
cd "$WORK_DIR"
rm -rf "proj3"

# proj4での開発
svn checkout file://"$REPO_PATH"/branches/proj4 "proj4"
cd "proj4"
cat > feature4.py << 'EOF'
def feature4():
    print("Feature 4")
EOF
svn add feature4.py
svn commit -m "proj4: Add feature4"
cd "$WORK_DIR"
rm -rf "proj4"

# 7. タグの作成
svn copy file://"$REPO_PATH"/trunk \
  file://"$REPO_PATH"/tags/v1.0.0 \
  -m "Release v1.0.0"

svn copy file://"$REPO_PATH"/branches/proj3 \
  file://"$REPO_PATH"/tags/proj3-v1.0 \
  -m "Release proj3 v1.0"

echo "✓ タグ作成: v1.0.0, proj3-v1.0"

# 8. trunkでさらに開発
cd "$CHECKOUT_PATH"
svn update
echo "version = '1.1.0'" > version.py
svn commit -m "Bump version to 1.1.0"
echo "✓ trunk: バージョンアップ"

# クリーンアップ
cd "$WORK_DIR"
rm -rf "$CHECKOUT_PATH"

echo ""
echo "=== サンプルSVNリポジトリ作成完了 ==="
echo "リポジトリパス: file://$REPO_PATH"
echo ""
echo "構造確認:"
svn list -R file://"$REPO_PATH" | head -30
echo ""
echo "=== 使用方法 ==="
echo "1. リポジトリ構造の確認:"
echo "   svn list -R file://$REPO_PATH"
echo ""
echo "2. コミット履歴の確認:"
echo "   svn log file://$REPO_PATH"
echo ""
echo "3. チェックアウト:"
echo "   svn checkout file://$REPO_PATH/trunk"
echo ""
echo "4. Git移行テスト:"
echo "   cd $SCRIPT_DIR"
echo "   git svn clone file://$REPO_PATH --authors-file=authors.txt -T trunk -b branches -t tags"
