#!/bin/bash

# 1. 複雑な構造に対応した設定ファイルを作成
cat > svn-structure.txt << 'EOF'
branches = branches/*:refs/remotes/svn/branches/*
branches = branches/old_branch/*:refs/remotes/svn/old_branches/*
tags = tags/*:refs/remotes/svn/tags/*
trunk = trunk:refs/remotes/svn/trunk
EOF

# 2. git-svnでクローン
git svn clone http://svn-server/repo \
  --authors-file=authors.txt \
  --no-metadata \
  --prefix=svn/ \
  -T trunk \
  -b branches \
  -b branches/old_branch \
  -t tags \
  my-git-repo

cd my-git-repo

# 3. SVNブランチをGitブランチに変換
for branch in $(git branch -r | grep "svn/branches/" | sed 's|svn/branches/||'); do
  git branch "$branch" "remotes/svn/branches/$branch"
done

# old_branchは別の命名規則で
for branch in $(git branch -r | grep "svn/old_branches/" | sed 's|svn/old_branches/||'); do
  git branch "archive/$branch" "remotes/svn/old_branches/$branch"
done

# 4. SVNタグをGitタグに変換
for tag in $(git branch -r | grep "svn/tags/" | sed 's|svn/tags/||'); do
  git tag -a "$tag" -m "Converted from SVN tag" "remotes/svn/tags/$tag"
done

# 5. リモートブランチを削除
git branch -r -d trunk
git branch -r -D $(git branch -r | grep "svn/")