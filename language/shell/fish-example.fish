#!/usr/local/bin/fish
# man set

echo hello world | wc

# grep fish < /etc/shells > ~/output.txt ^ ~/errors.txt

# =====================================================
# Variables

echo My home directory is $HOME
#=> My home directory is /home/username
cd ~/
echo "My current directory is $PWD"
#=>My current directory is /home/username
echo 'My current directory is $PWD'
#=> My current directory is $PWD

set name 'Mister Noodle'
echo $name
#=> Mister Noodle

# =====================================================
# Exit Status
false
echo $status
#=> 1

# =====================================================
# Export
set -x MyVariable SomeValue
env | grep MyVariable
# MyVariablem=SomeValue

## Remove Variable
set -e MyVariable
env | grep MyVariable
#=> (no output)

# =====================================================
# Command Substitutions
echo In (pwd), running (uname)
# In /home/tutorial, running FreeBSD

# In Bash
# echo In `pwd`, running `uname`

# =====================================================
# Conditionals (If, Else, Switch)
if grep fish /etc/shells
  echo Found fish
else if grep bash /etc/shells
  echo Found bash
else
  echo Got nothing
end

switch (uname)
case Linux
  echo Hi Tux!
case Darwin
  echo Hi Hexley!
case FreeBSD NetBSD DragonFly
  echo Hi Beastie!
case '*'
  echo Hi, stranger!
end


# =====================================================
# Loops

# while true
#   echo "Loop forever"
# end
# Loop forever
# Loop forever
# Loop forever
# ...

# For loops can be used to iterate over a list. For example, a list of files:

for file in *.txt
  cp $file $file.bak
end

# Iterating over a list of numbers can be done with seq:

for x in (seq 5)
  touch file_$x.txt
end

# =====================================================
# List

count $PATH
#=> 30

echo $PATH
#=> /usr/bin /bin /usr/sbin /sbin /usr/local/bin
echo $PATH[1]
#=> /usr/bin
> echo $PATH[-1]
#=> /usr/local/bin

echo $PATH[1..2]
#=> /usr/bin /bin
echo $PATH[-1..2]
#=> /usr/local/bin /sbin /usr/sbin /bin

for val in $PATH
  echo "entry: $val"
end
#=> entry: /usr/bin/
#=> entry: /bin
#=> entry: /usr/sbin
#=> entry: /sbin
#=> entry: /usr/local/bin

set -l a 1 2 3
set -l 1 a b c
echo $a$1
#=> 1a 2a 3a 1b 2b 3b 1c 2c 3c

echo $a" banana"
#=> 1 banana 2 banana 3 banana
echo "$a banana"
#=> 1 2 3 banana

# =====================================================
# Tips

# Set environment path
# --export -x (Export variable to subprocess)
set -x PATH /usr/local/bin /usr/sbin $PATH

# --universal -U (Share variable persistently across sessions)
set -U fish_user_paths /usr/local/bin $fish_user_paths

# Set prompt
function fish_prompt
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal
  echo -n ' > '
end

# check file path
if test -e ~/.foobar
  echo "file exists"
end

# check directory path
if test -d ~/.hello
  echo "directory exists"
end

# set rbenv
which rbenv > /dev/null 2>&1
if [ $status = 0 ] # コマンドが存在すれば
  # status --is-interactive; and . (rbenv init -|psub)
  rbenv init - | source
else
    echo "can't load rbenv."
end
