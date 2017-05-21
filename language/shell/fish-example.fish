#!/usr/local/bin/fish
# man set

echo hello world | wc

# grep fish < /etc/shells > ~/output.txt ^ ~/errors.txt

# =====================================================
# Variables
echo My home directory is $HOME
#=> My home directory is /home/tutorial
echo "My current directory is $PWD"
#=>My current directory is /home/tutorial
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
# Tips

# Set Path
set PATH /usr/local/bin /usr/sbin $PATH

set -U fish_user_paths /usr/local/bin $fish_user_paths

# Set prompt
function fish_prompt
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal
  echo -n ' > '
end

if test -e ~/.foobar
  echo "file exists"
end

if test -d ~/.hello
  echo "directory exists"
end

# set rbenv
which rbenv > /dev/null 2>&1
if [ $status = 0 ] # コマンドが存在すれば
    status --is-interactive; and . (rbenv init -|psub)
else
    echo "can't load rbenv."
end
