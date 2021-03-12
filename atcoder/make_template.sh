#!/usr/bin/env fish

function usage
    cat "\
usage:
    ./make_template rust abc123 a
    ./make_template rust abc123 a b c d e
"
end

set args_count (count $argv)
if test $args_count -lt 3
    usage
    exit
end

set language_name $argv[1]
set contest_name $argv[2]

if not test -d $language_name
    echo "Can't find $language_name directory."
    read -l -P "Do you want to create directory? [y/N] " confirm
    switch $confirm
        case '' N n
            exit
        case Y y
            if not test -d $language_name
                mkdir -p -v $language_name/template
                echo "Done. Next, Please create ./$language_name/template directory and template files."
                exit
            else
                echo "$language_name directory already exists."
                exit 1
            end
        case '*'
            exit
    end
end

pushd "$language_name" > /dev/null

if test -d $contest_name
    while true
        echo "$contest_name directory already exists."

        read -l -P "Do you want to overwrite? [y/N] " confirm
        switch $confirm
            case '' N n
                popd
                exit
            case Y y
                if test -d $contest_name
                    rm -rf $contest_name
                    mkdir -p $contest_name
                    break
                else
                    echo "$contest_name: No such file or directory."
                    exit 1
                end
            case '*'
                popd
                exit
        end
    end
else
    mkdir -p $contest_name
end

if string match -q "$language_name" "rust"
    set parent_path (realpath (dirname (status -f)))
    # なぜかbasenameが使えないため文字列処理で頑張る
    set splitted (string split "/" $parent_path)
    if string match -q $splitted[(count $splitted)] "$language_name"
        pushd "$parent_path/template"
        cargo clean
        popd
    end
end

if not test -d ./template
    echo "Please create template directory."
    popd
    exit
end

for x in $argv[3..-1]
    set dest "$contest_name$x"
    echo $dest
    cp -R ./template "$contest_name/$contest_name$x"
end

popd
