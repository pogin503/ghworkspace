#!/usr/bin/env fish

function die
    set -l msg argv[1]
    set -l code 1 # default exit status 1
    msg "$msg"
    exit "$code"
end
function usage
    cat "\
usage:
    ./make_template <language> <contest>
example:
    ./make_template rust b084
"
end

set args_count (count $argv)
if test $args_count -lt 2
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

for x in $argv[2..-1]
    # set -l problem_name "$contest_name$x"
    set -l dest "$contest_name"
    echo "$language_name/$dest"
    cp -R ./template/* $dest
    # switch $language_name
    #     case rust
    #         pushd $dest > /dev/null
    #         cargo build
    #         popd
    # end
end

popd
