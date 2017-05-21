#!/usr/bin/env bash

# for-each
for x in "test1" \
             "test2" \
             "test3"
do
    echo $x
done


# AWK to use multiple spaces as delimiter
# separate fields with tab in awk
docker images | awk -F'[[:space:]][[:space:]]+' 'BEGIN{OFS="\t"} {print $1,$2,$3,$4,$5}'

# check xbit OS
if [[ $(uname -m) =~ x86_64 ]];then
	  echo "x86_64"
	  echo '`uname -m`='"$(uname -m)"
else
	  echo "else"
fi
