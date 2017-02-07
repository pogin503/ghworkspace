#!/usr/bin/env bash

set -ue

function kill_process_1(){
  local RET
  RET=$(ps aux | grep "$1" | grep  -v)
  if -z "$RET" ;then
    echo "can't find $1 process ID."
  else
    kill -9 "$(RET | awk '{print $2}')"
  fi
}

function kill_process_2(){
  local RET
  RET=$(pgrep "$1" | wc -l | awk '{print $1}')
  if [ "$RET" -eq 0 ]; then
    echo "can't find $1 process."
  elif [ "$RET" -ge 2 ]; then
    echo "can't identify process."
    echo "process count: $RET"
    echo "processes:"
    pgrep "$1"
    
  else
    kill -9 "$1"
  fi
}

# kill_process_1 process_name
# kill_process_2 proces_name
# killall "process_name"
# pkill proces_name
