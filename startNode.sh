#!/bin/bash

help()
{
echo "Usage: startNode.sh <opts>
Opts are those:
    -h,  --help                : Print this message and exit
    -t,  --threads <number>    : How many thready should be made available
    -ip  --ip                  : Host of the node
    -p,  --port <number>       : Port of the node
    -mip --masterip <number>   : Host of the master
    -mp  --masterport <number> : Port of the master"
exit $1
}

threads=`nproc`
let port=50000
ip=`hostname -I`
mip=`hostname -I`
let mp=42000

while [ $# -gt 0 ]; do
  case $1 in
    -t | --threads)
      shift
      let threads=$1
      ;;
    -ip | --ip)
      shift
      ip=$1
      ;;
    -p | --port)
      shift
      port=$1
      ;;
    -mip | --masterip)
      shift
      mip=$1
      ;;
    -mp | --masterport)
      shift
      mp=$1
      ;;
    -h | --help)
      help 0
      ;;
    *)
      help 1;;
  esac
  shift
done

./dist/build/Beekeeper/Beekeeper "node" $ip $port $mip $mp $threads
