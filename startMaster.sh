#!/bin/bash


help()
{
echo "Usage: startMaster.sh <opts>
Opts are those:
    -h,  --help              : Print this message and exit
    -p,  --port <number>     : Port of the master"
exit $1
}

let port=42000
ip=`hostname -I`

while [ $# -gt 0 ]; do
  case $1 in
    -ip)
      shift
      ip=$1
      ;;
    -p | --port)
      shift
      let port=$1
      ;;
    -h | --help)
      help 0
      ;;
    *)
      help 1;;
  esac
  shift
done

./dist/build/Beekeeper/Beekeeper +RTS -N$(nproc) -RTS "master" $ip $port
