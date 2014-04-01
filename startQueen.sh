#!/bin/bash

help()
{
echo "Usage: startQueen.sh <opts>
Opts are those:
    -h,  --help              : Print this message and exit
    -p,  --port <number>     : Port of the first drone"
exit $1
}

port=42000
ip=$(hostname -I)

while [ $# -gt 1 ]; do
  case $1 in
    -ip)
      shift
      ip=$1
    ;;
    -p | --port)
      shift
      port=$1
    ;;
    -h | --help)
      help 0
    ;;
    *)
      help 1
    ;;
  esac
  shift
done

echo "starting queen ..."
./dist/build/Beekeeper/Beekeeper "queen" $ip $port
