#!/bin/bash

help()
{
echo "Usage: startDrones.sh <opts>
Opts are those:
    -h,  --help              : Print this message and exit
    -d,  --drones <number>   : How many drones should be started
    -p,  --port <number>     : Port of the first drone"
exit $1
}

drones=$(nproc)
port=50000
ip=$(hostname -I)

if [ $# -gt 1 ]; then
  case $1 in
    -d | --drones)
      shift
      drones=$1
    ;;
    --ip)
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
fi

for i in $(seq 1 $drones); do
  echo "starting drone "${i}"..."
  ./dist/build/Beekeeper/Beekeeper "drone" $ip $port &
  port=$(($port+1))
done
