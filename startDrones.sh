#!/bin/bash

help()
{
echo "Usage: startDrones.sh <opts>
Opts are those:
    -h,  --help               : Print this message and exit
    -d,  --drones <number>    : How many drones should be started
    -ip  --ip                 : Host of the drones
    -p,  --port <number>      : Port of the first drone (next drones use subsequent ports)
    -qip --queenip <number>   : Host of the queen
    -qp  --queenport <number> : Port of the queen"
exit $1
}

drones=$(nproc)
port=50000
ip=$(hostname -I)
qip=$(hostname -I)
qp=42000

while [ $# -gt 1 ]; do
  case $1 in
    -d | --drones)
      shift
      drones=$1
    ;;
    -ip | --ip)
      shift
      ip=$1
    ;;
    -p | --port)
      shift
      port=$1
    ;;
    -qip | --queenip)
      shift
      qip=$1
    ;;
    -qp | --queenport)
      shift
      qp=$1
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

for i in $(seq 1 $drones); do
  echo "starting drone "${i}"..."
  ./dist/build/Beekeeper/Beekeeper "drone" $ip $port $qip $qp &
  port=$(($port+1))
done
