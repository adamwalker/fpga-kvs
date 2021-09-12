#!/bin/sh 
set -e

# Enable the interface connected to the Arty
ip link set dev $1 up
# Give that interface an IP address
ip addr add 192.168.5.1/24 dev $1
# Give the Arty a dummy MAC so it doesn't need to respond to ARPs
ip neigh add 192.168.5.2 lladdr 00:80:77:31:01:07 nud permanent dev $1
