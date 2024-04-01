# FPGA key-value store

An FPGA based networked key-value store. Responds to requests to lookup/insert/delete key-value pairs via a simple UDP based protocol.

See the [blog post](https://adamwalker.github.io/Building-FPGA-KVS/)

Implemented in [Clash](https://clash-lang.org/).

Runs on the [Arty board](https://store.digilentinc.com/arty-a7-artix-7-fpga-development-board-for-makers-and-hobbyists/).

An improved version of this hashtable can be found [here](https://github.com/adamwalker/fpga-hashmap).

## Build

Make sure Vivado is in your path, then

```
$ ./build.sh
```

## Setup

Program the image onto the FPGA using the Vivado hardware manager, then

```
$ sudo ./setup_net.sh <network interface connected to the fpga>
```

## Test

```
$ cd test
$ make
$ ./test
```
