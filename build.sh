#!/bin/bash
set -e

pushd clash
stack exec clash -- --verilog kvs.hs
popd
cp clash/verilog/Main.topEntity/* fpga/src/
pushd fpga
vivado -mode batch -nojournal -source compile.tcl

