#!/bin/bash
set -e

pushd clash
stack exec clash -- --verilog kvs.hs
popd
mkdir -p fpga/src/generated
cp clash/verilog/Main.topEntity/* fpga/src/generated
pushd fpga
vivado -mode batch -nojournal -source compile.tcl

