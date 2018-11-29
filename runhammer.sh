#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

VERILOG_FILE=$1
FPGA_CONFIG=$2

if [ -z "$VERILOG_FILE" ]
then
  echo "Need a verilog file"
  exit
fi

if [ ! -e "config/${FPGA_CONFIG}.yml" ]
then
  echo "config/${FPGA_CONFIG}.yml does not exist!"
  exit
fi

VERILOG_MODULE=$(basename ${VERILOG_FILE})
VERILOG_TOP=${VERILOG_MODULE%.v}

OUTDIR=out_${VERILOG_TOP}_${FPGA_CONFIG}

export HAMMER_HOME=$DIR/hammer

source $HAMMER_HOME/sourceme.sh

cd "$DIR" && hammer-vlsi synthesis -o syn-output.json -v ${VERILOG_FILE} --top ${VERILOG_TOP} -p config/clocks.json -p config/use_vivado.json  -p config/${FPGA_CONFIG}.yml --obj_dir ${OUTDIR}

source config/vivado_setup.sh
vivado -mode batch -source extract_vivado_report.tcl -tclargs ${OUTDIR}
