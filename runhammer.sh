#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

VERILOG_FILE=$1

if [ -z "$VERILOG_FILE" ]
then
  echo "Need a verilog file"
  exit
fi

VERILOG_MODULE=$(basename ${VERILOG_FILE})
VERILOG_TOP=${VERILOG_MODULE%.v}

export HAMMER_HOME=$DIR/hammer

source $HAMMER_HOME/sourceme.sh

cd "$DIR" && hammer-vlsi synthesis -o syn-output.json -v ${VERILOG_FILE} --top ${VERILOG_TOP} -p config/clocks.json -p config/use_vivado.json  --obj_dir out_${VERILOG_TOP}

vivado -mode batch -source extract_vivado_report.tcl -tclargs ${VERILOG_TOP}
