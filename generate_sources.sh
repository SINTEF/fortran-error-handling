#!/bin/bash

readonly SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

for infile in $(find $SCRIPT_DIR -name *.fypp); do
    outfile=${infile%.fypp}.f90
    echo " - Processing $infile..."
    fypp $infile $outfile || exit $?
done

echo " - Done"
