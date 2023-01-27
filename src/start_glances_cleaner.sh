#!/bin/bash
DIR="/sbidata/glances/"
FILES="$DIR/*"
LINE_LIMIT=150 # number of lines to start cleaning
LINE_EXT=100   # number of lines to extract from existing file

if (( LINE_EXT >= LINE_LIMIT )); then
   echo "ERROR: LINE_EXT must be smaller than LINE_LIMIT!"
   exit 1
fi

for F in $FILES
do
  echo "Processing $F file..."
  nline=$(wc -l < $F | bc)
  echo $nline
  if (( $nline >= $LINE_LIMIT  )); then
    echo "Cleaning..."
    head -n 1 $F > $DIR/temp.csv # copy header
    tail -n $LINE_EXT $F >> $DIR/temp.csv # copy LINE_EXT lines except the header
    cat $DIR/temp.csv > $F
    rm $DIR/temp.csv
  fi
done
