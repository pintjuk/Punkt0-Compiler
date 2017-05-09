#!/bin/bash
rm -rf testprograms/lab5/check
mkdir testprograms/lab5/check
for i in $( ls testprograms/our_valid/*.p0); do
    echo item: $i
    scala -cp lib/cafebabe_2.11-1.2.jar punkt0_2.11-1.0.jar -d dest $i
    java -cp dest Main > $i.check 
    cat $i.check
done
