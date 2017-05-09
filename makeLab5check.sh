#!/bin/bash
rm -rf testprograms/lab5/check
mkdir testprograms/lab5/check
for i in $( ls testprograms/lab5/valid/); do
    echo item: $i
    scala -cp lib/cafebabe_2.11-1.2.jar punkt0_2.11-1.0.jar -d dest testprograms/lab5/valid/$i
    java -cp dest Main > testprograms/lab5/check/$i.check 
    cat testprograms/lab5/check/$i.check
done
