# Punkt0 Compiler 


__Authors:__ *Daniil Pintjuk, Kim Börk*


Final Product of Compiler Construction labs 2 through 7

## How to build
1. install JDK or java runtime

2. install scala

3. install sbt

4. in projects root directory start sbt:

`` sbt``

5. in sbt type


`` compile ``




## How to run

to run tests in sbt type:

`` test ``

to compile a punkt0 program in sbt type:

`` run -d <outdir>  <path to pinkt0 program>``

to compile with tail recursion optimization:

`` run -o -d <outdir>  <path to pinkt0 program>``

to print ast before and after tail recursion optimization:

`` run -o --symid  <path to pinkt0 program>``

to execute your compiled Punkt0 program:

`` cd <outdir>

   java Main ``

