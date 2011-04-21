#!/bin/bash

M="./frkMandelbrot.sh"
J="./frkJulia.sh"

# mandelbrot
$M -W 350 -H 200 -c "palettered" -f "mandelbrot"

# julia
$J -W 700 -H 700 -I 100 -r 0.70176 -i 0.3842 -c "palettered" -f "J1"
