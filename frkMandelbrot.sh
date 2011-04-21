#!/bin/bash

#--------------------------
# M A N D E L B R O T
# Luca Amore
# http://www.lucaamore.com
# luca.amore at gmail.com
#--------------------------

CMD=$(basename $0)

W=350
H=200
XMin=-2.5
XMax=1.0
YMin=-1.0
YMax=1.0
MaxIterations=40

N=$(grep -c 'model name' /proc/cpuinfo)
NameColorPalette='palettered'

ImgOutDir='save'
ImgOutFormat='png'
ConvertOutputOptions='-antialias'

Verbose=0
NoConvert=0

FileName="out"

usage()
{
cat << EOF

FRK: MANDELBROT GENERATOR

    usage: ${CMD} options

    frk Mandelbrot set generator

    OPTIONS:
    -W      width ($W)
    -H      heigth ($H)
    -N      split into N concurrent processes ($N)
    -I      iterations ($MaxIterations)
    -c      name color palette ($NameColorPalette)
    -f      output filename ($FileName)
    -N      no convert to $ImgOutFormat ($NoConvert)
    -x      XMin ($XMin)
    -X      XMax ($XMax)
    -y      XMin ($YMin)
    -Y      YMax ($YMax)
    -v      Verbose ($Verbose)
    -h      this help

EOF
}

while getopts “hW:H:N:I:c:f:x:X:y:Y:v” OPTION
do
     case $OPTION in
         h)
             usage
             exit 1
             ;;
         W)
             W=$OPTARG
             ;;
         H)
             H=$OPTARG
             ;;
         N)
             N=$OPTARG
             ;;
         I)
             MaxIterations=$OPTARG
             ;;
         c)
             NameColorPalette=$OPTARG
             ;;
         f)
             FileName=$OPTARG
             ;;
         d)
             MakeDescription=1
             ;;
         x)
             XMin=$OPTARG
             ;;
         X)
             XMax=$OPTARG
             ;;
         y)
             YMin=$OPTARG
             ;;
         Y)
             YMax=$OPTARG
             ;;
         v)
             Verbose=1
             ;;
         ?)
             usage
             exit
             ;;
     esac
done

FullFileName="${ImgOutDir}/${FileName}"
FullFileNameRaw="${FullFileName}.ppm"
FullFileNameDescr="${FullFileName}.txt"
FullFileNameDest="${FullFileName}.${ImgOutFormat}"

ListArgs="${W},${H},${XMin},${XMax},${YMin},${YMax},${MaxIterations},'${FullFileNameRaw}',${N},${NameColorPalette}"

Log=$(printf "FN: %s H: %s W: %s I: %s Z: [%s,%s] x [%s, %s] N: %s c: %s - %s\n" \
    ${FileName} ${W} ${H} ${MaxIterations} ${XMin} ${XMax} ${YMin} ${YMax} ${N} ${NameColorPalette} ${CMD} \
)

echo $Log

echo $Log > ${FullFileNameDescr}
date >> ${FullFileNameDescr}

erl -pz ebin -noshell -eval "parallel:mandelbrot($ListArgs)." -run init stop

convert ${ConvertOutputOptions} ${FullFileNameRaw} ${FullFileNameDest}

echo "saved: ${FullFileNameDest}"
