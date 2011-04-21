#!/bin/bash

#------------------------------------------------------------------------
#    Copyright (C) 2011 Luca Amore <luca.amore at gmail.com>
#
#    frk is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    frk is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with frk.  If not, see <http://www.gnu.org/licenses/>.
#------------------------------------------------------------------------

CMD=$(basename $0)

W=720
H=600
XMin=-1.8
XMax=1.8
YMin=-1.0
YMax=1.0
KRe=-0.835
KIm=0.2321
MaxIterations=150

N=$(grep -c 'model name' /proc/cpuinfo)
NameColorPalette='paletteblu'

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
    -r      K re ($KRe)
    -i      K im ($KIm)
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

while getopts “hW:H:N:r:i:I:c:f:x:X:y:Y” OPTION
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
         r)
             KRe=$OPTARG
             ;;
         i)
             KIm=$OPTARG
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

ListArgs="${W},${H},${XMin},${XMax},${YMin},${YMax},${KRe},${KIm},${MaxIterations},'${FullFileNameRaw}',${N},${NameColorPalette}"

Log=$(printf "FN: %s H: %s W: %s I: %s K: %s, %si Z: [%s,%s] x [%s, %s] N: %s C: %s - %s\n" \
    ${FileName} ${W} ${H} ${MaxIterations} ${KRe} ${KIm} ${XMin} ${XMax} ${YMin} ${YMax} ${N} ${NameColorPalette} ${CMD} \
)

echo $Log

echo $Log > ${FullFileNameDescr}
date >> ${FullFileNameDescr}

erl -pz ebin -noshell -eval "parallel:julia($ListArgs)." -run init stop

convert ${ConvertOutputOptions} ${FullFileNameRaw} ${FullFileNameDest}

echo "saved: ${FullFileNameDest}"
