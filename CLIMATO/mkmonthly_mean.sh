#!/bin/bash
dataset=ERAinterim

nday=( 31 28 31 30 31 30 31 31 30 31 30 31)
echo ${nday[@]}
for f in *y0000.nc ; do
n0=0
for n in $(seq -w 01 12) ; do

   i=$(( n - 1 ))
   nd=${nday[$i]}
   nf=$(( n0 + nd - 1 ))
   ncra -O -d time,$n0,$nf  $f ${f%nc}m$n.nc
   n0=$(( nf + 1 ))
done

done

for typ in  precip radlw radsw snow q2 t2 u10 v10 ; do
    ncrcat -O drowned_${typ}_${dataset}_y0000.m??.nc drowned_${typ}_${dataset}_y1979-2015.1m.nc
    mv drowned_${typ}_${dataset}_y0000.nc drowned_${typ}_${dataset}_y1979-2015.1d.nc
done



for f in *1d.nc ; do
   g=$( echo $f | sed -e 's/1d/1y/' )
   ncra -O $f $g
done


