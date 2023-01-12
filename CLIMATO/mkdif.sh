#!/bin/bash
dataset1=DFS5.2
dataset2=ERAinterim

for freq in  1m 1y ; do
for var in precip snow radlw radsw t2 q2 u10 v10 ; do
   ncdiff -O  ${var}_${dataset1}_y1979-2015.${freq}.nc ${var}_${dataset2}_y1979-2015.${freq}.nc Diff_${var}_${dataset1}-${dataset2}_y1979-2015.${freq}.nc
done
done
