#!/bin/bash
dataset=DFS5.2

for freq in 1d 1m 1y ; do

for var in precip snow radlw radsw t2 q2 u10 v10 ; do
   ~/sosie_sourceforge_trunk/bin/mask_drown_field.x -M -i drowned_${var}_${dataset}_y1979-2015.${freq}.nc -v ${var} -x lon0 -y lat0 -m lsm_erainterim.nc  -o ${var}_${dataset}_y1979-2015.${freq}.nc
done
done
