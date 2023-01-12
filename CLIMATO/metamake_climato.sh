#!/bin/bash
# @ job_type = serial
# @ requirements = (Feature == "prepost")
# @ wall_clock_limit=5:30:00
# @ job_name = zclimato
# @ output   = $(job_name).$(jobid)
# @ error    = $(job_name).$(jobid)
# @ queue

set -x 
year1=2008
year2=2008
DATADIR=$WORKDIR/DATA_FORCING/DFS5.2_RD/ALL
dataset=DFS5.2
WKDIR=$WORKDIR/TMPCLIMATO
CLIMATODIR=$WORKDIR/DATA_FORCING/DFS5.2_RD/CLIMATO_${year1}-${year2}
#-----------------------------------------------------
# function for building the list of file for a given type for a range of years
mklist() {
     typ=$1
     y1=$2
     y2=$3
     lst=''
   for y in $(seq $y1 $y2 ) ; do
     lst="$lst $( ls *${typ}_*y${y}.nc) "
   done
         }
#-----------------------------------------------------
mkdir -p $WKDIR/TMP
mkdir -p $CLIMATODIR


cd $WKDIR
ln -sf $DATADIR/drowned*nc ./

for y in $(seq $year1 $year2) ; do
   echo $y
   # build w10 and Wu, Wv
   mkw10.x   y$y ${dataset} lon0 lat0 time drowned_
   mkmodxu.x y$y ${dataset} lon0 lat0 time drowned_
done

   # build daily climatology of turbulent variables
   mklist w10 $year1 $year2
   mkclimato.x $lst
     mv climato.nc TMP/drowned_w10_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_w10FL10_${dataset}_y0000.nc
     
   mklist umodU10 $year1 $year2
   mkclimato.x $lst
     mv climato.nc TMP/drowned_wu10_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_wu10FL10_${dataset}_y0000.nc

   mklist vmodU10 $year1 $year2
   mkclimato.x $lst
     mv climato.nc TMP/drowned_wv10_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_wv10FL10_${dataset}_y0000.nc

   mklist t2 $year1 $year2
   mkclimato.x $lst
     mv climato.nc TMP/drowned_t2_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_t2FL10_${dataset}_y0000.nc

   mklist q2 $year1 $year2
   mkclimato.x $lst
     mv climato.nc TMP/drowned_q2_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_q2FL10_${dataset}_y0000.nc
  
   # build daily climatology of flux variables
   mklist precip $year1 $year2
   mkclimato_daily.x $lst
     mv climato.nc TMP/drowned_precip_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_precipFL10_${dataset}_y0000.nc

   mklist snow  $year1 $year2
   mkclimato_daily.x $lst
     mv climato.nc TMP/drowned_snow_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_snowFL10_${dataset}_y0000.nc

   mklist radsw  $year1 $year2
   mkclimato_daily.x $lst
     mv climato.nc TMP/drowned_radsw_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_radswFL10_${dataset}_y0000.nc

   mklist radlw  $year1 $year2
   mkclimato_daily.x $lst
     mv climato.nc TMP/drowned_radlw_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_radlwFL10_${dataset}_y0000.nc

   # hanning filtering
   cd TMP
   for var in w10 wu10 wv10 t2 q2 precip snow radsw radlw ; do
     mkonlyhanning.x drowned_${var}_${dataset}_y0000.nc 2
     mv climato-hann2.nc $CLIMATODIR/drowned_${var}_${dataset}_CLIM_0001.nc
   done
