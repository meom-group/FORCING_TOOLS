#!/bin/bash
# @ job_type = serial
# @ requirements = (Feature == "prepost")
# @ wall_clock_limit=5:30:00
# @ job_name = zclimatouv
# @ output   = $(job_name).$(jobid)
# @ error    = $(job_name).$(jobid)
# @ queue

set -x 
export PATH=$PATH:./
year1=1979
year2=2015
DATADIR=/fsnet/data/meom/DATA_SET/FORCING_ATMOSPHERIQUE/ERA_INTERIM/drowned
dataset=ERAinterim
WKDIR=$WORKDIR/TMPCLIMATO1
CLIMATODIR=$WORKDIR/ERAinterim/CLIMATO_${year1}-${year2}
mkdir -p $CLIMATODIR
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

#or y in $(seq $year1 $year2) ; do
#  echo $y
#  # build w10 and Wu, Wv
#  mkw10   y$y ${dataset} lon0 lat0 time drowned_
#  mkmodxu y$y ${dataset} lon0 lat0 time drowned_
#one

   # build daily climatology of turbulent variables
   mklist u10 $year1 $year2
   mkclimato.x $lst
     mv climato.nc TMP/drowned_u10_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_u10FL10_${dataset}_y0000.nc
     
   mklist v10 $year1 $year2
   mkclimato.x $lst
     mv climato.nc TMP/drowned_v10_${dataset}_y0000.nc
     mv climato-f.nc TMP/drowned_v10FL10_${dataset}_y0000.nc

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
   for var in u10 v10 t2 q2 precip snow radsw radlw ; do
     mkonlyhanning.x drowned_${var}_${dataset}_y0000.nc 2
     mv climato-hann2.nc $CLIMATODIR/drowned_${var}_${dataset}_CLIM_0001.nc
   done
