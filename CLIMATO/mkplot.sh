#!/bin/bash

mkclrmark() {
    nticks=$( echo $1 $2 $3 | awk '{print ($2 - $1 )/$3+1 }' )
    minval=$1
    dlta=$3
    for tick in $(seq 1 $nticks) ; do
      echo $minval
      minval=$( echo $1 | awk '{print minval+dlta}' minval=$minval dlta=$dlta )
    done
             }

export PATH=/home/users/molines/DEV/FORCING_TOOLS/SRC/:/home/users/molines/DEV/CHART_7.0/:$PATH
dataset1=DFS5.2
dataset2=ERAinterim
climlimit=1979-2015
freq=1y

for typ in t2 q2 u10 v10 radsw radlw precip snow ; do 
    if [ ! -f Diff_${typ}_${dataset1}-${dataset2}_y${climlimit}.${freq}.cgm ] ; then

    case $typ in 
    ( t2 )  title="$dataset1 - $dataset2 $climlimit : $typ" 
           unit=K
           min=-2 ; max=2  ; dlta=0.5 ; format=f4.1 
           CLROPT="-clrmark zclrmark.txt  -format PALETTE $format -p blue_red.pal" ;;
    ( q2 )  title="$dataset1 - $dataset2 $climlimit : $typ" 
            unit="g/kg"
           min=-0.3 ; max=0.3  ; dlta=0.1 ; format=f4.1 
           CLROPT="-clrscale 1000 -clrmark zclrmark.txt  -format PALETTE $format -p blue_red.pal" ;;
    ( u10 | v10 )  title="$dataset1 - $dataset2 $climlimit : $typ"
           unit="m/s"
           min=-0.8 ; max=0.8  ; dlta=0.2 ; format=f4.1
           CLROPT="-clrmark zclrmark.txt  -format PALETTE $format -p blue_red.pal" ;;
    ( radsw | radlw )  title="$dataset1 - $dataset2 $climlimit : $typ"
           unit="W/m2"
           min=-50 ; max=50  ; dlta=5 ; format=I3
           CLROPT="-clrmark zclrmark.txt  -format PALETTE $format -p blue_red.pal" ;;
    ( precip | snow )  title="$dataset1 - $dataset2 $climlimit : $typ"
           unit="mm/day"
           min=-4 ; max=4  ; dlta=1 ; format=I2
           CLROPT="-clrscale 86400 -clrmark zclrmark.txt  -format PALETTE $format -p blue_red.pal" ;;
    esac

mkclrmark $min $max $dlta > zclrmark.txt

upside_down_var -f Diff_${typ}_${dataset1}-${dataset2}_y${climlimit}.${freq}.nc -v ${typ} -l lat0 

chart -clrdata Diff_${typ}_${dataset1}-${dataset2}_y${climlimit}.${freq}.nc.sym \
   -clrmodif "x=lon0,y=lat0,time_counter=time,nav_lon=lon0,nav_lat=lat0" -clrvar ${typ} \
   $CLROPT -title "$title" \
   -clrxypal 0.1 0.95 0.2 0.3 -noteam \
   -string 0.95 0.22 1 1 $unit \
   -marg 0 360 -90 90 -xstep 30 -ystep 30 -xgrid -ygrid \
   -o Diff_${typ}_${dataset1}-${dataset2}_y${climlimit}.${freq}.cgm

# -p nrl_grey_back.pal
    else
       echo Diff_${typ}_${dataset1}-${dataset2}_y${climlimit}.${freq}.cgm already done
    fi

#if [ $typ = radlw ] ; then exit ; fi
done
