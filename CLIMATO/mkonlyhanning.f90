PROGRAM mkonlyhanning
!!!----------------------------------------------------------------------------
!!!                           ***  PROGRAM mkonlyhanning  ***
!!!  * Purpose : lanczos filter (hence periodisation) of the climatology
!!!  * method : 
!!!   history : original : J.M. Molines Sep. 2008
!!!----------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  CHARACTER(LEN=80), PARAMETER :: cp_lonname='lon0', cp_latname='lat0', cp_timname='time'
  !----------------------------------------------------------------------------------------
  ! command line stuff
  INTEGER :: narg, iargc

  INTEGER :: ji,jj,jfich, jv, jt, jat, jh
  INTEGER :: npiglo,npjglo, nt, nvar, natt
  INTEGER :: idum, i1,i2
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rlon, rlat, rtime
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: var
  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: varday
  REAL(KIND=4) :: spval = -9999.

  CHARACTER(LEN=80) :: cfile, cvar, cfileoutf='climato-f.nc', catt, cdum
  ! filtering stuff
  INTEGER :: nhan, nspan
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: tmp, tmpf

  ! netcdf stuff
  INTEGER :: istatus, ncid, ncout, ncoutf
  INTEGER :: id_lon, id_lat, id_dlon, id_dlat, id_var, id_varo, id_varof
  INTEGER :: id_dtime, id_time

  ! parse command line
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' usage :  mkonlyhanning ''climato'' ''number of hanning'' '
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       Apply hanning filtering on input file, at order sepcified on the' 
     PRINT *,'       command line. This time filtering is done after periodisation of'
     PRINT *,'       the input file to that all points are filtered'
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'        climato : name of climatological file to filter'
     PRINT *,'        number of hanning : order of the hanning filter.'
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'        none'
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       none'
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf file : climato-hann<order>.nc : filtered climatological file.'
     PRINT *,'         variables : same as in input files'
     PRINT *,'      '
     PRINT *,'     SEE ALSO :'
     PRINT *,'       mkclimato  mkclimato_daily  mkmodxu  mkonlyfilter  mkonlyhanning mkw10'
     PRINT *,'      '
     STOP
  ENDIF

  CALL getarg(1,cfile)
  CALL getarg(2,cdum) ; READ(cdum,*) nhan
  cfileoutf='climato-hann'//TRIM(cdum)//'.nc'

  ! init 10 days lancsos filter
  nspan=4*nhan

  ! CDF stuff in the code
  istatus= NF90_OPEN(cfile,NF90_NOWRITE,ncid)
  ! get lon lat time dimension 
  ! assume all file identical
  istatus=NF90_INQ_DIMID(ncid,TRIM(cp_lonname) ,id_dlon)
  istatus=NF90_INQ_DIMID(ncid,TRIM(cp_latname) ,id_dlat)
  istatus=NF90_INQ_DIMID(ncid,TRIM(cp_timname) ,id_dtime)

  istatus=NF90_INQUIRE_DIMENSION(ncid,id_dlon,len=npiglo)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_dlat,len=npjglo)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_dtime,len=nt)

  ! look for variable name in file (the only XYT var in file)
  istatus=NF90_INQUIRE(ncid,nVariables=nvar)
  DO jv=1,nvar
     istatus=NF90_INQUIRE_VARIABLE(ncid,jv,name=cvar,ndims=idum)
     IF ( idum == 3 ) EXIT
  END DO
  PRINT *,' working for variable ', TRIM(cvar),' with ', narg,' files'


  PRINT *,' ALLOCATE space ...'
  ALLOCATE ( rlon(npiglo), rlat(npjglo), var(npiglo,npjglo) )
  ALLOCATE ( rtime(nt) ,varday(npiglo,npjglo,nt) )
  ALLOCATE (tmp(-nspan+1:nt+nspan), tmpf(-nspan+1:nt+nspan) )
  PRINT *,' ALLOCATE space done'
  ! get lon lat array, once for all
  istatus=NF90_INQ_VARID(ncid,TRIM(cp_lonname) ,id_var)
  istatus=NF90_GET_VAR(ncid,id_var,rlon(:),start=(/1/), count=(/npiglo/) )
  istatus=NF90_INQ_VARID(ncid,TRIM(cp_latname) ,id_var)
  istatus=NF90_GET_VAR(ncid,id_var,rlat(:),start=(/1/), count=(/npjglo/) )
  istatus=NF90_INQ_VARID(ncid,TRIM(cp_timname) ,id_time)
  istatus=NF90_GET_VAR(ncid,id_time,rtime(:),start=(/1/), count=(/nt/) )

  ! idem for filtered fields

  istatus=NF90_CREATE(cfileoutf,NF90_CLOBBER,ncoutf)
  ! define dims
  istatus=NF90_DEF_DIM(ncoutf,TRIM(cp_lonname) ,npiglo,id_dlon)
  istatus=NF90_DEF_DIM(ncoutf,TRIM(cp_latname) ,npjglo,id_dlat)
  istatus=NF90_DEF_DIM(ncoutf,TRIM(cp_timname) ,NF90_UNLIMITED,id_dtime)
  ! define var
  istatus=NF90_DEF_VAR(ncoutf,TRIM(cp_lonname) ,NF90_FLOAT,(/id_dlon/),id_lon)
  istatus=NF90_DEF_VAR(ncoutf,TRIM(cp_latname) ,NF90_FLOAT,(/id_dlat/),id_lat)
  istatus=NF90_DEF_VAR(ncoutf,TRIM(cp_timname) ,NF90_FLOAT,(/id_dtime/),id_time)
  istatus=NF90_DEF_VAR(ncoutf,cvar,NF90_FLOAT,(/id_dlon,id_dlat,id_dtime/),id_varof)
  ! attributes of variable
  istatus=NF90_INQ_VARID(ncid,cvar,id_var)
  istatus=NF90_INQUIRE_VARIABLE(ncid,id_var,nAtts=natt)
  DO jat=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,id_var,jat,catt)
     IF ( catt /= 'add_offset' .AND. catt /= 'scale_factor' ) THEN
       istatus=NF90_COPY_ATT(ncid,id_var,catt,ncoutf,id_varof)
     ENDIF
  ENDDO
  istatus=NF90_COPY_ATT(ncid,NF90_GLOBAL,'history', ncoutf, NF90_GLOBAL)
  WRITE(cdum,'("Hanning filter at order ",i2)') nhan
  istatus=NF90_PUT_ATT(ncoutf, NF90_GLOBAL, 'filter', cdum )

  istatus=NF90_ENDDEF(ncoutf)
  ! copy lon lat and time in the output file
  istatus=NF90_PUT_VAR(ncoutf,id_lon,rlon)
  istatus=NF90_PUT_VAR(ncoutf,id_lat,rlat)
  istatus=NF90_PUT_VAR(ncoutf,id_time,rtime)


  istatus=NF90_CLOSE(ncid)

     CALL getarg(1,cfile)
     istatus= NF90_OPEN(cfile,NF90_NOWRITE,ncid)
     istatus=NF90_INQ_VARID(ncid,cvar,id_var)
     istatus=NF90_GET_ATT(ncid,id_var,'missing_value',spval)
     DO jt=1,nt
        istatus=NF90_GET_VAR(ncid,id_var,varday(:,:,jt),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
     END DO

     PRINT *, nhan,' hanning applied and periodisation '
     DO ji=1,npiglo
       DO jj=1,npjglo
           tmp=0.
!          IF ( varday(ji,jj,1) /= spval ) THEN
           tmp(1:nt)=varday(ji,jj,1:nt)
           tmp(nt+1:nt+nspan)=varday(ji,jj,1:nspan)
           tmp(-nspan+1:0)=varday(ji,jj,nt-nspan+1:nt)
             DO jh=1,nhan
               DO jt=-nspan+2,nt+nspan-1
                tmpf(jt)=0.25*tmp(jt-1) + 0.5*tmp(jt) + 0.25*tmp(jt+1)
               END DO
               tmp(:)=tmpf(:)
             END DO
!          ELSE
!          tmpf(:)=spval
!          ENDIF
           varday(ji,jj,1:nt)=tmpf(1:nt)
       ENDDO
     ENDDO

     PRINT *, 'Final output of filtered, periodized fields'
     DO jt=1,nt
       istatus=NF90_PUT_VAR(ncoutf,id_varof,varday(:,:,jt),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
     END DO
     istatus=NF90_CLOSE(ncoutf)


   END PROGRAM mkonlyhanning
