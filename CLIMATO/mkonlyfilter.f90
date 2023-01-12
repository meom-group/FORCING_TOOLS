PROGRAM mkonlyfilter
!!!----------------------------------------------------------------------------
!!!                           ***  PROGRAM mkonlyfilter  ***
!!!  * Purpose : lanczos filter (hence periodisation) of the climatology
!!!  * method : 
!!!   history : original : J.M. Molines Sep. 2008
!!!----------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  ! command line stuff
  INTEGER :: narg, iargc

  INTEGER :: ji,jj,jfich, jv, jt, jat
  INTEGER :: npiglo,npjglo, nt, nvar, natt
  INTEGER :: idum, i1,i2
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rlon, rlat, rtime
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: var
  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: varday
  REAL(KIND=4) :: spval = -9999.

  CHARACTER(LEN=80) :: cfile, cvar, cfileout='climato.nc',cfileoutf='climato-f.nc', catt, cdum
  ! filtering stuff
  INTEGER :: nspan
  INTEGER, DIMENSION(:), ALLOCATABLE :: iw
  REAL(KIND=4) :: tcoup, dt, fn, dayfilt
  REAL(KIND=4)                           ::  zpi,zden,zyy,zey, zcoef, zcoef2
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: tmp, tmpf
  REAL(KIND=8), DIMENSION(:), ALLOCATABLE ::  zec,ze

  ! netcdf stuff
  INTEGER :: istatus, ncid, ncout, ncoutf
  INTEGER :: id_lon, id_lat, id_dlon, id_dlat, id_var, id_varo, id_varof
  INTEGER :: id_dtime, id_time

  ! parse command line
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : mkonlyfilter ''climato'' ''days'' '
     PRINT *,' result in climato-f$days.nc '
     PRINT *,' same variable name, but each fied is a filtered climatic field'
     STOP
  ENDIF
  CALL getarg(1,cfile)
  CALL getarg(2,cdum) ; READ(cdum,*) dayfilt
  cfileoutf='climato-f'//TRIM(cdum)//'.nc'

  ! init 10 days lancsos filter
  tcoup=dayfilt*24.  !  10 days in hours
  dt=24.         ! hours filter is applied on daily fields
  fn=dt/tcoup ! tcoup/dt is the number of data point during tcoup. fn is just the inverse.
  nspan=NINT(2./fn)
  ! *  weight coefficients
  ALLOCATE (zec(0:nspan), ze(0:nspan ))
  CALL initlanc(nspan,fn)

  ! CDF stuff in the code
  istatus= NF90_OPEN(cfile,NF90_NOWRITE,ncid)
  ! get lon lat time dimension 
  ! assume all file identical
  istatus=NF90_INQ_DIMID(ncid,'lon',id_dlon)
  istatus=NF90_INQ_DIMID(ncid,'lat',id_dlat)
  istatus=NF90_INQ_DIMID(ncid,'time',id_dtime)

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
  ALLOCATE (iw(-nspan+1:nt+nspan) )
  iw(:)=1
  PRINT *,' ALLOCATE space done'
  ! get lon lat array, once for all
  istatus=NF90_INQ_VARID(ncid,'lon',id_var)
  istatus=NF90_GET_VAR(ncid,id_var,rlon(:),start=(/1/), count=(/npiglo/) )
  istatus=NF90_INQ_VARID(ncid,'lat',id_var)
  istatus=NF90_GET_VAR(ncid,id_var,rlat(:),start=(/1/), count=(/npjglo/) )
  istatus=NF90_INQ_VARID(ncid,'time',id_time)
  istatus=NF90_GET_VAR(ncid,id_time,rtime(:),start=(/1/), count=(/nt/) )

  ! idem for filtered fields

  istatus=NF90_CREATE(cfileoutf,NF90_CLOBBER,ncoutf)
  ! define dims
  istatus=NF90_DEF_DIM(ncoutf,'lon',npiglo,id_dlon)
  istatus=NF90_DEF_DIM(ncoutf,'lat',npjglo,id_dlat)
  istatus=NF90_DEF_DIM(ncoutf,'time',NF90_UNLIMITED,id_dtime)
  ! define var
  istatus=NF90_DEF_VAR(ncoutf,'lon',NF90_FLOAT,(/id_dlon/),id_lon)
  istatus=NF90_DEF_VAR(ncoutf,'lat',NF90_FLOAT,(/id_dlat/),id_lat)
  istatus=NF90_DEF_VAR(ncoutf,'time',NF90_FLOAT,(/id_dtime/),id_time)
  istatus=NF90_DEF_VAR(ncoutf,cvar,NF90_FLOAT,(/id_dlon,id_dlat,id_dtime/),id_varof)
  ! attributes of variable
  istatus=NF90_INQ_VARID(ncid,cvar,id_var)
  istatus=NF90_INQUIRE_VARIABLE(ncid,id_var,nAtts=natt)
  DO jat=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,id_var,jat,catt)
     istatus=NF90_COPY_ATT(ncid,id_var,catt,ncoutf,id_varof)
  ENDDO
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

     PRINT *, ' 10 days filtering and periodisation nspan=', nspan
     DO ji=1,npiglo
       DO jj=1,npjglo
           tmp=0.
           IF ( varday(ji,jj,1) /= spval ) THEN
           tmp(1:nt)=varday(ji,jj,1:nt)
           tmp(nt+1:nt+nspan)=varday(ji,jj,1:nspan)
           tmp(-nspan+1:0)=varday(ji,jj,nt-nspan+1:nt)
           CALL lislanczos(tmp,iw,tmpf,nt+2*nspan,fn,nspan)
           ELSE
           tmpf(:)=spval
           ENDIF
           varday(ji,jj,1:nt)=tmpf(1:nt)
       ENDDO
     ENDDO

     PRINT *, 'Final output of filtered, periodized fields'
     DO jt=1,nt
       istatus=NF90_PUT_VAR(ncoutf,id_varof,varday(:,:,jt),start=(/1,1,jt/), count=(/npiglo,npjglo,1/) )
     END DO
     istatus=NF90_CLOSE(ncoutf)


CONTAINS
  SUBROUTINE initlanc(knj, pfn)
    !! ---------------------------------------------------------------------------
    !!          ***  SUBROUTINE initlanc  ***
    !!  
    !! ** Purpose: init lancsos coeficient
    !!
    !! ---------------------------------------------------------------------------
    INTEGER, INTENT(in) :: knj
    REAL(KIND=4), INTENT(in):: pfn
   
    zpi=ACOS(-1.)
    ze(0)=2.*pfn
    zcoef=2.*pfn*zpi ; zcoef2=zpi/knj

    DO  ji=1,knj
       ze(ji)=SIN(zcoef*ji)/(zpi*ji)
    END DO
    !
    zec(0)=2.*pfn
    DO jj=1,knj
       zey=zcoef2*jj
       zec(jj)=ze(jj)*SIN(zey)/zey
    END DO
    END SUBROUTINE initlanc

  SUBROUTINE lislanczos(px,kiw,py,kn,pfn,knj)
    !! ---------------------------------------------------------------------------
    !!           ***   SUBROUTINE lislanczos   ***
    !!
    !!   ** Purpose :  apply lanczos filter to input vector
    !!
    !!   ** Method  :  lanczos weight are computed
    !!               then convolution is done
    !      x=input data
    !      kiw = validity of input data
    !      y=output filter
    !      kn=number of input/output data
    !      knj= bandwith of the filter
    !      pfn= cutoff frequency
    !! * history
    !!      Eric Blayo d'apres une source CLS fournie par F. BLANC.
    !!           et grosse(s)  optimization(s).
    !!      J.M. Molines : Dr Norm : 11/03/2002 19:35
    !!      J.M. Molines : F90 : 31/05/2007 20:29
    !!---------------------------------------------------------------------------
    ! * Arguments
    REAL(KIND=4), DIMENSION(:), INTENT(in)  :: px  !: Input time series
    REAL(KIND=4), DIMENSION(:), INTENT(out) :: py  !: output filtered time series
    REAL(KIND=4),               INTENT(in)  :: pfn !: cutoff freq.
    INTEGER,      DIMENSION(:), INTENT(in)  :: kiw !: flag for missing data 1=good, 0= bad
    INTEGER,                    INTENT(in)  :: kn  !: number of points in px and py
    INTEGER,                    INTENT(in)  :: knj !: band width of the filter

    ! * Local variables
    INTEGER  :: ji,jj,jm,jk,kk
    INTEGER  :: nmin,nmax,k1,k2
    !

    ! * Filtering
    nmin=knj
    nmax=kn-knj+1
    !
    DO jm=1,kn
       k1=-knj
       k2=knj
       IF (jm <= nmin) k1=1-jm
       IF (jm >= nmax) k2=kn-jm
       !
       zyy=0.
       zden=0.
       !
       DO jk=k1,k2
          kk=ABS(jk)
          IF (kiw(jk+jm) == 1) THEN
             zden=zden+zec(kk)
             zyy=zyy+zec(kk)*px(jk+jm)
          END IF
       END DO
       IF (zden /= 0) THEN
          py(jm)=zyy/zden
       ELSE
          py(jm)=999.0
       END IF
    END DO
  END SUBROUTINE lislanczos

   END PROGRAM mkonlyfilter
