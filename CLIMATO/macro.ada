
NCDF=-lnetcdf

F90=ifort -openmp -i-static
FFLAGS= -O2   $(NCDF) -assume byterecl -convert big_endian
#FFLAGS=    $(NCDF) -assume byterecl -convert big_endian -CB -traceback -ftrapuv -fpe0 -g
INSTALL=$(WORKDIR)/bin

