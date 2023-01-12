include make.macro

all: mkclimato.x mkw10.x mkonlyfilter.x mkonlyhanning.x mkmodxu.x mkclimato_daily.x

mkclimato.x: mkclimato.f90
	$(F90) $(FFLAGS) mkclimato.f90 -o mkclimato.x $(NCDF)

mkclimato_daily.x: mkclimato_daily.f90
	$(F90) $(FFLAGS) mkclimato_daily.f90 -o mkclimato_daily.x $(NCDF)

mkw10.x: mkw10.f90
	$(F90) $(FFLAGS) mkw10.f90 -o mkw10.x $(NCDF)

mkmodxu.x: mkmodxu.f90
	$(F90) $(FFLAGS) mkmodxu.f90 -o mkmodxu.x $(NCDF)

mkonlyfilter.x: mkonlyfilter.f90
	$(F90) $(FFLAGS) mkonlyfilter.f90 -o mkonlyfilter.x $(NCDF)

mkonlyhanning.x: mkonlyhanning.f90
	$(F90) $(FFLAGS) mkonlyhanning.f90 -o mkonlyhanning.x $(NCDF)


install: mkclimato.x mkclimato.x_daily.x mkw10.x mkmodxu.x mkonlyfilter.x mkonlyhanning.x
	cp  mkclimato.x mkclimato_daily.x mkw10.x mkmodxu.x mkonlyfilter.x mkonlyhanning.x $(INSTALL)


clean:
	@\rm *~

cleanexe: clean
	@\rm mkclimato.x mkclimato_daily.x mkw10.x mkmodxu.x mkonlyfilter.x mkonlyhanning.x

