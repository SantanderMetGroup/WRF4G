
BINDIR = ../../../../wn/bin
FC = pgf90
FFLAGS = -Mfree
LIBFLAGS = -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm
INCFLAGS = -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include

default: $(BINDIR)/wrfnc_push_soil_data.exe

$(BINDIR)/wrfnc_push_soil_data.exe: wrfnc_push_soil_data.f90
	$(FC) $< $(LIBFLAGS) $(INCFLAGS) $(FFLAGS) -o $@
