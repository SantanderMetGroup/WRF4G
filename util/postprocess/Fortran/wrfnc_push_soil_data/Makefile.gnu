BINDIR = ../../../../wn/bin
FC = gfortran
FFLAGS = -g -O2
LIBFLAGS = -L/software/ScientificLinux/4.6/netcdf/4.1/gcc_gfortran/netcdf_4.1/lib -lnetcdf -L/software/ScientificLinux/4.6/netcdf/4.1/gcc_gfortran/hdf5_v1.8.4/lib -lhdf5_hl -lhdf5  -L/software/ScientificLinux/4.6/netcdf/4.1/gcc_gfortran/zlib_v1.2.3/lib -lz -lm
INCFLAGS = -I/software/ScientificLinux/4.6/netcdf/4.1/gcc_gfortran/netcdf_4.1/include

default: $(BINDIR)/wrfnc_push_soil_data.exe

$(BINDIR)/wrfnc_push_soil_data.exe: wrfnc_push_soil_data.f90
	$(FC) $< $(LIBFLAGS) $(INCFLAGS) $(FFLAGS) -o $@
