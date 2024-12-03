FC = ifort

HDF5_LIBS = -L$(LDT_HDF5)/lib -lhdf5_fortran -lhdf5
HDF5_INCLUDE = -I$(LDT_HDF5)/include

NETCDF_LIBS = -L$(LDT_NETCDF)/lib -lnetcdff -lnetcdf
NETCDF_INCLUDE = -I$(LDT_NETCDF)/include

LIBS = $(HDF5_LIBS) $(NETCDF_LIBS)
INCLUDES = $(HDF5_INCLUDE) $(NETCDF_INCLUDE)

INPUT_FILE = /discover/nobackup/ejalilva/data/AMSR2/PMW/GW1AM2_201207022318_175D_L1SGRTBR_2220220.h5

read_amsr: read_amsr.f90
	$(FC) $(INCLUDES) $^ -o $@ $(LIBS)

run: read_amsr
	./read_amsr $(INPUT_FILE)

clean:
	rm -f *.o *.mod read_amsr
