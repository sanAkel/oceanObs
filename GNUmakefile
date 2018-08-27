.SUFFIXES: .o .f90 .x

.PHONY:
	all clean

# Start of this crude makefile
exec = read_ocn_obs.x
#objects = rd_wr_prof.o netcdf_read_write_.o read_ocn_obs.o rd_trak.o rd_swh.o rd_ship.o error_exit.o rd_wr_prof.mod netcdf_read_write_.mod
objects = rd_wr_prof.o netcdf_read_write_.o read_ocn_obs.o rd_trak.o rd_swh.o rd_ship.o error_exit.o 

BASEDIR = /discover/nobackup/projects/gmao/share/gmao_ops/Baselibs/v4.0.11_build1/x86_64-unknown-linux-gnu/ifort_18.0.1.163-mpt_2.17/Linux
BASEINC = $(BASEDIR)/include
BASELIB = $(BASEDIR)/lib
BASEBIN = $(BASEDIR)/bin

LIB_NETCDF = $(shell $(BASEBIN)/nf-config --flibs)
INC_NETCDF = -I$(BASEINC)/netcdf

#FC = ifort
FC = mpif90
OPT = -O3
FLFLAGS = -g
FCFLAGS = -convert big_endian -traceback

all: exec

exec: $(objects)
	$(FC) -o $(exec) $(OPT) $(FCFLAGS) $(objects) $(LIB_NETCDF)

netcdf_read_write_.o: netcdf_read_write_.f90
	$(FC) -c $(OPT) $(FLFLAGS) $(FCFLAGS) netcdf_read_write_.f90 $(INC_NETCDF)

rd_wr_prof.o: rd_wr_prof.f90 netcdf_read_write_.o
	$(FC) -c $(OPT) $(FLFLAGS) $(FCFLAGS) rd_wr_prof.f90 -I/netcdf_read_write_.mod

read_ocn_obs.o: read_ocn_obs.f90
	$(FC) -c $(OPT) $(FLFLAGS) $(FCFLAGS) read_ocn_obs.f90

rd_ship.o: rd_ship.f90
	$(FC) -c $(OPT) $(FLFLAGS) $(FCFLAGS) rd_ship.f90

rd_swh.o: rd_swh.f90
	$(FC) -c $(OPT) $(FLFLAGS) $(FCFLAGS) rd_swh.f90

rd_trak.o: rd_trak.f90
	$(FC) -c $(OPT) $(FLFLAGS) $(FCFLAGS) rd_trak.f90

error_exit.o: error_exit.f90
	$(FC) -c $(OPT) $(FLFLAGS) $(FCFLAGS) error_exit.f90

clean:
	rm -f $(objects) $(exec) *.mod

# End of this crude makefile
