.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

.PHONY : usage
usage:
	@echo ""
	@echo "    * USAGE * "
	@echo ""
	@echo " make test       : compiles the test program test_ncio.x"
	@echo " make f2py       : compiles the ncio source for use as a Python module using f2py."
	@echo " make clean      : cleans object and executable files"
	@echo ""


objdir = .obj

# Command-line options at make call
ifort ?= 0
debug ?= 0 

## GFORTRAN OPTIONS (default) ##
FC = gfortran
#LIB = /usr/lib
#INC = /usr/include
LIB = /opt/local/lib
INC = /opt/local/include

FLAGS  = -I$(objdir) -J$(objdir) -I$(INC)
LFLAGS = -L$(LIB) -lnetcdff -lnetcdf

DFLAGS = -O3
ifeq ($(debug), 1)
    DFLAGS   = -w -g -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all
endif

ifeq ($(ifort),1) 
	## IFORT OPTIONS ##
    FC = ifort 
    LIB = /home/robinson/apps/netcdf/netcdf/lib
    INC = /home/robinson/apps/netcdf/netcdf/include

	FLAGS        = -module $(objdir) -L$(objdir) -I$(INC)
	LFLAGS		 = -L$(LIB) -lnetcdf

	DFLAGS   = -vec-report0 -O3
	ifeq ($(debug), 1)
	    DFLAGS   = -C -traceback -ftrapuv -fpe0 -check all -vec-report0
	    # -w 
	endif
endif

## Individual libraries or modules ##
$(objdir)/nml.o: nml.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

$(objdir)/ncio.o: ncio.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

$(objdir)/ensembles.o: ensembles.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

## Complete programs

ens-rembo: $(objdir)/nml.o $(objdir)/ncio.o $(objdir)/ensembles.o
	$(FC) $(DFLAGS) $(FLAGS) -o ens_rembo.x $^ ens_rembo.f90 $(LFLAGS)
	@echo " "
	@echo "    ens_rembo.x is ready."
	@echo " "

clean:
	rm -f *.x $(objdir)/*.o $(objdir)/*.mod

