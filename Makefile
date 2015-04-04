.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

# PATH options
objdir = .obj
libdir = libs

# Command-line options at make call
env   ?= None      # options: manto,eolo,airaki,iplex
debug ?= 0 

ifeq ($(env),manto) ## env=manto

    ## IFORT OPTIONS ##
    FC  = ifort
    INC_NC  = -I/home/jalvarez/work/librairies/netcdflib/include
    LIB_NC  = -L/home/jalvarez/work/librairies/netcdflib/lib -lnetcdf
    LIB_MKL = -L/opt/intel/mkl/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread

    FLAGS    = -module $(objdir) -L$(objdir) $(INC_NC)
    LFLAGS   = $(LIB_NC) $(LIB_MKL)

    DFLAGS   = -vec-report0 -O2 -fp-model precise -i_dynamic 
    ifeq ($(debug), 1)
        DFLAGS   = -C -traceback -ftrapuv -fpe0 -check all -vec-report0 -fp-model precise -i_dynamic 
    endif

else ifeq ($(env),eolo) ## env=eolo

    ## IFORT OPTIONS ##
    FC  = ifort
    INC_NC  = -I/home/fispalma22/work/librairies/netcdflib/include
    LIB_NC  = -L/home/fispalma22/work/librairies/netcdflib/lib -lnetcdf
    LIB_MKL = -L/opt/intel/mkl/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread

    FLAGS    = -module $(objdir) -L$(objdir) $(INC_NC)
    LFLAGS   = $(LIB_NC) $(LIB_MKL)

    DFLAGS   = -vec-report0 -O2 -fp-model precise
    ifeq ($(debug), 1)
        DFLAGS   = -C -traceback -ftrapuv -fpe0 -check all -vec-report0 -fp-model precise
    endif

else ifeq ($(env),airaki) ## env=airaki

    ## GFORTRAN OPTIONS ##
    FC  = gfortran
    INC_NC  = -I/opt/local/include
    LIB_NC  = -L/opt/local/lib -lnetcdff -lnetcdf
    INC_COORD = -I/Users/robinson/models/EURICE/coord/.obj
	LIB_COORD = /Users/robinson/models/EURICE/coord/libcoordinates.a

    FLAGS  = -I$(objdir) -J$(objdir) $(INC_COORD) $(INC_NC) 
    LFLAGS = $(LIB_COORD) $(LIB_NC)

    DFLAGS = -O3
    ifeq ($(debug), 1)  # ,underflow
        DFLAGS   = -w -g -p -ggdb -ffpe-trap=invalid,zero,overflow -fbacktrace -fcheck=all
    endif

else ifeq ($(env),iplex) ## env=iplex

    ## IFORT OPTIONS ##
    FC  = ifort
    INC_NC  = -I/home/robinson/apps/netcdf/netcdf/include
    LIB_NC  = -L/home/robinson/apps/netcdf/netcdf/lib -lnetcdf
    LIB_MKL = -L/opt/intel/mkl/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread
    INC_COORD = -I/iplex/01/tumble/robinson/EURICE/coord/.obj
	LIB_COORD = /iplex/01/tumble/robinson/EURICE/coord/libcoordinates.a

    FLAGS    = -module $(objdir) -L$(objdir) $(INC_COORD) $(INC_NC) 
    LFLAGS   = $(LIB_COORD) $(LIB_NC)

    DFLAGS   = -vec-report0 -O3
    ifeq ($(debug), 1)
        DFLAGS   = -C -g -traceback -ftrapuv -fpe0 -check all -vec-report0
    endif

else 
    
    ## None ##
    FC = $(error "Define env")

endif

## Individual libraries or modules ##
$(objdir)/nml.o: $(libdir)/nml.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

$(objdir)/parameters.o: $(libdir)/parameters.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

$(objdir)/ncio.o: $(libdir)/ncio.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

$(objdir)/ensembles.o: ensembles.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

## Complete programs

ens-test:  $(objdir)/nml.o $(objdir)/parameters.o $(objdir)/ncio.o \
		   $(objdir)/ensembles.o
	$(FC) $(DFLAGS) $(FLAGS) -o ens_test.x $^ ens_test.f90 $(LFLAGS)
	@echo " "
	@echo "    ens_test.x is ready."
	@echo " "

ens-rembo: $(objdir)/nml.o $(objdir)/parameters.o $(objdir)/ncio.o \
		   $(objdir)/ensembles.o
	$(FC) $(DFLAGS) $(FLAGS) -o ens_rembo.x $^ ens_rembo.f90 $(LFLAGS)
	@echo " "
	@echo "    ens_rembo.x is ready."
	@echo " "

ens-mis11: $(objdir)/nml.o $(objdir)/parameters.o $(objdir)/ncio.o \
		   $(objdir)/ensembles.o
	$(FC) $(DFLAGS) $(FLAGS) -o ens_mis11.x $^ ens_rembo-mis11.f90 $(LFLAGS)
	@echo " "
	@echo "    ens_mis11.x is ready."
	@echo " "

clean:
	rm -f *.x $(objdir)/*.o $(objdir)/*.mod

