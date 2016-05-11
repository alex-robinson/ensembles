# ensembles

This Fortran program can be used to combine individual output files from an
ensemble of simulations into one file. Time series can be interpolated to
a new set of time steps (only those of interest, for example).

# Steps

1. Clone the repository:
`git clone git@github.com:alex-robinson/ensembles.git`

2. Make sure you can compile properly using the test program:
```
cd ensembles
make clean
make ens-test [env=eolo]   # Use env=eolo if compiling on eolo
mkdir output
./ens_test.x
```

If the program compiles and runs without any issues, you are ready to prepare
your own ensemble generation program.

3. Make a new program to handle your ensemble, based on an old one for example:
`cp ens_test.f90 ens_new.f90`

4. Add your program to the Makefile, make sure it also compiles.

5. Modify program, particularly calls to `ens_write_par`, `ens_init` and `ens_write` to produce your own ensemble output. `ens_write_par` is used to write parameter values to an ensemble netcdf file, while variables are written to their own netcdf file in two steps. `ens_init` is used to write the dimensions needed for each variable to the new netcdf file. Then `ens_write` is called once for each variable to be written to file. 
