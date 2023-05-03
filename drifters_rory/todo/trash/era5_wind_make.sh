#!/bin/bash
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_files.f90 
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_time.f90 
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_netcdf.f90 -I/Users/rb13801/local/inlude 
gfortran -c -O3 era5_wind.f90 
gfortran *.o
rm -f *.o *.mod
