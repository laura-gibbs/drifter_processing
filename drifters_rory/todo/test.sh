#!/bin/bash
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_netcdf.f90 -I /usr/local/Cellar/netcdf/4.8.1_2/include -L/usr/local/Cellar/netcdf/4.8.1_2/lib -lnetcdff
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_time.f90 
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_stats.f90 
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_spatial.f90 
gfortran -c -O3 mod_drifter.f90 
gfortran -c -O3 ekman_params_refine.f90 
gfortran *.o
rm -f *.o *.mod
