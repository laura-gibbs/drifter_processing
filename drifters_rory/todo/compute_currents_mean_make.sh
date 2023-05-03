#!/bin/bash
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_time.f90 
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_stats.f90 
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_spatial.f90 
gfortran -c -O3 mod_drifter.f90 
gfortran -c -O3 compute_currents_mean.f90 
gfortran *.o
rm -f *.o *.mod
