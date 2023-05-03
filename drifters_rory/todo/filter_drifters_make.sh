#!/bin/bash
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_files.f90 
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_time.f90 
gfortran -c -O3 mod_drifter.f90 
gfortran -c -O3 -fbounds-check filter_drifters.f90 
gfortran *.o
rm -f *.o *.mod
