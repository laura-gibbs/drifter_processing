#!/bin/bash
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_files.f90 
gfortran -c -O3 /Users/rb13801/tools/my_code/fortran/modules/mod_time.f90 
gfortran -c -O3 mod_drifter.f90 
gfortran -c -O3 find_ekman_params.f90 
gfortran *.o
rm -f *.o *.mod
