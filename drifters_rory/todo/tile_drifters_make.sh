#!/bin/bash
gfortran -c -O3 tile_drifters.f90 
gfortran *.o
rm -f *.o *.mod
