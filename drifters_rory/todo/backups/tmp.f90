module drifter

  use iso_fortran_env, only : int32, real32

  implicit none

  private

  type, public :: drift_rec
    integer(int32) :: id, mnth, yr
    real(real32) :: day, lat, lon, temp, ve, vn, spd 
    real(real32) :: latv, lonv, tempv
  end type 

end module drifter  

program read_drifters

  use iso_fortran_env, only : int32, real32

  use mod_files, only : count_recs_in_ascii_file

  implicit none

  integer(int32) :: Nrecs, n
  character(len=128) :: path0, pin1, pin2, pout
  character(len=128) :: fn, ffn, tmp
  character(len=128) :: frmt 

  integer(int32) :: id, mnth, yr
  real(real32) :: day, lat, lon, temp, ve, vn, spd 
  real(real32) :: latv, lonv, tempv

  frmt = '(I8, I5, f7.3, I6, 6f10.3, 3e13.2)'

  fn = 'buoydata_10001_jun14.dat'
  
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'

  ffn = trim(pin1)//trim(fn)
  !Nrecs = count_recs_in_ascii_file(ffn)

  print *, 'number of records in file:', Nrecs

  open(1, file = ffn, form ='formatted')
  do n = 1, 1 ! Nrecs
    !read(1, '(A)') tmp
    !read(1, '(I8, I5, f7.3, I6, 6f10.3, 3e13.2)') id, mnth, day, yr, lat, lon, &
    read(1, frmt) id, mnth, day, yr, lat, lon, &
                                                  temp, ve, vn, spd, &
                                                  latv, lonv, tempv
    print *, id, mnth, day, yr, lat, lon, temp, ve, vn, spd, latv, lonv, tempv 
    stop
  end do

contains



end program read_drifters
