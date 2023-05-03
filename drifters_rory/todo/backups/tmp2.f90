module mod_drifter

  use iso_fortran_env, only : int32, real32

  implicit none

  private

  type, public :: drift_record
    integer(int32) :: id, mnth, yr
    real(real32) :: day, lat, lon, temp, ve, vn, spd 
    real(real32) :: latv, lonv, tempv
  end type drift_record

  public :: read_drift_rec

contains

  function read_drift_rec(funit) result(drec)
    
    integer(int32), intent(in) :: funit
    
    type(drift_record) :: drec
  
    character(len=128) :: frmt 

    frmt = '(I8, I5, f7.3, I6, 6f10.3, 3e13.2)'

    read(funit, frmt) drec % id, & 
                  drec % mnth, drec % day, drec % yr, &
                  drec % lat, drec % lon, &
                  drec % temp, &
                  drec % ve, drec % vn, drec % spd, &
                  drec % latv, drec % lonv, drec % tempv

  end function read_drift_rec

end module mod_drifter  

program read_drifters

  use iso_fortran_env, only : int32, real32
  use mod_files, only : count_recs_in_ascii_file
  use mod_drifter

  implicit none

  integer(int32) :: Nrecs, n
  character(len=128) :: path0, pin1, pin2, pout
  character(len=128) :: fn, ffn, tmp
  type(drift_record) :: drec

  fn = 'buoydata_10001_jun14.dat'
  
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'

  ffn = trim(pin1)//trim(fn)
  Nrecs = count_recs_in_ascii_file(ffn)

  print *, 'number of records in file:', Nrecs

  open(1, file = ffn, form ='formatted')
  do n = 1, Nrecs
    !read(1, '(A)') tmp
    drec = read_drift_rec(1)
    print *, drec
    !stop
  end do
  close(1)

end program read_drifters
