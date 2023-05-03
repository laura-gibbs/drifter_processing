program era5_wind

  use iso_fortran_env, only : int32, real32
  use mod_files, only : count_recs_in_ascii_file
  use mod_time, only : days_in_mnth
  use mod_netcdf, only : read_nc 

  implicit none

  integer(int32), parameter :: timestep = 6.0 ! hours
  integer(int32), parameter :: yr_strt = 1901, yr_end = 2100
  integer(int32), parameter :: Nyrs = yr_end - yr_strt + 1
  integer(int32), parameter :: Ndays = 366 * Nyrs
  integer(int32), parameter :: Ntsteps_max = 4 * Ndays

  integer(int32) :: Nrecs(3), Ndrfs
  integer(int32) :: i, j, k, h, d, y
  integer(int32) :: n, n0, NN
  integer(int32) :: current_id 
  integer(int32) :: drft_loc_cnt(20000) 
  integer(int32) :: drft_loc_cnt_6hr(Ntsteps_max) 
  integer(int32) :: Nlocs_tstep_max
  integer(int32) :: year_min, year_max
  character(len=128) :: path0, pin1, pin2, pout
  character(len=128) :: fn(3), ffn, fout, tmp
  character(len=4) :: yr_strng
  character(len=3) :: dy_strng
  character(len=2) :: hr_strng
  character(len=9) :: date_strng
  real(real32), allocatable :: coords(:, :, :)
   
  type(drift_record) :: drec
 !----------------------------------------------------------

 !----------------------------------------------------------
  fn = ['buoydata_1_5000.dat     ', &
        'buoydata_5001_10000.dat ', &
        'buoydata_10001_jun14.dat']
 !----------------------------------------------------------
  
 !----------------------------------------------------------
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'
  pin1 = 'data/SVP/'
  pin2 = 'data/era5/'
  pout = 'data/meta/'
 !----------------------------------------------------------

 !----------------------------------------------------------
  ffn = trim(pin2)//'era5_wind_u10m_79-85.nc'
  call read_nc(ffn)
  stop 
 !----------------------------------------------------------
 
 
 
 !----------------------------------------------------------
  Ndrfs = 0
  current_id = -1
  year_min = 9999
  year_max = -9999
  drft_loc_cnt = 0
  drft_loc_cnt_6hr = 0
 !----------------------------------------------------------

 !----------------------------------------------------------
  do i = 1, 3
    ffn = trim(pin1)//trim(fn(i))
    Nrecs(i) = count_recs_in_ascii_file(ffn)
    print *, 'number of records in file', i, ':', Nrecs(i)
    open(1, file = ffn, form ='formatted')
    do n = 1, Nrecs(i)
      drec = read_drift_rec(1)
      if (drec % id /= current_id) then
        Ndrfs = Ndrfs + 1
        current_id = drec % id
        drft_loc_cnt(Ndrfs) = 1
      else
        drft_loc_cnt(Ndrfs) = drft_loc_cnt(Ndrfs) + 1
      end if
      if (drec % yr < year_min) year_min = drec % yr
      if (drec % yr > year_max) year_max = drec % yr
      n0 = drifter_index(drec, yr_strt, timestep)
      drft_loc_cnt_6hr(n0) = drft_loc_cnt_6hr(n0) + 1
    end do
    close(1)
  end do
  Nlocs_tstep_max = maxval(drft_loc_cnt_6hr, 1)
  print *, 'total number of records:', sum(Nrecs)
  print *, 'number of drifters:', Ndrfs 
  print *, 'max locations for a drifter:', maxval(drft_loc_cnt, 1)
  print *, 'the first year is:', year_min
  print *, 'the last year is:', year_max
  print *, 'the maximum locations in a time period:', Nlocs_tstep_max
  !stop
 !----------------------------------------------------------

 ! Group locations by timestep
 ! - This is useful for interpolation of
 !   time-slice data such as wind fields.
 !----------------------------------------------------------
  print *, 'Grouping locations by timestep...'
  drft_loc_cnt_6hr = 0
  NN = (year_max - year_min + 1) * 366 * 4
  allocate(coords(Nlocs_tstep_max, 2, NN))
  coords = 0.0
  do i = 1, 3
    ffn = trim(pin1)//trim(fn(i))
    open(1, file = ffn, form ='formatted')
    do n = 1, Nrecs(i)
      drec = read_drift_rec(1)
      n0 = drifter_index(drec, year_min, timestep)
      drft_loc_cnt_6hr(n0) = drft_loc_cnt_6hr(n0) + 1
      !print *, n, n0, drft_loc_cnt_6hr(n0)
      coords(drft_loc_cnt_6hr(n0), 1, n0) = drec % lat
      coords(drft_loc_cnt_6hr(n0), 2, n0) = drec % lon
    end do
    close(1)
  end do
  !stop
 !----------------------------------------------------------

 ! Save the drifter locations grouped by timestep
 !----------------------------------------------------------
  n = 0
  do y = year_min, year_max
    do d = 1, sum(days_in_mnth(y))
      do h = 1, 4
        n = n + 1
        !print *, n, drft_loc_cnt_6hr(n)
        if (drft_loc_cnt_6hr(n) > 0) then
          write(yr_strng, '(I4)') y
          write(dy_strng, '(I3)') d
          if (dy_strng(1:1) == ' ') dy_strng(1:1) = '0'
          if (dy_strng(2:2) == ' ') dy_strng(2:2) = '0'
          write(hr_strng, '(I2)') 6 * (h - 1)
          if (hr_strng(1:1) == ' ') hr_strng(1:1) = '0'
          date_strng = yr_strng//dy_strng//hr_strng
          !print *, date_strng, n, drft_loc_cnt_6hr(n)
          fout = trim(pout)//'drifter_coords_'//date_strng//'.ascii'
          open(10, file = fout, form = 'formatted')
          do k = 1, drft_loc_cnt_6hr(n)
            !print *, k, coords(k, :, n)
            write(10, *)coords(k, :, n)
          end do
          close(10)
          !stop
        end if
      end do
    end do
  end do
 !----------------------------------------------------------

end program read_drifters
