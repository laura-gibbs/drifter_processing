program read_drifters

  use iso_fortran_env, only : int32, int64, real32
  use mod_files, only : count_recs_in_ascii_file
  use mod_time, only : days_in_mnth
  use mod_drifter

  implicit none

  integer(int32), parameter :: timestep = 6.0 ! hours
  integer(int32), parameter :: yr_strt = 1901, yr_end = 2100
  integer(int32), parameter :: Nyrs = yr_end - yr_strt + 1
  integer(int32), parameter :: Ndays = 366 * Nyrs
  integer(int32), parameter :: Ntsteps_max = 4 * Ndays

  integer(int32), parameter :: Nfiles = 4
  integer(int32), parameter :: Ndrftrs_max = 30000

  integer(int32) :: Nrecs(Nfiles)
  integer(int32) :: drft_cnt, drft_cnt_file(Nfiles)
  integer(int32) :: i, j, k, h, d, y
  integer(int32) :: n, n0, NN
  integer(int64) :: current_id 
  integer(int32) :: loc_cnt, Nlocs
  integer(int32) :: drft_loc_cnt(Ndrftrs_max) 
  integer(int32), allocatable :: drft_loc_cnt_6hr(:) 
  integer(int32) :: Nlocs_tstep_max
  integer(int32) :: year_min, year_max
  character(len=128) :: path0, pin1, pin2, pout, pout2
  character(len=128) :: fn(Nfiles), ffn, fout, tmp
  character(len=4) :: yr_strng
  character(len=3) :: dy_strng
  character(len=2) :: hr_strng
  character(len=9) :: date_strng
  integer(int32), allocatable :: drifter_list(:, :, :) 
  real(real32), allocatable :: drift_vel(:, :)
  real(real32), allocatable :: coords(:, :, :)
   
  type(drift_record) :: drec

 !----------------------------------------------------------
  fn = ['buoydata_1_5000.dat     ', &
        'buoydata_5001_10000.dat ', &
        'buoydata_10001_15000.dat', &
        'buoydata_15001_mar22.dat']
 !----------------------------------------------------------
  
 !----------------------------------------------------------
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'
  pout = trim(path0)//'bin/drifters/svp/meta/'
  pout2 = trim(path0)//'bin/drifters/svp/drifters/'
 !----------------------------------------------------------

 !----------------------------------------------------------
  year_min = 9999
  year_max = -9999
  drft_loc_cnt = 0
 !----------------------------------------------------------

 !----------------------------------------------------------
  allocate(drft_loc_cnt_6hr(Ntsteps_max))
  drft_loc_cnt_6hr = 0
  drft_cnt = 0
  drft_cnt_file = 0
  do i = 1, Nfiles
    current_id = -1
    ffn = trim(pin1)//trim(fn(i))
    Nrecs(i) = count_recs_in_ascii_file(ffn)
    print *, 'number of records in file', i, ':', Nrecs(i)
    open(1, file = ffn, form ='formatted')
    do n = 1, Nrecs(i)
      !print *, n
      drec = read_drift_rec(1, i)
      if (drec % id /= current_id) then
        drft_cnt = drft_cnt + 1
        drft_cnt_file(i) = drft_cnt_file(i) + 1
        current_id = drec % id
        drft_loc_cnt(drft_cnt) = 1
      else
        drft_loc_cnt(drft_cnt) = drft_loc_cnt(drft_cnt) + 1
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
  print *, 'number of drifters:', drft_cnt, sum(drft_cnt_file)
  print *, 'max locations for a drifter:', maxval(drft_loc_cnt, 1)
  print *, 'the first year is:', year_min
  print *, 'the last year is:', year_max
  print *, 'the maximum locations in a time period:', Nlocs_tstep_max
  !stop
 !----------------------------------------------------------

 !----------------------------------------------------------
 !----------------------------------------------------------
 
 
 ! Create a list of the all the drifters giving:
 !   - drifter ID
 !   - File number (1, 2, 3, or 4)
 !   - First record number
 !   - Last record number
 !----------------------------------------------------------
  allocate(drifter_list(maxval(drft_cnt_file, 1), 3, Nfiles))
  drifter_list = 0
  drft_cnt_file = 0
  do i = 1, Nfiles
    current_id = -1
    ffn = trim(pin1)//trim(fn(i))
    Nrecs(i) = count_recs_in_ascii_file(ffn)
    print *, 'number of records in file', i, ':', Nrecs(i)
    open(1, file = ffn, form ='formatted')
    do n = 1, Nrecs(i)
      drec = read_drift_rec(1, i)
      if (drec % id /= current_id) then
        drft_cnt_file(i) = drft_cnt_file(i) + 1
        current_id = drec % id
        drifter_list(drft_cnt_file(i), 1, i) = drec % id
        drifter_list(drft_cnt_file(i), 2, i) = n
      else
        drifter_list(drft_cnt_file(i), 3, i) = n
      end if
    end do
    close(1)
  end do
  !stop
 !----------------------------------------------------------

 ! Create binary file for all drifters
 ! Here I would like to create a drifter type that has a
 ! specified length corresponding to number of obs.
 !----------------------------------------------------------
  open(21, file = trim(pout2)//'drifters.dat', form = 'unformatted')
  write(21) drft_cnt
  do i = 1, Nfiles
    ffn = trim(pin1)//trim(fn(i))
    open(1, file = ffn, form ='formatted')
    do j = 1, drft_cnt_file(i)
      Nlocs = drifter_list(j, 3, i) - drifter_list(j, 2, i) + 1
      allocate(drift_vel(1:Nlocs, 5))
      do k = 1, Nlocs
        drec = read_drift_rec(1, i)
        n0 = drifter_index(drec, yr_strt, timestep)
        drift_vel(k, :) = [1.0 * n0, drec % lon, drec % lat, drec % ve, drec % vn]
      end do
      write(21)drec % id, Nlocs
      write(21)drift_vel
      deallocate(drift_vel)
    end do
  end do
  close(21)
  !stop
 !----------------------------------------------------------
 
 ! Group locations by timestep
 ! - This is useful for interpolation of
 !   time-slice data such as wind fields.
 !----------------------------------------------------------
  print *, 'Grouping locations by timestep...'
  NN = (year_max - year_min + 1) * 366 * 4
  deallocate(drft_loc_cnt_6hr)
  allocate(drft_loc_cnt_6hr(NN))
  drft_loc_cnt_6hr = 0
  allocate(coords(Nlocs_tstep_max, 3, NN))
  coords = 0.0
  do i = 1, Nfiles
    ffn = trim(pin1)//trim(fn(i))
    open(1, file = ffn, form ='formatted')
    do n = 1, Nrecs(i)
      drec = read_drift_rec(1, i)
      n0 = drifter_index(drec, year_min, timestep)
      drft_loc_cnt_6hr(n0) = drft_loc_cnt_6hr(n0) + 1
      !print *, n, n0, drft_loc_cnt_6hr(n0)
      coords(drft_loc_cnt_6hr(n0), 1, n0) = drec % id
      coords(drft_loc_cnt_6hr(n0), 2, n0) = drec % lon
      coords(drft_loc_cnt_6hr(n0), 3, n0) = drec % lat
    end do
    close(1)
  end do

  open(21, file = trim(pout)//'drifter_coords_timestep.dat', form = 'unformatted')
  write(21) Nlocs_tstep_max, year_min, year_max, NN
  write(21) drft_loc_cnt_6hr
  write(21) coords
  close(21)

  stop
 !----------------------------------------------------------

 ! Save the drifter locations grouped by timestep
 !----------------------------------------------------------
  stop
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
