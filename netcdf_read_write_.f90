module netcdf_read_write_
use netcdf
!
!=============================================================================
!BOP
!
! DESCRIPTION:  Reads in OR Writes out a netCDF file containing ocean profile observations
!               Feb, 2018, Santha Akella
!
!
!EOP
!=============================================================================
!
   implicit none
   private
   public   :: write_nc_prof, check_ncerr, check_nc

!  to use later in the analysis:
!  type profile
!     character(len=7) :: id
!     real             :: lat
!     real             :: lon
!     integer          :: nlev_temp
!     integer          :: nlev_salt
!     real             :: bot_depth
!     character(len=12):: rcpt_time
!     character(len=12):: obs_time
!     character(len=12):: data_type_temp
!     character(len=12):: data_type_salt
!  end type profile

   contains
      subroutine write_nc_prof(file_name, nobs, nlev, wmoid, temp_type, salt_type, &
                               lon, lat, lev, lev_temp, lev_salt,                  &
                               ob_time, rcpt_time, btm_dep, temp, salt)
        implicit none

        character (len=256), intent(in) :: file_name
        character (len=7),   intent(in) :: wmoid(nobs)
        integer,             intent(in) :: nobs, nlev

        integer,            dimension(nobs),       intent(in) :: temp_type, salt_type, lev_temp, lev_salt
        real,               dimension(nobs),       intent(in) :: lon, lat, btm_dep
        character (len=12), dimension(nobs),       intent(in) :: ob_time, rcpt_time
        real,               dimension(nlev, nobs), intent(in) :: lev, temp, salt

        integer      :: incid, ierr,                                           &
                        nobs_dim_id, lev_dim_id, wmo_id, lon_id, lat_id,       &
                        char1_dim_id,  char2_dim_id,                           &
                        lev_id, ob_time_id, rcpt_time_id,                      &
                        depth_id, temp_lev_id, salt_lev_id,                    &
                        temp_typ_id, salt_typ_id, temp_id, salt_id,            &
                        iProf, iLev

        integer, parameter :: iMissing = -999.

        real,    dimension(nlev, nobs) :: my_levs, my_temp, my_salt

        logical      :: exist

!       type(profile), dimension(nobs) :: profiles
! .................................................................................................
!
! fill up to all levels
!
        my_levs = lev
        my_temp = temp
        my_salt = salt
        do iProf = 1, nobs
           my_levs(lev_temp(iProf)+1:nlev, iProf) = iMissing
           my_temp(lev_temp(iProf)+1:nlev, iProf) = iMissing
           my_salt(lev_temp(iProf)+1:nlev, iProf) = iMissing ! stange, right? this is whats done in http://www.usgodae.org/ftp/outgoing/fnmoc/data/ocn/docs/ocn_obs.f

!          my_levs(lev_salt(iProf)+1:nlev, iProf) = iMissing
!          my_salt(lev_salt(iProf)+1:nlev, iProf) = iMissing
        end do
! .................................................................................................

!
! create nc file
!
        print *, ' '
        print *, 'netcdf_read_write_: init_, Writing Profiles in netcdf format to: ', file_name

        inquire (file=file_name, exist=exist)
        if (exist) then
          print *, 'netcdf_read_write_: write_nc_prof, File: ', file_name, 'Already Exists. STOPPING!'
          stop
!         Reopen the file for writing
!         ierr = nf90_open(path = file_name, mode = nf90_write, ncid = file_id)
!         call check_ncerr(ierr, "Error in re-writing to output file")
        else
          ierr = nf90_create ( path=file_name, cmode=nf90_clobber, ncid=incid )
          call check_ncerr(ierr, "Error creating output file")
        end if
!
! define dimensions
!
        call check_nc( nf90_def_dim(incid, 'num_prof', nobs, nobs_dim_id))  ! num of profiles
        call check_nc( nf90_def_dim(incid, 'num_lev',  nlev, lev_dim_id))   ! num of levels
        call check_nc( nf90_def_dim(incid, 'num_char1',12,   char1_dim_id)) ! just to write char string 
                                                                            ! https://www.unidata.ucar.edu/support/help/MailArchives/netcdf/msg08285.html
        call check_nc( nf90_def_dim(incid, 'num_char2',7,    char2_dim_id)) ! just to write char string 

!
! define the variables 
!
        call check_nc( nf90_def_var(incid, 'wmo_id',    nf90_char, (/ char2_dim_id, nobs_dim_id /),  wmo_id))

        call check_nc( nf90_def_var(incid, 'lon',       nf90_real, (/ nobs_dim_id /),      lon_id))
        call check_nc( nf90_def_var(incid, 'lat',       nf90_real, (/ nobs_dim_id /),      lat_id))

        call check_nc( nf90_def_var(incid, 'obs_time',  nf90_char, (/ char1_dim_id, nobs_dim_id /),  ob_time_id))
        call check_nc( nf90_def_var(incid, 'rcpt_time', nf90_char, (/ char1_dim_id, nobs_dim_id /), rcpt_time_id))

        call check_nc( nf90_def_var(incid, 'bot_depth', nf90_real, (/ nobs_dim_id /),     depth_id))
        call check_nc( nf90_def_var(incid, 'temp_lev',  nf90_int,  (/ nobs_dim_id /),  temp_lev_id))
        call check_nc( nf90_def_var(incid, 'salt_lev',  nf90_int,  (/ nobs_dim_id /),  salt_lev_id))

        call check_nc( nf90_def_var(incid, 'temp_type', nf90_int,  (/ nobs_dim_id /),  temp_typ_id))
        call check_nc( nf90_def_var(incid, 'salt_type', nf90_int,  (/ nobs_dim_id /),  salt_typ_id))

        call check_nc( nf90_def_var(incid, 'levels',    nf90_real, (/ lev_dim_id, nobs_dim_id /),  lev_id))
        call check_nc( nf90_def_var(incid, 'temp',      nf90_real, (/ lev_dim_id, nobs_dim_id /),  temp_id))
        call check_nc( nf90_def_var(incid, 'salt',      nf90_real, (/ lev_dim_id, nobs_dim_id /),  salt_id))
        
!
! put attributes
!
        ierr = nf90_put_att(incid, wmo_id, 'long_name', 'WMO_ID_or_ship_call_sign')

        ierr = nf90_put_att(incid, lon_id, 'long_name', 'longitude')
        ierr = nf90_put_att(incid, lon_id, 'units',     'degrees_east')

        ierr = nf90_put_att(incid, lat_id, 'long_name', 'latitude')
        ierr = nf90_put_att(incid, lat_id, 'units',     'degrees_north')

        ierr = nf90_put_att(incid, ob_time_id,   'long_name', 'observation_time_YYYYMMDDHHMMSS')
        ierr = nf90_put_att(incid, rcpt_time_id, 'long_name', 'FNMOC_receipt_time_YYYYMMDDHHMMSS')

        ierr = nf90_put_att(incid, depth_id,     'long_name', 'bottom_depth from DBDBV data base at profile lat,lon')
        ierr = nf90_put_att(incid, depth_id,     'units',     'meters')
        ierr = nf90_put_att(incid, depth_id,     'positive',  'down')

        ierr = nf90_put_att(incid, temp_lev_id,  'long_name', 'num_of_levels')
        ierr = nf90_put_att(incid, salt_lev_id,  'long_name', 'num_of_levels')

        ierr = nf90_put_att(incid, temp_typ_id,  'long_name', 'type_of_temperature_observation')
        ierr = nf90_put_att(incid, salt_typ_id,  'long_name', 'type_of_salinity_observation')

        ierr = nf90_put_att(incid, lev_id,       'long_name',   'profile_levels')
        ierr = nf90_put_att(incid, lev_id,       'units',       'meters')
        ierr = nf90_put_att(incid, lev_id,       'positive',    'down')

!x      ierr = nf90_put_att(incid, lev_id,       'coordinates', 'num_prof num_lev')
!x      ierr = nf90_put_att(incid, temp_id,      'coordinates', 'num_prof num_lev')
!x      ierr = nf90_put_att(incid, salt_id,      'coordinates', 'num_prof num_lev')

!
! global history attribute
!
        ierr = nf90_put_att(incid, nf90_global, 'source',  'ocean profile observations from FNMOC USGODAE http://www.usgodae.org/pub/outgoing/fnmoc/data/ocn/profile/')
        ierr = nf90_put_att(incid, nf90_global, 'history', 'Processed using netcdf converter written by Santha Akella, 02/2018.')
!
! done defining
!
        ierr = nf90_enddef(incid)
!
! write data
!
        ierr = nf90_put_var(incid, wmo_id, wmoid)
        call check_ncerr(ierr, "Error writing WMO ID")

        ierr = nf90_put_var(incid, lon_id, lon)
        call check_ncerr(ierr, "Error writing longitude")

        ierr = nf90_put_var(incid, lat_id, lat)
        call check_ncerr(ierr, "Error writing latitude")

        ierr = nf90_put_var(incid, ob_time_id, ob_time, start=(/1, 1/), count=(/12, nobs/))
        call check_ncerr(ierr, "Error writing observation time")

        ierr = nf90_put_var(incid, rcpt_time_id, rcpt_time, start=(/1, 1/), count=(/12, nobs/))
        call check_ncerr(ierr, "Error writing FNMOC receipt time")

        ierr = nf90_put_var(incid, depth_id, btm_dep)
        call check_ncerr(ierr, "Error writing bottom depth")

        ierr = nf90_put_var(incid, temp_lev_id, lev_temp)
        call check_ncerr(ierr, "Error writing temperature levels")

        ierr = nf90_put_var(incid, salt_lev_id, lev_salt)
        call check_ncerr(ierr, "Error writing salinity  levels")

        ierr = nf90_put_var(incid, temp_typ_id, temp_type)
        call check_ncerr(ierr, "Error writing temperature obs type")

        ierr = nf90_put_var(incid, salt_typ_id, salt_type)
        call check_ncerr(ierr, "Error writing salinity obs type")

        ierr = nf90_put_var(incid, lev_id, my_levs)
        call check_ncerr(ierr, "Error writing profile levels")

        ierr = nf90_put_var(incid, temp_id, my_temp)
        call check_ncerr(ierr, "Error writing temperature")

        ierr = nf90_put_var(incid, salt_id, my_salt)
        call check_ncerr(ierr, "Error writing salinity")
!
! close nc file
!
        print *, 'netcdf_read_write_: write_nc_prof, Finished Writing Profiles to: ', file_name
        print *, ' '
        call check_nc( nf90_close(incid))

      return
      end subroutine write_nc_prof
!.......................................................................
      subroutine check_nc(status)
        implicit none

        integer, intent ( in) :: status

        if(status /= nf90_noerr) then
          print *, trim(nf90_strerror(status))
          stop "Stopped"
        end if

      end subroutine check_nc
!.......................................................................
! https://bitbucket.csiro.au/projects/CCAM/repos/pcc2hist/browse/ncutils_m.f90
      subroutine check_ncerr(status, mesg)
        implicit none

        integer, intent(in) :: status
        character(len=*), intent(in), optional :: mesg

        if ( status /= nf90_noerr ) then
         if ( present(mesg) ) then
            print*, mesg
         end if
         print*, trim(nf90_strerror(status))
         stop
        end if

      end subroutine check_ncerr
!.......................................................................
end module netcdf_read_write_
