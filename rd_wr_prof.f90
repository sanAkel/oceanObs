module rd_wr_prof
use netcdf_read_write_, only: write_nc_prof
!
!=============================================================================
!BOP
!
! DESCRIPTION:  reads the PROFILE ocean obs files. Variables that are read in are common to read_(...) and write_(...)
!
!               Downloaded from 
!               \url{http://www.usgodae.org/ftp/outgoing/fnmoc/data/ocn/docs/ocn_obs.f}
!               Re-formatted and recoded in {\it this} style, 
!               in Feb, 2018 by Santha Akella
!
! PARAMETERS:
!      Name          Type        Usage            Description
!   ----------     ---------     ------    ---------------------------
!   UNIT            integer      input     FORTRAN unit number
!   obs_typ         char         input     observation type
!   file_dtg        char         input     date time group (YYYYMMDDHH)
!   n_obs           integer      input     number profile obs
!   n_lvl           integer      input     number profile levels
!   vrsn            integer      input     version number data file
!
! PROFILE VARIABLES:
!    Name       Type                     Description
!   --------  --------    ----------------------------------------------
!   ob_btm     real       bottom depth in meters from DBDBV data base at profile lat,lon
!   ob_lat     real       profile observation latitude (south negative)
!   ob_lon     real       profile observation longitude (west negative)
!   ob_ls      integer    number of observed profile salinity levels (a zero indicates temperature-only profile)
!   ob_lt      integer    number of observed profile temperature levels
!
!   ob_ssh     real       SSHA of profile dynamic height from long-term hydrographic mean.  dynamic height has been
!                         calculated relative to 2000 m or the bottom whichever is shallower.  the profile may have
!                         been vertically extended in the dynamic height computation, so the ob_ssh values must be used
!                         with care for profiles with shallow maximum observation depths.
!   ob_sst     real       SST estimate (in order of high resoloution regional analysis if available, global
!                         analysis if available, profile SST if observed shallow enough or SST climatology
!                         (MODAS or GDEM)) valid at profile observation location and sampling time
!
!   ob_sal_typ integer    profile salinity data type (see ocean_types.h for codes)
!   ob_sal_qc  real       salinity profile overall probability of gross error (integrates level-by-level errors taking
!                         into account layer thicknesses)
!
!   ob_tmp_typ integer    profile temperature data type (see ocean_types.h for codes)
!   ob_tmp_qc  real       temperature profile overall probability of gross error (integrates level-by-level errors
!
!   ob_lvl     real       observed profile levels
!   ob_sal     real       observed  profile salinities, if salinity has not been observed it has been estimated from climatological T/S regressions
!   ob_sal_err real       salinity observation errors (use with caution, reported values are experimental)
!   ob_sal_prb real       salinity profile level-by-level probability of a gross error
!   ob_tmp     real       observed profile temperatures
!   ob_tmp_err real       temperature observation errors (use with caution, reported values are experimental)
!   ob_tmp_prb real       temperature profile level-by-level probability of a gross error taking into account layer thicknesses)
!
!   ob_dtg     character  profile observation sampling date time group in the form  year, month, day, hour, minute, second (YYYYMMDDHHMMSS)
!   ob_rcpt    character  profile observation receipt time at FNMOC in the form year, month, day, hour, minute
!                         (YYYYMMDDHHMM); the difference between ob_rcpt and ob_dtg gives the timeliness of the observation at FNMOC
!   ob_scr     character  profile obs security classification code; "U" for unclassified
!   ob_sign    character  profile observation call sign
!
!   ob_clm_sal real       GDEM 3.0 salinity climatology estimate at profile location, levels and sampling time
!   ob_clm_tmp real       GDEM 3.0 temperature climatology estimate at profile location, levels and sampling time
!   ob_clm_ssd real       GDEM 3.0 climate salinitiy variability
!   ob_clm_tsd real       GDEM 3.0 climate temperature variability
!   ob_glb_sal real       global analysis estimate of profile salinities at profile obs location, levels, and sampling time
!   ob_glb_tmp real       global analysis estimate of profile temperatures at profile obs location, levels, and sampling time
!   ob_glb_ssd real       global analysis salinity errors
!   ob_glb_tsd real       global analysis temperature errors
!   ob_mds_sal real       modas synthetic salinity profile estimate at profile location, levels, and sampling time based on ob_mds_tmp or ob_tmp predictors
!   ob_mds_tmp real       modas synthetic temperature profile estimate at profile location, levels, and sampling time.  The predictor variables used in the
!                         generation of the modas synthetic profile are the ob_sst (SST) and ob_ssh (SSHA)variables.
!   ob_rgn_sal real       regional analysis estimate of profile salinities at profile obs location, levels, and sampling time
!   ob_rgn_tmp real       regional analysis estimate of profile temperatures at profile obs location, levels, and sampling time
!   ob_rgn_ssd real       regional analysis salinity errors
!   ob_rgn_tsd real       regional analysis temperature errors
!
!   ob_sal_xvl real       salinity profile from cross validation (GDEM 3.0 climate profile in absence of near-by data)
!   ob_sal_xsd real       salinity cross validation profile error (based on error reduction of GDEM 3.0 climate variability)
!   ob_tmp_xvl real       temperature profile from cross validation (GDEM 3.0 climate profile in absence of near-by data)
!   ob_tmp_xsd real       temperature cross validation profile error (based on error reduction of GDEM 3.0 climate variability)
!
! **********************
! UN USED:
!   ob_id      character  unique identifier of profile observation at FNMOC this is the CRC number computed
!                         from the WMO message at NAVO this is a home-grown number that has no meaning to the rest of world 
!   ob_sal_std real       climatolgical estimates of variability of salinity at profile location, levels and sampling time (one standard deviation)
!   ob_tmp_tsd real       climatolgical estimates of variability of temperature at profile location, levels and sampling time (one standard deviation)
! **********************
!
!EOP
!=============================================================================
!
   implicit none
   private  :: write_fmt_fnmoc
   public   :: read_write_prof, subset_obs_type

   contains
      subroutine read_write_prof (UNIT, obs_typ, subType, file_dtg, output_format, n_obs, n_lvl, vrsn)
        implicit none

        include 'ocn_obs_types.h'

        integer,             intent(in) :: UNIT, n_obs, n_lvl, vrsn
        integer,             intent(in) :: subType
        character (len=10) , intent(in) :: obs_typ, file_dtg, output_format
!
! local variables
!
        character (len=256) :: output_file_name
        character (len=12)  :: ob_dtg (n_obs), ob_rcpt(n_obs)
        character (len=10)  :: ob_id  (n_obs)
        character (len=7)   :: ob_sign(n_obs)
        character (len=1)   :: ob_scr (n_obs)

        integer            :: i, j
        integer, parameter :: iMissing = -999.

        integer, dimension(n_obs) :: ob_ls, ob_lt, ob_sal_typ, ob_tmp_typ
        real,    dimension(n_obs) :: ob_btm, ob_lat, ob_lon, ob_ssh, ob_sst, ob_sal_qc, ob_tmp_qc

        real,    dimension(n_lvl, n_obs) :: ob_lvl, ob_sal, ob_tmp, &
                                            ob_clm_sal, ob_clm_ssd, ob_clm_tmp, ob_clm_tsd, &
                                            ob_glb_sal, ob_glb_ssd, ob_glb_tmp, ob_glb_tsd, &
                                            ob_mds_sal, ob_mds_tmp, ob_rgn_sal, ob_rgn_ssd, &
                                            ob_rgn_tmp, ob_rgn_tsd, ob_sal_err, ob_sal_prb, &
                                            ob_sal_xvl, ob_sal_xsd, ob_tmp_err, ob_tmp_prb, &
                                            ob_tmp_xvl, ob_tmp_xsd

        integer                         :: my_nobs
        character (len=12), allocatable :: my_dtg(:),   my_rcpt(:)
        character (len=7),  allocatable :: my_sign(:)
        integer,            allocatable :: my_lt(:),    my_ls(:),    my_tmp_typ(:), my_sal_typ(:)
        real,               allocatable :: my_lon(:),   my_lat(:),   my_btm(:)
        real,               allocatable :: my_lvl(:,:), my_tmp(:,:), my_sal(:,:)

!
!.......................................................................
!
! initialize supplemental variables
!
        do i = 1, n_obs
          ob_id(i) = '          '
          do j = 1, n_lvl
            ob_sal_xvl(j,i) = iMissing
            ob_sal_xsd(j,i) = iMissing
            ob_tmp_xvl(j,i) = iMissing
            ob_tmp_xsd(j,i) = iMissing
          enddo
        enddo

        print *, ' '
        print *, 'rd_wr_prof: read_prof, Reading Profiles...'
!
! read profile variables
!
        read (UNIT) (ob_btm(i),     i = 1, n_obs)
        read (UNIT) (ob_lat(i),     i = 1, n_obs)
        read (UNIT) (ob_lon(i),     i = 1, n_obs)
        read (UNIT) (ob_ls(i),      i = 1, n_obs)
        read (UNIT) (ob_lt(i),      i = 1, n_obs)
        read (UNIT) (ob_ssh(i),     i = 1, n_obs)
        read (UNIT) (ob_sst(i),     i = 1, n_obs)
        read (UNIT) (ob_sal_typ(i), i = 1, n_obs)
        read (UNIT) (ob_sal_qc(i),  i = 1, n_obs)
        read (UNIT) (ob_tmp_typ(i), i = 1, n_obs)
        read (UNIT) (ob_tmp_qc(i),  i = 1, n_obs)

        do i = 1, n_obs
          read (UNIT) (ob_lvl(j,i),     j = 1, ob_lt(i))
          read (UNIT) (ob_sal(j,i),     j = 1, ob_lt(i))
          read (UNIT) (ob_sal_err(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_sal_prb(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_tmp(j,i),     j = 1, ob_lt(i))
          read (UNIT) (ob_tmp_err(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_tmp_prb(j,i), j = 1, ob_lt(i))
        enddo

        read (UNIT) (ob_dtg(i),  i = 1, n_obs)
        read (UNIT) (ob_rcpt(i), i = 1, n_obs)
        read (UNIT) (ob_scr(i),  i = 1, n_obs)
        read (UNIT) (ob_sign(i), i = 1, n_obs)

        do i = 1, n_obs
          read (UNIT) (ob_clm_sal(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_clm_tmp(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_clm_ssd(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_clm_tsd(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_glb_sal(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_glb_tmp(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_glb_ssd(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_glb_tsd(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_mds_sal(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_mds_tmp(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_rgn_sal(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_rgn_tmp(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_rgn_ssd(j,i), j = 1, ob_lt(i))
          read (UNIT) (ob_rgn_tsd(j,i), j = 1, ob_lt(i))
        enddo

        if (vrsn .gt. 1) then
         do i = 1, n_obs
           read (UNIT) (ob_sal_xvl(j,i), j = 1, ob_lt(i))
           read (UNIT) (ob_sal_xsd(j,i), j = 1, ob_lt(i))
           read (UNIT) (ob_tmp_xvl(j,i), j = 1, ob_lt(i))
           read (UNIT) (ob_tmp_xsd(j,i), j = 1, ob_lt(i))
         enddo
         if (vrsn .gt. 2) then
           read (UNIT) (ob_id(i), i = 1, n_obs)
         endif
        endif
!
        print *, 'rd_wr_prof: read_prof, Finished Reading Profiles.'
        print *, ' '

!
! write out- based on: output_format
!
        if (output_format == 'fnmoc') then
          output_file_name = 'report.' // trim(obs_typ) // '_' // trim(file_dtg) // '.txt'

          call write_fmt_fnmoc (output_file_name, n_obs, n_lvl,                               &
                                ob_dtg, ob_rcpt, ob_sign, ob_scr,                             &
                                ob_ls, ob_lt, ob_sal_typ, ob_tmp_typ,                         &
                                ob_btm, ob_lat, ob_lon, ob_ssh, ob_sst, ob_sal_qc, ob_tmp_qc, &
                                ob_lvl, ob_sal, ob_tmp, ob_clm_sal, ob_clm_ssd, ob_clm_tmp, ob_clm_tsd, &
                                ob_glb_sal, ob_glb_ssd, ob_glb_tmp, ob_glb_tsd,               &
                                ob_mds_sal, ob_mds_tmp, ob_rgn_sal, ob_rgn_ssd,               &
                                ob_rgn_tmp, ob_rgn_tsd, ob_sal_err, ob_sal_prb,               &
                                ob_sal_xvl, ob_sal_xsd, ob_tmp_err, ob_tmp_prb,               &
                                ob_tmp_xvl, ob_tmp_xsd)

        else if (output_format == 'netcdf') then
          output_file_name = 'report.' // trim(obs_typ) // '_' // trim(file_dtg) // '.nc'

          if (subType > 0) then
            my_nobs = count( ob_tmp_typ == subType)
            print*, 'Num of: ', data_lbl(subType), ' = ', my_nobs
          else                           ! NO subsetting, ALL observations are to be written out to netcdf file
            my_nobs = n_obs
          end if

          if (my_nobs > 0) then
            allocate( my_dtg    (my_nobs))
            allocate( my_rcpt   (my_nobs)) 
            allocate( my_sign   (my_nobs)) 

            allocate( my_lt     (my_nobs)) 
            allocate( my_ls     (my_nobs)) 
            allocate( my_tmp_typ(my_nobs)) 
            allocate( my_sal_typ(my_nobs)) 

            allocate( my_lon    (my_nobs)) 
            allocate( my_lat    (my_nobs)) 
            allocate( my_btm    (my_nobs)) 

            allocate( my_lvl    (n_lvl, my_nobs)) 
            allocate( my_tmp    (n_lvl, my_nobs)) 
            allocate( my_sal    (n_lvl, my_nobs)) 
          else
            print *, 'Found ', my_nobs, ' of the sub- type asked for. STOPPING!'
            stop
          end if
!
! subset for the asked sub type (subType)
!
          if (subType > 0) then

            call subset_obs_type(n_obs, n_lvl, my_nobs,                                    &
                                 ob_sign, ob_lon, ob_lat, ob_dtg, ob_rcpt, ob_btm, ob_lvl, &
                                 ob_lt, ob_ls, ob_tmp_typ, ob_sal_typ, ob_tmp, ob_sal,     &
                                 subType,                                               &
                                 my_sign, my_lon, my_lat, my_dtg, my_rcpt, my_btm, my_lvl, &
                                 my_lt, my_ls, my_tmp_typ, my_sal_typ, my_tmp, my_sal)
          else                           ! write all observation types; no subsetting

            my_dtg = ob_dtg; my_rcpt = ob_rcpt; my_sign    = ob_sign 
            my_lt  = ob_lt;   my_ls  = ob_ls;   my_tmp_typ = ob_tmp_typ; my_sal_typ = ob_sal_typ
            my_lon = ob_lon;  my_lat = ob_lat;  my_btm     = ob_btm
            my_lvl = ob_lvl;  my_tmp = ob_tmp;  my_sal     = ob_sal
          end if
!
! write out to a netcdf file
!
          call write_nc_prof (output_file_name, my_nobs, n_lvl, my_sign,                    &
                              my_tmp_typ, my_sal_typ, my_lon, my_lat, my_lvl, my_lt, my_ls, &
                              my_dtg, my_rcpt, my_btm,                                      &
                              my_tmp, my_sal)

          if(allocated( my_dtg )) deallocate(my_dtg)
          if(allocated( my_rcpt)) deallocate(my_rcpt)
          if(allocated( my_sign)) deallocate(my_sign)

          if(allocated( my_lt)) deallocate(my_lt)
          if(allocated( my_ls)) deallocate(my_ls)

          if(allocated( my_tmp_typ)) deallocate(my_tmp_typ)
          if(allocated( my_sal_typ)) deallocate(my_sal_typ)

          if(allocated( my_lon)) deallocate(my_lon)
          if(allocated( my_lat)) deallocate(my_lat)
          if(allocated( my_btm)) deallocate(my_btm)

          if(allocated( my_lvl)) deallocate(my_lvl)
          if(allocated( my_tmp)) deallocate(my_tmp)
          if(allocated( my_sign))deallocate(my_sal)
        endif ! if (output_format == 'fnmoc')

      end subroutine read_write_prof
!.......................................................................
!
      subroutine subset_obs_type(n_obs, n_lvl, my_nobs,                                    &
                                 ob_sign, ob_lon, ob_lat, ob_dtg, ob_rcpt, ob_btm, ob_lvl, &
                                 ob_lt, ob_ls, ob_tmp_typ, ob_sal_typ, ob_tmp, ob_sal,     &
                                 subType,                                               &
                                 my_sign, my_lon, my_lat, my_dtg, my_rcpt, my_btm, my_lvl, &
                                 my_lt, my_ls, my_tmp_typ, my_sal_typ, my_tmp, my_sal)

        implicit none

        integer,                              intent(in) :: n_obs, n_lvl, my_nobs, subType
        character (len=12), dimension(n_obs), intent(in) :: ob_dtg, ob_rcpt
        character (len=7),  dimension(n_obs), intent(in) :: ob_sign
        integer, dimension(n_obs),            intent(in) :: ob_lt,  ob_ls,  ob_tmp_typ, ob_sal_typ
        real,    dimension(n_obs),            intent(in) :: ob_lon, ob_lat, ob_btm
        real,    dimension(n_lvl, n_obs),     intent(in) :: ob_lvl, ob_tmp, ob_sal

        character (len=12), dimension(my_nobs),        intent(inout) :: my_dtg, my_rcpt
        character (len=7),  dimension(my_nobs),        intent(inout) :: my_sign
        integer,            dimension(my_nobs),        intent(inout) :: my_lt,  my_ls,  my_tmp_typ, my_sal_typ
        real,               dimension(my_nobs),        intent(inout) :: my_lon, my_lat, my_btm
        real,               dimension(n_lvl, my_nobs), intent(inout) :: my_lvl, my_tmp, my_sal
!
! local variables
!
        integer :: i, n
!.......................................................................

        n = 0
        do i = 1, n_obs
            if ( ob_tmp_typ(i) == subType) then  ! ob_sal_typ? like: subType_T and subType_S??
            n = n + 1

            if (n > my_nobs) then
              print*, 'Error in subsetting obs for the chosen sub-type. STOPPING!'
              stop
            end if

            my_dtg (n)    = ob_dtg (i)
            my_rcpt(n)    = ob_rcpt(i)          
            my_sign(n)    = ob_sign(i) 

            my_lt     (n) = ob_lt     (i)
            my_ls     (n) = ob_ls     (i)
            my_tmp_typ(n) = ob_tmp_typ(i)
            my_sal_typ(n) = ob_sal_typ(i)
           
            my_lon (n)    = ob_lon(i)
            my_lat (n)    = ob_lat(i)
            my_btm (n)    = ob_btm(i)

            my_lvl (:, n) = ob_lvl(:, i)
            my_tmp (:, n) = ob_tmp(:, i)
            my_sal (:, n) = ob_sal(:, i)
          end if
        end do        
!.......................................................................
!
      end subroutine subset_obs_type
!.......................................................................
!
! Keep ability to write out in FNMOC format for backward compatibility and 
! to be able to check with FNMOC ascii output in future
!
      subroutine write_fmt_fnmoc (output_file_name, n_obs, n_lvl,                               &
                                  ob_dtg, ob_rcpt, ob_sign, ob_scr,                             &
                                  ob_ls, ob_lt, ob_sal_typ, ob_tmp_typ,                         &
                                  ob_btm, ob_lat, ob_lon, ob_ssh, ob_sst, ob_sal_qc, ob_tmp_qc, &
                                  ob_lvl, ob_sal, ob_tmp, ob_clm_sal, ob_clm_ssd, ob_clm_tmp, ob_clm_tsd, &
                                  ob_glb_sal, ob_glb_ssd, ob_glb_tmp, ob_glb_tsd,               &
                                  ob_mds_sal, ob_mds_tmp, ob_rgn_sal, ob_rgn_ssd,               &
                                  ob_rgn_tmp, ob_rgn_tsd, ob_sal_err, ob_sal_prb,               &
                                  ob_sal_xvl, ob_sal_xsd, ob_tmp_err, ob_tmp_prb,               &
                                  ob_tmp_xvl, ob_tmp_xsd)
        implicit none

        include 'ocn_obs_types.h'

        character (len=256), intent(in) :: output_file_name
        integer,             intent(in) :: n_obs, n_lvl
!
! local variables
!
        character (len=12), intent(in)  :: ob_dtg (n_obs), ob_rcpt(n_obs)
        character (len=7),  intent(in)  :: ob_sign(n_obs)
        character (len=1),  intent(in)  :: ob_scr (n_obs)

        integer, dimension(n_obs), intent(in) :: ob_ls, ob_lt, ob_sal_typ, ob_tmp_typ
        real,    dimension(n_obs), intent(in) :: ob_btm, ob_lat, ob_lon, ob_ssh, ob_sst, ob_sal_qc, ob_tmp_qc

        real,    dimension(n_lvl, n_obs), intent(in) :: ob_lvl, ob_sal, ob_tmp,             &
                                            ob_clm_sal, ob_clm_ssd, ob_clm_tmp, ob_clm_tsd, &
                                            ob_glb_sal, ob_glb_ssd, ob_glb_tmp, ob_glb_tsd, &
                                            ob_mds_sal, ob_mds_tmp, ob_rgn_sal, ob_rgn_ssd, &
                                            ob_rgn_tmp, ob_rgn_tsd, ob_sal_err, ob_sal_prb, &
                                            ob_sal_xvl, ob_sal_xsd, ob_tmp_err, ob_tmp_prb, &
                                            ob_tmp_xvl, ob_tmp_xsd

        integer                         :: i, j, n
!
!.......................................................................
!
        print *, ' '
        print *, 'rd_wr_prof: write_fmt_fnmoc, Writing Profiles in FNMOC format to: ', output_file_name

        open (45, file=output_file_name, status='unknown', form='formatted')

        n = 0
        do i = 1, n_obs
          n = n + 1
          write (45, '(110(''-''))')
          write (45, '(''profile number in file      : '', i12)')       n
          write (45, '(''profile call sign           : "'', a, ''"'')') ob_sign(i)
          write (45, '(''profile latitude            : '', f12.2)')     ob_lat(i)
          write (45, '(''profile longitude           : '', f12.2)')     ob_lon(i)
          write (45, '(''profile observed DTG        : "'', a, ''"'')') ob_dtg(i)
          write (45, '(''profile received DTG        : "'', a, ''"'')') ob_rcpt(i)
          write (45, '(''DBDBV bottom depth          : '', f12.1)')     ob_btm(i)
          write (45, '(''profile data type codes     : '', 2i6)')       ob_tmp_typ(i), ob_sal_typ(i)
          write (45, '(''temp data type              : "'', a, ''"'')') data_lbl(ob_tmp_typ(i))
          write (45, '(''salt data type              : "'', a, ''"'')') data_lbl(ob_sal_typ(i))
          write (45, '(''observed temperature levels : '', i12)')       ob_lt(i)
          write (45, '(''observed salinity levels    : '', i12)')       ob_ls(i)
          write (45, '(''temperature gross error     : '', f12.4)')     ob_tmp_qc(i)
          write (45, '(''salinity gross error        : '', f12.4)')     ob_sal_qc(i)
          write (45, '(''sea surface height anomaly  : '', f12.4)')     ob_ssh(i)
          write (45, '(''sea surface temperature     : '', f12.2)')     ob_sst(i)
          write (45, '(''security classification     : '', 9x,           ''"'', a, ''"'')') ob_scr(i)

! write profile temperature
          write (45, '(5x,''depth'',   6x,''temp'', 3x,''clm_std'', 3x,''tmp_err'', 3x,''tmp_prb'', 3x,''clm_tmp'', 3x,''mds_tmp'', 3x,''glb_tmp'', 3x,''rgn_tmp'', 3x,''glb_std'', 3x,''rgn_std'', 3x,''tmp_xvl'', 3x,''tmp_xsd'')')

          do j = 1, ob_lt(i)
            write (45, '(f10.1, 3f10.2, f10.3, 8f10.2)')    &
                 ob_lvl(j,i), ob_tmp(j,i), ob_clm_tsd(j,i), ob_tmp_err(j,i), ob_tmp_prb(j,i), &
                 ob_clm_tmp(j,i), ob_mds_tmp(j,i),          ob_glb_tmp(j,i), ob_rgn_tmp(j,i), &
                 ob_glb_tsd(j,i), ob_rgn_tsd(j,i),          ob_tmp_xvl(j,i), ob_tmp_xsd(j,i)
          end do

! write profile salinity (if its there)
          if (ob_ls(i) .gt. 0) then
            write (45, '(5x,''depth'', 6x,''salt'', 3x,''clm_std'', 3x,''sal_err'', 3x,''sal_prb'', 3x,''clm_sal'', 3x,''mds_sal'', 3x,''glb_sal'', 3x,''rgn_sal'', 3x,''glb_std'', 3x,''rgn_std'', 3x,''sal_xvl'', 3x,''sal_xsd'')')

            do j = 1, ob_lt(i)
              write (45, '(f10.1, 3f10.2, f10.3, 8f10.2)')      &
                    ob_lvl(j,i), ob_sal(j,i), ob_clm_ssd(j,i),  ob_sal_err(j,i), ob_sal_prb(j,i),           &
                    ob_clm_sal(j,i), ob_mds_sal(j,i),           ob_glb_sal(j,i), ob_rgn_sal(j,i),           &
                    ob_glb_ssd(j,i), ob_rgn_ssd(j,i),           ob_sal_xvl(j,i), ob_sal_xsd(j,i)
            enddo
          endif ! if (ob_ls(i) .gt. 0)
        end do  ! i = 1, n_obs

        close(45)
!
!.......................................................................
!
        print *, 'rd_wr_prof: write_fmt_fnmoc, Finished Writing Profiles in FNMOC format.'
        print *, ' '
      end subroutine write_fmt_fnmoc
!.......................................................................
end module rd_wr_prof
