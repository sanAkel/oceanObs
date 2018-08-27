      subroutine rd_trak (UNIT, obs_typ, sub_typ, file_dtg, out_fmt, n_obs, vrsn)
!
!.............................START PROLOGUE............................
!
! MODULE NAME:  rd_trak
!
! DESCRIPTION:  reads the TRAK ocean obs files and produces a
!               report whether you want one or not
!
! PARAMETERS:
!      Name          Type        Usage            Description
!   ----------     ---------     ------    ---------------------------
!   obs_typ         char         input     observation type
!   sub_typ         char         input     observation sub- type
!   file_dtg        char         input     date time group (YYYYMMDDHH)
!   out_fmt         char         input     output format
!   n_obs           integer      input     number swh obs
!   unit            integer      input     FORTRAN unit number
!   vrsn            integer      input     version number data file
!
! SWH VARIABLES:
!     ame       Type                     Description
!   --------  --------    ----------------------------------------------
!   ob_age     real       age of the observation in hours since
!                         January 1, 1992.  provides a continuous
!                         time variable.  reported to the nearest
!                         minute.
!   ob_csgm    real       SST climate variability
!   ob_csal    real       SSS climate estimate at the obs location
!                         and sampling time
!   ob_csst    real       SST climate estimate at the obs location
!                         and sampling time
!   ob_dtg     character  TRAKOBS retrieval date time group in the form
!                         year, month, day, hour, minute, second
!                         (YYYYMMDDHHMMSS)
!   ob_gsal    real       SSS global analysis estimate at the obs
!                         location and sampling time
!   ob_gsgm    real       SST global analysis variability
!   ob_gsst    real       SST global analysis estimate at the obs
!                         location and sampling time
!   ob_lat     real       TRAKOBS latitude (south negative)
!   ob_lon     real       TRAKOBS longitude (west negative)
!   ob_qc_sal  real       TRAKOBS salinity probability of gross error
!                         (assumes normal pdf of salinity errors)
!   ob_qc_sst  real       TRAKOBS temperature probability of gross error
!                         (assumes normal pdf of temperature errors)
!   ob_qc_vel  real       TRAKOBS velocity probability of gross error
!                         (assumes normal pdf of velocity errors)
!   ob_rcpt    character  SWH retrieval FNMOC receipt date time group
!                         in the form year, month, day, hour, minute,
!                         second (YYYYMMDDHHMMSS)
!   ob_rgn     real       SWH regional analysis estimate at the obs
!                         location and sampling time
!   ob_rsal    real       SSS regional analysis estimate at the obs
!                         location and sampling time
!   ob_rsgm    real       SST regional analysis variability
!   ob_rsst    real       SST regional analysis estimate at the obs
!                         location and sampling time
!   ob_sal     real       TRAKOBS SSS observation
!   ob_scr     character  TRAKOBS security classification code
!   ob_sgn     character  TRAKOBS call sign
!   ob_sst     real       TRAKOBS SST observation
!   ob_typ     integer    type code indicator; see ocn_types.h for codes
!   ob_uuu     real       TRAKOBS u velocity observation
!   ob_vvv     real       TRAKOBS v velocity observation
!   ob_wm      integer    SST water mass indicator from Bayesian
!                         classification scheme.  
!
!..............................END PROLOGUE.............................
!
      implicit none
!
      include 'ocn_obs_types.h'
!
! local array dimension
!
      character output_file_name * 256

      integer   n_obs
      character obs_typ   * 9
      character sub_typ   * 9
      character file_dtg  * 10
      character out_fmt   * 9
!
      integer   i
      real      ob_age (n_obs)
      real      ob_csal (n_obs)
      real      ob_csst (n_obs)
      real      ob_csgm (n_obs)
      character ob_dtg (n_obs) * 12
      real      ob_gsal (n_obs)
      real      ob_gsst (n_obs)
      real      ob_gsgm (n_obs)
      real      ob_lat (n_obs)
      real      ob_lon (n_obs)
      real      ob_qc_sal (n_obs)
      real      ob_qc_sst (n_obs)
      real      ob_qc_vel (n_obs)
      character ob_rcpt (n_obs) * 12
      real      ob_rsal (n_obs)
      real      ob_rsst (n_obs)
      real      ob_rsgm (n_obs)
      real      ob_sal (n_obs)
      character ob_scr (n_obs) * 1
      character ob_sgn (n_obs) * 6
      real      ob_sst (n_obs)
      integer   ob_typ (n_obs)
      real      ob_uuu (n_obs)
      real      ob_vvv (n_obs)
      integer   ob_wm (n_obs)
      integer   UNIT
      integer   vrsn
!
!...............................executable..............................
!
! read trak obs variables
!
      read (UNIT) (ob_wm(i),     i = 1, n_obs)
      read (UNIT) (ob_gsal(i),   i = 1, n_obs)
      read (UNIT) (ob_gsst(i),   i = 1, n_obs)
      read (UNIT) (ob_lat(i),    i = 1, n_obs)
      read (UNIT) (ob_lon(i),    i = 1, n_obs)
      read (UNIT) (ob_age(i),    i = 1, n_obs)
      read (UNIT) (ob_csal(i),   i = 1, n_obs)
      read (UNIT) (ob_csst(i),   i = 1, n_obs)
      read (UNIT) (ob_qc_sal(i), i = 1, n_obs)
      read (UNIT) (ob_qc_sst(i), i = 1, n_obs)
      read (UNIT) (ob_qc_vel(i), i = 1, n_obs)
      read (UNIT) (ob_rsal(i),   i = 1, n_obs)
      read (UNIT) (ob_rsst(i),   i = 1, n_obs)
      read (UNIT) (ob_sal(i),    i = 1, n_obs)
      read (UNIT) (ob_sst(i),    i = 1, n_obs)
      read (UNIT) (ob_typ(i),    i = 1, n_obs)
      read (UNIT) (ob_uuu(i),    i = 1, n_obs)
      read (UNIT) (ob_vvv(i),    i = 1, n_obs)
      read (UNIT) (ob_dtg(i),    i = 1, n_obs)
      read (UNIT) (ob_rcpt(i),   i = 1, n_obs)
      read (UNIT) (ob_scr(i),    i = 1, n_obs)
      read (UNIT) (ob_sgn(i),    i = 1, n_obs)
      read (UNIT) (ob_csgm(i),   i = 1, n_obs)
      read (UNIT) (ob_gsgm(i),   i = 1, n_obs)
      read (UNIT) (ob_rsgm(i),   i = 1, n_obs)
!
! produce trak obs report
!
      output_file_name = &
      'report.' // trim(obs_typ) // '.' // trim(file_dtg)

      open (45, file=output_file_name, status='unknown', form='formatted')

      write (45, '(/, ''Sea Surface Temperature'')')
      write (45, '(9x,''dtg'', 3x,''sign '', 5x,''lat'', 5x,''lon'', &
                   1x,''typ'', 5x,''sst'', 4x,''clim'', 4x,''glbl'', &
                   4x,''regn'', 6x,''qc'', 4x,''csgm'', 4x,''gsgm'', &
                   4x,''rsgm'', 2x,''wm'', 2x,''sc'')')
      do i = 1, n_obs
         write (45, '(a,2x,a,2f8.2,i4,4f8.2,f8.3,3f8.2,i4,3x,a,2x,a)') &
                ob_dtg(i), ob_sgn(i), ob_lat(i), ob_lon(i),            &
                ob_typ(i), ob_sst(i), ob_csst(i), ob_gsst(i),          &
                ob_rsst(i), ob_qc_sst(i), ob_csgm(i), ob_gsgm(i),      &
                ob_rsgm(i), ob_wm(i), ob_scr(i), data_lbl(ob_typ(i))
      enddo
!
      write (45, '(/, ''Sea Surface Salinity'')')
      write (45, '(9x,''dtg'', 3x,''sign '', 5x,''lat'', 5x,''lon'',   &
                   1x,''typ'', 5x,''sal'', 4x,''clim'', 4x,''glbl'',   &
                   4x,''regn'', 6x,''qc'')')
      do i = 1, n_obs
         ob_typ(i) = 35
         write (45, '(a,2x,a,2f8.2,i4,4f8.2,f8.3,34x,a)')      &
                ob_dtg(i), ob_sgn(i), ob_lat(i), ob_lon(i),    &
                ob_typ(i), ob_sal(i), ob_csal(i), ob_gsal(i),  &
                ob_rsal(i), ob_qc_sal(i), data_lbl(ob_typ(i))
      enddo
!
      write (45, '(/, ''Sea Surface U, V Velocity'')')
      write (45, '(9x,''dtg'', 3x,''sign '', 5x,''lat'', 5x,''lon'', &
                   1x,''typ'', 5x,''uuu'', 5x,''vvv'', 22x,''qc'')')
      do i = 1, n_obs
         write (45, '(a,2x,a,2f8.2,i4,2f8.2,16x,f8.3)')      &
                ob_dtg(i), ob_sgn(i), ob_lat(i), ob_lon(i),  &
                ob_typ(i), ob_uuu(i), ob_vvv(i), ob_qc_vel(i)
      enddo
!
      return
      end subroutine rd_trak
