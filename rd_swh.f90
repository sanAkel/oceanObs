      subroutine rd_swh (UNIT, obs_typ, sub_typ, file_dtg, out_fmt, n_obs, vrsn)
!
!.............................START PROLOGUE............................
!
! MODULE NAME:  rd_swh
!
! DESCRIPTION:  reads the altimeter SWH ocean obs files and produces a
!               report whether you want one or not
!
! NOTES:        the qc probablity of error includes flags indicating
!               swh retrieval in ice covered seas and/or shallow
!               water.  a value of 510 is added to the underlying
!               probability to indicate if the ice concentration
!               exceeds 33% and a value of 512 is added if the
!               bottom depth is less than 5 meters.  note that a
!               composite flag of 510 + 512 can also occur.
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
!   ob_clm     real       SWH climate (not available)
!   ob_dtg     character  SWH retrieval date time group in the form
!                         year, month, day, hour, minute, second
!                         (YYYYMMDDHHMMSS)
!   ob_glb     real       SWH global FNMOC analysis estimate at the
!                         obs location and sampling time
!   ob_lat     real       SWH retrieval latitude (south negative)
!   ob_lon     real       SWH retrieval longitude (west negative)
!   ob_qc      real       SWH retrieval probability of a gross error
!                         (assumes normal pdf of SWH retrieval errors)
!   ob_rcpt    character  SWH retrieval FNMOC receipt date time group
!                         in the form year, month, day, hour, minute,
!                         second (YYYYMMDDHHMMSS)
!   ob_rgn     real       SWH regional FNMOC analysis estimate at the
!                         obs location and sampling time
!   ob_swh     real       SWH retrieval (m)
!   ob_typ     integer    satellite ID (ERS2, Topex, Jason, GFO,
!                         ENVISAT, Topex Interleaved); see ocn_types.h
!                         for codes
!   ob_xvl     real       cross validation SWH value from QC
!   ob_wnd     real       altimeter colocated wind retrieval (m/s)
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
      integer   i, k
      real      ob_age (n_obs)
      real      ob_clm (n_obs)
      character ob_dtg (n_obs) * 14
      real      ob_glb (n_obs)
      real      ob_lat (n_obs)
      real      ob_lon (n_obs)
      real      ob_qc (n_obs)
      character ob_rcp (n_obs) * 14
      real      ob_rgn (n_obs)
      real      ob_swh (n_obs)
      integer   ob_typ (n_obs)
      real      ob_xvl (n_obs)
      real      ob_wnd (n_obs)
      integer   UNIT
      integer   vrsn
!
!...............................executable..............................
!
! read swh variables
!
      read (UNIT) (ob_glb(i), i = 1, n_obs)
      read (UNIT) (ob_lat(i), i = 1, n_obs)
      read (UNIT) (ob_lon(i), i = 1, n_obs)
      read (UNIT) (ob_age(i), i = 1, n_obs)
      read (UNIT) (ob_clm(i), i = 1, n_obs)
      read (UNIT) (ob_qc(i),  i = 1, n_obs)
      read (UNIT) (ob_typ(i), i = 1, n_obs)
      read (UNIT) (ob_rgn(i), i = 1, n_obs)
      read (UNIT) (ob_swh(i), i = 1, n_obs)
      read (UNIT) (ob_wnd(i), i = 1, n_obs)
      read (UNIT) (ob_xvl(i), i = 1, n_obs)
      read (UNIT) (ob_dtg(i), i = 1, n_obs)
      read (UNIT) (ob_rcp(i), i = 1, n_obs)
!
! produce swh report
!
      output_file_name = &
      'report.' // trim(obs_typ) // '.' // trim(file_dtg)

      open (45, file=output_file_name, status='unknown', form='formatted')

      k = 1
      write (45, '(''  reporting skip factor: '', i10)') k
      write (45, '(11x,''dtg'', 11x,''rcpt'', 5x,''lat'', 5x,''lon'',  &
                   4x,''type'', 5x,''swh'', 4x,''wind'', 4x,''clim'',  &
                   4x,''glbl'', 4x,''regn'', 4x,''xval'', 8x,''qc'')')
      do i = 1, n_obs, k
         write (45, '(a,1x,a,2f8.2,i8,6f8.1,2x,f8.3,2x,a)')  &
                ob_dtg(i), ob_rcp(i), ob_lat(i), ob_lon(i),  &
                ob_typ(i), ob_swh(i), ob_wnd(i), ob_clm(i),  &
                ob_glb(i), ob_rgn(i), ob_xvl(i), ob_qc(i),  &
                data_lbl(ob_typ(i))
      enddo
!
      return
      end subroutine rd_swh
