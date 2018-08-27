      subroutine rd_ship (UNIT, obs_typ, sub_typ, file_dtg, out_fmt, n_obs, vrsn)
!
!.............................START PROLOGUE............................
!
! MODULE NAME:  rd_ship
!
! DESCRIPTION:  reads the SHIP ocean obs files and produces a report
!
! PARAMETERS:
!      Name          Type        Usage            Description
!   ----------     ---------     ------    ---------------------------
!   obs_typ         char         input     observation type
!   sub_typ         char         input     observation sub- type
!   file_dtg        char         input     date time group (YYYYMMDDHH)
!   out_fmt         char         input     output format
!   n_obs           integer      input     number ship obs
!   unit            integer      input     FORTRAN unit number
!   vrsn            integer      input     version number data file
!
! MCSST VARIABLES:
!      Name      Type                     Description
!   ----------  -------     -------------------------------------------
!   ob_age      real        age of the observation in hours since
!                           January 1, 1992.  provides a continuous
!                           time variable.  reported to the nearest
!                           minute.
!   ob_clm      real        GDEM SST climatological estimate at obs
!                           location and sampling time
!   ob_csgm     real        GDEM SST climatology variability estimate
!                           at obs location and samling time
!   ob_dtg      character   SST obs date time group in the form year,
!                           month, day, hour, minute (YYYYMMDDHHMM)
!   ob_glb      real        SST global analysis estimate at obs
!                           location and samplingr time
!   ob_gsgm     real        global SST analysis variability estimate
!                           at obs location and sampling time
!   ob_lat      real        SST obs latitude (south negative)
!   ob_lon      real        SST obs longitude (west negative)
!   ob_qc       real        SST obs probability of a gross error
!                           (assumes normal pdf of SST errors)
!   ob_rcpt     character   SST observation receipt time at FNMOC in
!                           the form year, month, day, hour, minute
!                           (YYYYMMDDHHMM); the difference between
!                           ob_rcpt and ob_dtg gives the timeliness 
!                           of the observation and the validity of
!                           ob_glb and ob_rgn background estimates 
!   ob_rgn                  SST regional analysis estimate at obs
!                           location and sampling time
!   ob_rsgm     real        regional SST analysis variability estimate
!                           at obs location and sampling time
!   ob_scr      character   SST obs security classification code; "U"
!                           for unclassified
!   ob_sign     character   SST observation call sign
!   ob_sst      real        SST observation
!   ob_typ      integer     SST obseration data type; ship (ERI, bucket,
!                           hull contact), buoy (fixed, drifting), CMAN
!                           (see ocn_types.h for codes)
!   ob_wm       integer     SST water mass indicator from Bayesian
!                           classification scheme.  
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
      character obs_typ     * 9
      character sub_typ     * 9
      character file_dtg    * 10
      character out_fmt     * 9
!
      integer   i
      real      ob_age (n_obs)
      real      ob_clm (n_obs)
      real      ob_csgm (n_obs)
      character ob_dtg (n_obs) * 12
      real      ob_glb (n_obs)
      real      ob_gsgm (n_obs)
      real      ob_lat (n_obs)
      real      ob_lon (n_obs)
      real      ob_qc (n_obs)
      character ob_rcpt (n_obs) * 12
      real      ob_rgn (n_obs)
      real      ob_rsgm (n_obs)
      character ob_scr (n_obs) * 1
      character ob_sign (n_obs) * 7
      real      ob_sst (n_obs)
      integer   ob_typ (n_obs)
      integer   ob_wm (n_obs)
      integer   UNIT
      integer   vrsn
!
!...............................executable..............................
!
! read ship variables
!
      read (UNIT) (ob_wm(i),   i = 1, n_obs)
      read (UNIT) (ob_glb(i),  i = 1, n_obs)
      read (UNIT) (ob_lat(i),  i = 1, n_obs)
      read (UNIT) (ob_lon(i),  i = 1, n_obs)
      read (UNIT) (ob_age(i),  i = 1, n_obs)
      read (UNIT) (ob_clm(i),  i = 1, n_obs)
      read (UNIT) (ob_qc(i),   i = 1, n_obs)
      read (UNIT) (ob_rgn(i),  i = 1, n_obs)
      read (UNIT) (ob_sst(i),  i = 1, n_obs)
      read (UNIT) (ob_typ(i),  i = 1, n_obs)
      read (UNIT) (ob_dtg(i),  i = 1, n_obs)
      read (UNIT) (ob_rcpt(i), i = 1, n_obs)
      read (UNIT) (ob_scr(i),  i = 1, n_obs)
      if (vrsn .le. 2) then
         ob_sign(:) = " "
         read (UNIT) (ob_sign(i)(1:6), i = 1, n_obs)
      else
         read (UNIT) (ob_sign(i), i = 1, n_obs)
      endif
      if (vrsn .gt. 1) then
         read (UNIT) (ob_csgm(i), i = 1, n_obs)
         read (UNIT) (ob_gsgm(i), i = 1, n_obs)
         read (UNIT) (ob_rsgm(i), i = 1, n_obs)
      else
         do i = 1, n_obs
            ob_csgm(i) = -999.
            ob_gsgm(i) = -999.
            ob_rsgm(i) = -999.
         enddo
      endif
!
! produce ship report
!
      output_file_name = &
      'report.' // trim(obs_typ) // '.' // trim(file_dtg)

      open (45, file=output_file_name, status='unknown', form='formatted')

      write (45, '(9x,''dtg'', 4x,''sign '', 5x,''lat'', 5x,''lon'',  &
                   1x,''typ'', 5x,''sst'', 4x,''clim'', 4x,''glbl'',  &
                   4x,''regn'', 6x,''qc'', 4x,''csgm'', 4x,''gsgm'',  &
                   4x,''rsgm'', 2x,''wm'', 2x,''sc'')')
      do i = 1, n_obs
         write (45, '(a,2x,a,2f8.2,i4,4f8.2,f8.3,3f8.2,i4,3x,a,2x,a)') &
                ob_dtg(i), ob_sign(i), ob_lat(i), ob_lon(i),           &
                ob_typ(i), ob_sst(i), ob_clm(i), ob_glb(i),            &
                ob_rgn(i), ob_qc(i), ob_csgm(i), ob_gsgm(i),           &
                ob_rsgm(i), ob_wm(i), ob_scr(i), data_lbl(ob_typ(i))
      enddo
!
      return
      end subroutine rd_ship
