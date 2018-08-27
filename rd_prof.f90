      subroutine rd_prof (UNIT, obs_typ, sub_typ, file_dtg, out_fmt, n_obs, n_lvl, vrsn)
!
!.............................START PROLOGUE............................
!
! MODULE NAME:  rd_prof
!
! DESCRIPTION:  reads the PROFILE ocean obs files and produces a report
!
! PARAMETERS:
!      Name          Type        Usage            Description
!   ----------     ---------     ------    ---------------------------
!   obs_typ         char         input     observation type
!   sub_typ         char         input     observation sub- type
!   file_dtg        char         input     date time group (YYYYMMDDHH)
!   out_fmt         char         input     output format
!   n_obs           integer      input     number profile obs
!   n_lvl           integer      input     number profile levels
!   unit            integer      input     FORTRAN unit number
!   vrsn            integer      input     version number data file
!
! PROFILE VARIABLES:
!    Name       Type                     Description
!   --------  --------    ----------------------------------------------
!   ob_btm     real       bottom depth in meters from DBDBV data base
!                         at profile lat,lon
!   ob_clm_sal real       GDEM 3.0 salinity climatology estimate at
!                         profile location, levels and sampling time
!   ob_clm_ssd real       GDEM 3.0 climate salinitiy variability
!   ob_clm_tmp real       GDEM 3.0 temperature climatology estimate at
!                         profile location, levels and sampling time
!   ob_clm_tsd real       GDEM 3.0 climate temperature variability
!   ob_dtg     character  profile observation sampling date time group
!                         in the form  year, month, day, hour, minute,
!                         second (YYYYMMDDHHMMSS)
!   ob_glb_sal real       global analysis estimate of profile
!                         salinities at profile obs location,
!                         levels, and sampling time
!   ob_glb_ssd real       global analysis salinity errors
!   ob_glb_tmp real       global analysis estimate of profile
!                         temperatures at profile obs location,
!                         levels, and sampling time
!   ob_glb_tsd real       global analysis temperature errors
!   ob_id      character  unique identifier of profile observation
!                         at FNMOC this is the CRC number computed
!                         from the WMO message
!                         at NAVO this is a home-grown number that 
!                         has no meaning to the rest of world 
!   ob_lat     real       profile observation latitude (south negative)
!   ob_lon     real       profile observation longitude (west negative)
!   ob_ls      integer    number of observed profile salinity levels
!                         (a zero indicates temperature-only profile)
!   ob_lt      integer    number of observed profile temperature levels
!   ob_lvl     real       observed profile levels
!   ob_mds_sal real       modas synthetic salinity profile estimate at
!                         profile location, levels, and sampling time
!                         based on ob_mds_tmp or ob_tmp predictors
!   ob_mds_tmp real       modas synthetic temperature profile estimate
!                         at profile location, levels, and sampling
!                         time.  the predictor variables used in the
!                         generation of the modas synthetic profile
!                         are the ob_sst (SST) and ob_ssh (SSHA)
!                         variables.
!   ob_rcpt    character  profile observation receipt time at FNMOC in
!                         the form year, month, day, hour, minute
!                         (YYYYMMDDHHMM); the difference between
!                         ob_rcpt and ob_dtg gives the timeliness
!                         of the observation at FNMOC
!   ob_rgn_sal real       regional analysis estimate of profile
!                         salinities at profile obs location,
!                         levels, and sampling time
!   ob_rgn_ssd real       regional analysis salinity errors
!   ob_rgn_tmp real       regional analysis estimate of profile
!                         temperatures at profile obs location,
!                         levels, and sampling time
!   ob_rgn_tsd real       regional analysis temperature errors
!   ob_sal     real       observed  profile salinities, if salinity has
!                         not been observed it has been estimated from
!                         climatological T/S regressions
!   ob_sal_err real       salinity observation errors (use with
!                         caution, reported values are experimental)
!   ob_sal_prb real       salinity profile level-by-level probability
!                         of a gross error
!   ob_sal_qc  real       salinity profile overall probability of gross
!                         error (integrates level-by-level errors taking
!                         into account layer thicknesses)
!   ob_sal_std real       climatolgical estimates of variability of
!                         salinity at profile location, levels and
!                         sampling time (one standard deviation)
!   ob_sal_typ integer    profile salinity data type (see ocean_types.h
!                         for codes)
!   ob_sal_xvl real       salinity profile from cross validation
!                         (GDEM 3.0 climate profile in absence of
!                         near-by data)
!   ob_sal_xsd real       salinity cross validation profile error
!                         (based on error reduction of GDEM 3.0 climate
!                         variability)
!   ob_scr     character  profile obs security classification code; "U"
!                         for unclassified
!   ob_sgn     character  profile observation call sign
!   ob_ssh     real       SSHA of profile dynamic height from long-term
!                         hydrographic mean.  dynamic height has been
!                         calculated relative to 2000 m or the bottom
!                         whichever is shallower.  the profile may have
!                         been vertically extended in the dynamic height
!                         computation, so the ob_ssh values must be used
!                         with care for profiles with shallow maximum
!                         observation depths.
!   ob_sst     real       SST estimate (in order of high resoloution
!                         regional analysis if available, global
!                         analysis if available, profile SST if
!                         observed shallow enough or SST climatology
!                         (MODAS or GDEM)) valid at profile observation
!                         location and sampling time
!   ob_tmp     real       observed profile temperatures
!   ob_tmp_err real       temperature observation errors (use with
!                         caution, reported values are experimental)
!   ob_tmp_prb real       temperature profile level-by-level probability
!                         of a gross error
!   ob_tmp_qc  real       temperature profile overall probability of
!                         gross error (integrates level-by-level errors
!                         taking into account layer thicknesses)
!   ob_tmp_tsd real       climatolgical estimates of variability of
!                         temperature at profile location, levels and
!                         sampling time (one standard deviation)
!   ob_tmp_typ integer    profile temperature data type (see
!                         ocean_types.h for codes)
!   ob_tmp_xvl real       temperature profile from cross validation
!                         (GDEM 3.0 climate profile in absence of
!                         near-by data)
!   ob_tmp_xsd real       temperature cross validation profile error
!                         (based on error reduction of GDEM 3.0 climate
!                         variability)
!
!..............................END PROLOGUE.............................
!
      implicit none
!
      include 'ocn_obs_types.h'
!
! local array dimensions
!
      character output_file_name * 256

      integer   n_obs
      character obs_typ          * 9
      character sub_typ          * 9
      character file_dtg         * 10
      character out_fmt          * 9
      integer   n_lvl
!
      character err_msg * 256

      integer   i, j, n
      real      ob_btm (n_obs)
      real      ob_clm_sal (n_lvl, n_obs)
      real      ob_clm_ssd (n_lvl, n_obs)
      real      ob_clm_tmp (n_lvl, n_obs)
      real      ob_clm_tsd (n_lvl, n_obs)
      character ob_dtg (n_obs) * 12
      real      ob_glb_sal (n_lvl, n_obs)
      real      ob_glb_ssd (n_lvl, n_obs)
      real      ob_glb_tmp (n_lvl, n_obs)
      real      ob_glb_tsd (n_lvl, n_obs)
      character ob_id (n_obs) * 10
      real      ob_lat (n_obs)
      real      ob_lon (n_obs)
      real      ob_lvl (n_lvl, n_obs)
      integer   ob_ls (n_obs)
      integer   ob_lt (n_obs)
      real      ob_mds_sal (n_lvl, n_obs)
      real      ob_mds_tmp (n_lvl, n_obs)
      character ob_rcpt (n_obs) * 12
      real      ob_rgn_sal (n_lvl, n_obs)
      real      ob_rgn_ssd (n_lvl, n_obs)
      real      ob_rgn_tmp (n_lvl, n_obs)
      real      ob_rgn_tsd (n_lvl, n_obs)
      character ob_scr (n_obs) * 1
      character ob_sign (n_obs) * 7
      real      ob_sal (n_lvl, n_obs)
      real      ob_sal_err (n_lvl, n_obs)
      real      ob_sal_prb (n_lvl, n_obs)
      real      ob_sal_qc (n_obs)
      integer   ob_sal_typ (n_obs)
      real      ob_sal_xvl (n_lvl, n_obs)
      real      ob_sal_xsd (n_lvl, n_obs)
      real      ob_ssh (n_obs)
      real      ob_sst (n_obs)
      real      ob_tmp (n_lvl, n_obs)
      real      ob_tmp_err (n_lvl, n_obs)
      real      ob_tmp_prb (n_lvl, n_obs)
      real      ob_tmp_qc (n_obs)
      integer   ob_tmp_typ (n_obs)
      real      ob_tmp_xvl (n_lvl, n_obs)
      real      ob_tmp_xsd (n_lvl, n_obs)
      integer   UNIT
      integer   vrsn
!
!...............................executable..............................
!
! initialize supplemental variables
!
      do i = 1, n_obs
         ob_id(i) = '          '
         do j = 1, n_lvl
            ob_sal_xvl(j,i) = -999.
            ob_sal_xsd(j,i) = -999.
            ob_tmp_xvl(j,i) = -999.
            ob_tmp_xsd(j,i) = -999.
         enddo
      enddo
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
! produce profile report
!
      if (out_fmt == 'fnmoc') then 
!       call __write_prof_fnmoc_fmt(...)
      else if(out_fmt == 'nc') then
!       call __...__
      else if(out_fmt == 'bufr') then
!       call __...__
      else
        write (err_msg, '(''Unsupported output format'')')
        call error_exit ('OCN_OBS: rd_prof', err_msg)
      end if
 
      output_file_name = &
      'report.' // trim(obs_typ) // '.' // trim(file_dtg)

      open (45, file=output_file_name, status='unknown', form='formatted')

      n = 0
      do i = 1, n_obs
         n = n + 1
! from ocn_types.h, if ob_tmp_typ == 36, it is "Argo Float Temperature (C)"
!        if ( (ob_tmp_typ(i) == 36) .OR. (ob_sal_typ(i) == 37)) then 
!          print*, ob_tmp_typ(i)
           write (45, '(110(''-''))')
           write (45, '(''profile number in file      : '', i12)') n
           write (45, '(''profile call sign           : "'', a, ''"'')')   &
                ob_sign(i)
           write (45, '(''profile latitude            : '', f12.2)')       &
                ob_lat(i)
           write (45, '(''profile longitude           : '', f12.2)')       &
                ob_lon(i)
           write (45, '(''profile observed DTG        : "'', a, ''"'')')   &
                ob_dtg(i)
           write (45, '(''profile received DTG        : "'', a, ''"'')')   &
                ob_rcpt(i)
           write (45, '(''DBDBV bottom depth          : '', f12.1)')       &
                ob_btm(i)
           write (45, '(''profile data type codes     : '', 2i6)')         &
                ob_tmp_typ(i), ob_sal_typ(i)
           write (45, '(''temp data type              : "'', a, ''"'')')   &
                data_lbl(ob_tmp_typ(i))
           write (45, '(''salt data type              : "'', a, ''"'')')   &
                data_lbl(ob_sal_typ(i))
           write (45, '(''observed temperature levels : '', i12)')         &
                ob_lt(i)
           write (45, '(''observed salinity levels    : '', i12)')         &
                ob_ls(i)
           write (45, '(''temperature gross error     : '', f12.4)')       &
                ob_tmp_qc(i)
           write (45, '(''salinity gross error        : '', f12.4)')       &
                ob_sal_qc(i)
           write (45, '(''sea surface height anomaly  : '', f12.4)')       &
                ob_ssh(i)
           write (45, '(''sea surface temperature     : '', f12.2)')       &
                ob_sst(i)
           write (45, '(''security classification     : '', 9x,            &
                ''"'', a, ''"'')') ob_scr(i)
           write (45, '(5x,''depth'',   6x,''temp'', 3x,''clm_std'', 3x,''tmp_err'', 3x,''tmp_prb'', 3x,''clm_tmp'', 3x,''mds_tmp'', 3x,''glb_tmp'', 3x,''rgn_tmp'', 3x,''glb_std'', 3x,''rgn_std'', 3x,''tmp_xvl'', 3x,''tmp_xsd'')')
           do j = 1, ob_lt(i)
             write (45, '(f10.1, 3f10.2, f10.3, 8f10.2)')     &
                   ob_lvl(j,i), ob_tmp(j,i), ob_clm_tsd(j,i), &
                   ob_tmp_err(j,i), ob_tmp_prb(j,i),          &
                   ob_clm_tmp(j,i), ob_mds_tmp(j,i),          &
                   ob_glb_tmp(j,i), ob_rgn_tmp(j,i),          &
                   ob_glb_tsd(j,i), ob_rgn_tsd(j,i),          &
                   ob_tmp_xvl(j,i), ob_tmp_xsd(j,i)           
           enddo
           if (ob_ls(i) .gt. 0) then
             write (45, '(5x,''depth'', 6x,''salt'',     &
                         3x,''clm_std'', 3x,''sal_err'', &
                         3x,''sal_prb'', 3x,''clm_sal'', &
                         3x,''mds_sal'', 3x,''glb_sal'', &
                         3x,''rgn_sal'', 3x,''glb_std'', &
                         3x,''rgn_std'', 3x,''sal_xvl'', &
                         3x,''sal_xsd'')')
             do j = 1, ob_lt(i)
               write (45, '(f10.1, 3f10.2, f10.3, 8f10.2)')       &
                      ob_lvl(j,i), ob_sal(j,i), ob_clm_ssd(j,i),  &
                      ob_sal_err(j,i), ob_sal_prb(j,i),           &
                      ob_clm_sal(j,i), ob_mds_sal(j,i),           &
                      ob_glb_sal(j,i), ob_rgn_sal(j,i),           &
                      ob_glb_ssd(j,i), ob_rgn_ssd(j,i),           &
                      ob_sal_xvl(j,i), ob_sal_xsd(j,i)            
             enddo
           endif
!        endif
      enddo
!
      return
      end subroutine rd_prof
