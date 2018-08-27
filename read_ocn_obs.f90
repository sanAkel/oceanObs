      program read_ocn_obs
      use rd_wr_prof, only: read_write_prof
!
!=============================================================================
!BOP

!=============================================================================

! !DESCRIPTION:  simple driver for processing GODAE server ocean qc obs files
!
!                Downloaded from 
!                \url{http://www.usgodae.org/ftp/outgoing/fnmoc/data/ocn/docs/}
!                Re-formatted and recoded in {\it this} style, 
!                in Feb, 2018 by Santha Akella
!
!                Log from USGODAE:
!                UPDATE DATE:  12 March 2015
!                - Update to sfobs read for 7-character call signs in v3 of the files
!                - Update goes, lac to version 3
!                - Update mcsst to version 4
!                - Update metop, metop_lac to version 2
!
!                File structure:
!                The first record in all of the ocean obs files
!                contains three variables:
!                \begin{enumerate}
!                  \item number of observations in the file
!                  \item maximum number of observations levels in the file
!                  \item version number of file
!                \end{enumerate}
!   
!                Note: 
!                \begin{itemize}
!                  \item 
!                    The maximum number of observation levels can only be $> 1$
!                    for the {dtg}.profile and {dtg}.glider files
!                  \item 
!                    The purpose of the version number is to support new observation 
!                    variables without having to reformat the entire data archive. 
!                    An example of this is the appending of surface wind speed to the
!                    satellite sst retrievals files.
!                  \item 
!                    All data files are written using ieee 32 bit fortran
!                    sequential writes -- BIG ENDIAN
!                    You may need to specify "big_endian" I/O for your
!                    specific compiler.
!                \end{itemize}
!
!                UNITS:                
!                \begin{enumerate}
!                  \item Temperature             : deg C
!                  \item Salinity                : PSU
!                  \item Sea Surface Height      : meters
!                  \item Sea ice concentration   : per cent
!                  \item Significant wave height : meters
!                \end{enumerate}
!
!                MISSING VALUES:
!                Unless otherwise specified, missing values are
!                set to -999 in all of the ocean obs data files
!
!EOP
!=============================================================================
!
!.......................................................................
!
! SUBROUTINES CALLED:
!
!        Name                    Description
!   --------------     -------------------------------------
!
!   error_exit         standard error processing
!   GETARG             retrieve command line argument
!
!   rd_altim           read altimeter observations
!   rd_gldr            read glider profile observations
!   rd_ship            read sfc in situ SST observations
!   rd_ssmi            read SSM/I sea ice observations
!   rd_swh             read altimeter SWH observations
!   rd_trak            read TSG track observations
!
!.......................................................................
!
      implicit  none
!
! set local work file fortran unit number
!
      integer    UNIT
      parameter (UNIT = 60)
!
      character arg             * 256
      character data_dir        * 256
      character file_dtg        * 10
      character input_file_name * 256
      character obs_typ         * 10
      integer   sub_typ
      character output_fmt      * 10
      character err_msg         * 256

      logical   exist
      integer   len_dir
      integer   n_arg
      integer   n_lvl
      integer   n_obs
      integer   n_vrsn
!
! functions
!
      integer   IARGC
!.......................................................................
!
! count number of command line arguments
!
      n_arg = IARGC ()

      if (n_arg < 5) then
       print *, 'READ_OCN_OBS takes following five (5) input arguments:'
       print *, 'data dir '
       print *, 'obs typ       (altim/gldr/profile/ship/ssmi/swh/trak)'
       print *, 'sub typ       (0: ALL/4:MOR/36:ARGO/...)'
       print *, 'date          (YYYYMMDDHH)'
       print *, 'output fmt    (fnmoc/netcdf/bufr)'
       call error_exit ('READ_OCN_OBS', err_msg)
       stop
      end if

!
! read input arguments
!
      data_dir   = ' '
      obs_typ    = ' '
      sub_typ    = -999
      file_dtg   = ' '
      output_fmt = ' '

! data dir
      call GETARG (1, arg)
      data_dir = trim (arg)

! observation type
      call GETARG (2, arg)
      obs_typ = trim(arg)

! observation sub type
      call GETARG (3, arg)
      read(arg, *) sub_typ

! date time group (dtg) argument
      call GETARG (4, arg)
      file_dtg = arg(1:10)

! output format
      call GETARG (5, arg)
      output_fmt = trim(arg)
!
! echo input arguments
!
      len_dir = len_trim (data_dir)
      input_file_name = data_dir(1:len_dir) // '/' // &
                        file_dtg // '.' // trim(obs_typ)
      input_file_name = trim(input_file_name)

      print *, '   '
      print *, '   ****** READ_OCN_OBS         ******'
      print *, '   '
      print *, '   data directory path: ', data_dir(1:len_dir)
      print *, '   file date time     : ', file_dtg
      print *, '   obs  type          : ', obs_typ
      print *, '   sub  type          : ', sub_typ
      print *, '   file name          : ', input_file_name
      print *, '   output format      : ', output_fmt
      print *, '   '
!
! read header info
! 
      inquire (file=input_file_name(1: len_trim (input_file_name)), exist=exist)

      if (exist) then
        open (UNIT, file=input_file_name(1: len_trim (input_file_name)), &
              status='old', form='unformatted')
        read (UNIT) n_obs, n_lvl, n_vrsn

        print *, '        number of obs:   ', n_obs
        print *, '      max number levels: ', n_lvl
        print *, '    file version number: ', n_vrsn
!
! read data, differently based on: obs type
!
        if (n_obs .gt. 0) then

          if (trim(obs_typ) .eq. 'profile') then              ! profile
            call read_write_prof (UNIT, obs_typ, sub_typ, file_dtg, output_fmt, n_obs, n_lvl, n_vrsn)

          else if (trim(obs_typ) .eq. 'ship') then            ! surface ship/buoy SST
            call rd_ship (UNIT, obs_typ, sub_typ, file_dtg, output_fmt, n_obs, n_vrsn)

          else if (trim(obs_typ) .eq. 'swh') then             ! SWH
            call rd_swh  (UNIT, obs_typ, sub_typ, file_dtg, output_fmt, n_obs, n_vrsn)

          else if (trim(obs_typ) .eq. 'trak') then            ! TRAK
            call rd_trak (UNIT, obs_typ, sub_typ, file_dtg, output_fmt, n_obs, n_vrsn)

!         ---------------- FNMOC doesn't have post following obs types on USGODAE
          else if (trim(obs_typ) .eq. 'glider') then          ! glider
!           call rd_gldr (UNIT, n_obs, n_lvl, n_vrsn)         ! ------------->> SA: Skip; fnmoc not available
            print *, 'Unsupported obs_typ, STOPPING!'
            stop
          else if (trim(obs_typ) .eq. 'altim') then           ! altimeter
!           call rd_altim (UNIT, n_obs, n_vrsn)               ! ------------->> SA: Skip; fnmoc not available
            print *, 'Unsupported obs_typ, STOPPING!'
            stop
          else if (trim(obs_typ) .eq. 'ssmi') then            ! SSM/I sea ice
!           call rd_ssmi (UNIT, n_obs, n_vrsn)                ! ------------->> SA: Skip; fnmoc not available
            print *, 'Unsupported obs_typ, STOPPING!'
            stop
!         -----------------------------------------------------------------------
          else
            print *, 'Unsupported obs_typ, STOPPING!'
            stop
          endif
        endif
        close (UNIT)

      else
        write (err_msg, '(''file "'', a, ''" does not exist'')') &
               input_file_name(1: len_trim (input_file_name))
        call error_exit ('READ_OCN_OBS', err_msg)
      endif ! if(exist)
!.......................................................................
!
      stop
      end program read_ocn_obs
!-----------------------------------------------------------------------
