!
      subroutine error_exit (routine, message)
!
!.............................START PROLOGUE............................
!
! MODULE NAME:  error_exit
!
! DESCRIPTION:  prints a fatal error message and terminates the program.
!
! PARAMETERS:
!    Name          Type       Usage            Description
!   -------     ----------    ------    ---------------------------
!   routine     char * (*)    input     name of routine
!   message     char * (*)    input     user supplied error message
!
!..............................END PROLOGUE.............................
!
      implicit  none
!
      integer   ln
      character message * (*)
      character routine * (*)
!
!..............................executable...............................
!
!     ..determine message string length
!
      ln = len_trim (message)
!
      write (*, '(//, ''*** FATAL ERROR ('', a, '') ***'')') routine
      write (*, '(/, a)') message(1:ln)
      write (*, '(/, ''*** PROGRAM TERMINATED ***'', /)')
!
      return
      end subroutine error_exit
