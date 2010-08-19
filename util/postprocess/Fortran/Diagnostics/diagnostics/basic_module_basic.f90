MODULE module_basic
  
  CONTAINS
! Diagnostic module of compuation of any variable 
! GMS. UC: August 2010. v0.0
!

  SUBROUTINE basic(debg, dx, dy, dz, dt, [input fields], [output])
!  Subroutine to compute [base] in [variables]

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dz, dt
  REAL, DIMENSION(dx,dy,dz,dt), INTENT(IN)                :: [input fields]
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: [output]
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i,j,k,it, ijk, nozero
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  
!!!!!!!!!!!!!! Variables
! dx, dy, dz, dt: dimensions of fields
! [input fields]: Description of input variables
! [output]: description of output variable

  section="'module_[basic]'"
  
  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'Dimensions: ',dx,CHAR(44), dy,CHAR(44), dz,CHAR(44), dt

  IF (debg >= 75)  PRINT *,'Computing [basic]....'

  (...)

  IF (debg >= 75) PRINT *,'[basic] at the center dimx/2:', halfdim(dx),' dimy/2:', &
    halfdim(dy),' dt/2:', halfdim(dt), ' =', [output](halfdim(dx),halfdim(dy),halfdim(dt))

  END SUBROUTINE basic

END MODULE module_basic
