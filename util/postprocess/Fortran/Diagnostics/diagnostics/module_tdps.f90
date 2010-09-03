MODULE module_tdps
  
  CONTAINS
! Diagnostic module of compuation of dew point temperature at 2 m
! GMS. UC: August 2010. v0.0
!

  SUBROUTINE tdps(debg, dx, dy, dt, q2, psfc, t2, tempd2)
!  Subroutine to compute td2 in K

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dt
  REAL, DIMENSION(dx,dy,dt), INTENT(IN)                   :: q2, psfc, t2
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: tempd2
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i,j,it, ijk, nozero
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  REAL, DIMENSION(dx, dy, dt)                             :: e, c
  
!!!!!!!!!!!!!! Variables
! dx, dy, dt: dimensions of fields
! q2: 2m mixing ratio [kg kg-1]
! psfc: surface prssure (assumed the same at 2m) [Pa]
! tempd2: 2m de point temperature [K]
! e: vapor pressure in air [Pa]

  section="'module_tdps'"
  
  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'  Dimensions: ',dx,CHAR(44), dy,CHAR(44), dt

  IF (debg >= 75)  PRINT *,'  Computing tdps....'

  tempd2=0.
  e=q2*psfc/(epsilon_gamma+q2)
  c=log10(e/epsilon_gamma)
!  WHERE (t2-tkelvin <= 3.) tempd2=(c*es_Btetens_ice)/(es_Atetens_ice-c)
!  WHERE (t2-tkelvin > 3.) tempd2=(c*es_Btetens_vapor)/(es_Atetens_vapor-c)
  tempd2=(c*es_Btetens_vapor)/(es_Atetens_vapor-c)
  
  IF (debg >= 100) THEN
    PRINT *,'  dim/2 q2: ',q2(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 psfc: ',psfc(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 t2: ',t2(halfdim(dx),halfdim(dy),halfdim(dt))    
    PRINT *,'  dim/2 e: ',e(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 c: ',c(halfdim(dx),halfdim(dy),halfdim(dt))
  END IF

  IF (debg >= 75) PRINT *,'  tdps at the center dimx/2:', halfdim(dx),' dimy/2:',               &
    halfdim(dy),' dt/2:', halfdim(dt), ' =', tempd2(halfdim(dx),halfdim(dy),halfdim(dt))

  END SUBROUTINE tdps

END MODULE module_tdps
