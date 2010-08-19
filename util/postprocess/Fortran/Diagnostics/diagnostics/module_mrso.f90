MODULE module_mrso
  
  CONTAINS
! Diagnostic module of compuation of Total soil moisture content 
! GMS. UC: August 2010. v0.0
!

  SUBROUTINE mrso(debg, dx, dy, dz, dt, levmois, totsoilmois)
!  Subroutine to compute total soil moisture content in kgm-2

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dz, dt
  REAL, DIMENSION(dx,dy,dz,dt), INTENT(IN)                :: levmois
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: totsoilmois
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i, j, k, l
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  
!!!!!!!!!!!!!! Variables
! dx, dy, dz, dt: dimensions of fields
! levmois: Description of input variables
! totsoilmois: description of output variable

  section="'module_mrso'"
  
  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'Dimensions: ',dx,CHAR(44), dy,CHAR(44), dz,CHAR(44), dt

  IF (debg >= 75)  PRINT *,'Computing mrso....'

  DO i=1,dx
    DO j=1,dy
      DO l=1,dt
        totsoilmois(i,j,l)=SUM(levmois(i,j,1:dz,l))
      END DO
    END DO
  END DO

  IF (debg >= 75) THEN
    DO k=1,dz
      PRINT *,'soil moisture at level ',k,' : ',levmois(halfdim(dx), halfdim(dy), k,            &
        halfdim(dt))
    END DO
    PRINT *,'mrso at the center dimx/2:', halfdim(dx),' dimy/2:', &
    halfdim(dy),' dt/2:', halfdim(dt), ' =', totsoilmois(halfdim(dx),halfdim(dy),halfdim(dt))
  END IF

  END SUBROUTINE mrso

END MODULE module_mrso
