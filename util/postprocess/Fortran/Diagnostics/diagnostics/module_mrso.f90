MODULE module_mrso
  
  CONTAINS
! Diagnostic module of computation of Total soil moisture content 
! GMS. UC: August 2010. v0.0
!

  SUBROUTINE mrso(debg, dx, dy, dz, dt, levmois, dzlev, totsoilmois)
!  Subroutine to compute total soil moisture content in kgm-2

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dz, dt
  REAL, DIMENSION(dx,dy,dz,dt), INTENT(IN)                :: levmois
  REAL, DIMENSION(dz,dt), INTENT(IN)                      :: dzlev
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: totsoilmois
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i, j, k, l
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  
!!!!!!!!!!!!!! Variables
! dx, dy, dz, dt: dimensions of fields
! dzlev: thickness of level [m]
! levmois: moisture at each level [m3 m-3]
! totsoilmois: total moisture content [kg m-3]

  section="'mrso'"
  
  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'  Dimensions: ',dx,CHAR(44), dy,CHAR(44), dz,CHAR(44), dt

  DO i=1,dx
    DO j=1,dy
      DO l=1,dt
        IF (ALL (levmois(i,j,1:dz,l) == 1)) THEN
	  totsoilmois(i,j,l)=1.
	ELSE
! Converting 1 m3 H2O --> 1000 kg H2O ; we want kg m-3
          totsoilmois(i,j,l)=SUM(levmois(i,j,1:dz,l)*dzlev(1:dz,l))*1000.
	END IF
      END DO
    END DO
  END DO

  IF (debg >= 150) THEN
    DO k=1,dz
      PRINT *,'  level ',k,' thickness: ',dzlev(k, halfdim(dt)),' dim/2 soil moisture: ',          &
        levmois(halfdim(dx), halfdim(dy), k, halfdim(dt))
    END DO
  END IF

  IF (debg >= 75) THEN
    PRINT *,'  mrso at the center dimx/2:', halfdim(dx),' dimy/2:', halfdim(dy),' dt/2:',          &
      halfdim(dt), ' =', totsoilmois(halfdim(dx),halfdim(dy),halfdim(dt))
  END IF

  END SUBROUTINE mrso

END MODULE module_mrso
