MODULE module_prw
  
  CONTAINS
! Diagnostic module of computation of column water vpor content
! GMS. UC: August 2010. v0.0
!

  SUBROUTINE prw(debg, dx, dy, dz, dt, qv, rho, dzh, vintq)
!  Subroutine to compute column water vapor content in kg m-2

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dz, dt
  REAL, DIMENSION(dx,dy,dz,dt), INTENT(IN)                :: qv
  REAL, DIMENSION(dx,dy,dt), INTENT(IN)                   :: rho
  REAL, DIMENSION(dz,dt), INTENT(IN)                      :: dzh
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: vintq
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i,j,k,l
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  
!!!!!!!!!!!!!! Variables
! dx, dy, dz, dt: dimensions of fields
! qvapor: vapor mixing ratio [kg kg-1]
! dzq: vertical grid length []
! vintq: total vertical water vapor [kg m-2]
! dxm, dym: horizontal grid dimensions [m]

  section="'prw'"
  
  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'  Dimensions: ',dx,CHAR(44), dy,CHAR(44), dz,CHAR(44), dt

! NOTE
!
! In sigma coordinates, (as output in wrfout), dz is dimensionless []. In grid volume variables
! [var/kg] in this vertical coordinate; any vertical integration takes account of air density (in
! wrfout as rho = mu + mub [Pa]) and has to be divided by gravity 'grav' [m s-2]. Thus, resultant 
! vertical integral becomes [kg m-2]:
!
! Pa = [Power] m-2=[kg m s-2]m-2
!
! Int[var]=SUM(var(k)*rho*dz(k)])/grav=[var kg-1]*[Pa]*[]/[ms-2]=
!         =[var kg-1]*[kg m s-2]m-2][]/[ms-2]=var m-2
!

  vintq=0.
  
  DO i=1,dx
    DO j=1,dy
      DO l=1,dt
        DO k=1,dz
          vintq(i,j,l) = vintq(i,j,l) + qv(i,j,k,l)*dzh(k,1)
        END DO
      END DO
    END DO
  END DO
  IF (debg >= 100) THEN
    DO k=1,dt
      PRINT *,'  t: ',k,'= ',qv( halfdim(dx), halfdim(dy), halfdim(dz), k)
    END DO
  END IF

  IF (debg >= 150) THEN
    DO k=1,dz
      PRINT *,'  dim/2 lev. :',k,' vert. space: ',dzh(k,1),' vapor mixing ratio: ',               &
        qv( halfdim(dx), halfdim(dy), k, halfdim(dt) ),' rho:', rho( halfdim(dx), halfdim(dy),  &
	halfdim(dt) )
    END DO
  END IF
  
  vintq=vintq*rho/grav

  IF (debg >= 75) PRINT *,'  prw at the center dimx/2:', halfdim(dx),' dimy/2:',                  &
    halfdim(dy),' dt/2:', halfdim(dt), ' =', vintq(halfdim(dx),halfdim(dy),halfdim(dt))

  END SUBROUTINE prw

END MODULE module_prw
