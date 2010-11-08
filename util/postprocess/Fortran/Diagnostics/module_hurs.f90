MODULE module_hurs

  CONTAINS
! Diagnostic module of computation of relative humidty at 2 m
! GMS. UC: August 2010. v0.0
!

  SUBROUTINE hurs(debg, dx, dy, dt, t2, q2, psfc, rh2)
!  Subroutine to compute td2 in K

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dt
  REAL, DIMENSION(dx,dy,dt), INTENT(IN)                   :: t2, q2, psfc
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: rh2
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i,j,l
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  REAL, DIMENSION(dx, dy, dt)                             :: e, es

!!!!!!!!!!!!!! Variables
! dx, dy, dt: dimensions of fields
! t2: 2m temperature [K]
! q2: 2m mixing ratio [kg kg-1]
! psfc: surface prssure (assumed the same at 2m) [Pa]
! rh2: 2m relative humidity [1]
! e: vapor pressure in air [Pa]
! es: saturation vapor pressure [Pa]

  section="'hurs'"

  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'  Dimensions: ',dx,CHAR(44), dy,CHAR(44), dt

  e=q2*psfc/(epsilon_gamma+q2)
  WHERE (t2-tkelvin <= 3. ) es=es_base_tetens*10.**(((t2-tkelvin)*es_Atetens_ice)/((t2-tkelvin)  &
    +es_Btetens_ice))
  WHERE (t2-tkelvin > 3.) es=es_base_tetens*10.**(((t2-tkelvin)*es_Atetens_vapor)/((t2-tkelvin)+ &
    es_Btetens_vapor))

!  DO i=1,dx
!    DO j=1,dy
!      DO l=1,dt
!        IF ((t2(i,j,l)-tkelvin) <= 3.) THEN
!          es(i,j,l)=es_base_tetens*10.**(((t2(i,j,l)-tkelvin)*es_Atetens_vapor)/((t2(i,j,l)-     &
!           tkelvin)+es_Btetens_vapor))
!        ELSE
!          es(i,j,l)=es_base_tetens*10.**(((t2(i,j,l)-tkelvin)*es_Atetens_ice)/((t2(i,j,l)-       &
!           tkelvin)+es_Btetens_ice))
!       END IF
!      END DO
!    END DO
!  END DO
  rh2=e/es

  rh2=MAX(MIN(rh2, 1.0), 0.0)

  IF (debg >= 100) THEN
    PRINT *,'  dim/2 values_____________'
    PRINT *,'  t2: ',t2(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  q2: ',q2(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  psfc: ',psfc(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  e: ',e(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  es: ',es(halfdim(dx),halfdim(dy),halfdim(dt))
  END IF

  IF (debg >= 75) PRINT *,'  rh2 at the center dimx/2:', halfdim(dx),' dimy/2:', halfdim(dy),     &
    ' dt/2:', halfdim(dt), ' =', rh2(halfdim(dx),halfdim(dy),halfdim(dt))

  END SUBROUTINE hurs

END MODULE module_hurs
