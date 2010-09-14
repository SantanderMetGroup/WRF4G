MODULE module_pot_t
  
  CONTAINS
! Diagnostic module of computation of temperatures [K] from potential temperatures [K]
! GMS. UC: September 2010. v0.0
!

  SUBROUTINE pot_t(debg, dx, dy, dz, dt, pres, pot, temp)
!  Subroutine to compute temperature [K] from potential temperatures [K]

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dz, dt
  REAL, DIMENSION(dx,dy,dz,dt), INTENT(IN)                :: pres, pot
  REAL, DIMENSION(dx,dy,dz,dt), INTENT(OUT)               :: temp
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i,j,k,it, ijk, nozero
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  
!!!!!!!!!!!!!! Variables
! dx, dy, dz, dt: dimensions of fields
! pot: potential temperature [K]
! pres: pressure [Pa]
! temp: temperature [K]

  section="'pot_t'"
  
  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'  Dimensions: ',dx,CHAR(44), dy,CHAR(44), dz,CHAR(44), dt

  temp = pot * ( pres / pref )**(Rd/Cp)

  IF (debg >= 100) THEN
    PRINT *,'  pot. temp.: ',pot(halfdim(dx),halfdim(dy),halfdim(dz),halfdim(dt)),' pres:',     &
      pres(halfdim(dx),halfdim(dy),halfdim(dz),halfdim(dt))
  END IF

  IF (debg >= 75) PRINT *,'  temperature at the center dimx/2:', halfdim(dx),' dimy/2:',          &
    halfdim(dy),' dt/2:',halfdim(dz),' dt/2:', halfdim(dt), ' =', temp(halfdim(dx),halfdim(dy), &
    halfdim(dz),halfdim(dt))

  END SUBROUTINE pot_t

END MODULE module_pot_t
