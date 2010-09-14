MODULE module_vt
  
  CONTAINS
! Diagnostic module of computation of virtual temperature [K]
! GMS. UC: September 2010. v0.0
!

  SUBROUTINE vt(debg, dx, dy, dz, dt, temp, q, vtemp)
!  Subroutine to compute [base] in [variables]

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dz, dt
  REAL, DIMENSION(dx,dy,dz,dt), INTENT(IN)                :: temp, q
  REAL, DIMENSION(dx,dy,dt,dt), INTENT(OUT)               :: vtemp
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i,j,k,it, ijk, nozero
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  
!!!!!!!!!!!!!! Variables
! dx, dy, dz, dt: dimensions of fields
! temp: temperature [K]
! q: mixing ratio kgkg-1
! vtemp: virtual temperature [K]

  section="'vt'"
  
  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'Dimensions: ',dx,CHAR(44), dy,CHAR(44), dz,CHAR(44), dt

  vtemp=temp*(0.622+q)/(0.622*(1.+q))

  IF (debg >= 100) THEN
    PRINT *,'  temp.:',temp(halfdim(dx),halfdim(dy),halfdim(dz),halfdim(dt)),' q:',             &
      q(halfdim(dx),halfdim(dy),halfdim(dz),halfdim(dt))
  END IF
  
  IF (debg >= 75) PRINT *,'vtemp at the center dimx/2:', halfdim(dx),' dimy/2:', halfdim(dy),   &
    ' dt/2:', halfdim(dt), ' =', vtemp(halfdim(dx),halfdim(dy),halfdim(dz),halfdim(dt))

  END SUBROUTINE vt

END MODULE module_vt
