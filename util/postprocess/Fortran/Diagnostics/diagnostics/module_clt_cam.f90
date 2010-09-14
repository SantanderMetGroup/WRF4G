MODULE module_clt_cam
  
  CONTAINS
! Diagnostic module of computation of total cloud cover fraction following as it is done in CAM model
! GMS. UC: August 2010. v0.0
!

  SUBROUTINE clt_cam(debg, dx, dy, dz, dt, cldfra, totcfr)
!  Subroutine to compute total cloud cover in base 1.

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dz, dt
  REAL, DIMENSION(dx,dy,dz,dt), INTENT(IN)                :: cldfra
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: totcfr
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i,j,k,it
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  
!!!!!!!!!!!!!! Variables
! dx, dy, dz, dt: dimensions of fields
! cldfra: cloud fraction at each level
! totcfr: total cloud fraction

  section="'clt_cam'"
  
  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'  Dimensions: ',dx,CHAR(44), dy,CHAR(44), dz,CHAR(44), dt

  totcfr=0.
  
  DO i=1,dx
    DO j=1,dy
      DO it=1,dt
        totcfr(i,j,it)=MAXVAL(cldfra(i,j,:,it))
      END DO
    END DO
  END DO

  IF (debg >= 150) THEN
    DO k=1, dz
      PRINT *,'  At level :',k,' cloud fraction at center: ', cldfra(halfdim(dx), halfdim(dy), k, &
        halfdim(dt))
    END DO
  END IF

  IF (debg >= 75) PRINT *,'  Total cloud fraction at the center dimx/2:', halfdim(dx),' dimy/2:', &
    halfdim(dy),' dt/2:', halfdim(dt), ' =', totcfr(halfdim(dx),halfdim(dy),halfdim(dt))

  END SUBROUTINE clt_cam

END MODULE module_clt_cam
