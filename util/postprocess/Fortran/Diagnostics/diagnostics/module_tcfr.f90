MODULE module_tcfr
  CONTAINS
! Diagnostic module of computation of total cloud cover fraction following Sundqvist, 1989, Mont.
!   Weather Rev. 
! GMS. UC: August 2010. v0.0
!
!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 module_diagnostic.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -c
!
  SUBROUTINE tcfr(debg, dx, dy, dz, dt, cldfra, totcfr)
!  Subroutine to compute total cloud cover in base 1.

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER                                                 :: dx, dy, dz, dt, k
  REAL, DIMENSION(dx,dy,dz,dt), INTENT(IN)                :: cldfra
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: totcfr
  INTEGER, INTENT(IN)                                     :: debg

! Local
  CHARACTER(LEN=50)                                       :: section
  
  section="'tcfr'"
  
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  
  totcfr=1.

  vertical_levels: DO k=1, dz-1
    totcfr(:,:,:)=totcfr(:,:,:)*(1-MAX(cldfra(:,:,k,:),cldfra(:,:,k+1,:))/(1.-cldfra(:,:,k,:)))

  END DO vertical_levels

  totcfr=1.-totcfr

  IF (debg >= 75) PRINT *,'  Total cloud fraction at the center dimx/2:', dx/2,' dimy/2:', dy/2,   &
    ' dt/2:', dt/2, ' =', totcfr(dx/2,dy/2,dt/2)

  END SUBROUTINE tcfr

END MODULE module_tcfr
