MODULE module_SeaLevelPressure

  CONTAINS
  
  SUBROUTINE SeaLevelPress (debg, dx, dy, dz, dt, refp, pres, psfc, ter, temp, qv, mslp)
! Subroutine to compute mean_sealevel pressure values, based on 'p_interp.F90'

    USE module_constants
    USE module_calc_tools, ONLY: virtual

    IMPLICIT NONE

    INTEGER, INTENT(IN)                                           :: debg, dx, dy, dz, dt
    REAL, DIMENSION(dx, dy, dz, dt), INTENT(IN)                   :: pres, temp, qv
    REAL, DIMENSION(dx, dy, dt), INTENT(IN)                       :: psfc
    REAL, DIMENSION(dx, dy), INTENT(IN)                           :: ter
    REAL, INTENT(IN)                                              :: refp
    REAL, DIMENSION(dx, dy, dt), INTENT(OUT)                      :: mslp

! Local
    INTEGER                                                       :: i,j,k,itt
    INTEGER                                                       :: kin, kupper
    CHARACTER(LEN=50)                                             :: section
    REAL                                                          :: dpmin, ptarget, dp, expon, &
      tbotextrap, tvbotextrap
    REAL, DIMENSION(dz)                                           :: dpres
    LOGICAL                                                       :: tervalue
    
!!!!!!! Variables
! d[x/y/t/z]: dimensions of matrixs
! refp: reference pressure from which mslp will be computed (as constant of method) [Pa]
! pres: pressure field [Pa]
! psfc: pressure at surface [Pa]
! ter: terrein height [m]
! temp: temperature [K]
! qv: mixing ratio [kg kg-1]
! mslp: mean sea level pressure [Pa]
! tervalue: flag to show mslp value computation above a terrain value [1000 m height]

    section="'SeaLevelPressure'"
    IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
    
    expon=287.04*.0065/9.81
!    expon=Rd*0.0065/grav
    PRINT *,'expon: ',expon,' -->',287.04*.0065/9.81

    mslp=0.
    tervalue=.FALSE.
    DO itt = 1, dt
      DO j = 1, dy
        DO i = 1, dx

!         We are below both the ground and the lowest data level.

!         First, find the model level that is closest to a "target" pressure
!         level, where the "target" pressure is delta-p less that the local
!         value of a horizontally smoothed surface pressure field.  We use
!         delta-p = 150 hPa here. A standard lapse rate temperature profile
!         passing through the temperature at this model level will be used
!         to define the temperature profile below ground.  This is similar
!         to the Benjamin and Miller (1990) method, using  
!         'refp' (usually 700 hPa) everywhere for the "target" pressure.

          dpres=pres(i,j,:,itt) - refp
 
! Minimum difference pressure
          dpmin=1.e6

          kupper = 0
          loop_kIN : DO kin=dz,1,-1
            kupper = kin
            dp=ABS ( pres(i,j,kin,itt) - refp )
            IF (dp.gt.dpmin) EXIT loop_kIN
	    
            dpmin=MIN(dpmin, dp)
          END DO loop_kIN
	  
!	  dpmin=MINVAL(ABS(dpres))
!	  DO kin=dz,1,-1
!	    IF (dpmin == ABS(dpres(kin))) kupper = kin
!	  END DO

          ptarget=refp

          tbotextrap=temp(i,j,kupper,itt)*(psfc(i,j,itt)/ptarget)**expon
          tvbotextrap=virtual(debg, tbotextrap, qv(i,j,kupper,itt))

          mslp(i,j,itt) = psfc(i,j,itt)*((tvbotextrap+0.0065*ter(i,j))/tvbotextrap)**           &
	    (1./expon)

          IF ((debg >= 100) .AND. ( i==dx/2 ) .AND. (j==dy/2 ) .AND. ( itt==dt/2 )) THEN
            PRINT *,'  dim/2 values_______'
	    PRINT *,'  i:',i,' j:',j,' itt: ',itt
            PRINT *,'  ',itt,' ptarget',ptarget,'kupper:',kupper,' pres:',pres(i,j,kupper,itt)
            PRINT *,'  temp:',temp(i,j,kupper,itt),'psfc:',psfc(i,j,itt)
            PRINT *,'  tbot:',tbotextrap,'tvbot:',tvbotextrap,'ter:',ter(i,j)
            PRINT *,'  qv:',qv(i,j,kupper,itt)
	    PRINT *,'  dpres: ',dpres
	    PRINT *,'  mslp:',mslp(i,j,itt)
	  END IF
	  IF ((debg >= 100) .AND. .NOT.tervalue .AND. ( ter(i,j) >= 1000.)) THEN
            PRINT *,'  tervalue values_______'
	    PRINT *,'  i:',i,' j:',j,' itt: ',itt
            PRINT *,'  ',itt,' ptarget',ptarget,'kupper:',kupper,' pres:',pres(i,j,kupper,itt)
            PRINT *,'  temp:',temp(i,j,kupper,itt),'psfc:',psfc(i,j,itt)
            PRINT *,'  tbot:',tbotextrap,'tvbot:',tvbotextrap,'ter:',ter(i,j)
            PRINT *,'  qv:',qv(i,j,kupper,itt)
	    PRINT *,'  dpres: ',dpres
	    PRINT *,'  mslp:',mslp(i,j,itt)
	    tervalue=.TRUE.	  
	  END IF

        END DO
      END DO
    END DO

 END SUBROUTINE SeaLevelPress

END MODULE module_SeaLevelPressure
