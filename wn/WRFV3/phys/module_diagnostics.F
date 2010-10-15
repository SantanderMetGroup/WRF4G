!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diagnostics
CONTAINS
   SUBROUTINE diagnostic_output_calc(                                 &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,dpsdt,dmudt                                     &
                     ,p8w,pk1m,mu_2,mu_2m                             &
                     ,u,v                                             &
                     ,raincv,rainncv,rainc,rainnc                     &
                     ,i_rainc,i_rainnc                                &
                     ,hfx,sfcevp,lh                                   &
                     ,ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC               & ! Optional
                     ,ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC               & ! Optional
                     ,ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC               & ! Optional
                     ,ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC               & ! Optional
                     ,I_ACSWUPT,I_ACSWUPTC,I_ACSWDNT,I_ACSWDNTC       & ! Optional
                     ,I_ACSWUPB,I_ACSWUPBC,I_ACSWDNB,I_ACSWDNBC       & ! Optional
                     ,I_ACLWUPT,I_ACLWUPTC,I_ACLWDNT,I_ACLWDNTC       & ! Optional
                     ,I_ACLWUPB,I_ACLWUPBC,I_ACLWDNB,I_ACLWDNBC       & ! Optional
                     ,dt,xtime,sbw                                    &
                     ,diag_print                                      &
                     ,bucket_mm, bucket_J                             &
                                                                      )
!----------------------------------------------------------------------

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval

   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DIAG_PRINT    print control: 0 - no diagnostics; 1 - dmudt only; 2 - all
!-- DT            time step (second)
!-- XTIME         forecast time
!-- SBW           specified boundary width - used later
!
!-- P8W           3D pressure array at full eta levels
!-- MU            dry column hydrostatic pressure
!-- RAINC         cumulus scheme precipitation since hour 0
!-- RAINCV        cumulus scheme precipitation in one time step (mm)
!-- RAINNC        explicit scheme precipitation since hour 0
!-- RAINNCV       explicit scheme precipitation in one time step (mm)
!-- HFX           surface sensible heat flux
!-- LH            surface latent heat flux
!-- SFCEVP        total surface evaporation
!-- U             u component of wind - to be used later to compute k.e.
!-- V             v component of wind - to be used later to compute k.e.
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
!======================================================================

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                  &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::   diag_print
   REAL,      INTENT(IN   )    ::   bucket_mm, bucket_J

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                       u  &
                                                    ,         v  &
                                                    ,       p8w

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
                                                           MU_2  &
                                                    ,   RAINNCV  &
                                                    ,    RAINCV  &
                                                    ,       HFX  &
                                                    ,    SFCEVP  &  
                                                    ,        LH  

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                DPSDT  &
                                                    ,     DMUDT  &
                                                    ,    RAINNC  &
                                                    ,     RAINC  &
                                                    ,     MU_2M  &
                                                    ,      PK1M
 
   REAL,  INTENT(IN   ) :: DT, XTIME
   INTEGER,  INTENT(IN   ) :: SBW
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::     &
                                                       I_RAINC,  &
                                                       I_RAINNC
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC,          &
                      ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC,          &
                      ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC,          &
                      ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC
   INTEGER, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      I_ACSWUPT,I_ACSWUPTC,I_ACSWDNT,I_ACSWDNTC,  &
                      I_ACSWUPB,I_ACSWUPBC,I_ACSWDNB,I_ACSWDNBC,  &
                      I_ACLWUPT,I_ACLWUPTC,I_ACLWDNT,I_ACLWDNTC,  &
                      I_ACLWUPB,I_ACLWUPBC,I_ACLWDNB,I_ACLWDNBC

! LOCAL  VAR

   INTEGER :: i,j,k,its,ite,jts,jte,ij
   INTEGER :: idp,jdp,irc,jrc,irnc,jrnc,isnh,jsnh
   INTEGER :: prfreq

   REAL              :: no_points
   REAL              :: dpsdt_sum, dmudt_sum, dardt_sum, drcdt_sum, drndt_sum
   REAL              :: hfx_sum, lh_sum, sfcevp_sum, rainc_sum, rainnc_sum, raint_sum
   REAL              :: dmumax, raincmax, rainncmax, snowhmax
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256     :: outstring
   CHARACTER*6       :: grid_str

!-----------------------------------------------------------------
! Handle accumulations with buckets to prevent round-off truncation in long runs
! This is done every 360 minutes assuming time step fits exactly into 360 minutes
   IF(bucket_mm .gt. 0. .AND. MOD(NINT(XTIME),360) .EQ. 0)THEN
! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   DO ij = 1 , num_tiles

      IF (xtime .eq. 0.0)THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_rainnc(i,j) = 0
          i_rainc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(rainnc(i,j) .gt. bucket_mm)THEN
          rainnc(i,j) = rainnc(i,j) - bucket_mm
          i_rainnc(i,j) =  i_rainnc(i,j) + 1
        ENDIF
        IF(rainc(i,j) .gt. bucket_mm)THEN
          rainc(i,j) = rainc(i,j) - bucket_mm
          i_rainc(i,j) =  i_rainc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO

      IF (xtime .eq. 0.0 .and. PRESENT(ACSWUPT))THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_acswupt(i,j) = 0
          i_acswuptc(i,j) = 0
          i_acswdnt(i,j) = 0
          i_acswdntc(i,j) = 0
          i_acswupb(i,j) = 0
          i_acswupbc(i,j) = 0
          i_acswdnb(i,j) = 0
          i_acswdnbc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      IF (xtime .eq. 0.0 .and. PRESENT(ACLWUPT))THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_aclwupt(i,j) = 0
          i_aclwuptc(i,j) = 0
          i_aclwdnt(i,j) = 0
          i_aclwdntc(i,j) = 0
          i_aclwupb(i,j) = 0
          i_aclwupbc(i,j) = 0
          i_aclwdnb(i,j) = 0
          i_aclwdnbc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      IF (PRESENT(ACSWUPT))THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(acswupt(i,j) .gt. bucket_J)THEN
          acswupt(i,j) = acswupt(i,j) - bucket_J
          i_acswupt(i,j) =  i_acswupt(i,j) + 1
        ENDIF
        IF(acswuptc(i,j) .gt. bucket_J)THEN
          acswuptc(i,j) = acswuptc(i,j) - bucket_J
          i_acswuptc(i,j) =  i_acswuptc(i,j) + 1
        ENDIF
        IF(acswdnt(i,j) .gt. bucket_J)THEN
          acswdnt(i,j) = acswdnt(i,j) - bucket_J
          i_acswdnt(i,j) =  i_acswdnt(i,j) + 1
        ENDIF
        IF(acswdntc(i,j) .gt. bucket_J)THEN
          acswdntc(i,j) = acswdntc(i,j) - bucket_J
          i_acswdntc(i,j) =  i_acswdntc(i,j) + 1
        ENDIF
        IF(acswupb(i,j) .gt. bucket_J)THEN
          acswupb(i,j) = acswupb(i,j) - bucket_J
          i_acswupb(i,j) =  i_acswupb(i,j) + 1
        ENDIF
        IF(acswupbc(i,j) .gt. bucket_J)THEN
          acswupbc(i,j) = acswupbc(i,j) - bucket_J
          i_acswupbc(i,j) =  i_acswupbc(i,j) + 1
        ENDIF
        IF(acswdnb(i,j) .gt. bucket_J)THEN
          acswdnb(i,j) = acswdnb(i,j) - bucket_J
          i_acswdnb(i,j) =  i_acswdnb(i,j) + 1
        ENDIF
        IF(acswdnbc(i,j) .gt. bucket_J)THEN
          acswdnbc(i,j) = acswdnbc(i,j) - bucket_J
          i_acswdnbc(i,j) =  i_acswdnbc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO
      ENDIF
      IF (PRESENT(ACLWUPT))THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(aclwupt(i,j) .gt. bucket_J)THEN
          aclwupt(i,j) = aclwupt(i,j) - bucket_J
          i_aclwupt(i,j) =  i_aclwupt(i,j) + 1
        ENDIF
        IF(aclwuptc(i,j) .gt. bucket_J)THEN
          aclwuptc(i,j) = aclwuptc(i,j) - bucket_J
          i_aclwuptc(i,j) =  i_aclwuptc(i,j) + 1
        ENDIF
        IF(aclwdnt(i,j) .gt. bucket_J)THEN
          aclwdnt(i,j) = aclwdnt(i,j) - bucket_J
          i_aclwdnt(i,j) =  i_aclwdnt(i,j) + 1
        ENDIF
        IF(aclwdntc(i,j) .gt. bucket_J)THEN
          aclwdntc(i,j) = aclwdntc(i,j) - bucket_J
          i_aclwdntc(i,j) =  i_aclwdntc(i,j) + 1
        ENDIF
        IF(aclwupb(i,j) .gt. bucket_J)THEN
          aclwupb(i,j) = aclwupb(i,j) - bucket_J
          i_aclwupb(i,j) =  i_aclwupb(i,j) + 1
        ENDIF
        IF(aclwupbc(i,j) .gt. bucket_J)THEN
          aclwupbc(i,j) = aclwupbc(i,j) - bucket_J
          i_aclwupbc(i,j) =  i_aclwupbc(i,j) + 1
        ENDIF
        IF(aclwdnb(i,j) .gt. bucket_J)THEN
          aclwdnb(i,j) = aclwdnb(i,j) - bucket_J
          i_aclwdnb(i,j) =  i_aclwdnb(i,j) + 1
        ENDIF
        IF(aclwdnbc(i,j) .gt. bucket_J)THEN
          aclwdnbc(i,j) = aclwdnbc(i,j) - bucket_J
          i_aclwdnbc(i,j) =  i_aclwdnbc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO
      ENDIF
   ENDDO
!  !$OMP END PARALLEL DO
   ENDIF
   if (diag_print .eq. 0 ) return

   IF ( xtime .ne. 0. ) THEN

    if(diag_print.eq.1) then
       prfreq = dt
!      prfreq = max(2,int(dt/60.))   ! in min
    else
       prfreq=10                   ! in min
    endif
   
    IF (MOD(nint(dt),prfreq) == 0) THEN

! COMPUTE THE NUMBER OF MASS GRID POINTS
   no_points = float((ide-ids)*(jde-jds))

! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   dmumax = 0.
   DO ij = 1 , num_tiles

!     print *, i_start(ij),i_end(ij),j_start(ij),j_end(ij)
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         dpsdt(i,j)=(p8w(i,kms,j)-pk1m(i,j))/dt
         dmudt(i,j)=(mu_2(i,j)-mu_2m(i,j))/dt
         if(abs(dmudt(i,j)*dt).gt.dmumax)then
           dmumax=abs(dmudt(i,j)*dt)
           idp=i
           jdp=j
         endif
      ENDDO      
      ENDDO

   ENDDO
!  !$OMP END PARALLEL DO

! convert DMUMAX from (PA) to (bars) per time step
   dmumax = dmumax*1.e-5
! compute global MAX
   CALL wrf_dm_maxval ( dmumax,  idp, jdp )

!  print *, 'p8w(30,1,30),pk1m(30,30) : ', p8w(30,1,30),pk1m(30,30)
!  print *, 'mu_2(30,30),mu_2m(30,30) : ', mu_2(30,30),mu_2m(30,30)
   dpsdt_sum = 0.
   dmudt_sum = 0.

   DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       dpsdt_sum = dpsdt_sum + abs(dpsdt(i,j))
       dmudt_sum = dmudt_sum + abs(dmudt(i,j))
     ENDDO
   ENDDO

! compute global sum
   dpsdt_sum = wrf_dm_sum_real ( dpsdt_sum )
   dmudt_sum = wrf_dm_sum_real ( dmudt_sum )

!  print *, 'dpsdt, dmudt : ', dpsdt_sum, dmudt_sum

   IF ( diag_print .eq. 2 ) THEN
   dardt_sum = 0.
   drcdt_sum = 0.
   drndt_sum = 0.
   rainc_sum = 0.
   raint_sum = 0.
   rainnc_sum = 0.
   sfcevp_sum = 0.
   hfx_sum = 0.
   lh_sum = 0.

   DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       drcdt_sum = drcdt_sum + abs(raincv(i,j))
       drndt_sum = drndt_sum + abs(rainncv(i,j))
       dardt_sum = dardt_sum + abs(raincv(i,j)) + abs(rainncv(i,j))
       rainc_sum = rainc_sum + abs(rainc(i,j))
! MAX for accumulated conv precip
       IF(rainc(i,j).gt.raincmax)then
          raincmax=rainc(i,j)
          irc=i
          jrc=j
       ENDIF
       rainnc_sum = rainnc_sum + abs(rainnc(i,j))
! MAX for accumulated resolved precip
       IF(rainnc(i,j).gt.rainncmax)then
          rainncmax=rainnc(i,j)
          irnc=i
          jrnc=j
       ENDIF
       raint_sum = raint_sum + abs(rainc(i,j)) + abs(rainnc(i,j))
       sfcevp_sum = sfcevp_sum + abs(sfcevp(i,j))
       hfx_sum = hfx_sum + abs(hfx(i,j))
       lh_sum = lh_sum + abs(lh(i,j))
     ENDDO
   ENDDO

! compute global MAX
   CALL wrf_dm_maxval ( raincmax, irc, jrc )
   CALL wrf_dm_maxval ( rainncmax, irnc, jrnc )

! compute global sum
   drcdt_sum = wrf_dm_sum_real ( drcdt_sum )
   drndt_sum = wrf_dm_sum_real ( drndt_sum )
   dardt_sum = wrf_dm_sum_real ( dardt_sum )
   rainc_sum = wrf_dm_sum_real ( rainc_sum )
   rainnc_sum = wrf_dm_sum_real ( rainnc_sum )
   raint_sum = wrf_dm_sum_real ( raint_sum )
   sfcevp_sum = wrf_dm_sum_real ( sfcevp_sum )
   hfx_sum = wrf_dm_sum_real ( hfx_sum )
   lh_sum = wrf_dm_sum_real ( lh_sum )

   ENDIF

! print out the average values

   CALL get_current_grid_name( grid_str )

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
#endif
     WRITE(outstring,*) grid_str,'Domain average of dpsdt, dmudt (mb/3h): ', xtime, &
           dpsdt_sum/no_points*108., &
           dmudt_sum/no_points*108.
     CALL wrf_message ( TRIM(outstring) )

     WRITE(outstring,*) grid_str,'Max mu change time step: ', idp,jdp,dmumax
     CALL wrf_message ( TRIM(outstring) )

     IF ( diag_print .eq. 2) THEN
     WRITE(outstring,*) grid_str,'Domain average of dardt, drcdt, drndt (mm/sec): ', xtime, &
           dardt_sum/dt/no_points, &
           drcdt_sum/dt/no_points, &
           drndt_sum/dt/no_points
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Domain average of rt_sum, rc_sum, rnc_sum (mm): ', xtime, &
           raint_sum/no_points, &
           rainc_sum/no_points, &
           rainnc_sum/no_points
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Max Accum Resolved Precip,   I,J  (mm): '               ,&
           rainncmax,irnc,jrnc
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Max Accum Convective Precip,   I,J  (mm): '             ,&
           raincmax,irc,jrc
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Domain average of sfcevp, hfx, lh: ', xtime, &
           sfcevp_sum/no_points, &
           hfx_sum/no_points, &
           lh_sum/no_points
     CALL wrf_message ( TRIM(outstring) )
     ENDIF
#ifdef DM_PARALLEL
   ENDIF
#endif

    ENDIF        ! print frequency
   ENDIF

! save values at this time step
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
   DO ij = 1 , num_tiles

      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         pk1m(i,j)=p8w(i,kms,j)
         mu_2m(i,j)=mu_2(i,j)
      ENDDO
      ENDDO

      IF ( xtime .lt. 0.0001 ) THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         dpsdt(i,j)=0.
         dmudt(i,j)=0.
      ENDDO
      ENDDO
      ENDIF

   ENDDO
   !$OMP END PARALLEL DO

   END SUBROUTINE diagnostic_output_calc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef CLWRF

   SUBROUTINE clwrf_output_calc(                                      &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,dpsdt,dmudt                                     &
                     ,p8w,pk1m,mu_2,mu_2m                             &
                     ,u,v                                             &
                     ,clwrfH,t2,q2,u10,v10, skintemp                  & ! CLWRF
                     ,t2clmin,t2clmax,tt2clmin,tt2clmax               & ! CLWRF
                     ,t2clmean,t2clstd                                & ! CLWRF
                     ,q2clmin,q2clmax,tq2clmin,tq2clmax               & ! CLWRF
                     ,q2clmean,q2clstd                                & ! CLWRF
                     ,u10clmax,v10clmax,spduv10clmax,tspduv10clmax    & ! CLWRF
                     ,u10clmean,v10clmean,spduv10clmean               & ! CLWRF
                     ,u10clstd,v10clstd,spduv10clstd                  & ! CLWRF
                     ,raincclmax,rainncclmax,traincclmax,trainncclmax & ! CLWRF
                     ,raincclmean,rainncclmean,raincclstd,rainncclstd & ! CLWRF
                     ,skintempclmin,skintempclmax                     & ! CLWRF
                     ,tskintempclmin,tskintempclmax                   & ! CLWRF
                     ,skintempclmean,skintempclstd                    & ! CLWRF
                     ,raincv,rainncv,rainc,rainnc                     &
                     ,i_rainc,i_rainnc                                &
                     ,hfx,sfcevp,lh                                   &
                     ,dt,xtime,sbw                                    &
                     ,diag_print                                      &
                     ,bucket_mm, bucket_J                             &
                                                                      )
!----------------------------------------------------------------------

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
  USE module_configure 

   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DT            time step (second)
!-- XTIME         forecast time
!-- SBW           specified boundary width - used later
!
!-- P8W           3D pressure array at full eta levels
!-- MU            dry column hydrostatic pressure
!-- RAINC         cumulus scheme precipitation since hour 0
!-- RAINCV        cumulus scheme precipitation in one time step (mm)
!-- RAINNC        explicit scheme precipitation since hour 0
!-- RAINNCV       explicit scheme precipitation in one time step (mm)
!-- HFX           surface sensible heat flux
!-- LH            surface latent heat flux
!-- SFCEVP        total surface evaporation
!-- U             u component of wind - to be used later to compute k.e.
!-- V             v component of wind - to be used later to compute k.e.
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
! CLWRF-UC May.09 definitions
!-----------
! clwrfH: Interval (hour) of accumulation for computations 
! [var]cl[min/max]: [minimum/maximum] of variable [var] during interval
! t[var]cl[min/max]: Time (minutes) of [minimum/maximum] of variable 
!    [var] during interval 
! [var]clmean: mean of variable [var] during interval
! [var]clstd: standard dev. of variable [var] during interval
!    Variables are written on aux_hist_out7 (established
!    in Registry)
!
!======================================================================

   INTEGER,      INTENT(IN   )                     ::            &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN)       :: i_start,   &
                                      i_end,j_start,j_end

   INTEGER,      INTENT(IN   )                     :: diag_print
   REAL,      INTENT(IN   )                        :: bucket_mm, &
                                      bucket_J

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
                                       INTENT(IN ) :: u,v,p8w    

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: MU_2,     & 
                                      RAINNCV, RAINCV, HFX,      &
                                      SFCEVP, LH, SKINTEMP 

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                                     INTENT(INOUT) :: DPSDT,     &
                                     DMUDT, RAINNC, RAINC,       &
                                     MU_2M, PK1M
 
   REAL,  INTENT(IN   )                            :: DT, XTIME
   INTEGER,  INTENT(IN   )                         :: SBW
   INTEGER, DIMENSION( ims:ime , jms:jme ),                      & 
                                     INTENT(INOUT) :: I_RAINC,   &
                                     I_RAINNC

! LOCAL  VAR

   INTEGER                                   :: i,j,k,its,ite,jts,jte,ij
   INTEGER                                   :: idp,jdp,irc,jrc,irnc,jrnc,isnh,jsnh
   INTEGER                                   :: prfreq

   REAL                                      :: dpsdt_sum, dmudt_sum, dardt_sum,  & 
                          drcdt_sum, drndt_sum
   REAL                                      :: hfx_sum, lh_sum, sfcevp_sum,      &
                          rainc_sum, rainnc_sum, raint_sum
   REAL                                      :: dmumax, raincmax, rainncmax,      &
                          snowhmax
   LOGICAL, EXTERNAL                         :: wrf_dm_on_monitor
   CHARACTER*256                             :: outstring
   CHARACTER*6                               :: grid_str

!!-------------------
!! CLWRF-UC Nov.09

   CHARACTER (LEN=80)                        :: timestr

   REAL, DIMENSION( ims:ime , jms:jme ),                                          & 
                          INTENT(IN)         :: t2, q2, u10, v10 
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: t2clmin, t2clmax, tt2clmin,       &
                          tt2clmax, t2clmean, t2clstd,                            & 
                          q2clmin, q2clmax, tq2clmin, tq2clmax, q2clmean, q2clstd,&
                          u10clmax, v10clmax, spduv10clmax, tspduv10clmax,        &
                          u10clmean, v10clmean, spduv10clmean,                    &
                          u10clstd, v10clstd, spduv10clstd, skintempclmin,        &
                          skintempclmax, tskintempclmin, tskintempclmax,          &
                          skintempclmean, skintempclstd
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: raincclmax, rainncclmax,          &
                          traincclmax, trainncclmax, raincclmean, rainncclmean,   & 
                          raincclstd, rainncclstd 
   REAL, PARAMETER                           :: minimum0= 1000000.,               &
                          maximum0= -1000000. 
   REAL                                      :: value
   INTEGER, INTENT(IN)                       :: clwrfH
   CHARACTER (LEN=1024)                      :: message
   REAL, SAVE                                :: nsteps

!-----------------------------------------------------------------
! Compute minutes from reference times clwrfH

! Initialize [var] values
! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

!  IF ( MOD(NINT(XTIME), clwrfH) == 0 ) THEN
  IF ( MOD(NINT(XTIME*60./dt),NINT(clwrfH*60./dt)) == 0) THEN
    DO ij = 1 , num_tiles
      IF  ( wrf_dm_on_monitor() ) THEN
        WRITE(message, *)'CLWRFdiag - T2; tile: ',ij,' T2clmin:',           & 
          t2clmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' T2clmax:',               &
          t2clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TT2clmin:',              &
          tt2clmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TT2clmax:',              &
          tt2clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' T2clmean:',              &
          t2clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' T2clstd:',               &
          t2clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
        WRITE(message, *)'CLWRFdiag - Q2; tile: ',ij,' Q2clmin:',           &
          q2clmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' Q2clmax:',               &
          q2clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TQ2clmin:',              &
          tq2clmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TQ2clmax:',              &
          tq2clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' Q2clmean:',              &
          q2clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' Q2clstd:',               &
          q2clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                    &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
        WRITE(message, *)'CLWRFdiag - WINDSPEED; tile: ',ij,' U10clmax:',   &
          u10clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' V10clmax:',              &
          v10clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SPDUV10clmax:',          &
          spduv10clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,               &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TSPDUV10clmax:',         &
          tspduv10clmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' U10clmean:',             &
          u10clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                  &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' V10clmean:',             &
          v10clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                  &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SPDUV10clmean:',         &
          spduv10clmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' U10clstd:',              &
          u10clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' V10clstd:',              &
          v10clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                   &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SPDUV10clstd:',          &
          spduv10clstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,               &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
        WRITE(message, *)'CLWRFdiag - RAIN; tile: ',ij,' RAINCclmax:',      &
          raincclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                 &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINNCclmax:',           &
          rainncclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TRAINCclmax:',           &
          traincclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,                &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TRAINNCclmax:',          &
          trainncclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,               &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINCclmean:',           &
          raincclmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,                &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINNCclmean:',          &
          rainncclmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,               &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINCclstd:',            &
          raincclstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                 &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' RAINNCclstd:',           &
          rainncclstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,                &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
        WRITE(message,*)'CLWRFdiag - SKINTEMP; tile: ',ij,' SKINTEMPclmin:',&
          skintempclmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SKINTEMPclmax:',         &
          skintempclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TSKINTEMPclmin:',        &
          tskintempclmin(i_start(ij)+(i_end(ij)-i_start(ij))/2,             &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' TSKINTEMPclmax:',        &
          tskintempclmax(i_start(ij)+(i_end(ij)-i_start(ij))/2,             &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SKINTEMPclmean:',        &
          skintempclmean(i_start(ij)+(i_end(ij)-i_start(ij))/2,             &
          j_start(ij)+(j_end(ij)-j_start(ij))/2),' SKINTEMPclstd:',         &
          skintempclstd(i_start(ij)+(i_end(ij)-i_start(ij))/2,              &
          j_start(ij)+(j_end(ij)-j_start(ij))/2)
        CALL wrf_debug(75, message)
     ENDIF
    DO j = j_start(ij), j_end(ij)
      DO i = i_start(ij), i_end(ij)
        t2clmin(i,j)=minimum0
        t2clmax(i,j)=maximum0
        t2clmean(i,j)=0.0
        t2clstd(i,j)=0.0
        q2clmin(i,j)=minimum0
        q2clmax(i,j)=maximum0
        q2clmean(i,j)=0.0
        q2clstd(i,j)=0.0
        spduv10clmax(i,j)=0.0
        u10clmean(i,j)=0.0
        v10clmean(i,j)=0.0
        spduv10clmean(i,j)=0.0
        u10clstd(i,j)=0.0
        v10clstd(i,j)=0.0
        spduv10clstd(i,j)=0.0
        raincclmax(i,j)=0.0
        rainncclmax(i,j)=0.0
        raincclmean(i,j)=0.0
        rainncclmean(i,j)=0.0
        raincclstd(i,j)=0.0
        rainncclstd(i,j)=0.0
        skintempclmin(i,j)=minimum0
        skintempclmax(i,j)=maximum0
        skintempclmean(i,j)=0.0
        skintempclstd(i,j)=0.0
!        nsteps=0.
      ENDDO
    ENDDO
  ENDDO
    nsteps=clwrfH*60./dt
  ELSE
!    nsteps=nsteps+1.
    nsteps=clwrfH*60./dt
!      DO j = j_start(ij), j_end(ij)
!        DO i = i_start(ij), i_end(ij)
!     DO j = jps, jpe
!       DO i = ips, ipe
! Temperature
          CALL varstatistics(t2,xtime,ime-ims+1,jme-jms+1,t2clmin,t2clmax,   &
            tt2clmin,tt2clmax,t2clmean,t2clstd)
! Water vapor mixing ratio
          CALL varstatistics(q2,xtime,ime-ims+1,jme-jms+1,q2clmin,q2clmax,   &
            tq2clmin,tq2clmax,q2clmean,q2clstd)
! Wind speed
          CALL varstatisticsWIND(u10,v10,xtime,ime-ims+1,jme-jms+1,u10clmax, &
            v10clmax,spduv10clmax,tspduv10clmax,u10clmean,v10clmean,         &
            spduv10clmean,u10clstd,v10clstd,spduv10clstd)
! Precipitation flux
          CALL varstatisticsMAX(raincv/dt,xtime,ime-ims+1,jme-jms+1,         &
            raincclmax,traincclmax,raincclmean,raincclstd) 
          CALL varstatisticsMAX(rainncv/dt,xtime,ime-ims+1,jme-jms+1,        &
            rainncclmax,trainncclmax,rainncclmean,rainncclstd)
! Skin Temperature 
          CALL varstatistics(skintemp,xtime,ime-ims+1,jme-jms+1,skintempclmin,&
            skintempclmax, tskintempclmin,tskintempclmax,skintempclmean,      &
            skintempclstd)

!          IF (MOD(NINT(XTIME),clwrfH) == 0) THEN
!          IF (MOD(NINT(XTIME+dt/60.),clwrfH) == 0) THEN
           IF (MOD(NINT((XTIME+dt/60.)*60./dt),NINT(clwrfH*60./dt)) == 0) THEN 
             IF  ( wrf_dm_on_monitor() ) PRINT *,'nsteps=',nsteps,' xtime:',xtime,' clwrfH:',clwrfH 
            t2clmean=t2clmean/nsteps
            t2clstd=SQRT(t2clstd/nsteps-t2clmean**2.)
            q2clmean=q2clmean/nsteps
            q2clstd=SQRT(q2clstd/nsteps-q2clmean**2.)
            u10clmean=u10clmean/nsteps
            v10clmean=v10clmean/nsteps
            spduv10clmean=spduv10clmean/nsteps
            u10clstd=SQRT(u10clstd/nsteps-u10clmean**2.)
            v10clstd=SQRT(v10clstd/nsteps-v10clmean**2.)
            spduv10clstd=SQRT(spduv10clstd/nsteps-                 &
              spduv10clmean**2)
            raincclmean=raincclmean/nsteps
            rainncclmean=rainncclmean/nsteps
            raincclstd=SQRT(raincclstd/nsteps-raincclmean**2.)
            rainncclstd=SQRT(rainncclstd/nsteps-rainncclmean**2.)
            skintempclmean=skintempclmean/nsteps
            skintempclstd=SQRT(skintempclstd/nsteps-skintempclmean**2.)
          END IF
!        ENDDO
!      ENDDO
  ENDIF
!  !$OMP END PARALLEL DO

   END SUBROUTINE clwrf_output_calc

! UC.CLWRF Nov.09
SUBROUTINE varstatisticsWIND(varu, varv, tt, dx, dy, varumax, varvmax,       &
  varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd, varvstd,     & 
  varuvstd) 
! Subroutine to compute variable statistics for a wind somponents 

IMPLICIT NONE

INTEGER                                                        :: i, j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: varu, varv
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varumax,   &
  varvmax, varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd,      & 
  varvstd, varuvstd
REAL                                                           :: varuv

DO i=1,dx
  DO j=1,dy
    varuv=sqrt(varu(i,j)*varu(i,j)+varv(i,j)*varv(i,j))
      IF (varuv > varuvmax(i,j)) THEN
        varumax(i,j)=varu(i,j)
        varvmax(i,j)=varv(i,j)
        varuvmax(i,j)=varuv
        tvaruvmax(i,j)=tt
      END IF
    varuvmean(i,j)=varuvmean(i,j)+varuv
    varuvstd(i,j)=varuvstd(i,j)+varuv**2
  END DO
END DO
varumean=varumean+varu
varvmean=varvmean+varv
varustd=varustd+varu**2
varvstd=varvstd+varv**2

END SUBROUTINE varstatisticsWIND

SUBROUTINE varstatisticsMAX(var, tt, dx, dy, varmax, tvarmax, varmean,       &
   varstd)
! Subroutine to compute variable statistics for a max only variable values

IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmax,    &
  tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var**2

END SUBROUTINE varstatisticsMAX 

SUBROUTINE varstatistics(var, tt, dx, dy, varmin, varmax, tvarmin, tvarmax,  & 
  varmean, varstd) 
! Subroutine to compute variable statistics

IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmin,    &
  varmax, tvarmin, tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) < varmin(i,j)) THEN
      varmin(i,j)=var(i,j)
      tvarmin(i,j)=tt
    END IF
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var**2

END SUBROUTINE varstatistics

SUBROUTINE diagnostic_clwrf_clim(                                     &
! clWRF module to compute necessary climatic variables
!!
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,dpsdt,dmudt                                     &
                     ,pk1m,mu_2,mu_2m                                 &
                     ,imestag, jmestag                                &
                     ,u,v,raincv,rainncv                              &
                     ,geob, geop, topography, potemp                  &
                     ,sinusa, cosena                                  &
                     ,qratio,lratio,tkenergy,pblheight                &
                     ,histout, swdown                                 &
                     ,dsunshine, initime_sunshine                     &
                     ,maxrain1h, tmaxrain1h, accum1h                  &
                     ,gustwindflag                                    & 
                     ,ugustwind, vgustwind                            &
                     ,ugustwindlow, vgustwindlow                      &
                     ,ugustwindup, vgustwindup                        &
                     ,umaxgust1h, vmaxgust1h, tmaxgust1h              &
                     ,dt,xtime,sbw                                    &
                     ,diag_print                                      &
                                                                      )
!----------------------------------------------------------------------

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval

   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DIAG_PRINT    print control: 0 - no diagnostics; 1 - dmudt only; 2 - all
!-- DT            time step (second)
!-- XTIME         forecast time
!-- SBW           specified boundary width - used later
!
!-- MU            dry column hydrostatic pressure
!-- RAINCV        cumulus scheme precipitation in one time step (mm)
!-- RAINNCV       explicit scheme precipitation in one time step (mm)
!-- U             3D u component of wind 
!-- V             3D v component of wind
!-- SWDOWN        DOWNWARD SHORT WAVE FLUX AT GROUND SURFACE
!-- GEOB          BASE of GEOPOTENTIAL HEIGHT 
!-- GEOP          PERTURBATION of GEOPOTENTIAL HEIGHT 
!-- TOPOGRAPHY    TOPOGRAPHY OD DOMAIN
!-- QRATIO        VAPOUR WATER MXIXING RATIO
!-- LRATIO        LIQUID WATER MXIXING RATIO
!-- POTEMP        POTENTIAL TEMPERTAURE
!-- TKENERGY      TURBULENT KINETIC ENERGY 
!-- PBLHEIGHT     PBL HEIGHT 
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
!======================================================================

  INTEGER, INTENT(IN)                                   :: ids,ide, jds,jde, kds,kde,           &
     ims,ime, jms,jme, kms,kme, &
     ips,ipe, jps,jpe, kps,kpe, &
     kts,kte, &
     num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN)             :: i_start,i_end,j_start,j_end 

   INTEGER, INTENT(IN)                                   :: diag_print

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)      :: MU_2

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)   :: DPSDT, DMUDT, MU_2M, PK1M  

   REAL,  INTENT(IN)                                     :: DT, XTIME
   INTEGER,  INTENT(IN)                                  :: SBW

! LOCAL  VAR

   INTEGER                                               :: i,j,k,its,ite,jts,jte,ij, im2, jm2
   INTEGER                                               :: idp,jdp,irc,jrc,irnc,jrnc,isnh,jsnh
   INTEGER                                               :: prfreq

   REAL                                                  :: no_points
   LOGICAL, EXTERNAL                                     :: wrf_dm_on_monitor
   CHARACTER(LEN=1024)                                   :: outstring
   CHARACTER(LEN=6)                                      :: grid_str

! CLWRF.April 2010
!-----------------------------------------------------------------
! 34567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
   INTEGER, INTENT(IN)                                   :: imestag, jmestag
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)   :: dsunshine, initime_sunshine,        &
     maxrain1h, tmaxrain1h, accum1h, ugustwind, vgustwind, umaxgust1h, vmaxgust1h, tmaxgust1h,  &
     ugustwindlow, vgustwindlow, ugustwindup, vgustwindup
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                                                &
         INTENT(IN)                                      :: geob, geop, potemp, qratio,         &
     lratio, tkenergy  
   REAL, DIMENSION( ims:imestag, kms:kme, jms:jme ),                                            &
         INTENT(IN)                                      :: u
   REAL, DIMENSION( ims:ime, kms:kme, jms:jmestag ),                                            &
         INTENT(IN)                                      :: v
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )          :: uwind, vwind, geopotheight

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)      :: swdown, raincv, rainncv, pblheight, &
     topography, sinusa, cosena

! Local
   INTEGER                                               :: istart, iend, jstart, jend
   INTEGER                                               :: Nlevs
   REAL, PARAMETER                                       :: sunshine_start=120., kct=0.2854,    &
     nullvalue=-999.9, pt0=300.
   REAL                                                  :: gustwind, maxgust1h
   INTEGER, INTENT(IN)                                   :: histout, gustwindflag

!-----------------------------------------------------------------

   im2=INT(ims+(ime-ims)/2)
   jm2=INT(jms+(jme-jms)/2)

! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   DO ij = 1 , num_tiles
!     IF (num_tiles == 1) THEN
!       istart=ims
!       iend=MIN(ime,10000)
!       jstart=jms
!       jend=MIN(jme,10000)
!     ELSE
!       istart=MAX(i_start(ij),1)
!       iend=MIN(i_end(ij),10000)
!       jstart=MAX(j_start(ij),1)
!       jend=MIN(j_end(ij),10000)
       istart=i_start(ij)
       iend=i_end(ij)
       jstart=j_start(ij)
       jend=j_end(ij)
!     ENDIF
     uwind(:,:,:)=0.5*(u(istart:iend,:,:)+u(istart+1:imestag,:,:))
     vwind(:,:,:)=0.5*(v(:,:,jstart:jend)+v(:,:,jstart+1:jmestag))
     geopotheight = geob + geop

! Initialization of variables
!      IF (xtime == 0.0) THEN
!      IF (MOD(NINT(XTIME),histout) == 0) THEN
     IF (MOD(NINT(XTIME*60./dt),NINT(histout*60./dt)) == 0) THEN 

        DO j=jstart,jend
        DO i=istart,iend
          dsunshine(i,j) = 0.
          initime_sunshine(i,j) = 0. 
          maxrain1h(i,j) = 0.
          tmaxrain1h(i,j) = 0.
          accum1h(i,j) = 0.
          umaxgust1h(i,j) = 0.
          vmaxgust1h(i,j) = 0.
          tmaxgust1h(i,j) = 0.
        ENDDO
        ENDDO
      ENDIF
!       gust wind
!!

#ifdef WGUST
      IF ((xtime > 0.).AND.(gustwindflag==1)) THEN
!     Only compute wind gust when diagnostics fiels have been initialized and option has been   &
!       activated in namelist (time_control section, variable: gust_wind)'
!        PRINT *,'jstart: ',jstart,' jend:',jend,' istart:',istart,' iend:',iend
        PRINT *,'tke: ',tkenergy(im2,kms+1,jm2)
        CALL gustwindBrasseur01(iend-istart+1, kme-kms+1, jend-jstart+1, geopotheight, qratio,  &
          lratio, potemp+300., tkenergy, uwind, vwind, pblheight, topography, sinusa, cosena,   &
          ugustwind, vgustwind, ugustwindlow, vgustwindlow, ugustwindup, vgustwindup) 
      END IF
#endif

! 1 hour
!      PRINT *,'GUST gust wind calculated...'
      DO j=jstart,jend
      DO i=istart,iend
        IF (MOD(NINT(XTIME*60./dt),NINT(60.*60./dt)) == 0) THEN 
!        IF (MOD(NINT(XTIME),60) == 0) THEN
          accum1h(i,j) = 0.
!          umaxgust1h(i,j) = 0.
!          vmaxgust1h(i,j) = 0.

        ELSE
          accum1h(i,j) = raincv(i,j) + rainncv(i,j) 

        ENDIF
      ENDDO
      ENDDO

! Climatic diagnostics
!!

      DO j=jstart,jend
      DO i=istart,iend
        IF (gustwindflag==1) THEN
!        PRINT *,'GUST max gust searching...'
!         max gust 1h:
!!
          gustwind=SQRT(ugustwind(i,j)*ugustwind(i,j)+vgustwind(i,j)*vgustwind(i,j))
          maxgust1h=SQRT(umaxgust1h(i,j)*umaxgust1h(i,j)+vmaxgust1h(i,j)*vmaxgust1h(i,j))
          IF (gustwind > maxgust1h) THEN
            umaxgust1h(i,j)=ugustwind(i,j)
            vmaxgust1h(i,j)=vgustwind(i,j)
            tmaxgust1h(i,j)=xtime
          END IF
        ELSE
            umaxgust1h(i,j)=0.
            vmaxgust1h(i,j)=0.
            tmaxgust1h(i,j)=0.
        ENDIF
!       max 1h rain:
!!
        IF (accum1h(i,j)/3600. > maxrain1h(i,j)) THEN
          maxrain1h(i,j)=accum1h(i,j)/3600.
          tmaxrain1h(i,j)=xtime
        END IF

!       diag_sunshine: 
!!
        IF ((swdown(i,j) >= sunshine_start) .AND. (initime_sunshine(i,j) == 0.)) THEN
          initime_sunshine(i,j)=xtime
          dsunshine(i,j)=0.
        ELSE IF ((swdown(i,j) >= sunshine_start)) THEN
          dsunshine(i,j)=dsunshine(i,j)+dt
        ENDIF 
      ENDDO
      ENDDO

!!!!!  !!!!!  !!!!!
! Printing some results
!!
     IF ( wrf_dm_on_monitor() ) THEN
       WRITE(outstring,*) 'CLWRFclim values: ',ij,' sunshine:', dsunshine(im2, jm2),            &
         ' tsunshine:', initime_sunshine(im2, jm2),' swdown:',swdown(im2,jm2)
       CALL wrf_debug ( 75, TRIM(outstring) )
       WRITE(outstring,*) 'CLWRFclim values: ',ij,' maxrain1h:', maxrain1h(im2, jm2),           &
         ' tmaxrain1h:', tmaxrain1h(im2, jm2),' accum1h:', accum1h(im2, jm2)
       CALL wrf_debug ( 75, TRIM(outstring) )
       IF (gustwindflag == 1) THEN
         WRITE(outstring,*) 'CLWRFclim values: ',ij,' umaxgust1h:', umaxgust1h(im2, jm2),       &
           ' vmaxgust1h:', vmaxgust1h(im2, jm2),' tmaxgust1h:', tmaxgust1h(im2, jm2),           &
           ' ugustwind:',ugustwind(im2, jm2),' vgustwind:',vgustwind(im2, jm2)
         CALL wrf_debug ( 75, TRIM(outstring) )
       END IF
     END IF

   ENDDO
!  !$OMP END PARALLEL DO

   END SUBROUTINE diagnostic_clwrf_clim

#ifdef WGUST
SUBROUTINE gustwindBrasseur01(dx, dz, dy, geopoth, qr, lr, pt, tke, u, v, hpbl, topo, sina,     &
  cosa, ugustwind, vgustwind, ugustlow, vgustlow, ugustup, vgustup) 
! Subroutine to compute wind gust following Brasseur (2001), MWR

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: dx, dy, dz
  REAL, DIMENSION(dx,dz,dy), INTENT(IN)                  :: geopoth, qr, lr, pt, tke, u, v
  REAL, DIMENSION(dx,dy), INTENT(IN)                     :: hpbl, topo, sina, cosa
  REAL, DIMENSION(dx,dy), INTENT(OUT)                    :: ugustwind, vgustwind, ugustlow,     &
    vgustlow, ugustup, vgustup
  
! Local
  INTEGER                                                :: i,j,k, Nlevs, im2, jm2, ilev
  INTEGER                                                :: Ngustlevels, Ngustlowlev, maxlev
  REAL, PARAMETER                                        :: grav=9.81, zero=10.e-6 
  REAL, DIMENSION(dz)                                    :: vpt, heights, dheights 
  REAL, DIMENSION(dz,2)                                  :: wge, wge_low, wge_up
  INTEGER, DIMENSION(dz)                                 :: gustlevels
  REAL                                                   :: preshpbl, tke_int, vpt_int
  REAL                                                   :: height2pres, virtualpottemp,        &
    pres2height
  REAL                                                   :: wgemax, wgemaxlow, wgemaxup
  REAL                                                   :: ugust, vgust, ugustl, vgustl,       &
    ugustu, vgustu

!!!!!!!!!! Variables
! geopoth: 3D geopotential height
! qr: 3D vapour water mixing ratio
! lr: 3D liquid water mixing ratio
! pt: 3D potential temperature
! tke: 3D turbulent kinetic energy
! u, v: 3D wind components
! hpbl: pbl height
! topo: topography
! sina, cosa: Map factors sinus and cosionus
! ugustwind, vgustwind: gust wind components
! ugustlow, vgustlow: gust wind components of lower bounder
! ugustup, vgustup: gust wind components of upper bounder
! Nlevs: number of layers within pbl
! Ngustlevels: number of pbl layers with a int_tke >= int_ept
! Ngustlowlev: number of pbl layers with a 2.5*tke(lay)/11. >= int_ept
! vpt: virtual potential temperature
! heights: vector of heights according hydrostatic conversion from pressure
! dheights: vector of differences between heights
! wge: estimated wind gusts 
! wge_low: low doundaries of estimated wind gust
! wge_up: up boundary of estimated wind gust
! gustlevels: levels of pbl with a possible deflected air parcel
! preshpbl: hydrostatic equivalent pbl height pressure
! tke_int: tke integral up to a given layer
! vpt_int: vpt integral up to a given layer
! wgemax: maximum wge
! wgemaxlow: lower wge boundary
! wgemaxup: upper wge boundary
! zero: zero values

!!!!!!!!!! Functions
! height2pres: convert hydrostatically from height in pressures
! mixratio2relative: convert from mixing ratio to relative humidity
! virtualpottemp: calculates virtual potential temperature
! pres2height: convert hydrostatically from pressure to height

  im2=INT(dx/2)
  jm2=INT(dy/2)

  ugustwind=0.
  vgustwind=0.
  ugustlow=0.
  vgustlow=0.
  ugustup=0.
  vgustup=0.

  DO i=1,dx
    DO j=1,dy
  
      maxlev=0
      heights=0.
      dheights=0.
      vpt=0.
      wge=0.
      wge_low=0.
      wge_up=0.

      preshpbl=height2pres(hpbl(i,j)+topo(i,j))
      IF ((i == im2) .AND. (j==jm2)) PRINT *,im2,char(44),jm2,' hpbl:',hpbl(i,j),' preshpbl:', &
        preshpbl, ' tke:', tke(i,dz/2,j)

      k=1  
!      PRINT *,'Brasseur. Calculating generic values...'
      DO WHILE (( (geopoth(i,k,j)/grav) <= (hpbl(i,j)+topo(i,j))) .AND. (k <= dz))
        vpt(k)=virtualpottemp(pt(i,k,j), qr(i,k,j), lr(i,k,j))
        heights(k)=geopoth(i,k,j)/grav
        k=k+1
      END DO
      Nlevs=k-1
      wge_up(1:Nlevs,1)=u(i,1:Nlevs,j)
      wge_up(1:Nlevs,2)=v(i,1:Nlevs,j)

!      PRINT *,'Brasseur. Integrating...'
      dheights(2:Nlevs)=heights(2:Nlevs)-heights(1:Nlevs-1)
        
      Ngustlevels=0
      Ngustlowlev=0
      DO ilev=2,Nlevs
        tke_int=0.
        vpt_int=0.
! Trapezoidal integrations
        tke_int=SUM(dheights(2:ilev)*(tke(i,1:ilev-1,j)+tke(i,2:ilev,j))/2.)/heights(ilev)
        vpt_int=SUM(dheights(2:ilev)*((vpt(1:ilev-1)+vpt(2:ilev))/2.)/vpt(1:ilev))/grav

!        IF ((i == im2) .AND. (j==jm2)) PRINT *,'ilev:',ilev,' tke_int:',tke_int,' vpt_int:',    &
!          vpt_int,' tke:',tke(i,ilev,j),' 25tke11:',2.5*tke(i,ilev,j)/11.
        IF (tke_int >= vpt_int) THEN
          Ngustlevels=Ngustlevels+1
          gustlevels(Ngustlevels)=ilev
          wge(Ngustlevels,1)=u(i,ilev,j)
          wge(Ngustlevels,2)=v(i,ilev,j)         
        END IF
        IF (2.5*tke(i,ilev,j)/11. >= vpt_int) THEN
          Ngustlowlev=Ngustlowlev+1
          wge_low(Ngustlowlev,1)=u(i,ilev,j)
          wge_low(Ngustlowlev,2)=v(i,ilev,j)
        END IF
      END DO
!      PRINT *,'Brasseur. Given wind gust estimation...'
! gustwind
!!
      IF (Ngustlevels > 0 ) THEN
        wgemax=MAXVAL(SQRT(wge(:,1)*wge(:,1)+wge(:,2)*wge(:,2)))
        DO ilev=1,Ngustlevels
          IF (ABS(wgemax - SQRT(wge(ilev,1)*wge(ilev,1)+wge(ilev,2)*wge(ilev,2))) < zero) THEN
            maxlev=ilev
            EXIT
          END IF
        END DO
        ugust=wge(maxlev,1)
        vgust=wge(maxlev,2)
      ELSE
        ugust=0.
        vgust=0.
      ENDIF
!        IF ((i == im2) .AND. (j==jm2)) THEN
!          PRINT *,'Number of gust levels:',Ngustlevels,' :',gustlevels(1:Ngustlevels),' wge:',  &
!            wge(1:Ngustlevels,1)
!        END IF
! gustwind lower bound
!!
      IF (Ngustlowlev > 0) THEN
        wgemaxlow=MAXVAL(SQRT(wge_low(:,1)*wge_low(:,1)+wge_low(:,2)*wge_low(:,2)))
        DO ilev=1,Ngustlowlev
          IF (ABS(wgemaxlow - SQRT(wge_low(ilev,1)*wge_low(ilev,1)+wge_low(ilev,2)*             &
            wge_low(ilev,2))) < zero) THEN
            maxlev=ilev
            EXIT
          END IF
        END DO
        ugustl=wge_low(maxlev,1)
        vgustl=wge_low(maxlev,2)          
      ELSE
        ugustl=0.
        vgustl=0.
      END IF
!      IF ((i == im2) .AND. (j==jm2)) PRINT *,'Synoptic wind angle for WGE_low:',               &
!        wge_low(maxlev,2)
! gustwind upperbound
!!
      IF (Nlevs > 0) THEN
        wgemaxup=MAXVAL(SQRT(wge_up(:,1)*wge_up(:,1)+wge_up(:,2)*wge_up(:,2)))
        DO ilev=1,Nlevs
          IF (ABS(wgemaxup - SQRT(wge_up(ilev,1)*wge_up(ilev,1)+wge_up(ilev,2)*wge_up(ilev,2))) &
            < zero ) THEN
            maxlev=ilev
            EXIT
          END IF
        END DO
        ugustu=wge_up(maxlev,1)
        vgustu=wge_up(maxlev,2)
      ELSE
        ugustu=0.
        vgustu=0.
      END IF
!      IF ((i == im2) .AND. (j==jm2)) PRINT *,'Synoptic wind angle for WGE_up:',                 &
!        wge_up(maxlev,2)

!     Rotating obtained winds to Earth surface
#if (EM_CORE == 1)
      ugustwind(i,j)=ugust*cosa(i,j)-vgust*sina(i,j)
      vgustwind(i,j)=vgust*cosa(i,j)+ugust*sina(i,j)
      ugustlow(i,j)=ugustl*cosa(i,j)-vgustl*sina(i,j)
      vgustlow(i,j)=vgustl*cosa(i,j)+ugustl*sina(i,j)
      ugustup(i,j)=ugustu*cosa(i,j)-vgustu*sina(i,j)
      vgustup(i,j)=vgustu*cosa(i,j)+ugustu*sina(i,j)
#else
      ugustwind(i,j)=ugust
      vgustwind(i,j)=vgust
      ugustlow(i,j)=ugustl
      vgustlow(i,j)=vgustl
      ugustup(i,j)=ugustu
      vgustup(i,j)=vgustu
#endif

!      IF ((i == im2) .AND. (j==jm2)) THEN
!        PRINT *,'Estimated gust wind: ',ugustwind(im2,jm2), vgustwind(im2,jm2) 
!        PRINT *,'Low bound gust wind: ',ugustlow(im2,jm2), vgustlow(im2,jm2) 
!        PRINT *,'Upper bound gust wind: ',ugustup(im2,jm2), vgustup(im2,jm2) 
!      END IF
    END DO
  END DO

END SUBROUTINE gustwindBrasseur01

REAL FUNCTION virtualpottemp(pt_vpt, r_vpt, l_vpt)
! Function to compute virtual potential temperature

  IMPLICIT NONE

  REAL, INTENT(IN)                                       :: pt_vpt, r_vpt, l_vpt 
! Local
  REAL, PARAMETER                                        :: vptA=0.61

  virtualpottemp=pt_vpt*(1+vptA*r_vpt-l_vpt)

END FUNCTION virtualpottemp

REAL FUNCTION pres2height(pressure)
! Function to convert hidrostatically a pressure to height in meters

  IMPLICIT NONE

  REAL, INTENT(IN)                                       :: pressure
! Local
  REAL, PARAMETER                                        :: T0=288.15, L=0.0065, p0=101325.,   &
    R=8.31447, grav=9.80665, M=0.0289644

  pres2height=T0/L*(1.-(pressure/p0)**(R*L/(grav*M)))
  IF (pres2height < 0.) pres2height=0.

END FUNCTION pres2height

REAL FUNCTION height2pres(height)
! Function to convert hidrostatically a height to pressure in Pa 

  IMPLICIT NONE

  REAL, INTENT(IN)                                       :: height 
! Local
  REAL, PARAMETER                                        :: T0=288.15, L=0.0065, p0=101325.,   &
    R=8.31447, grav=9.80665, M=0.0289644

  height2pres=p0*(1.-L*height/T0)**(grav*M/(R*L))

END FUNCTION height2pres

REAL FUNCTION pres2heightvis5d(pressure)
! Function to convert hidrostatically a pressure to a height in meters from vis5D conversion

  IMPLICIT NONE

  REAL, INTENT(IN)                                       :: pressure
! Local
  REAL, PARAMETER                                        :: refpres=101250., Hc=-7200.

  pres2heightvis5d=Hc*LOG(pressure/refpres)
  IF (pres2heightvis5d < 0.) pres2heightvis5d=0.

END FUNCTION pres2heightvis5d

REAL FUNCTION height2presvis5d(height)
! Function to convert hidrostatically a height in meters to pressure from vis5D conversion

  IMPLICIT NONE

  REAL, INTENT(IN)                                       :: height
! Local
  REAL, PARAMETER                                        :: refpres=101250., Hc=-7200.

  height2presvis5d=refpres*EXP(height/Hc)

END FUNCTION height2presvis5d
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#endif

END MODULE module_diagnostics
