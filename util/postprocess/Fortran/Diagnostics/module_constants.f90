MODULE module_constants
! Diagnostic modules with constants of computation of atmospheric doagnostic variables from netcdf files
! GMS. UC: December 2009. version v0.0
!
!!!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 module_constants.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -c

  IMPLICIT NONE
  
!   456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
  SAVE

  CHARACTER(LEN=50), PARAMETER                             :: blanks=' '
  REAL, PARAMETER                                          :: Lv=2500000.
  REAL, PARAMETER                                          :: Pi=3.141596
  REAL, PARAMETER                                          :: Rt=6371227.
  REAL, PARAMETER                                          :: Rv=461.5
  REAL, PARAMETER                                          :: e0=611.
  REAL, PARAMETER                                          :: epsilon_gamma=0.62197
  CHARACTER(LEN=50)                                        :: errmsg='ERROR - error - ERROR'//   &
    ' - error - ERROR'
  REAL, PARAMETER                                          :: es_base_tetens=610.78
  REAL, PARAMETER                                          :: es_Atetens_vapor=7.5
  REAL, PARAMETER                                          :: es_Btetens_vapor=237.3
  REAL, PARAMETER                                          :: es_Atetens_ice=9.5
  REAL, PARAMETER                                          :: es_Btetens_ice=265.5
  REAL, PARAMETER                                          :: extremeval=10000000000.
  REAL, PARAMETER                                          :: grav=9.807
  REAL, PARAMETER                                          :: missingval=-99999.
  REAL, PARAMETER                                          :: rocp=2./7.
  REAL, PARAMETER                                          :: tkelvin=273.15
  INTEGER, PARAMETER                                       :: yearref = 1970
  INTEGER, PARAMETER                                       :: yearleap = 1900

!!!!!!!!!!!!!!!!!! Variables
!      blanks: 50 character string of blanks
!      Lv: latent heat of vaporization of water [Jkg-1]
!      Pi: number pi [ACOS(-1.)]
!      Rt:  
!      Rv: gas constant for water vapor [J K-1kg-1]
!      e0: reference saturation vapor pressure (at T=273.15 K) [Pa]
!      eps_gamma: epsilon constant from 'psychrometer constant' (gamma)
!      error: error value
! Tetens equation (Tetens, 1930) for saturated vapor pressure, es=es_base*10**((T*A)/(T+B)) [Pa]
!      es_base_tetens: base value [Pa]
!      es_Atetens_vapor: constant A for vapor [C]
!      es_Btetens_vapor: constant B for vapor [C]
!      es_Atetens_ice: constant A for ice [C]
!      es_Btetens_vapor: constant B for ice [C]
!      extremeval: allowed maximum(minimum) value for diagnostic variables
!      grav: gravity acceleration [ms-2]
!      missingval: value for missings
!      rocp: 
!      tkelvin: conversion from celsius degrees to kelvin [K]
!      yearref: year as reference
!      yearleap: year as reference of leap years

END MODULE module_constants
