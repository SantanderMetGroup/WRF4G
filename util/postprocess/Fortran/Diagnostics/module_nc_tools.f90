MODULE module_nc_tools
  USE module_constants
  
  CONTAINS
! Subroutines/functions to make some general processes on netCDF files
! GMS. UC: January 2010. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!! Subroutines/Functions
! compute_dimensions: Subroutine to compute dimensions of output file
! compute_1Dmethod_values: Subroutine to compute 'method' values of dimensions 1D methods
! copy_nc_att: Subroutine to copy all global attributes from a netCDF file to other open one
! create_output: Subroutine to create netCDF output
! def_dimension: Subroutine to define a dimension from a dimensiondef type
! def_variable: Subroutine to define a diagnostic from a variabledef type
! def_dim: Subroutine to define a 1D dimension 
! def_dim_ver: Subroutine to define a vertical coordinate
! def_dim_time: Subroutine to define a time coordinate
! def_nc_var: Subroutine to define a variable inside a netCDF file 
! def_nc_gatt_text: Subroutine to define a global text attribute inside a netCDF file 
! diff_dimtimes: Function to give the difference in seconds between two time coordinate values
! error_nc: Subroutine to print error nc messages
! exists_dim: Function to determine if a dimension exists
! exists_var: Function to determine if a variable exists
! fill_inputs_50char: Subroutine to fill a kind-shape of input 50-character fields
! fill_inputs_real: Subroutine to fill a kind-shape of input real fields
! gattribute_REALvalue: Subroutine to obtain a real value from a global attribute from a netCDF
! gattribute_STRINGvalue: Subroutine to obtain a string value from an attribute from a netCDF file
! nc_gatts: Subroutine to print all global attributes of a netCDF file 
! nc_dimensions: Subroutine to obtain range of dimensions of a netCDF file
! nc_last_iddim: Function to give higest iddim of a netCDF file
! nc_last_idvar: Function to give higest idvar of a netCDF file
! nc_Ndim: Subroutine to obtain number of dimensions of a netCDF file
! search_dimensions: Subroutine to search a dimension from a given set of 'Nnc' netcCDF files  
! search_variables: Subroutine to search a list of 'Nsvars' variables from a given set of 'Nnc'
!   netcCDF files  

SUBROUTINE compute_1Dmethod_values(debg, Ninc, incs, dimensioncalc)
! Subroutine to compute 'method' values of dimensions 1D methods

  USE netcdf
  USE module_constants
  USE module_types
  USE module_gen_tools
  USE module_calc_tools

  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: debg, Ninc
  CHARACTER(LEN=50), DIMENSION(Ninc), INTENT(IN)          :: incs
  TYPE(dimensiondef), INTENT(INOUT)                       :: dimensioncalc
  
! Local
  INTEGER                                                 :: i, ival, ivar
  INTEGER                                                 :: rcode
  INTEGER, SAVE                                           :: dimcalcrange
  CHARACTER(LEN=50)                                       :: section
  INTEGER, DIMENSION(3)                                   :: dimfound
  REAL, DIMENSION(:), ALLOCATABLE                         :: dimvalues
  REAL(KIND=Rhigh), DIMENSION(:), ALLOCATABLE, TARGET,                                          &
    SAVE                                                  :: dimvalues_targ
  REAL, DIMENSION(:,:), ALLOCATABLE                       :: diminvalues
  INTEGER, DIMENSION(:,:), ALLOCATABLE                    :: dimvarfound, dimvardimfound
  REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE               :: valuesMATin

    
!!!!!!! Variables
! Ninc: number of input netCDF
! incs: names of input netCDF files
! dimensioncalc: dimension to compute its values by method wether it is possible
! dimfound: characteristics of existing dimension in input files
! diminvalues: input values to compute 'dimvalues'
! dimvarfound: where is located each variable in input files
! dimvardimfound: dimensions of 'dimvarfound'
! valuesMATin: 6D matrix with values of a variable from a netCDF
! dimcalcrange: range of calculations
! dimvalues: values of dimension

  section="'compute_1Dmethod_values'"

  IF (debg >= 150) PRINT *,'Section '//section//'... .. .'

!
!! Dimension takes values from an existing dimension. Resultant dimension will be an incremental 
!     list of values from 1 to range of input dimension

  IF (TRIM(dimensioncalc%INname) /= '-') THEN
    CALL search_dimensions(debg, incs, Ninc, dimensioncalc%INname, dimfound)
    
    IF (ALLOCATED(diminvalues)) DEALLOCATE(diminvalues)
    ALLOCATE (diminvalues(dimfound(3),1))
    
    DO ival=1, dimfound(3)
      diminvalues(ival,1)=ival*1.
    END DO

! Dimension range values
    dimensioncalc%Nvalues=dimfound(3)
    IF (ALLOCATED(dimvalues)) DEALLOCATE(dimvalues)
    ALLOCATE(dimvalues(dimfound(3)))
    IF (ALLOCATED(dimvalues_targ)) DEALLOCATE(dimvalues_targ)
    ALLOCATE(dimvalues_targ(dimfound(3)))

  END IF
!
!! Dimension takes values from input variables
  IF (dimensioncalc%NinVarnames > 0) THEN
    IF (ALLOCATED(dimvarfound)) DEALLOCATE(dimvarfound)
    ALLOCATE(dimvarfound(dimensioncalc%NinVarnames,2))
    IF (ALLOCATED(dimvardimfound)) DEALLOCATE(dimvardimfound)
    ALLOCATE(dimvardimfound(dimensioncalc%NinVarnames,6))
	  
! Looking for variables characteristics
!! 
    CALL search_variables(debg, incs, Ninc,                                                    &
      dimensioncalc%INvarnames(1:dimensioncalc%NinVarnames), dimensioncalc%NinVarnames,        &
      dimvarfound, dimvardimfound)

! Getting variables values and keeping only values from desired dimension
!!
    IF (ALLOCATED(diminvalues)) DEALLOCATE(diminvalues)
    ALLOCATE(diminvalues(dimvardimfound(1, dimensioncalc%indimensions(1)),                     &
      dimensioncalc%NinVarnames))
    
    DO ivar=1, dimensioncalc%NinVarnames
      IF (ALLOCATED(valuesMATin)) DEALLOCATE(valuesMATin)
      ALLOCATE(valuesMATin(dimvardimfound(ivar,1),dimvardimfound(ivar,2),                      &
        dimvardimfound(ivar,3),dimvardimfound(ivar,4),dimvardimfound(ivar,5),                  &
        dimvardimfound(ivar,6)))
      
      CALL fill_inputs_real(debg, incs, Ninc, dimvarfound(ivar, :),                             &
        dimvardimfound(ivar, :), valuesMATin)
      CALL give1D_from6D(debg, valuesMATin, dimvardimfound(ivar, :),                            &
        dimensioncalc%indimensions(ivar), (/1,1,1,1,1/),diminvalues(:,dimensioncalc%NinVarnames))
    END DO

! Dimension range values
    dimcalcrange = dimvardimfound(1, dimensioncalc%indimensions(1))
    dimensioncalc%Nvalues=dimcalcrange
    IF (ALLOCATED(dimvalues)) DEALLOCATE(dimvalues)
    ALLOCATE(dimvalues(dimvardimfound(1, dimensioncalc%indimensions(1))))
    IF (ALLOCATED(dimvalues_targ)) DEALLOCATE(dimvalues_targ)
    ALLOCATE(dimvalues_targ(dimvardimfound(1, dimensioncalc%indimensions(1))))

  END IF
  
  CALL calc_method1D(debg, dimensioncalc%method, dimvardimfound(1,                              &
    dimensioncalc%indimensions(1)), dimensioncalc%NinVarnames, diminvalues,                     &
    dimensioncalc%constant, dimvalues)
  dimvalues_targ=REAL(dimvalues, KIND=Rhigh)
  dimensioncalc%values=>dimvalues_targ
  IF (debg >= 150) PRINT *,'  ',dimensioncalc%Nvalues,' values of dimension: ',                 &
    dimensioncalc%values

END SUBROUTINE compute_1Dmethod_values

SUBROUTINE compute_dimensions(debg, ncid, Ndims, dimvec, xcar, ycar, names4basedims, Nfiles,    &
  files, file_gatt)
! Subroutine to compute dimensions of output file

  USE netcdf
  USE module_constants
  USE module_gen_tools
  USE module_calc_tools
  USE module_types

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, ncid, Ndims, Nfiles
  CHARACTER(LEN=50), DIMENSION(Ndims), INTENT(IN)         :: dimvec
  CHARACTER(LEN=500), DIMENSION(Nfiles), INTENT(IN)       :: files
  CHARACTER(LEN=500), INTENT(IN)                          :: file_gatt
  LOGICAL, INTENT(IN)                                     :: xcar, ycar
  CHARACTER(LEN=50), DIMENSION(4), INTENT(IN)             :: names4basedims
  TYPE(dimensiondef)                                      :: dimensioncompute, dimensionnew
  
! Local
  INTEGER                                                 :: i,idim
  INTEGER                                                 :: rcode, dimidin
  INTEGER                                                 :: jvar, lastdim, jodim
  CHARACTER(LEN=50)                                       :: section
  INTEGER, DIMENSION(3)                                   :: dimfound
  INTEGER, DIMENSION(:,:), ALLOCATABLE                    :: dimvarfound, dimvardimfound,       &
    invarvalues
  INTEGER, DIMENSION(Ndims,2)                             :: dimsid
  CHARACTER(LEN=250)                                      :: coordlong, messg
  REAL, DIMENSION(:), ALLOCATABLE                         :: dimensionvalues
  REAL, DIMENSION(:,:,:,:,:,:,:), ALLOCATABLE             :: valuesMATinA, valuesMATinB,        &
    valuesMATinC,valuesMATinD,valuesMATinE,valuesMATinF
  REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE               :: valuesMATdim, coordinate
  CHARACTER(LEN=50)                                       :: coordname, coordstd, coordunits,   &
    coordscoord
  CHARACTER(LEN=50), DIMENSION(:,:,:,:,:), ALLOCATABLE    :: charcoor
  REAL(KIND=Rhigh), DIMENSION(:), ALLOCATABLE             :: cdimvalues
  CHARACTER(LEN=1)                                        :: udate
  CHARACTER(LEN=4)                                        :: yearref

!!!!!!! Variables
! ncid: netCDF if where to write dimensions
! Ndims: number of dimensions to write (if they do not already exist)
! dimvec: vector with dimensions name to compute (they must be in 'dimensions_diagnostics.inf')
! dimensioncom: specific dimension to compute
! dimidin: dimension id in input file
! [x/y]car: are x and y coordinates cartesian?
! names4basedims: names of 4 basic dimensions (X, Y, Z, T) in input files
! Nfiles: number of input netCDF files in which range of dimensions should be found
! files: vector with input netCDF files
! file_gatt: file of 'namelist.gatt' from which global attributes will be retrieved and temporal 
!    dimension
! jvar: id of last variable in netCDF file
! lastdim: if of last dimension in netCDF file
! jodim: id of dimension (not 4 basic ones)
! dimfound: found characteristics of dimension in input files
! dimvarfound: found characteristics of variables related to dimension
! dimvardimfound: dimensions of dimvarfound
! invarvalues: values of variables used to compute dimension values (when they are 1D)
! dimsid: id of dimension as they are defined in 'ncid'. Id of dimensions will preserve order from
!   'dimensions_diagnostics.inf' file and their range
! valuesMATin[A/F]: 6D real matrix with values to compute dimensions
! valuesMATdim: 6D real matrix with 'dimension'-related matrix values
! coordname, coordstd, coordunits, coordscoord, coordaxis, coordpositive: strings for direct
!   definition of coordinates as 2D variables

  section="'compute_dimensions'"
  IF (debg >= 100) PRINT *,'Section '//section//'... .. .'

  jvar=nc_last_idvar(debg, ncid)
  lastdim=nc_last_iddim(debg, ncid)
  jodim=4

  com_dimensions: DO idim=1, Ndims
  
    IF (debg >= 150) PRINT *,"  Computing '"//TRIM(dimvec(idim))//"' dimension"
  
    CALL diagnostic_dim_inf(debg, dimvec(idim), nc_last_iddim(debg, ncid), dimensioncompute)

! Checking if dimension can be computed by 1D methods
    IF ( ANY(generic_calcs1D == dimensioncompute%method)) THEN
      CALL compute_1Dmethod_values(debg,   &
      Nfiles, files, dimensioncompute)
    END IF

    indiv_dim: SELECT CASE (dimvec(idim))
    
      CASE('lon')
! Longitude. Standard x-axis dimension
!!
        cartesian: IF (xcar) THEN
! lon coordinate is cartesian, thus it can be defined directly from input values

          IF (.NOT.exists_dim(ncid, dimvec(idim))) THEN
            jvar = jvar + 1
            CALL def_dimension(debg, ncid, dimensioncompute)
	    dimsid(1,1)=nc_last_iddim(debg, ncid)
	    dimsid(1,2)=dimensioncompute%Nvalues
          ELSE
            CALL def_dimension(debg, ncid, dimensionnew)
            rcode = nf90_inq_dimid(ncid, TRIM(dimvec(idim)), dimsid(1,1))
            CALL error_nc(section, rcode)
            rcode = nf90_inquire_dimension(ncid, dimsid(1,1), len=dimsid(1,2))
            CALL error_nc(section, rcode)
          ENDIF
    
        ELSE
! x coordinate is not cartesian, longitudes will be given by a 2D matrix driven by a base 2D 
!   coordinates labelled 'xc', 'yc', as it is stated by CF-1.4 conventions 
!      see http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/ch05s02.html
          IF (debg >= 100) PRINT *,'  X dimension is not cartesian...'
   
          IF (.NOT.exists_dim(ncid, 'xc'//blanks(1:50-LEN('xc')))) THEN
	    IF (debg >= 100) PRINT *, "  Dimension 'xc' will be created!"
	  
	    CALL search_dimensions(debg, files, Nfiles, names4basedims(1), dimfound)
	    
            IF (ALLOCATED(cdimvalues)) DEALLOCATE(cdimvalues)
            ALLOCATE(cdimvalues(dimfound(3)))

            DO i=1,dimfound(3)
	      cdimvalues(i)=i*1._Rhigh
	    END DO

            CALL fill_dimension_type(debg,'xc', nc_last_iddim(debg, ncid)+1, 'H', 'X','-',      &
	      dimfound(3), 0, (/'-'/), 'direct', 0., (/0/), 0, (/0/), '-',                      &
	      'X-coordinate in Cartesian system', 'm', dimfound(3), cdimvalues, '-', '-', '-',  &
	      dimensionnew)

            IF (debg >= 150) PRINT *,"  values 'xc':", cdimvalues

            CALL def_dimension(debg, ncid, dimensionnew)
	    jvar=jvar+1
	    dimsid(1,1)=nc_last_iddim(debg, ncid)
	    dimsid(1,2)=dimensionnew%Nvalues
          ELSE
            rcode = nf90_inq_dimid(ncid, 'xc', dimsid(1,1))
	    CALL error_nc(section, rcode)
            rcode = nf90_inquire_dimension(ncid, dimsid(1,1), len=dimsid(1,2))
	    CALL error_nc(section, rcode)
          END IF

          IF (.NOT.exists_dim(ncid, 'yc'//blanks(1:50-LEN('yc')))) THEN
	    IF (debg >= 100) PRINT *,"  Dimension 'yc' will be created!"
	    CALL search_dimensions(debg, files, Nfiles, names4basedims(2), dimfound)
	    
            IF (ALLOCATED(cdimvalues)) DEALLOCATE(cdimvalues)
   	    ALLOCATE(cdimvalues(dimfound(3)))

            DO i=1,dimfound(3)
	      cdimvalues(i)=i*1._Rhigh
	    END DO

            CALL fill_dimension_type(debg,'yc', nc_last_iddim(debg, ncid)+1, 'H', 'Y','-',      &
	      dimfound(3), 0, (/'-'/), 'direct', 0., (/0/), 0, (/0/), '-',                      &
	      'Y-coordinate in Cartesian system', 'm', dimfound(3), cdimvalues, '-', '-', '-',  &
	      dimensionnew)

            IF (debg >= 150) PRINT *,"  values 'yc':", cdimvalues

            CALL def_dimension(debg, ncid, dimensionnew)
            dimsid(2,1)=nc_last_iddim(debg, ncid)
	    dimsid(2,2)=dimensionnew%Nvalues
          ELSE
            rcode = nf90_inq_dimid(ncid, 'yc', dimsid(2,1))
	    CALL error_nc(section, rcode)
            rcode = nf90_inquire_dimension(ncid, dimsid(2,1), len=dimsid(2,2))
	    CALL error_nc(section, rcode)
          END IF

! Defining and writting real values of 2D longitudes
            coordname='lon'
            notexistnoCAR: IF (.NOT.exists_var(ncid, coordname)) THEN
	      jvar=jvar+1
              IF (debg >= 100) PRINT *,'  Including real values of 2D longitudes...'

! Loooking for 2D longitude variable values or any way to compute them
!!
              IF (ALLOCATED(dimvarfound)) DEALLOCATE(dimvarfound)
	      ALLOCATE(dimvarfound(Nfiles,dimensioncompute%NinVarnames))
	      
              IF (ALLOCATED(dimvardimfound)) DEALLOCATE(dimvardimfound)
	      ALLOCATE(dimvardimfound(Nfiles, 6))
	      
              CALL search_variables(debg, files, Nfiles, dimensioncompute%INvarnames,           &
	        dimensioncompute%NinVarnames, dimvarfound, dimvardimfound)

              IF (ALLOCATED(valuesMATinA)) DEALLOCATE(valuesMATinA)
              ALLOCATE(valuesMATinA(dimvardimfound(1,1), dimvardimfound(2,1),                   &
                dimvardimfound(3,1), dimvardimfound(4,1), dimvardimfound(5,1),                  &
                dimvardimfound(6,1), dimensioncompute%NinVarnames))

              IF (ALLOCATED(valuesMATdim)) DEALLOCATE(valuesMATdim)
              ALLOCATE(valuesMATdim(dimvardimfound(1,1), dimvardimfound(2,1),                   &
                dimvardimfound(3,1), dimvardimfound(4,1), dimvardimfound(5,1),                  &
		dimvardimfound(6,1)))
  
! Filling real values of input variables
!! 
              CALL fill_inputs_real(debg, files, Nfiles, dimvarfound, dimvardimfound(1,:),      &
	        valuesMATinA)

! Computation. There are generic pre-configured computations in 'calc_method_gen6D' (in 
!   'module_calc_tools')
!!
              IF (ANY(generic_calcs6D==dimensioncompute%method)) THEN
                CALL calc_method_gen6D(debg, dimensioncompute%method, dimvardimfound(1,:),      &
		dimensioncompute%NinVarnames, valuesMATinA, dimensioncompute%constant,          &
		0, (/0/), valuesMATdim ) 
              ELSE
                IF (debg>= 100) PRINT *,"  Specific dimension '"//TRIM(dimensioncompute%name)// &   
                  "' computation..." 
              END IF

              coordlong='longitude'
              coordstd='longitude'
              coordunits='degrees'
              coordscoord='lon lat'

              CALL def_nc_var(ncid, nc_last_idvar(debg, ncid)+1, dimensioncompute%name, 5, 2,   &
	        (/dimsid(1,1), dimsid(2,1), 1, 1, 1, 1/), "XY ", coordlong, coordstd,coordunits,&
		"-", coordscoord, debg)

              rcode = nf90_put_var(ncid, nc_last_idvar(debg, ncid), valuesMATdim(:,:,1,1,1,1))
              CALL error_nc(section, rcode)
            END IF notexistnoCAR
          END IF cartesian

! Y dimension
!!
      CASE('lat')
! Latitude. Standard y-axis dimension
!!
    
        cartesiany: IF (ycar) THEN
! lat coordinate is cartesian, thus it can be defined directly from input values

          IF (.NOT.exists_dim(ncid, dimvec(idim))) THEN
            jvar = nc_last_iddim(debg, ncid)+1
            CALL def_dimension(debg, ncid, dimensioncompute)
	    dimsid(2,1)=nc_last_iddim(debg, ncid)
	    dimsid(2,2)=dimensioncompute%Nvalues
          ELSE
            CALL def_dimension(debg, ncid, dimensionnew)
            rcode = nf90_inq_dimid(ncid, TRIM(dimvec(idim)), dimsid(2,1))
            CALL error_nc(section, rcode)
          ENDIF
    
        ELSE
! y coordinate is not cartesian, longitudes will be given by a 2D matrix driven by a base 2D 
!   coordinates labelled 'xc', 'yc', as it is stated by CF-1.4 conventions 
!      see http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/ch05s02.html
          IF (debg >= 100) PRINT *,'  Y dimension is not cartesian...'
   
          IF (.NOT.exists_dim(ncid, 'xc'//blanks(1:50-LEN('xc')))) THEN
	  
	    CALL search_dimensions(debg, files, Nfiles, names4basedims(1), dimfound)
	    
            IF (ALLOCATED(cdimvalues)) DEALLOCATE(cdimvalues)
            ALLOCATE(cdimvalues(dimfound(3)))

            DO i=1,dimfound(3)
	      cdimvalues(i)=i*1._Rhigh
	    END DO

            CALL fill_dimension_type(debg,'xc', nc_last_iddim(debg, ncid)+1, 'H', 'X','-',      &
	      dimfound(3), 0, (/'-'/), 'direct', 0., (/0/), 0, (/0/), '-',                      &
	      'X-coordinate in Cartesian system', 'm', dimfound(3), cdimvalues, '-', '-', '-',  &
	      dimensionnew)

            IF (debg >= 150) PRINT *,"  values 'xc':", cdimvalues

            CALL def_dimension(debg, ncid, dimensionnew)
	    jvar=jvar+1
	    dimsid(1,1)=nc_last_iddim(debg, ncid)
	    dimsid(1,2)=dimensionnew%Nvalues
   
          ELSE
            rcode = nf90_inq_dimid(ncid, 'xc', dimsid(1,1))
            CALL error_nc(section, rcode)
            rcode = nf90_inquire_dimension(ncid, dimsid(1,1), len=dimsid(1,2))
            CALL error_nc(section, rcode)

          END IF

          IF (.NOT.exists_dim(ncid, 'yc'//blanks(1:50-LEN('yc')))) THEN
	  
	    CALL search_dimensions(debg, files, Nfiles, names4basedims(2), dimfound)
	    
            IF (ALLOCATED(cdimvalues)) DEALLOCATE(cdimvalues)
   	    ALLOCATE(cdimvalues(dimfound(3)))

            DO i=1,dimfound(3)
	      cdimvalues(i)=i*1._Rhigh
	    END DO

            CALL fill_dimension_type(debg,'yc', nc_last_iddim(debg, ncid)+1, 'H', 'Y','-',      &
	      dimfound(3), 0, (/'-'/), 'direct', 0., (/0/), 0, (/0/), '-',                      &
	      'Y-coordinate in Cartesian system', 'm', dimfound(3), cdimvalues, '-', '-', '-',  &
	      dimensionnew)

            IF (debg >= 150) PRINT *,"  values 'yc':", cdimvalues

            CALL def_dimension(debg, ncid, dimensionnew)
	    dimsid(2,1)=nc_last_iddim(debg, ncid)
	    dimsid(2,2)=dimensionnew%Nvalues
          ELSE
            rcode = nf90_inq_dimid(ncid, 'yc', dimsid(2,1))
            CALL error_nc(section, rcode)
            rcode = nf90_inquire_dimension(ncid, dimsid(2,1), len=dimsid(2,2))
            CALL error_nc(section, rcode)
          END IF

! Defining and writting real values of 2D latitudes
            coordname='lat'
            notexistnoCARy: IF (.NOT.exists_var(ncid, coordname)) THEN
	      jvar=jvar+1
              IF (debg >= 100) PRINT *,'  Including real values of 2D latitudes...'

! Loooking for 2D latitude variable values or any way to compute them
!!
              IF (ALLOCATED(dimvarfound)) DEALLOCATE(dimvarfound)
	      ALLOCATE(dimvarfound(Nfiles,dimensioncompute%NinVarnames))
	      
              IF (ALLOCATED(dimvardimfound)) DEALLOCATE(dimvardimfound)
	      ALLOCATE(dimvardimfound(Nfiles, 6))
	      
              CALL search_variables(debg, files, Nfiles, dimensioncompute%INvarnames,           &
	        dimensioncompute%NinVarnames, dimvarfound, dimvardimfound)

              IF (ALLOCATED(valuesMATinA)) DEALLOCATE(valuesMATinA)
              ALLOCATE(valuesMATinA(dimvardimfound(1,1), dimvardimfound(2,1),                   &
                dimvardimfound(3,1), dimvardimfound(4,1), dimvardimfound(5,1),                  &
                dimvardimfound(6,1), dimensioncompute%NinVarnames))

              IF (ALLOCATED(valuesMATdim)) DEALLOCATE(valuesMATdim)
              ALLOCATE(valuesMATdim(dimvardimfound(1,1), dimvardimfound(2,1),                   &
                dimvardimfound(3,1), dimvardimfound(4,1), dimvardimfound(5,1),                  &
		dimvardimfound(6,1)))
  
! Filling real values of input variables
!! 
              CALL fill_inputs_real(debg, files, Nfiles, dimvarfound, dimvardimfound(1,:),      &
	        valuesMATinA)

! Computation. There are generic pre-configured computations in 'calc_method_gen6D' (in 
!   'module_calc_tools')
!!
              IF (ANY(generic_calcs6D==dimensioncompute%method)) THEN
                CALL calc_method_gen6D(debg, dimensioncompute%method, dimvardimfound(1,:),      &
		dimensioncompute%NinVarnames, valuesMATinA, dimensioncompute%constant,          &
		0, (/0/), valuesMATdim ) 
              ELSE
                IF (debg>= 100) PRINT *,"  Specific dimension '"//TRIM(dimensioncompute%name)// &
                  "' computation..."
              END IF

              coordlong='latitude'
              coordstd='latitude'
              coordunits='degrees'
              coordscoord='lon lat'

              CALL def_nc_var(ncid, nc_last_idvar(debg, ncid)+1, dimensioncompute%name, 5, 2,   &
	        (/dimsid(1,1), dimsid(2,1), 1, 1, 1, 1/), "XY ", coordlong, coordstd,coordunits,&
		"-", coordscoord, debg)

              rcode = nf90_put_var(ncid, nc_last_idvar(debg, ncid), valuesMATdim(:,:,1,1,1,1))
              CALL error_nc(section, rcode)
            END IF notexistnoCARy
          END IF cartesiany

! Z-dimension
!!
      CASE('depth')
        IF (.NOT.exists_dim(ncid,dimensioncompute%name)) THEN
          IF (debg >= 100) PRINT *,'  Creation of z dimension...'
          jodim = jodim + 1

            CALL def_dimension(debg, ncid, dimensioncompute)
	    dimsid(nc_last_iddim(debg, ncid),1)=nc_last_iddim(debg, ncid)
	    dimsid(nc_last_iddim(debg, ncid),2)=dimensioncompute%Nvalues
        ELSE
          rcode = nf90_inq_dimid(ncid, dimensioncompute%name, dimsid(jodim,1))
          CALL error_nc(section, rcode)
          rcode = nf90_inquire_dimension(ncid, dimsid(jodim,1), len=dimsid(jodim,2))
          CALL error_nc(section, rcode)
        END IF

      CASE('height')
        IF (.NOT.exists_dim(ncid,dimensioncompute%name)) THEN
          IF (debg >= 100) PRINT *,'  Creation of z dimension...'
          jodim = jodim + 1

            CALL def_dimension(debg, ncid, dimensioncompute)
	    dimsid(nc_last_iddim(debg, ncid),1)=nc_last_iddim(debg, ncid)
	    dimsid(nc_last_iddim(debg, ncid),2)=dimensioncompute%Nvalues
        ELSE
          rcode = nf90_inq_dimid(ncid, dimensioncompute%name, dimsid(jodim,1))
          CALL error_nc(section, rcode)
          rcode = nf90_inquire_dimension(ncid, dimsid(jodim,1), len=dimsid(jodim,2))
          CALL error_nc(section, rcode)
        END IF

      CASE('lev')
        IF (.NOT.exists_dim(ncid,dimensioncompute%name)) THEN
          IF (debg >= 100) PRINT *,'  Creation of z dimension...'
          jvar = jvar + 1

            CALL def_dimension(debg, ncid, dimensioncompute)
	    dimsid(3,1)=nc_last_iddim(debg, ncid)
	    dimsid(3,2)=dimensioncompute%Nvalues
        ELSE
          rcode = nf90_inq_dimid(ncid, dimensioncompute%name, dimsid(3,1))
          CALL error_nc(section, rcode)
          rcode = nf90_inquire_dimension(ncid, dimsid(3,1), len=dimsid(3,2))
          CALL error_nc(section, rcode)
        END IF

      CASE('plev')
        IF (.NOT.exists_dim(ncid,dimensioncompute%name)) THEN
          IF (debg >= 100) PRINT *,'  Creation of z dimension...'
          jodim = jodim + 1

            CALL def_dimension(debg, ncid, dimensioncompute)
            dimsid(nc_last_iddim(debg, ncid),1)=nc_last_iddim(debg, ncid)
            dimsid(nc_last_iddim(debg, ncid),2)=dimensioncompute%Nvalues
        ELSE
          rcode = nf90_inq_dimid(ncid, dimensioncompute%name, dimsid(jodim,1))
          CALL error_nc(section, rcode)
          rcode = nf90_inquire_dimension(ncid, dimsid(jodim,1), len=dimsid(jodim,2))
          CALL error_nc(section, rcode)
        END IF

! T-dimension
!!
      CASE('time')

        IF (.NOT.exists_dim(ncid,dimensioncompute%name)) THEN
          IF (debg >= 100) PRINT *,'  Creation of t dimension...'
          jvar = nc_last_idvar(debg, ncid) + 1
          SELECT CASE (dimensioncompute%method)
            CASE ('Y-m-d_h:m:s_refT')
! Time values have the format [YYYY]-[MM]-[DD]_[HH]:[MI]:[SS] in a character variable (of 19
!    characrters length as second dimension). Time variable is processed to give date as 
!    dimensioncompute%options(2) (1: seconds, 2: minutes, 3: hours, 4: days) since a reference date
!    dimensioncompute%options(1)-01-01_00:00:00 
!!
              IF (ALLOCATED(dimvarfound)) DEALLOCATE(dimvarfound)
	      ALLOCATE(dimvarfound(Nfiles, dimensioncompute%NinVarnames))
	      
              IF (ALLOCATED(dimvardimfound)) DEALLOCATE(dimvardimfound)
	      ALLOCATE(dimvardimfound(dimensioncompute%NinVarnames,6))
	      
              CALL search_variables(debg, files, Nfiles, dimensioncompute%INvarnames,           &
	        dimensioncompute%NinVarnames, dimvarfound, dimvardimfound)
	    
              IF (ALLOCATED(cdimvalues)) DEALLOCATE(cdimvalues)
              ALLOCATE(cdimvalues(dimvardimfound(1,1))) 
              IF (ALLOCATED(charcoor)) DEALLOCATE(charcoor)
              ALLOCATE(charcoor(dimvardimfound(1,2), dimvardimfound(1,3), dimvardimfound(1,4),  &
	        dimvardimfound(1,5), dimvardimfound(1,6)))

              CALL fill_inputs_50char(debg, (/file_gatt/), 1, dimvarfound(1,:),                 &
	        dimvardimfound(1,:), charcoor)

! Reference year and time units
              yearref=Int_String(debg, dimensioncompute%options(1))
	      SELECT CASE (dimensioncompute%options(2))
                CASE (1)
	          udate='s'
	        CASE (2)
		  udate='m'
		CASE (3)
		  udate='h'
		CASE (4)
		  udate='h'
		CASE DEFAULT
		  messg="Nothing to do with time-units '"//CHAR(dimensioncompute%options(2)+48) &
		    //"'"
		  CALL diag_fatal(messg)
	      END SELECT
              DO i=1, dimvardimfound(1,dimensioncompute%indimensions(1))
                CALL diff_dates(debg, yearref//'-01-01_00:00:00', charcoor(i,1,1,1,1), udate,   &
                  dimensioncompute%options(1), cdimvalues(i))
              END DO
              dimensioncompute%Nvalues=dimvardimfound(1,dimensioncompute%indimensions(1))

              IF (debg >= 100) PRINT *,"  values 'tc':", cdimvalues
	      CALL fill_dimension_type(debg,dimensioncompute%name, nc_last_iddim(debg, ncid)+1, &
	        'T', 'T', '-', dimensioncompute%Nvalues, 0, (/'-'/), '-', 0., (/0/), 0, (/0/),  &
		dimensioncompute%stdname, dimensioncompute%lonname, dimensioncompute%units,     &
		dimensioncompute%Nvalues, cdimvalues, dimensioncompute%coords,                  &
		dimensioncompute%positive, dimensioncompute%form, dimensionnew)

            END SELECT
           
            CALL def_dimension(debg, ncid, dimensionnew)
	    dimsid(4,1)=nc_last_iddim(debg, ncid)
	    dimsid(4,2)=dimensionnew%Nvalues

        ELSE
          rcode = nf90_inq_dimid(ncid, dimensioncompute%name, dimsid(4,1))
          CALL error_nc(section, rcode)
          rcode = nf90_inquire_dimension(ncid, dimsid(4,1), len=dimsid(4,2))
          CALL error_nc(section, rcode)
        END IF

      CASE DEFAULT
        messg="  Nothing to do with dimension: '"//TRIM(dimvec(idim))//"'"
	CALL diag_fatal(messg)
    
    END SELECT indiv_dim

  END DO com_dimensions
  IF (debg >= 100) THEN
    PRINT *,'  Output file dimensions id:'
    PRINT *,'  dx:',dimsid(1,1),' rg= ',dimsid(1,2),' dy:',dimsid(2,1),' rg= ',dimsid(2,2),     &
      ' dz:',dimsid(3,1),' rg= ',dimsid(3,2),' dt:',dimsid(4,1),' rg= ',dimsid(4,2)
    IF (Ndims > 4) PRINT *,'   ',(dimsid(idim,1),' rg= ',dimsid(idim,2), idim=5,Ndims)
  END IF

END SUBROUTINE compute_dimensions

SUBROUTINE copy_nc_gatt(debg, mcid, infile)
! Subroutine to copy all global attributes from a netCDF file to other open one

  USE netcdf
  USE module_constants
  
  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: debg, mcid
  CHARACTER(LEN=500), INTENT(IN)                          :: infile
  
! Local
  INTEGER                                                 :: iatt, rcode
  INTEGER                                                 :: ncid
  INTEGER                                                 :: ndims, nvars, ngatts, nunlimdimid
  CHARACTER(LEN=50)                                       :: section, attname

!!!!!!!!!! Variables
! mcid: netCDF id to which attributes will be copied
! infile: netCDF file from which attributes will be copied

  section="'copy_nc_att'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  
  rcode = nf90_open(TRIM(infile), 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)

  rcode = nf90_inquire(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)

  rcode = nf90_redef(mcid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
  
  DO iatt=1, ngatts
    rcode = nf90_inq_attname(ncid, NF90_GLOBAL, iatt, attname)
    rcode = nf90_copy_att (ncid, NF90_GLOBAL, attname, mcid, NF90_GLOBAL)
    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
  END DO

  rcode = nf90_enddef(mcid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//nf90_strerror(rcode)
  
  rcode = nf90_close(ncid)

END SUBROUTINE copy_nc_gatt

SUBROUTINE create_output(debg, outfile, dimsinname, Ndimsout, dimsoutname, file_gatt, xcar,     &
  ycar, ifiles, Nifiles)
! create_output: Subroutine to create netCDF output with 4 basic coordinates: lon, lat, lev, time

  USE netcdf
  USE module_constants
  USE module_calc_tools, ONLY: diff_dates

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: Ndimsout, debg, Nifiles
  CHARACTER(LEN=500), INTENT(IN)                         :: outfile, file_gatt
  CHARACTER(LEN=500), DIMENSION(Nifiles), INTENT(IN)     :: ifiles
  CHARACTER(LEN=50), DIMENSION(4), INTENT(IN)            :: dimsinname
  CHARACTER(LEN=50), DIMENSION(Ndimsout), INTENT(IN)     :: dimsoutname
  LOGICAL, INTENT(IN)                                    :: xcar, ycar

! Local
  INTEGER                                                :: i, icord
  INTEGER                                                :: rcode, oid, attlen, jv
  INTEGER, DIMENSION(4)                                  :: dimsid
  INTEGER, DIMENSION(4,2)                                :: foundCnames 
  INTEGER, DIMENSION(4,6)                                :: dimcoords
  CHARACTER(LEN=50)                                      :: section, att_text, attnamei,        &
    coordname, coordstd, coordunits, coordscoord, coordaxis, coordpositive
  CHARACTER(LEN=250)                                     :: coordlong
  CHARACTER(LEN=100)                                     :: attvalue, coordformula
  REAL, DIMENSION(:), ALLOCATABLE                        :: values_cord
  REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE              :: coordinate
  CHARACTER(LEN=50), DIMENSION(:,:,:,:,:), ALLOCATABLE   :: charcoor
  LOGICAL                                                :: exists_out

!!!!!!!!!!!!!!!!! Variables
! outfile: output netCDF file
! dim[x/y/z/t]: spatio-temporal range of dimensions
! dimsinname: vector with name of 4-basic dimensions from input files
! Ndimsout: number of dimensions ni output file
! dimsoutname: vector with name of dimensions in output file
! file_gatt: netCDF file from which all global attributes will be copyed and non-cartesian 
!    values of coordinates will be obtained
! [x/ycar]: whether x and y coordinates are cartesian (if not values from dimsvarname will be used)
! ifiles: vector with names of input netCDF files
! Nifiles: number of 'ifiles'
! outid: netCDF output id file
! jv: variable id in outfile
! dimids: vector with dimension id in output file
! values_cord: cartesian values of a coordinate
! coordinate: 2D values of a coordinate
! foundCnames: position in 'file_gatt' of coordinates
! dimcoords: dimension of coordinates

  section="'create_output'"
  jv = 0

!  CALL search_variables(debg, (/file_gatt/), 1, dimsvarname, 4, foundCnames, dimcoords)

  IF (debg >= 100) THEN
    PRINT *,"Creation of output file '"//TRIM(outfile)//"'..."
!    PRINT *,'  with dimensions: '
!    PRINT *,'  dimx: ',dimx,' dimy:',dimy,' dimz:',dimz,' dimt:',dimt
!    PRINT *,'  variables with dimension values: ',(TRIM(dimsvarname(i)), char(44), i=1,3),      &
!      TRIM(dimsvarname(4))
  END IF
  INQUIRE( FILE=TRIM(outfile), EXIST=exists_out) 
  IF (exists_out) THEN
    PRINT *,"Acting on a pre-existing file! '"//TRIM(outfile)//"'"
    rcode = nf90_open(TRIM(outfile), NF90_WRITE, oid)
    CALL error_nc(section, rcode)
  ELSE
    PRINT *,"Opening the new file! '"//TRIM(outfile)//"'"  
    rcode = nf90_create(TRIM(outfile), NF90_CLOBBER, oid)
    CALL error_nc(section, rcode)
  END IF

! Head section of new netCDF
!!  
  att_text='Conventions'
  attvalue='CF-1.4'
  CALL def_nc_gatt_text(debg, oid, att_text, attvalue)
  CALL copy_nc_gatt(debg, oid, file_gatt)

! Dimension section
!!
  CALL compute_dimensions(debg, oid, Ndimsout, dimsoutname, xcar, ycar, dimsinname, Nifiles,    &
    ifiles, file_gatt)
  
  rcode = nf90_close(oid)

END SUBROUTINE create_output

SUBROUTINE def_dimension(debg, ncid, dimval)
! Subroutine to define a dimension from a dimension type variable

  USE netcdf
  USE module_constants
  USE module_types

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'
!  INCLUDE 'include/types.inc'

  INTEGER, INTENT(IN)                                    :: debg, ncid
  TYPE(dimensiondef), INTENT(IN)                         :: dimval
  
! Local
  INTEGER                                                :: rcode, attlen, ivar, idim
  CHARACTER(LEN=50)                                      :: section, atttext
  REAL, DIMENSION(:), ALLOCATABLE                        :: dimension_values

!!!!!!!!!!!!! Variables
! ncid: netCDF id
! idim: id of dimension to be defined
! ivar: variable id assignated to values of dimension
! Text values of dimension (according to CF-1.4 conventions)

  section="'def_dimension'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  
  ivar=nc_last_idvar(debg, ncid)+1
  idim=nc_last_iddim(debg, ncid)+1

  IF (debg >= 100) PRINT *,"  Definition of dimension '"//TRIM(dimval%name)//"' number: ",idim, &
    'id associated var: ',ivar

  rcode = nf90_redef(ncid)
  CALL error_nc(section, rcode)

  IF (debg >= 100) PRINT *,'  dimension range: ',dimval%Nvalues
  IF (TRIM(dimval%name) == 'time') THEN
    rcode = nf90_def_dim(ncid, TRIM(dimval%name), NF90_UNLIMITED, idim)  
    CALL error_nc(section, rcode)
  ELSE
    rcode = nf90_def_dim(ncid, TRIM(dimval%name), dimval%Nvalues, idim)
    CALL error_nc(section, rcode, Ivalue = dimval%Nvalues)
  END IF

  IF (debg >= 100) PRINT *,'  dimension of associated variable (with dimension values): ',      &
    dimval%Nvalues,' of dimension #:', idim
    rcode = nf90_def_var(ncid, TRIM(dimval%name), NF90_DOUBLE, (/idim/), ivar)
  CALL error_nc(section, rcode)

  IF (TRIM(dimval%axis) /= '-') THEN
    IF (debg >= 100) PRINT *,"  axis attribute: '"//TRIM(dimval%axis)//"'"
    atttext = TRIM(dimval%axis) 
    attlen = LEN_TRIM(atttext)
    rcode = nf90_put_att(ncid, ivar, "axis", atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  IF (TRIM(dimval%stdname) /= '-') THEN
    IF (debg >= 100) PRINT *,"  std. name attribute: '"//TRIM(dimval%stdname)//"'"
    atttext = TRIM(dimval%stdname)
    attlen = len_trim(atttext)
    rcode = nf90_put_att(ncid, ivar, "standard_name", atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  IF (TRIM(dimval%lonname) /= '-') THEN
    IF (debg >= 100) PRINT *,"  long name attribute: '"//TRIM(dimval%lonname)//"'"
    atttext = TRIM(dimval%lonname)
    attlen = len_trim(atttext)
    rcode = nf90_put_att(ncid, ivar, "long_name", atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  IF (TRIM(dimval%units) /= '-') THEN 
    IF (debg >= 100) PRINT *,"  units attribute: '"//TRIM(dimval%units)//"'"
    atttext = TRIM(dimval%units)
    attlen = len_trim(atttext)
    rcode = nf90_put_att(ncid, ivar, "units", atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  IF (TRIM(dimval%coords) /= '-') THEN 
    IF (debg >= 100) PRINT *,"  coordinates attribute: '"//TRIM(dimval%coords)//"'"
    atttext = TRIM(dimval%coords)
    attlen = len_trim(atttext)
    rcode = nf90_put_att(ncid, ivar, "coords", atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  IF (TRIM(dimval%positive) /= '-') THEN 
    IF (debg >= 100) PRINT *,"  positive attribute: '"//TRIM(dimval%positive)//"'"
    atttext = TRIM(dimval%positive)
    attlen = len_trim(atttext)
    rcode = nf90_put_att(ncid, ivar, "positive", atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  IF (TRIM(dimval%form) /= '-') THEN 
    IF (debg >= 100) PRINT *,"  formula attribute: '"//TRIM(dimval%form)//"'"
    atttext = TRIM(dimval%form)
    attlen = len_trim(atttext)
    rcode = nf90_put_att(ncid, ivar, "formula_terms", atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  rcode = nf90_enddef(ncid)
  CALL error_nc(section, rcode)

  IF (debg >= 100) PRINT *,'  values: ',dimval%values
  rcode = nf90_put_var (ncid, ivar, dimval%values)
  CALL error_nc(section, rcode)

  RETURN
END SUBROUTINE def_dimension

SUBROUTINE def_variable (debg, mcid, varwrite )
! Subroutine to define a variable inside a netCDF file

  USE netcdf
  USE module_constants
  USE module_gen_tools, ONLY: diag_fatal
  USE module_types

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: mcid, debg
  TYPE(variabledef)                                      :: varwrite
  
! Local
  INTEGER                                                :: rcode, ilen, ivar
  CHARACTER (LEN=60)                                     :: att_text
  CHARACTER (LEN=50)                                     :: section
  CHARACTER(LEN=250)                                     :: messg

!!!!!!!!!! Variables
! mcid: netCDF file id
! varwrite: vriable to define and write in netCDF
! ivar: variable id
 
  section="'def_variable'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf90_redef(mcid)
  CALL error_nc(section, rcode)
  ivar=nc_last_idvar(debg, mcid)+1

! Initial definition
!!
  SELECT CASE (varwrite%type)
    CASE ( 2 ) 
!     Text variable
!!
      IF (debg >= 100) PRINT *,"  Defining text variable '"//TRIM(varwrite%name)//"' number:",  &
        ivar
      rcode = nf90_def_var(mcid, TRIM(varwrite%name), NF90_CHAR, varwrite%shape, ivar)
      CALL error_nc(section, rcode)
!      rcode = nf90_put_att_int(mcid, ivar, "FieldType", NF90_INT, 1, 104)
!      CALL error_nc(section, rcode)

    CASE ( 4 ) 
!     Integer variable
!!
      IF (debg >= 100) PRINT *,"  Defining real variable '"//TRIM(varwrite%name)//"' number:",  &
        ivar
      rcode = nf90_def_var(mcid, TRIM(varwrite%name), NF90_REAL, varwrite%shape, ivar)
      CALL error_nc(section, rcode)
      rcode = nf90_put_att(mcid, ivar, "FieldType", 106)
      CALL error_nc(section, rcode)
  
    CASE ( 5 ) 
!     Real variable
!!
      IF (debg >= 100) PRINT *,"  Defining real variable '"//TRIM(varwrite%name)//"' number:",  &
        ivar
      rcode = nf90_def_var(mcid, TRIM(varwrite%name), NF90_REAL, varwrite%shape, ivar)
      CALL error_nc(section, rcode)
      rcode = nf90_put_att(mcid, ivar, "FieldType", 104)
      CALL error_nc(section, rcode)
    CASE DEFAULT
      messg="Nothing to do with variable type '"//CHAR(varwrite%type+48)//"'"
      CALL diag_fatal(messg)
      
  END SELECT

  IF (TRIM(varwrite%stdname) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding standard name attribute"
    att_text = TRIM(varwrite%stdname)
    ilen = len_trim(att_text)
    rcode = nf90_put_att(mcid, ivar, "standard_name", att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

  IF (TRIM(varwrite%lonname) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding long name attribute"
    att_text = TRIM(varwrite%lonname)
    ilen = len_trim(att_text)
    rcode = nf90_put_att(mcid, ivar, "long_name", att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

  IF (TRIM(varwrite%units) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding units attribute"
    att_text = TRIM(varwrite%units)
    ilen = len_trim(att_text)
    rcode = nf90_put_att(mcid, ivar, "units", att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

  IF (TRIM(varwrite%coords) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding coordinates attribute"
    att_text = TRIM(varwrite%coords)
    ilen = len_trim(att_text)
    rcode = nf90_put_att(mcid, ivar, "coordinates", att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

  IF (TRIM(varwrite%form) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding formula attribute"
    att_text = TRIM(varwrite%form)
    ilen = len_trim(att_text)
    rcode = nf90_put_att(mcid, ivar, "formula", att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

!  IF (TRIM(varwrite%stdname) /= '-') THEN
!    IF (debg >= 100) PRINT *,"  Adding standard name attribute"
!    att_text = TRIM(varwrite%stdname)
!    ilen = len_trim(att_text)
!    rcode = nf90_put_att(mcid, ivar, "standard_name", att_text(1:ilen) )
!    CALL error_nc(section, rcode)
!    att_text = ' '
!  END IF

  IF (debg >= 100) PRINT *,"  Adding missing attribute"
  att_text = '-99999.' 
  ilen = len_trim(att_text)
  rcode = nf90_put_att(mcid, ivar, "missing_value", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = '-99999.'
  rcode = nf90_put_att(mcid, ivar, "_Fillvalue", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  rcode = nf90_enddef(mcid)
  CALL error_nc(section, rcode)

END SUBROUTINE def_variable

SUBROUTINE def_nc_gatt_text (debg, mcid, atdesc, atvalue)
! Subroutine to define a global text attribute inside a netCDF file

  USE netcdf

  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: mcid, debg
  CHARACTER(LEN=50), INTENT(IN)                           :: atdesc
  CHARACTER(LEN=100), INTENT(IN)                          :: atvalue
  
! Local
  INTEGER                                                 :: rcode, ilen
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=100)                                      :: att_text

!!!!!!!!! Variables
! mcid: id of netCDF to include a text attribute
! atdesc: name of attribute
! atvalue: value of new attribute

  section="'def_nc_att_text'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf90_redef(mcid)
  CALL error_nc(section, rcode)
 
  IF (debg >= 100) PRINT *,"Adding global attribute '"//TRIM(atdesc)//"' in netCDF file"
  att_text = atvalue
  ilen = len_trim(att_text)
  rcode = nf90_put_att(mcid, NF90_GLOBAL, TRIM(atdesc), att_text(1:ilen) )
  CALL error_nc(section, rcode)

  att_text = ' '

  rcode = nf90_enddef(mcid)
  CALL error_nc(section, rcode)

END SUBROUTINE def_nc_gatt_text

SUBROUTINE def_nc_var (mcid, ivar0, cval, itype, idm, jshape, order, desc, stddesc, units, stag,&
  coord, debg )
! Subroutine to define a variable inside a netCDF file

  USE netcdf
  USE module_constants

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: mcid, itype, idm, ivar0
  CHARACTER(LEN=50), INTENT(IN)                          :: cval
  INTEGER, DIMENSION(6), INTENT(IN)                      :: jshape
  CHARACTER(LEN=3), INTENT(IN)                           :: order
  CHARACTER(LEN=250), INTENT(IN)                         :: desc
  CHARACTER(LEN=50), INTENT(IN)                          :: units, stddesc
  CHARACTER(LEN=1), INTENT(IN)                           :: stag
  CHARACTER(LEN=50), INTENT(IN)                          :: coord
  INTEGER, INTENT(IN)                                    :: debg

! Local
  INTEGER                                                :: rcode, ilen, ivar
  CHARACTER (LEN=60)                                     :: att_text
  CHARACTER (LEN=50)                                     :: section

!!!!!!!!!! Variables
! mcid: netCDF file id
! ivar: variable id
! cval: variable name
! itype: type of variable (1: ; 2: integer ; 3: ; 4: ; 5: real)
! idm: number of dimensions of variable
! jshape: shape of variable (which coordinates)
! order: memory order of coordinates attribute
! desc: description attribute of varible
! stddesc: standard (CF convection, http://cf-pcmdi.llnl.gov/) name of variable
! units: units attribute of variable
! stag: staggered attribute of variable
! coord: coordinates labels attribute of variable
! section: name of subroutine
 
  section="'def_nc_var'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf90_redef(mcid)
  CALL error_nc(section, rcode)
  ivar=nc_last_idvar(debg, mcid)+1

  IF ( itype == 5 ) THEN
    IF (debg >= 100) PRINT *,"  Defining real variable '"//TRIM(cval)//"' number:",ivar
    rcode = nf90_def_var(mcid, TRIM(cval), NF90_REAL, jshape(1:idm), ivar)
    CALL error_nc(section, rcode)
    rcode = nf90_put_att(mcid, ivar, "FieldType", 104)
    CALL error_nc(section, rcode)
  ENDIF

  IF (debg >= 100) PRINT *,"  Adding MemoryOrder attribute"
  att_text = order
  ilen = len_trim(att_text)
  rcode = nf90_put_att(mcid, ivar, "MemoryOrder", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"  Adding long_name attribute"
  att_text = desc
  ilen = len_trim(att_text)
  rcode = nf90_put_att(mcid, ivar, "long_name", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"  Adding standard_name attribute"
  att_text = stddesc
  ilen = len_trim(att_text)
  rcode = nf90_put_att(mcid, ivar, "standard_name", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"  Adding units attribute"
  att_text = units
  ilen = len_trim(att_text)
  rcode = nf90_put_att(mcid, ivar, "units", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"  Adding stagger attribute"
  att_text = stag
  ilen = len_trim(att_text)
  rcode = nf90_put_att(mcid, ivar, "stagger", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"  Adding coordinates attribute"
  att_text = coord
  ilen = len_trim(att_text)
  rcode = nf90_put_att(mcid, ivar, "coordinates", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"  Adding missing attribute"
  att_text = '-99999.' 
  ilen = len_trim(att_text)
  rcode = nf90_put_att(mcid, ivar, "missing_value", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = '-99999.'
  rcode = nf90_put_att(mcid, ivar, "_Fillvalue", att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  rcode = nf90_enddef(mcid)
  CALL error_nc(section, rcode)

END SUBROUTINE def_nc_var

REAL FUNCTION diff_dimtimes(debg, oid, dimT, NdimTA, NdimTB)
! Function to give the difference in seconds between two time coordinate values

  USE netcdf
  USE module_types
  USE module_gen_tools

  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: debg, oid, dimT, NdimTA, NdimTB
  
! Local
  INTEGER                                                 :: i, funit, ios, rcode
  INTEGER                                                 :: Ndimsout
  INTEGER                                                 :: timeidvar, Ntimes
  INTEGER, DIMENSION(1)                                   :: timeiddim
  CHARACTER(LEN=4)                                        :: lev_process
  CHARACTER(LEN=50)                                       :: X_grid_spacing_gatt,               &
    Y_grid_spacing_gatt, T_grid_spacing_gatt
  CHARACTER(LEN=3000)                                     :: p_levels, dimension_in4names,      &
    dimension_outnames
  LOGICAL                                                 :: cartesian_x, cartesian_y
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250), DIMENSION(:), ALLOCATABLE           :: dimsout0
  CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE            :: dimsout
  LOGICAL                                                 :: is_used
  TYPE(dimensiondef)                                      :: dimtime
  CHARACTER(LEN=250)                                      :: messg
  REAL(KIND=Rhigh), DIMENSION(:), ALLOCATABLE             :: Rtimes
  
!!!!!!! Variables
! oid: id of netCDF output file
! dimT: number of time dimension (from 'namelist.dimension_outnames')
! NdimT[A/B]: number of time steps to compute difference between them timeB-timeA
! dimension_outnames: dimension output names
! Ndimsout: number of output dimensions
! dimsout: vector with dimensions name
! dimtime: definition of temporal dimension
! timeidvar: id of variable with values of time dimension in 'oid'
! timeiddim: id of time dimension in 'oid'
! Ntimes: number of times in 'oid'
! Rtimes: real vector with time values

    NAMELIST /dimensions/ lev_process, p_levels, X_grid_spacing_gatt, Y_grid_spacing_gatt,        &
      T_grid_spacing_gatt, cartesian_x, cartesian_y, dimension_in4names, dimension_outnames

  section="'diff_dimtimes'"
  IF ( debg>= 150) PRINT *,'Section '//section//' ... .. .'

! Reading parameters from namelist
  DO funit=10,100
    INQUIRE(unit=funit, opened=is_used)
    IF (.not. is_used) EXIT
  END DO
  OPEN(funit, file='namelist.diagnostics', status='old', form='formatted', iostat=ios)
  IF ( ios /= 0 ) STOP "ERROR opening 'namelist.diagnostics'"
  READ(funit,dimensions)
  CLOSE(funit)

! Dimensions in output file
!!
  IF (debg >= 150) PRINT *,'  Dimensions in output file...'
  CALL number_values(dimension_outnames, debg, Ndimsout)
  IF (ALLOCATED(dimsout)) DEALLOCATE(dimsout)
  ALLOCATE(dimsout(Ndimsout))
  IF (ALLOCATED(dimsout0)) DEALLOCATE(dimsout0)
  ALLOCATE(dimsout0(Ndimsout))
 
  CALL string_values(dimension_outnames, debg, Ndimsout, dimsout0)
  dimsout=dimsout0
  
  IF (debg >= 150) THEN
    PRINT *,'  Dimensions in output file: '
    DO i=1, Ndimsout
      PRINT *,'  #: ',i,"'"//TRIM(dimsout(i))//"'"
    END DO
  END IF

! time dimension information in 'oid' a variable with the name has the dimension values
  rcode = nf90_inq_varid (oid, dimsout(dimT), timeidvar)
  CALL error_nc(section, rcode)
  
  rcode = nf90_inquire_variable (oid, timeidvar, dimids=timeiddim)
  CALL error_nc(section, rcode)
  rcode = nf90_inquire_dimension (oid, timeiddim(1), len=Ntimes)
  CALL error_nc(section, rcode)
  
! Loading time dimension information  
  CALL diagnostic_dim_inf(debg, dimsout(dimT), 4, dimtime)
  
  timemethod: SELECT CASE (dimtime%method)
    CASE ('Y-m-d_h:m:s_refT')
      IF (ALLOCATED(Rtimes)) DEALLOCATE(Rtimes)
      ALLOCATE(Rtimes(Ntimes))
      
      rcode = nf90_get_var (oid, timeidvar, Rtimes)
      diff_dimtimes = REAL(Rtimes(NdimTB) - Rtimes(NdimTA))
      timeu: SELECT CASE (dimtime%options(2))
        CASE (1)
!       Time in seconds
          diff_dimtimes = diff_dimtimes
        CASE (2)
!       Time in minutes
          diff_dimtimes = diff_dimtimes * 60.
        CASE (3)
!       Time in hours
          diff_dimtimes = diff_dimtimes * 3600.
        CASE (4)
!       Time in days
          diff_dimtimes = diff_dimtimes * 3600. * 24.
	  
      END SELECT timeu
  
    CASE DEFAULT
      messg="Nothing to do for '"//TRIM(dimtime%method)//"' method"
      CALL diag_fatal(messg)
      
  END SELECT timemethod
  
  IF (debg >= 75 ) PRINT *,'  Difference time between #time ',NdimTA,' & ',NdimTB,': ',         &
    diff_dimtimes,' seconds', Rtimes(NdimTB) - Rtimes(NdimTA)

END FUNCTION diff_dimtimes

SUBROUTINE error_nc(sec, rc, Ivalue, Rvalue, Cvalue)
! Subroutine to print error nc messages

  USE netcdf
  USE module_constants
  
  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: rc
  CHARACTER(LEN=50), INTENT(IN)                           :: sec
  INTEGER, INTENT(IN), OPTIONAL                           :: Ivalue
  REAL, INTENT(IN), OPTIONAL                              :: Rvalue
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL                  :: Cvalue

! Local
  
  IF (rc /= 0) THEN
  
    IF (PRESENT(Ivalue)) THEN
      PRINT *,TRIM(errmsg)//" in "//TRIM(sec)//" "//nf90_strerror(rc)//' :',Ivalue
    ELSEIF (PRESENT(Rvalue)) THEN
      PRINT *,TRIM(errmsg)//" in "//TRIM(sec)//" "//nf90_strerror(rc)//' :',Rvalue
    ELSEIF (PRESENT(Cvalue)) THEN
      PRINT *,TRIM(errmsg)//" in "//TRIM(sec)//" "//nf90_strerror(rc)//" :'"//TRIM(Cvalue)
    ELSE 
      PRINT *,TRIM(errmsg)//" in "//TRIM(sec)//" "//nf90_strerror(rc)
    END IF
  END IF
END SUBROUTINE error_nc

LOGICAL FUNCTION exists_dim(ncid, dimname)
! Function to determine if a dimension exists

  USE netcdf

  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: ncid
  CHARACTER(LEN=50), INTENT(IN)                           :: dimname
  
! Local
  INTEGER                                                 :: rcode, dimid
  CHARACTER(LEN=50)                                       :: section

!!!!!!! Variables
! ncid: netCDF id
! dimname: name of dimension

  section="'exists_dim'"
  exists_dim = .FALSE.

  exists_dim = .TRUE.
  rcode = nf90_inq_dimid (ncid, dimname, dimid)
  IF (rcode /= 0) exists_dim = .FALSE.

END FUNCTION exists_dim

LOGICAL FUNCTION exists_var(ncid, varname)
! Function to determine if a variable exists

  USE netcdf

  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: ncid
  CHARACTER(LEN=50), INTENT(IN)                           :: varname
  
! Local
  INTEGER                                                 :: rcode, varid
  CHARACTER(LEN=50)                                       :: section

!!!!!!! Variables
! ncid: netCDF id
! varname: name of variable

  section="'exists_var'"
!  PRINT *,'Section '//TRIM(section)//'... .. .'

  exists_var = .TRUE.
  rcode = nf90_inq_varid (ncid, TRIM(varname), varid)
  IF (rcode /= 0) exists_var = .FALSE.

END FUNCTION exists_var

SUBROUTINE fill_inputs_50char(debg, ncs, Nncs, fvars, dimMin, matou)
! Subroutine to fill a kind-shape of input real fields

  USE netcdf
  USE module_constants
  USE module_gen_tools, ONLY: halfdim

  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, Nncs 
  INTEGER, DIMENSION(2), INTENT(IN)                       :: fvars
  INTEGER, DIMENSION(6), INTENT(IN)                       :: dimMin
  CHARACTER(LEN=50), DIMENSION(dimMin(2), dimMin(3),                                             &
    dimMin(4), dimMin(5), dimMin(6)), INTENT(OUT)         :: matou
  CHARACTER(LEN=500), DIMENSION(Nncs), INTENT(IN)         :: ncs

! Local
  INTEGER                                                 :: i,j,k,l,m,n
  INTEGER                                                 :: rcode, ncid
  CHARACTER(LEN=1)                                        :: char1
  CHARACTER(LEN=50)                                       :: section

!!!!!!!!!! Variables
! Nncs: number of input files
! ncs: netCDF input files
! fvars: file (col1) and position in it (col2) of variable
! dimMinn: dimensions of variable
! matin: matrix with values of variable

!!!!!!!!!! Function
! halfdim: Function to give the half value of a dimension

  section="'fill_inputs_50char'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) THEN
    PRINT *,'Filling matrices of dimension: ',UBOUND(matou)
    PRINT *,"Variable in file: '"//TRIM(ncs(fvars(1)))//"' with id:",fvars(2)
  END IF
  rcode = nf90_open(TRIM(ncs(fvars(1))), 0, ncid)
  call error_nc(section, rcode)

  matou=''
! netCDF specifications have encoded 1 character as a type of variable. Thus, larger variables are 
! constructed with matrices where first dimension of them are length of text variable. In wrfout,
! 'Times' variable is 19 character long in [AAAA]-[MM]-[DD]_[HH]:[MI]:[SS] format

  DO i=1,dimMin(1)
    DO j=1,dimMin(2)
      DO k=1,dimMin(3)
        DO l=1,dimMin(4)
          DO m=1,dimMin(5)
            DO n=1,dimMin(6)
              rcode = nf90_get_var ( ncid, fvars(2), char1, start=(/i, j, k, l, m, n /),         &
                count = (/1, 1, 1, 1, 1, 1/))
              call error_nc(section, rcode)
              matou(j,k,l,m,n)(i:i)=char1
	      IF ((debg >= 150) .AND. (j == halfdim(dimMin(2))) .AND. (k == halfdim(dimMin(3)))  &
	        .AND. (l == halfdim(dimMin(4))) .AND. (m == halfdim(dimMin(5))) .AND.            &
                (n == halfdim(dimMin(6)))) PRINT *,i,": '",char1,"' value: '"//                  &
		TRIM(matou(j,k,l,m,n))//"'"
            END DO
          END DO
        END DO
      END DO
    END DO
  END DO

  rcode = nf90_close(ncid)
  call error_nc(section, rcode)

  IF (debg >= 100) PRINT *,'Memory loaded input variable. Value (dimN/2)=',                     &
    TRIM(matou(halfdim(dimMin(2)), halfdim(dimMin(3)), halfdim(dimMin(4)), halfdim(dimMin(5)),  &
      halfdim(dimMin(6))))

  RETURN
END SUBROUTINE fill_inputs_50char

SUBROUTINE fill_inputs_real(debg, ncs, Nncs, fvars, dimMin, matin)
! Subroutine to fill a kind-shape of input real fields

  USE netcdf
  USE module_constants
  USE module_gen_tools, ONLY: halfdim

  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, Nncs 
  INTEGER, DIMENSION(2), INTENT(IN)                       :: fvars
  INTEGER, DIMENSION(6), INTENT(IN)                       :: dimMin
  REAL, DIMENSION(dimMin(1), dimMin(2), dimMin(3),                                               &
    dimMin(4), dimMin(5), dimMin(6)), INTENT(OUT)         :: matin
  CHARACTER(LEN=500), DIMENSION(Nncs), INTENT(IN)         :: ncs

! Local
  INTEGER                                                 :: rcode, ncid
  CHARACTER(LEN=50)                                       :: section
!  INTEGER                                                 :: halfdim

!!!!!!!!!! Variables
! Nncs: number of input files
! ncs: netCDF input files
! fvars: file (col1) and position in it (col2) of variable
! dimMinn: dimensions of variable
! matin: martix with values of variable

!!!!!!!!!! Function
! halfdim: Function to give the half value of a dimension

  section="'fill_inputs'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) THEN
    PRINT *,'Filling matrices of dimension: ',UBOUND(matin)
    PRINT *,"Variable in file: '"//TRIM(ncs(fvars(1)))//"' with id:",fvars(2)
  END IF
  rcode = nf90_open(TRIM(ncs(fvars(1))), 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
  rcode = nf90_get_var ( ncid, fvars(2), matin )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
  rcode = nf90_close(ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)

  IF (debg >= 100) PRINT *,'Memory loaded input variable. Value (dimN/2)=',                     &
    matin(halfdim(dimMin(1)), halfdim(dimMin(2)), halfdim(dimMin(3)), halfdim(dimMin(4)),       &
      halfdim(dimMin(5)), halfdim(dimMin(6)))
  RETURN
  
END SUBROUTINE fill_inputs_real

SUBROUTINE gattribute_REALvalue(file, debg, attributename, attelement, value)
! Subroutine to obtain a real value from an attribute from a netCDF file

  USE netcdf
  USE module_gen_tools, ONLY: diag_fatal

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: file
  CHARACTER(LEN=50), INTENT(IN)                          :: attributename
  REAL, INTENT(OUT)                                      :: value
  INTEGER, INTENT(IN)                                    :: debg, attelement
!!! Local vars
  INTEGER                                                :: ncid, attid, attlen, atttype
  INTEGER                                                :: rcode
  CHARACTER(LEN=50)                                      :: section, attname
  INTEGER                                                :: ndims, nvars, ngatts, nunlimdimid
  REAL, DIMENSION(:), ALLOCATABLE                        :: attrealvalues
  CHARACTER(LEN=250)                                     :: message

!!!!!!!!!!!!!!! Variables
! file: netCDF file
! attributename: name of attribute
! attelement: element of values to given back
! value: values recoded in attribute (only att. element component)
! 

  section="'attribute_REALvalue'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf90_open(file, 0, ncid)
  rcode = nf90_inquire(ncid, ndims, nvars, ngatts, nunlimdimid)

  IF (debg >= 100) PRINT *,"Reading in fle '"//TRIM(file)//"'"
  
  PRINT *,"Attribute name: '"//TRIM(attributename)//"' "
  rcode = nf90_inquire_attribute(ncid, NF90_GLOBAL, attributename, atttype)
  CALL error_nc(section, rcode)

! If attribute is not real (TYPE =5) stops
!!
  message = 'In '//TRIM(section)//" real attribute (type = 5) '"//TRIM(attributename)//              &
    "' is desired but it is of type: "//CHAR(48+atttype)
  IF (atttype /= 5) CALL diag_fatal(message)

! Looking for attribute length
!!
  rcode = nf90_inquire_attribute(ncid, NF90_GLOBAL, attributename, len=attlen)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)

! Allocating and getting value
!!
  IF (ALLOCATED(attrealvalues)) DEALLOCATE(attrealvalues)
  ALLOCATE (attrealvalues(attlen))
  rcode = nf90_get_att(ncid, NF90_GLOBAL, attributename, attrealvalues)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
  IF (debg >= 75) PRINT *,"attribute: '"//TRIM(attributename)//' values: ',attrealvalues(1:attlen)

  rcode = nf90_close(ncid)
  value=attrealvalues(attelement)
  IF (debg >= 75) PRINT *,'giving back value: ',value

  DEALLOCATE(attrealvalues)
  RETURN
END SUBROUTINE gattribute_REALvalue

SUBROUTINE gattribute_STRINGvalue(file, debg, attributename, attelement, value)
! Subroutine to obtain a string value from an attribute from a netCDF file

  USE netcdf
  USE module_gen_tools, ONLY: diag_fatal

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: file
  CHARACTER(LEN=50), INTENT(IN)                          :: attributename
  CHARACTER(LEN=250), INTENT(OUT)                        :: value
  INTEGER, INTENT(IN)                                    :: debg, attelement
!!! Local vars
  INTEGER                                                :: ncid, attid, attlen, atttype
  INTEGER                                                :: rcode
  CHARACTER(LEN=50)                                      :: section, attname
  INTEGER                                                :: ndims, nvars, ngatts, nunlimdimid
  CHARACTER(LEN=250)                                     :: attstringvalues
  CHARACTER(LEN=250)                                     :: message

!!!!!!!!!!!!!!! Variables
! file: netCDF file
! attributename: name of attribute
! attelement: element of values to given back
! value: values recoded in attribute (only att. element component)
! 

  section="'attribute_STRINGvalue'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf90_open(file, 0, ncid)
  rcode = nf90_inquire(ncid, ndims, nvars, ngatts, nunlimdimid)

  IF (debg >= 100) PRINT *,"  Reading in fle '"//TRIM(file)//"'"
  
  PRINT *,"  Attribute name: '"//TRIM(attributename)//"' "
  rcode = nf90_inquire_attribute(ncid, NF90_GLOBAL, attributename, atttype)
  CALL error_nc(section, rcode)

! If attribute is not string (TYPE =2) stops
!!
  message = '  In '//TRIM(section)//" string attribute (type = 2) '"//TRIM(attributename)//     &
    "' is desired but it is of type: "//CHAR(48+atttype)
  IF (atttype /= 2) CALL diag_fatal(message)

! Looking for attribute length
!!
  rcode = nf90_inquire_attribute(ncid, NF90_GLOBAL, attributename, len=attlen)
  CALL error_nc(section, rcode)

! Allocating and getting value
!!
  rcode = nf90_get_att(ncid, NF90_GLOBAL, attributename, attstringvalues)
  CALL error_nc(section, rcode)
  IF (debg >= 75) PRINT *,"  attribute: '"//TRIM(attributename)//" value: '"//                  &
    TRIM(attstringvalues)//"'"

  rcode = nf90_close(ncid)
  value=attstringvalues
  IF (debg >= 75) PRINT *,"  giving back value: '"//TRIM(value)//"'"

  RETURN
END SUBROUTINE gattribute_STRINGvalue

SUBROUTINE nc_gatts(fileinf, debg) 
! Subroutine to print all global attributes of a netCDF file 

  USE netcdf
  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: fileinf
  INTEGER, INTENT(IN)                                    :: debg
! Local
  INTEGER                                                :: iatt, j
  INTEGER                                                :: rcode, ncid, ndims, nvars, ngatts,  &
    nunlimdimid, atttype, attlen
  CHARACTER(LEN=50)                                      :: section
  CHARACTER(LEN=100)                                     :: attname
  INTEGER, DIMENSION(:), ALLOCATABLE                     :: attintvalues
  INTEGER, PARAMETER                                     :: long = SELECTED_INT_KIND(9)
  INTEGER(long), DIMENSION(:), ALLOCATABLE               :: attlongintvalues
  CHARACTER(LEN=100)                                     :: attcharvalues
  REAL, DIMENSION(:), ALLOCATABLE                        :: attrealvalues
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE            :: attdoublevalues

!!!!!!!!!!!!!!!!! Variables
! fileinf: netCDF file with path
! ngatts: number of global attributes

  section="'nc_atts'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf90_open(fileinf, 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
  rcode = nf90_inquire(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
  IF (debg >= 100) THEN
    PRINT *,'File: '//TRIM(fileinf)
    DO iatt=1, ngatts
      rcode = nf90_inq_attname(ncid, NF90_GLOBAL, iatt, attname)
      rcode = nf90_inquire_attribute(ncid, NF90_GLOBAL, attname, atttype, attlen)
      PRINT *,'     iatt:',iatt,'***'//TRIM(attname)//'*** type:',atttype,' length: ',attlen
      SELECT CASE(atttype)
      CASE(2)
        attcharvalues=''
        rcode = nf90_get_att(ncid, NF90_GLOBAL, attname, attcharvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
        PRINT *,"          * value: '"//TRIM(attcharvalues)//"'"
      CASE(3)
        IF (ALLOCATED(attintvalues)) DEALLOCATE(attintvalues)
        ALLOCATE (attintvalues(attlen))
        rcode = nf90_get_att(ncid, NF90_GLOBAL, TRIM(attname), attintvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
        PRINT *,'          * values:',(attintvalues(j),' ,',j=1,attlen)
        DEALLOCATE(attintvalues)
      CASE(4)
        IF (ALLOCATED(attintvalues)) DEALLOCATE(attintvalues)
        ALLOCATE (attintvalues(attlen))
        rcode = nf90_get_att(ncid, NF90_GLOBAL, TRIM(attname), attintvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
        PRINT *,'          * values:',(attintvalues(j),' ,',j=1,attlen)
        DEALLOCATE(attintvalues)
!      CASE(8)
!        IF (ALLOCATED(attlongintvalues)) DEALLOCATE(attlongintvalues)
!        ALLOCATE (attlongintvalues(attlen))
!        rcode = nf90_get_att(ncid, NF90_GLOBAL, TRIM(attname), attlongintvalues)
!	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
!        PRINT *,'          * values:',(attlongintvalues(j),' ,',j=1,attlen)
!        DEALLOCATE(attlongintvalues)
      CASE(5)
        IF (ALLOCATED(attrealvalues)) DEALLOCATE(attrealvalues)
        ALLOCATE (attrealvalues(attlen))
        rcode = nf90_get_att(ncid, NF90_GLOBAL, TRIM(attname), attrealvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
        PRINT *,'          * values:',(attrealvalues(j),' ,',j=1,attlen)
        DEALLOCATE(attrealvalues)
      CASE(6)
        IF (ALLOCATED(attdoublevalues)) DEALLOCATE(attdoublevalues)
        ALLOCATE (attdoublevalues(attlen))
        rcode = nf90_get_att(ncid, NF90_GLOBAL, TRIM(attname), attdoublevalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
        PRINT *,'          * values:',(attdoublevalues(j),' ,',j=1,attlen)
        DEALLOCATE(attdoublevalues)
      CASE DEFAULT
        PRINT *,'           ----- Not printable data type -----'
      END SELECT
    END DO
  ENDIF
  rcode = nf90_close(ncid)

  RETURN
END SUBROUTINE nc_gatts

SUBROUTINE nc_dimensions(file, dbg, ndims, Xdimname, Ydimname, Zdimname, Tdimname, dimsval,      &
  dimsname, dx, dy, dz, dt) 
! Subroutine to obtain range of dimensions of a netCDF file

  USE netcdf
  USE module_constants
  
  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

  INTEGER                                                :: idim
  CHARACTER(LEN=500), INTENT(IN)                         :: file
  INTEGER, INTENT(IN)                                    :: dbg
  INTEGER, INTENT(IN)                                    :: ndims
  CHARACTER(LEN=50), INTENT(IN)                          :: Xdimname, Ydimname, Zdimname, Tdimname
  INTEGER, DIMENSION(ndims), INTENT(OUT)                 :: dimsval
  CHARACTER(LEN=50), DIMENSION(ndims), INTENT(OUT)       :: dimsname
  INTEGER, INTENT(OUT)                                   :: dx, dy, dz, dt
  INTEGER                                                :: rcode, ncid
  CHARACTER(LEN=50)                                      :: dimname, section

!!!!!!!!!!!!!!!! Variables
! file: netCDF file
! ndims: number of dimensions
! [X/Y/Z/T]dimname: name of [X/Y/Z/T] dimensions
! dimsval: vector with ranges of all dimensions
! dimsname: vector with names of all dimensions
! dx, dy, dz, dt: ranges of X, Y, Z, T dimension 

  section="'nc_dimensions'"
  IF (dbg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  
  rcode = nf90_open(file, 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)

  DO idim=1, ndims

    rcode = nf90_inquire_dimension(ncid, idim, dimsname(idim), dimsval(idim))
    IF (TRIM(dimsname(idim)) == TRIM(Xdimname)) dx=dimsval(idim)
    IF (TRIM(dimsname(idim)) == TRIM(Ydimname)) dy=dimsval(idim)
    IF (TRIM(dimsname(idim)) == TRIM(Zdimname)) dz=dimsval(idim)
    IF (TRIM(dimsname(idim)) == TRIM(Tdimname)) dt=dimsval(idim)
  END DO

  IF (dbg >= 20) THEN
    PRINT *,'Dimensions of file______________'
    DO idim=1,ndims
      PRINT *,dimsname(idim),':',dimsval(idim)
    END DO
    PRINT *,'File dimensions. dimx:',dx,' dimy:',dy,' dimz:',dz,' dimt:',dt
  END IF
  rcode = nf90_close(ncid)
  RETURN

END SUBROUTINE nc_dimensions

INTEGER FUNCTION nc_last_iddim(debg, ncid)
! Function to give higest iddim of a netCDF file

  USE netcdf
  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: debg, ncid

! Local
  INTEGER                                                 :: rcode, idim
  INTEGER                                                 :: ndims, nvars, ngatts, nunlimdimid
  CHARACTER(LEN=50)                                       :: section, dimname
  
  section="'nc_last_iddim'"
  
!!!!!!! Variables
! ncid: netCDF id
  
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf90_inquire(ncid, ndims, nvars, ngatts, nunlimdimid)
  CALL error_nc(section, rcode)
  
  IF (debg >= 150) THEN
    PRINT *,' netCDF file has ',ndims, ' dimensions'
  ENDIF

  DO idim=1,ndims
    rcode = nf90_inquire_dimension (ncid, idim, dimname)
    CALL error_nc(section, rcode)  
    IF (debg >= 150 ) PRINT *,'var #',idim,' name: ',dimname
  END DO  

  nc_last_iddim = ndims

END FUNCTION nc_last_iddim

INTEGER FUNCTION nc_last_idvar(debg, ncid)
! Function to give higest idvar of a netCDF file

  USE netcdf
  
  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: debg, ncid

! Local
  INTEGER                                                 :: rcode, ivar
  INTEGER                                                 :: ndims, nvars, ngatts, nunlimdimid
  CHARACTER(LEN=50)                                       :: section, varname
  
  section="'nc_last_idvar'"
  
!!!!!!! Variables
! ncid: netCDF id
  
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf90_inquire(ncid, ndims, nvars, ngatts, nunlimdimid)
  CALL error_nc(section, rcode)
  
  IF (debg >= 150) THEN
    PRINT *,' netCDF file has ',nvars, ' variables'
  ENDIF

  DO ivar=1,nvars
    rcode = nf90_inquire_variable (ncid, ivar, name=varname)
    CALL error_nc(section, rcode)  
    IF (debg >= 150 ) PRINT *,'var #',ivar,' name: ',varname
  END DO  

  nc_last_idvar = nvars

END FUNCTION nc_last_idvar

SUBROUTINE nc_Ndim(fileinf, debg, ndims, nvars, ngatts, nunlimdimid) 
! Subroutine to obtain number of dimensions of a netCDF file 

  USE netcdf
  
  IMPLICIT NONE
  
!  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: fileinf
  INTEGER, INTENT(IN)                                    :: debg
  INTEGER, INTENT(OUT)                                   :: ndims, nvars, ngatts, nunlimdimid
! Local
  INTEGER                                                :: rcode, ncid
  CHARACTER(LEN=50)                                      :: section

!!!!!!!!!!!!!!!!! Variables
! fileinf: netCDF file with path
! ndims: number of dimensions
! nvars: number of variables
! ngatts: number of global attributes
! nunlimdimid: id dimension with unlimit range

  section="'nc_Ndim'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf90_open(fileinf, 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
  rcode = nf90_inquire(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
  IF (debg >= 20) THEN
    PRINT *,'File: '//TRIM(fileinf)
    PRINT *,' INPUT file has = ',ndims, ' dimensions, '
    PRINT *,'                  ',nvars, ' variables, and '
    PRINT *,'                  ',ngatts,' global attributes '
    PRINT *,'  '
  ENDIF
  rcode = nf90_close(ncid)

  RETURN
END SUBROUTINE nc_Ndim

SUBROUTINE search_dimensions(debg, ncs, Nnc, sdims, fdims)
! Subroutine to search a dimension from a given set of 'Nnc' netcCDF files 
!
! NOTE: Variable values are got from first file where it is found

  USE netcdf
  USE module_constants
  USE module_gen_tools, ONLY: diag_fatal

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, Nnc
  CHARACTER(LEN=500), DIMENSION(Nnc), INTENT(IN)          :: ncs
  CHARACTER(LEN=50), INTENT(IN)                           :: sdims
  INTEGER, DIMENSION(3), INTENT(OUT)                      :: fdims

! Local
  INTEGER                                                 :: ifile, idim
  INTEGER                                                 :: rcode, ncid, iddim
  INTEGER                                                 :: Ndimfdim, ncNdims
  CHARACTER(LEN=50)                                       :: section
  INTEGER, DIMENSION(6)                                   :: ndimsfdim
  INTEGER, DIMENSION(:), ALLOCATABLE                      :: ncdims
  CHARACTER(LEN=250)                                      :: messg

!!!!!!!!!! Variables
! ncs: vector with the names of netCDF files
! Nnc: number of netCDF files
! sdims: name of wanted dimensions
! fdims: matrix of location of desired dimension
!    col1: number of files (according to ncs)    col2: id dim in from file of 'col1'
!    col3: range of dimension
! ncNdims: number of dimensions of nerCDF file
! ncdims: range of dimensions of netCDF file

  section="'search_dimensions'"  
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  IF (debg >= 100) PRINT *,"  Searching dimensions in "//TRIM(section)//"..."
  fdims=0
  files_loop: DO ifile=1, Nnc
    rcode = nf90_open(TRIM(ncs(ifile)), 0, ncid)
    IF (debg >= 20) PRINT *,"  Reading in file: '"//TRIM(ncs(ifile))//"' ..."
    CALL error_nc(section, rcode)
    rcode = nf90_inquire(ncid, ncNdims)
    
    IF (ALLOCATED(ncdims)) DEALLOCATE(ncdims)
    ALLOCATE(ncdims(ncNdims))
    
    DO idim=1,ncNdims
      rcode = nf90_inquire_dimension(ncid, idim, len=ncdims(idim))
      CALL error_nc(section, rcode)
    END DO

! Searching dimension
!!
    rcode = nf90_inq_dimid(ncid, TRIM(sdims), iddim)
    CALL error_nc(section, rcode)
    IF (fdims(1) == 0 ) THEN
      fdims(1)=ifile
      fdims(2)=iddim
      rcode = nf90_inquire_dimension (ncid, iddim, len=fdims(3))

      IF (debg >= 75) PRINT *,"  Dimension: '"//TRIM(sdims)//"' found in '"//TRIM(ncs(ifile))// &
        ' dim id:',fdims(2),' range: ', fdims(3)
    END IF
    
    rcode = nf90_close(ncid)
    DEALLOCATE (ncdims)
  END DO files_loop

  IF (fdims(1)==0) PRINT *,TRIM(errmsg)//" dimension: '"//TRIM(sdims)//"' NOT found!"
  
  messg='  The dimension has not been not found !!'
  IF (fdims(1) == 0) CALL diag_fatal(messg)
  IF (debg >= 100) THEN
    PRINT *,'  Characterstics of found dimension_______________'
    PRINT *,"  dimension: '"//TRIM(sdims)//' file #',fdims(1),' position in file', fdims(2),     &
      ' range:',fdims(3)
  END IF

  RETURN
END SUBROUTINE search_dimensions

SUBROUTINE search_variables(debg, ncs, Nnc, svars, Nsvars, fvars, dimfvars)
! Subroutine to search a list of 'Nsvars' variables from a given set of 'Nnc' netcCDF files 
!
! NOTE: Variable values are got from first file where it is found

  USE netcdf
  USE module_constants
  USE module_gen_tools, ONLY: diag_fatal

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, Nnc, Nsvars
  CHARACTER(LEN=500), DIMENSION(Nnc), INTENT(IN)          :: ncs
  CHARACTER(LEN=250), DIMENSION(Nsvars), INTENT(IN)       :: svars
  INTEGER, DIMENSION(Nsvars, 2), INTENT(OUT)              :: fvars
  INTEGER, DIMENSION(Nsvars, 6), INTENT(OUT)              :: dimfvars

! Local
  INTEGER                                                 :: ifile, ivar, idim
  INTEGER                                                 :: rcode, ncid, idvar
  INTEGER                                                 :: Ndimfvar, ncNdims
  CHARACTER(LEN=50)                                       :: section
  INTEGER, DIMENSION(6)                                   :: ndimsfvar
  INTEGER, DIMENSION(:), ALLOCATABLE                      :: ncdims
  CHARACTER(LEN=250)                                      :: messg

!!!!!!!!!! Variables
! ncs: vector with the names of netCDF files
! Nnc: number of netCDF files
! svars: vector with name of wanted variables
! Nsvars: number of variables to search
! fvars: matrix of location of desired variables (line as variable number according to svars)
!    col1: number of files (according to ncs)    col2: id variable in from file of 'col1'
! dimfvars: shape of fvars
! Ndimfvar: number of dimensions of fvar
! ndimsfvar: id dimension of dimensions of fvar
! ncNdims: number of dimensions of nerCDF file
! ncdims: shape dimensions of netCDF file

  section="'search_variables'"  
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  IF (debg >= 100) THEN
    PRINT *,"  Searching ..."
    DO ivar=1, Nsvars
      PRINT *,ivar,"  '"//TRIM(svars(ivar))//"'"
    END DO
  
  END IF
  fvars=0
  dimfvars=1
  files_loop: DO ifile=1, Nnc
    rcode = nf90_open(TRIM(ncs(ifile)), 0, ncid)
    IF (debg >= 20) PRINT *,"  Reading in file: '"//TRIM(ncs(ifile))//"' ..."
    IF (rcode /= 0) PRINT *, '  '//TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
    rcode = nf90_inquire(ncid, ncNdims)
    
    IF (ALLOCATED(ncdims)) DEALLOCATE(ncdims)
    ALLOCATE(ncdims(ncNdims))
    
    DO idim=1,ncNdims
      rcode = nf90_inquire_dimension(ncid, idim, len=ncdims(idim))
    END DO

! Searching variables characteristics
!!
    DO ivar=1, Nsvars
      rcode = nf90_inq_varid(ncid, TRIM(svars(ivar)), idvar)
      IF (rcode /= 0) PRINT *,'  '//TRIM(errmsg)//" in "//TRIM(section)//" "//nf90_strerror(rcode)
      IF (fvars(ivar,1) == 0 ) THEN
        fvars(ivar,1)=ifile
	fvars(ivar,2)=idvar
	rcode = nf90_inquire_variable (ncid, idvar, ndims=Ndimfvar)
	rcode = nf90_inquire_variable (ncid, idvar, dimids=ndimsfvar)
	dimfvars(ivar,1:Ndimfvar) = ncdims(ndimsfvar(1:Ndimfvar))
	
        IF (debg >= 100) PRINT *,'  Variable # ',ivar,'/',Nsvars,": '"//TRIM(svars(ivar))//     &
	  "' found in '"//TRIM(ncs(ifile))//' var id:',fvars(ivar,2),' of dimensions: ',        &
          dimfvars(ivar,1:Ndimfvar)
      END IF
    END DO
    rcode = nf90_close(ncid)
    DEALLOCATE (ncdims)
  END DO files_loop

  DO ivar=1, Nsvars
    IF (fvars(ivar,1)==0) PRINT *,'  '//TRIM(errmsg)//" variable: '"//TRIM(svars(ivar))//       &
      "' NOT found!"
  END DO
  
  messg='Some variables have not been not found !!'
  IF (.NOT.(ALL(fvars(:,1) /= 0))) CALL diag_fatal(messg)
  IF (debg >= 100) THEN
    PRINT *,'  Characteristics of found fields_______________'
    DO ivar=1, Nsvars
      PRINT *,"  variable: '"//TRIM(svars(ivar))//' file #',fvars(ivar,1),' position in file',   &
        fvars(ivar,2)
      PRINT *,'    dimension of variable: ',(dimfvars(ivar,idim),' ,',idim=1,5), dimfvars(ivar,6)
    END DO
  END IF

  RETURN

END SUBROUTINE search_variables

END MODULE module_nc_tools
