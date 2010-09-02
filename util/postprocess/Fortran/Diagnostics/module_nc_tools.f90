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

  USE module_constants
  USE module_types
  USE module_gen_tools
  USE module_calc_tools

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!  INCLUDE 'include/types.inc'
  
  INTEGER, INTENT(IN)                                     :: debg, Ninc
  CHARACTER(LEN=50), DIMENSION(Ninc), INTENT(IN)          :: incs
  TYPE(dimensiondef), INTENT(INOUT)                       :: dimensioncalc
  
! Local
  INTEGER                                                 :: ival, ivar
  INTEGER                                                 :: rcode
  INTEGER, SAVE                                           :: dimcalcrange
  CHARACTER(LEN=50)                                       :: section
  INTEGER, DIMENSION(3)                                   :: dimfound
  REAL, DIMENSION(:), ALLOCATABLE, TARGET, SAVE           :: dimvalues
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

  END IF
  
  CALL calc_method1D(debg, dimensioncalc%method, dimvardimfound(1,                              &
    dimensioncalc%indimensions(1)), dimensioncalc%NinVarnames, diminvalues,                     &
    dimensioncalc%constant, dimvalues)
  dimensioncalc%values=>dimvalues
  IF (debg >= 150) PRINT *,'  ',dimensioncalc%Nvalues,' values of dimension: ',                 &
    dimensioncalc%values

END SUBROUTINE compute_1Dmethod_values

SUBROUTINE compute_dimensions(debg, ncid, Ndims, dimvec, xcar, ycar, names4basedims, Nfiles,    &
  files, file_gatt)
! Subroutine to compute dimensions of output file

  USE module_constants
  USE module_gen_tools
  USE module_calc_tools
  USE module_types
  
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'
!  INCLUDE 'include/types.inc'

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
  INTEGER                                                 :: jvar, lastdim
  CHARACTER(LEN=50)                                       :: section
  INTEGER, DIMENSION(3)                                   :: dimfound
  INTEGER, DIMENSION(:,:), ALLOCATABLE                    :: dimvarfound, dimvardimfound,       &
    invarvalues
  INTEGER, DIMENSION(Ndims)                               :: dimsid
  CHARACTER(LEN=250)                                      :: coordlong, messg
  REAL, DIMENSION(:), ALLOCATABLE                         :: dimensionvalues
  REAL, DIMENSION(:,:,:,:,:,:,:), ALLOCATABLE             :: valuesMATinA, valuesMATinB,        &
    valuesMATinC,valuesMATinD,valuesMATinE,valuesMATinF
  REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE               :: valuesMATdim, coordinate
  CHARACTER(LEN=50)                                       :: coordname, coordstd, coordunits,   &
    coordscoord
  CHARACTER(LEN=50), DIMENSION(:,:,:,:,:), ALLOCATABLE    :: charcoor
  REAL, DIMENSION(:), ALLOCATABLE                         :: cdimvalues

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
! dimfound: found characteristics of dimension in input files
! dimvarfound: found characteristics of variables related to dimension
! dimvardimfound: dimensions of dimvarfound
! invarvalues: values of variables used to compute dimension values (when they are 1D)
! dimsid: id of dimension as they are defined in 'ncid'. Id of dimensions will preserve order from
!   'dimensions_diagnostics.inf' file
! valuesMATin[A/F]: 6D real matrix with values to compute dimensions
! valuesMATdim: 6D real matrix with 'dimension'-related matrix values
! coordname, coordstd, coordunits, coordscoord, coordaxis, coordpositive: strings for direct
!   definition of coordinates as 2D variables

  section="'compute_dimensions'"
  IF (debg >= 100) PRINT *,'Section '//section//'... .. .'

  jvar=nc_last_idvar(debg, ncid)
  lastdim=nc_last_iddim(debg, ncid)

  com_dimensions: DO idim=1, Ndims
  
    IF (debg >= 150) PRINT *,"  Computing '"//TRIM(dimvec(idim))//"' dimension"
  
    CALL diagnostic_dim_inf(debg, dimvec(idim), nc_last_iddim(debg, ncid), dimensioncompute)

! Checking if dimension can be computed by 1D methods
    IF ( ANY(generic_calcs1D == dimensioncompute%method)) THEN
      CALL compute_1Dmethod_values(debg,   &
      Nfiles, files, dimensioncompute)
      PRINT *,'neng-range: ',dimensioncompute%Nvalues
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
	    dimsid(1)=nc_last_iddim(debg, ncid)
          ELSE
            CALL def_dimension(debg, ncid, dimensionnew)
            rcode = nf_inq_dimid(ncid, TRIM(dimvec(idim)), dimsid(1))
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
	      cdimvalues(i)=i*1.
	    END DO

            CALL fill_dimension_type(debg,'xc', nc_last_iddim(debg, ncid)+1, 'H', 'X','-',      &
	      dimfound(3), 0, (/'-'/), 'direct', 0., (/0/), 0, (/0/), '-',                      &
	      'X-coordinate in Cartesian system', 'm', dimfound(3), cdimvalues, '-', '-', '-',  &
	      dimensionnew)

            IF (debg >= 150) PRINT *,"  values 'xc':", cdimvalues

            CALL def_dimension(debg, ncid, dimensionnew)
	    jvar=jvar+1
	    dimsid(1)=nc_last_iddim(debg, ncid)
          ELSE
            rcode = nf_inq_dimid(ncid, 'xc', dimsid(1))
          END IF

          IF (.NOT.exists_dim(ncid, 'yc'//blanks(1:50-LEN('yc')))) THEN
	    IF (debg >= 100) PRINT *,"  Dimension 'yc' will be created!"
	    CALL search_dimensions(debg, files, Nfiles, names4basedims(2), dimfound)
	    
            IF (ALLOCATED(cdimvalues)) DEALLOCATE(cdimvalues)
   	    ALLOCATE(cdimvalues(dimfound(3)))

            DO i=1,dimfound(3)
	      cdimvalues(i)=i*1.
	    END DO

            CALL fill_dimension_type(debg,'yc', nc_last_iddim(debg, ncid)+1, 'H', 'Y','-',      &
	      dimfound(3), 0, (/'-'/), 'direct', 0., (/0/), 0, (/0/), '-',                      &
	      'Y-coordinate in Cartesian system', 'm', dimfound(3), cdimvalues, '-', '-', '-',  &
	      dimensionnew)

            IF (debg >= 150) PRINT *,"  values 'yc':", cdimvalues

            CALL def_dimension(debg, ncid, dimensionnew)
            dimsid(2)=nc_last_iddim(debg, ncid)
          ELSE
            rcode = nf_inq_dimid(ncid, 'yc', dimsid(2))
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

              PRINT *,'neng-idvar: ',nc_last_idvar(debg, ncid)
              CALL def_nc_var(ncid, nc_last_idvar(debg, ncid)+1, dimensioncompute%name, 5, 2,   &
	        (/dimsid(1), dimsid(2), 1, 1, 1, 1/), "XY ", coordlong, coordstd, coordunits,   &
		"-", coordscoord, debg)

              PRINT *,'neng-idvar: ',nc_last_idvar(debg, ncid)
              rcode = nf_put_vara_real(ncid, nc_last_idvar(debg, ncid), (/1, 1/),               &
	        (/dimvardimfound(1,1), dimvardimfound(1,2)/), valuesMATdim(:,:,1,1,1,1))
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
	    dimsid(2)=nc_last_iddim(debg, ncid)
          ELSE
            CALL def_dimension(debg, ncid, dimensionnew)
            rcode = nf_inq_dimid(ncid, TRIM(dimvec(idim)), dimsid(2))
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
	      cdimvalues(i)=i*1.
	    END DO

            CALL fill_dimension_type(debg,'xc', nc_last_iddim(debg, ncid)+1, 'H', 'X','-',      &
	      dimfound(3), 0, (/'-'/), 'direct', 0., (/0/), 0, (/0/), '-',                      &
	      'X-coordinate in Cartesian system', 'm', dimfound(3), cdimvalues, '-', '-', '-',  &
	      dimensionnew)

            IF (debg >= 150) PRINT *,"  values 'xc':", cdimvalues

            CALL def_dimension(debg, ncid, dimensionnew)
	    jvar=jvar+1
	    dimsid(1)=nc_last_iddim(debg, ncid)
    
          ELSE
            rcode = nf_inq_dimid(ncid, 'xc', dimsid(1))
          END IF

          IF (.NOT.exists_dim(ncid, 'yc'//blanks(1:50-LEN('yc')))) THEN
	  
	    CALL search_dimensions(debg, files, Nfiles, names4basedims(2), dimfound)
	    
            IF (ALLOCATED(cdimvalues)) DEALLOCATE(cdimvalues)
   	    ALLOCATE(cdimvalues(dimfound(3)))

            DO i=1,dimfound(3)
	      cdimvalues(i)=i*1.
	    END DO

            CALL fill_dimension_type(debg,'yc', nc_last_iddim(debg, ncid)+1, 'H', 'Y','-',      &
	      dimfound(3), 0, (/'-'/), 'direct', 0., (/0/), 0, (/0/), '-',                      &
	      'Y-coordinate in Cartesian system', 'm', dimfound(3), cdimvalues, '-', '-', '-',  &
	      dimensionnew)

            IF (debg >= 150) PRINT *,"  values 'yc':", cdimvalues

            CALL def_dimension(debg, ncid, dimensionnew)
	    dimsid(2)=nc_last_iddim(debg, ncid)
          ELSE
            rcode = nf_inq_dimid(ncid, 'yc', dimsid(2))
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
	        (/dimsid(1), dimsid(2), 1, 1, 1, 1/), "XY ", coordlong, coordstd, coordunits,   &
		"-", coordscoord, debg)

              rcode = nf_put_vara_real(ncid, nc_last_idvar(debg, ncid), (/1, 1/),               &
	        (/dimvardimfound(1,1), dimvardimfound(1,2)/), valuesMATdim(:,:,1,1,1,1))
              CALL error_nc(section, rcode)
            END IF notexistnoCARy
          END IF cartesiany

! Z-dimension
!!
      CASE('lev')
        IF (.NOT.exists_dim(ncid,dimensioncompute%name)) THEN
          IF (debg >= 100) PRINT *,'  Creation of z dimension...'
          jvar = jvar + 1

            CALL def_dimension(debg, ncid, dimensioncompute)
	    dimsid(3)=nc_last_iddim(debg, ncid)
        ELSE
          rcode = nf_inq_dimid(ncid, dimensioncompute%name, dimsid(3))
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
!    characrters length as second dimension). Time variable is processed to give date as seconds
!    since a reference date [yearref(in module_constants)]-01-01_00:00:00
!!
              IF (ALLOCATED(dimvarfound)) DEALLOCATE(dimvarfound)
	      ALLOCATE(dimvarfound(Nfiles, dimensioncompute%NinVarnames))
	      
              IF (ALLOCATED(dimvardimfound)) DEALLOCATE(dimvardimfound)
	      ALLOCATE(dimvardimfound(dimensioncompute%NinVarnames,6))
	      
              CALL search_variables(debg, files, Nfiles, dimensioncompute%INvarnames,           &
	        dimensioncompute%NinVarnames, dimvarfound, dimvardimfound)
	    
              IF (ALLOCATED(coordinate)) DEALLOCATE(coordinate)
              ALLOCATE(coordinate(dimvardimfound(1,1),dimvardimfound(1,2),dimvardimfound(1,3),  &
	        dimvardimfound(1,4),dimvardimfound(1,5),dimvardimfound(1,6))) 
              IF (ALLOCATED(charcoor)) DEALLOCATE(charcoor)
              ALLOCATE(charcoor(dimvardimfound(1,2), dimvardimfound(1,3), dimvardimfound(1,4),  &
	        dimvardimfound(1,5), dimvardimfound(1,6)))

              CALL fill_inputs_50char(debg, (/file_gatt/), 1, dimvarfound(1,:),                 &
	        dimvardimfound(1,:), charcoor)
              DO i=1, dimvardimfound(1,dimensioncompute%indimensions(1))
                CALL diff_dates(debg, '1950-01-01_00:00:00', charcoor(i,1,1,1,1), 's',          &
                  coordinate(i,1,1,1,1,1))
              END DO
              dimensioncompute%Nvalues=dimvardimfound(1,dimensioncompute%indimensions(1))

              IF (debg >= 100) PRINT *,"  values 'tc':", coordinate(:,1,1,1,1,1)
	      CALL fill_dimension_type(debg,dimensioncompute%name, nc_last_iddim(debg, ncid)+1, &
	        'T', 'T', '-', dimensioncompute%Nvalues, 0, (/'-'/), '-', 0., (/0/), 0, (/0/),  &
		dimensioncompute%stdname, dimensioncompute%lonname, dimensioncompute%units,     &
		dimensioncompute%Nvalues, coordinate(:,1,1,1,1,1), dimensioncompute%coords,     &
		dimensioncompute%positive, dimensioncompute%form, dimensionnew)

            END SELECT
           
            PRINT *,'neng tc-:',dimensionnew%values
            CALL def_dimension(debg, ncid, dimensionnew)
	    dimsid(4)=nc_last_iddim(debg, ncid)

        ELSE
          rcode = nf_inq_dimid(ncid, dimensioncompute%name, dimsid(4))
        END IF

      CASE DEFAULT
        messg="  Nothing to do with dimension: '"//TRIM(dimvec(idim))//"'"
	CALL diag_fatal(messg)
    
    END SELECT indiv_dim

    IF (debg >= 100) THEN
      PRINT *,'  Output file dimensions id:'
      PRINT *,'  dx:',dimsid(1),' dy:',dimsid(2),' dz:',dimsid(3),' dt:',dimsid(4)
    END IF

    IF (ALLOCATED(coordinate)) DEALLOCATE(coordinate)

  END DO com_dimensions

END SUBROUTINE compute_dimensions

SUBROUTINE copy_nc_gatt(debg, mcid, infile)
! Subroutine to copy all global attributes from a netCDF file to other open one

  USE module_constants
  
  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
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
  
  rcode = nf_open(TRIM(infile), 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

  rcode = nf_redef(mcid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  
  DO iatt=1, ngatts
    rcode = nf_inq_attname(ncid, NF_GLOBAL, iatt, attname)
    rcode = nf_copy_att (ncid, NF_GLOBAL, attname, mcid, NF_GLOBAL)
    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  END DO

  rcode = nf_enddef(mcid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//nf_strerror(rcode)
  
  rcode = nf_close(ncid)

END SUBROUTINE copy_nc_gatt

SUBROUTINE create_output(debg, outfile, dimsinname, Ndimsout, dimsoutname, file_gatt, xcar,     &
  ycar, ifiles, Nifiles)
! create_output: Subroutine to create netCDF output with 4 basic coordinates: lon, lat, lev, time

  USE module_constants
  USE module_calc_tools, ONLY: diff_dates

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

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
    rcode = nf_open(TRIM(outfile), NF_WRITE, oid)
    CALL error_nc(section, rcode)
  ELSE
    PRINT *,"Opening the new file! '"//TRIM(outfile)//"'"  
    rcode = nf_create(TRIM(outfile), NF_CLOBBER, oid)
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
  
  rcode = nf_close(oid)

END SUBROUTINE create_output

SUBROUTINE def_dimension(debg, ncid, dimval)
! Subroutine to define a dimension from a dimension type variable

  USE module_constants
  USE module_types

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'
!  INCLUDE 'include/types.inc'

  INTEGER, INTENT(IN)                                    :: debg, ncid
  TYPE(dimensiondef), INTENT(IN)                         :: dimval
  
! Local
  INTEGER                                                :: rcode, attlen, ivar, idim
  CHARACTER(LEN=50)                                      :: section, atttext

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

  rcode = nf_redef(ncid)
  CALL error_nc(section, rcode)

  IF (TRIM(dimval%name) == 'time') THEN
    IF (debg >= 100) PRINT *,'  dimension range: ',dimval%Nvalues
    rcode = nf_def_dim(ncid, TRIM(dimval%name), NF_UNLIMITED, idim)  
    CALL error_nc(section, rcode)
  ELSE
    IF (debg >= 100) PRINT *,'  dimension range: ',dimval%Nvalues
    rcode = nf_def_dim(ncid, TRIM(dimval%name), dimval%Nvalues, idim)
    CALL error_nc(section, rcode, Ivalue = dimval%Nvalues)
  END IF

  IF (debg >= 100) PRINT *,'  dimension of associated variable (with dimension values): ',      &
    dimval%Nvalues
  rcode = nf_def_var(ncid, TRIM(dimval%name), NF_REAL, 1, (/idim/), ivar)
  CALL error_nc(section, rcode)

  IF (TRIM(dimval%axis) /= '-') THEN
    IF (debg >= 100) PRINT *,"  axis attribute: '"//TRIM(dimval%axis)//"'"
    atttext = TRIM(dimval%axis) 
    attlen = LEN_TRIM(atttext)
    rcode = nf_put_att_text(ncid, ivar, "axis", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  IF (TRIM(dimval%stdname) /= '-') THEN
    IF (debg >= 100) PRINT *,"  std. name attribute: '"//TRIM(dimval%stdname)//"'"
    atttext = TRIM(dimval%stdname)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "standard_name", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  IF (TRIM(dimval%lonname) /= '-') THEN
    IF (debg >= 100) PRINT *,"  long name attribute: '"//TRIM(dimval%lonname)//"'"
    atttext = TRIM(dimval%lonname)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "long_name", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '
  END IF

  IF (TRIM(dimval%units) /= '-') THEN 
    IF (debg >= 100) PRINT *,"  units attribute: '"//TRIM(dimval%units)//"'"
    atttext = TRIM(dimval%units)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "units", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
  END IF

  IF (TRIM(dimval%coords) /= '-') THEN 
    IF (debg >= 100) PRINT *,"  coordinates attribute: '"//TRIM(dimval%coords)//"'"
    atttext = TRIM(dimval%coords)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "coords", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
  END IF

  IF (TRIM(dimval%positive) /= '-') THEN 
    IF (debg >= 100) PRINT *,"  positive attribute: '"//TRIM(dimval%positive)//"'"
    atttext = TRIM(dimval%positive)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "positive", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
  END IF

  IF (TRIM(dimval%form) /= '-') THEN 
    IF (debg >= 100) PRINT *,"  formula attribute: '"//TRIM(dimval%form)//"'"
    atttext = TRIM(dimval%form)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "formula_terms", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
  END IF

  rcode = nf_enddef(ncid)
  CALL error_nc(section, rcode)

  IF (debg >= 100) PRINT *,'  values: ',dimval%values
  rcode = nf_put_vara_real (ncid, ivar, (/1/), (/dimval%Nvalues/), dimval%values)
  CALL error_nc(section, rcode)

  RETURN
END SUBROUTINE def_dimension

SUBROUTINE def_variable (debg, mcid, varwrite )
! Subroutine to define a variable inside a netCDF file

  USE module_constants
  USE module_gen_tools, ONLY: diag_fatal
  USE module_types

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

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

  rcode = nf_redef(mcid)
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
      rcode = nf_def_var(mcid, TRIM(varwrite%name), NF_CHAR, varwrite%rank, varwrite%shape,ivar)
      CALL error_nc(section, rcode)
!      rcode = nf_put_att_int(mcid, ivar, "FieldType", NF_INT, 1, 104)
!      CALL error_nc(section, rcode)

    CASE ( 4 ) 
!     Integer variable
!!
      IF (debg >= 100) PRINT *,"  Defining real variable '"//TRIM(varwrite%name)//"' number:",  &
        ivar
      rcode = nf_def_var(mcid, TRIM(varwrite%name), NF_REAL, varwrite%rank, varwrite%shape, ivar)
      CALL error_nc(section, rcode)
      rcode = nf_put_att_int(mcid, ivar, "FieldType", NF_INT, 1, 106)
      CALL error_nc(section, rcode)
  
    CASE ( 5 ) 
!     Real variable
!!
      IF (debg >= 100) PRINT *,"  Defining real variable '"//TRIM(varwrite%name)//"' number:",  &
        ivar
      rcode = nf_def_var(mcid, TRIM(varwrite%name), NF_REAL, varwrite%rank, varwrite%shape, ivar)
      CALL error_nc(section, rcode)
      rcode = nf_put_att_int(mcid, ivar, "FieldType", NF_INT, 1, 104)
      CALL error_nc(section, rcode)
    CASE DEFAULT
      messg="Nothing to do with variable type '"//CHAR(varwrite%type+48)//"'"
      CALL diag_fatal(messg)
      
  END SELECT

  IF (TRIM(varwrite%stdname) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding standard name attribute"
    att_text = TRIM(varwrite%stdname)
    ilen = len_trim(att_text)
    rcode = nf_put_att_text(mcid, ivar, "standard_name", ilen, att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

  IF (TRIM(varwrite%lonname) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding long name attribute"
    att_text = TRIM(varwrite%lonname)
    ilen = len_trim(att_text)
    rcode = nf_put_att_text(mcid, ivar, "long_name", ilen, att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

  IF (TRIM(varwrite%units) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding units attribute"
    att_text = TRIM(varwrite%units)
    ilen = len_trim(att_text)
    rcode = nf_put_att_text(mcid, ivar, "units", ilen, att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

  IF (TRIM(varwrite%coords) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding coordinates attribute"
    att_text = TRIM(varwrite%coords)
    ilen = len_trim(att_text)
    rcode = nf_put_att_text(mcid, ivar, "coordinates", ilen, att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

  IF (TRIM(varwrite%form) /= '-') THEN
    IF (debg >= 100) PRINT *,"  Adding formula attribute"
    att_text = TRIM(varwrite%form)
    ilen = len_trim(att_text)
    rcode = nf_put_att_text(mcid, ivar, "formula", ilen, att_text(1:ilen) )
    CALL error_nc(section, rcode)
    att_text = ' '
  END IF

!  IF (TRIM(varwrite%stdname) /= '-') THEN
!    IF (debg >= 100) PRINT *,"  Adding standard name attribute"
!    att_text = TRIM(varwrite%stdname)
!    ilen = len_trim(att_text)
!    rcode = nf_put_att_text(mcid, ivar, "standard_name", ilen, att_text(1:ilen) )
!    CALL error_nc(section, rcode)
!    att_text = ' '
!  END IF

  IF (debg >= 100) PRINT *,"  Adding missing attribute"
  att_text = '-99999.' 
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "missing_value", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = '-99999.'
  rcode = nf_put_att_text(mcid, ivar, "_Fillvalue", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  rcode = nf_enddef(mcid)
  CALL error_nc(section, rcode)

END SUBROUTINE def_variable

SUBROUTINE def_dim(debg, ncid, ivar, dimname, dimax, dimlong, dimunit, dimrg, dimid, dimvalues)
! Subroutine to define a 1D dimension

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: debg, ncid, ivar, dimrg
  CHARACTER(LEN=50), INTENT(IN)                          :: dimname, dimax, dimlong, dimunit
  REAL, DIMENSION(dimrg), INTENT(IN)                     :: dimvalues
  INTEGER, INTENT(OUT)                                   :: dimid

! Local
  INTEGER                                                :: rcode, attlen
  CHARACTER(LEN=50)                                      :: section, atttext

!!!!!!!!!!!!! Variables
! ncid: netCDF id
! ivar: number of variable in netCDF
! Text values of dimension (according to CF-1.4 conventions):
!    dimname: dimension name
!    dimax: dimension axis
!    dimlong: dimension long_name
!    dimunits: dimension units
! dimrg: range of dimension
! dimid: id of dimension
! dimvalues: real vector with values of dimension

  section="'def_dim'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_redef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_def_dim(ncid, TRIM(dimname), dimrg, dimid)
  CALL error_nc(section, rcode)

  rcode = nf_def_var(ncid, TRIM(dimname), NF_REAL, 1, dimid, ivar)
  CALL error_nc(section, rcode)

  atttext = TRIM(dimax) 
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "axis", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimlong)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "long_name", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimunit)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "units", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)

  rcode = nf_enddef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_put_vara_real (ncid, ivar, (/1/), (/dimrg/), dimvalues)
  CALL error_nc(section, rcode)

  RETURN
END SUBROUTINE def_dim

SUBROUTINE def_dim_time(debg, ncid, ivar, dimname, dimlong, dimunit, dimrg, dimid, dimvalues)
! Subroutine to define a vertical coordinate

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: debg, ncid, ivar, dimrg
  CHARACTER(LEN=50), INTENT(IN)                          :: dimname, dimlong, dimunit
  REAL, DIMENSION(dimrg), INTENT(IN)                     :: dimvalues
  INTEGER, INTENT(OUT)                                   :: dimid

! Local
  INTEGER                                                :: rcode, attlen, lunits
  CHARACTER(LEN=50)                                      :: section, atttext

!!!!!!!!!!!!! Variables
! ncid: netCDF id
! ivar: number of variable in netCDF
! Text values of dimension (according to CF-1.4 conventions):
!    dimname: dimension name
!    dimlong: dimension long_name
!    dimunits: dimension units
! dimrg: range of dimension
! dimid: id of dimension
! dimvalues: real vector with values of dimension

  section="'def_dim_time'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_redef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_def_dim(ncid, TRIM(dimname), dimrg, dimid)
  CALL error_nc(section, rcode)

  rcode = nf_def_var(ncid, TRIM(dimname), NF_REAL, 1, dimid, ivar)
  CALL error_nc(section, rcode)

  atttext = TRIM(dimlong)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "long_name", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimunit)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "units", attlen, atttext(1:attlen))
  CALL error_nc(section, rcode)
  
  rcode = nf_enddef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_put_vara_real (ncid, ivar, (/1/), (/dimrg/), dimvalues)
  CALL error_nc(section, rcode)

  RETURN
  
END SUBROUTINE def_dim_time

SUBROUTINE def_dim_ver(debg, ncid, ivar, dimname, dimlong, dimunit, dimpos, dimstd, dimform,     &
  dimrg, dimid, dimvalues)
! Subroutine to define a vertical coordinate

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: debg, ncid, ivar, dimrg
  CHARACTER(LEN=50), INTENT(IN)                          :: dimname, dimlong, dimunit, dimpos,  &
    dimstd
  CHARACTER(LEN=100), INTENT(IN)                         :: dimform
  REAL, DIMENSION(dimrg), INTENT(IN)                     :: dimvalues
  INTEGER, INTENT(OUT)                                   :: dimid

! Local
  INTEGER                                                :: rcode, attlen, lunits
  CHARACTER(LEN=50)                                      :: section, atttext

!!!!!!!!!!!!! Variables
! ncid: netCDF id
! ivar: number of variable in netCDF
! Text values of dimension (according to CF-1.4 conventions):
!    dimname: dimension name
!    dimlong: dimension long_name
!    dimunits: dimension units
!    dimpos: dimension sign on level increase
!    dimform: dimension formula (whether is not a udunits variable)
! dimrg: range of dimension
! dimid: id of dimension
! dimvalues: real vector with values of dimension

  section="'def_dim_ver'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_redef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_def_dim(ncid, TRIM(dimname), dimrg, dimid)
  CALL error_nc(section, rcode)

  rcode = nf_def_var(ncid, TRIM(dimname), NF_REAL, 1, dimid, ivar)
  CALL error_nc(section, rcode)

  atttext = TRIM(dimlong)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "long_name", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimpos)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "positive", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimstd)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "standard_name", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  IF (TRIM(dimunit) == "NO") THEN

    atttext = ''
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "units", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '

    atttext = TRIM(dimform)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "formula_terms", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '

  ELSE
    atttext = TRIM(dimunit)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "units", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
  
  END IF
  
  rcode = nf_enddef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_put_vara_real (ncid, ivar, (/1/), (/dimrg/), dimvalues)
  CALL error_nc(section, rcode)

  RETURN
  
END SUBROUTINE def_dim_ver

SUBROUTINE def_nc_gatt_text (debg, mcid, atdesc, atvalue)
! Subroutine to define a global text attribute inside a netCDF file

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
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

  rcode = nf_redef(mcid)
  CALL error_nc(section, rcode)
 
  IF (debg >= 100) PRINT *,"Adding global attribute '"//TRIM(atdesc)//"' in netCDF file"
  att_text = atvalue
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, NF_GLOBAL, TRIM(atdesc), ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)

  att_text = ' '

  rcode = nf_enddef(mcid)
  CALL error_nc(section, rcode)

END SUBROUTINE def_nc_gatt_text

SUBROUTINE def_nc_var (mcid, ivar0, cval, itype, idm, jshape, order, desc, stddesc, units, stag,&
  coord, debg )
! Subroutine to define a variable inside a netCDF file

  USE module_constants

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

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

  rcode = nf_redef(mcid)
  CALL error_nc(section, rcode)
  ivar=nc_last_idvar(debg, mcid)+1

  IF ( itype == 5 ) THEN
    IF (debg >= 100) PRINT *,"  Defining real variable '"//TRIM(cval)//"' number:",ivar
    rcode = nf_def_var(mcid, TRIM(cval), NF_REAL, idm, jshape(1:idm), ivar)
    CALL error_nc(section, rcode)
    rcode = nf_put_att_int(mcid, ivar, "FieldType", NF_INT, 1, 104)
    CALL error_nc(section, rcode)
  ENDIF

  IF (debg >= 100) PRINT *,"  Adding MemoryOrder attribute"
  att_text = order
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "MemoryOrder", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"  Adding long_name attribute"
  att_text = desc
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "long_name", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"  Adding standard_name attribute"
  att_text = stddesc
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "standard_name", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"  Adding units attribute"
  att_text = units
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "units", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"  Adding stagger attribute"
  att_text = stag
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "stagger", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"  Adding coordinates attribute"
  att_text = coord
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "coordinates", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"  Adding missing attribute"
  att_text = '-99999.' 
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "missing_value", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = '-99999.'
  rcode = nf_put_att_text(mcid, ivar, "_Fillvalue", ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)
  att_text = ''

  rcode = nf_enddef(mcid)
  CALL error_nc(section, rcode)

END SUBROUTINE def_nc_var

SUBROUTINE error_nc(sec, rc, Ivalue, Rvalue, Cvalue)
! Subroutine to print error nc messages

  USE module_constants
  
  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: rc
  CHARACTER(LEN=50), INTENT(IN)                           :: sec
  INTEGER, INTENT(IN), OPTIONAL                           :: Ivalue
  REAL, INTENT(IN), OPTIONAL                              :: Rvalue
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL                  :: Cvalue

! Local
  
  IF (rc /= 0) THEN
  
    IF (PRESENT(Ivalue)) THEN
      PRINT *,TRIM(errmsg)//" in "//TRIM(sec)//" "//nf_strerror(rc)//' :',Ivalue
    ELSEIF (PRESENT(Rvalue)) THEN
      PRINT *,TRIM(errmsg)//" in "//TRIM(sec)//" "//nf_strerror(rc)//' :',Rvalue
    ELSEIF (PRESENT(Cvalue)) THEN
      PRINT *,TRIM(errmsg)//" in "//TRIM(sec)//" "//nf_strerror(rc)//" :'"//TRIM(Cvalue)
    ELSE 
      PRINT *,TRIM(errmsg)//" in "//TRIM(sec)//" "//nf_strerror(rc)
    END IF
  END IF
END SUBROUTINE error_nc

LOGICAL FUNCTION exists_dim(ncid, dimname)
! Function to determine if a dimension exists

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
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
  rcode = nf_inq_dimid (ncid, dimname, dimid)
  IF (rcode /= 0) exists_dim = .FALSE.

END FUNCTION exists_dim

LOGICAL FUNCTION exists_var(ncid, varname)
! Function to determine if a variable exists

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
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
  rcode = nf_inq_varid (ncid, TRIM(varname), varid)
  IF (rcode /= 0) exists_var = .FALSE.

END FUNCTION exists_var

SUBROUTINE fill_inputs_50char(debg, ncs, Nncs, fvars, dimMin, matou)
! Subroutine to fill a kind-shape of input real fields

  USE module_constants
  USE module_gen_tools, ONLY: halfdim

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

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
  IF (debg >= 75) THEN
    PRINT *,'Filling matrices of dimension: ',UBOUND(matou)
    PRINT *,"Variable in file: '"//TRIM(ncs(fvars(1)))//"' with id:",fvars(2)
  END IF
  rcode = nf_open(TRIM(ncs(fvars(1))), 0, ncid)
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
              rcode = nf_get_var1_text ( ncid, fvars(2), (/ i, j, k, l, m, n /), char1)
              call error_nc(section, rcode)
              matou(j,k,l,m,n)(i:i)=char1
	      IF ((j == halfdim(dimMin(2))) .AND. (k == halfdim(dimMin(3))) .AND.                &
                (l == halfdim(dimMin(4))) .AND. (m == halfdim(dimMin(5))) .AND.                  &
                (n == halfdim(dimMin(6)))) PRINT *,i,": '",char1,"' value: '"//                  &
		TRIM(matou(j,k,l,m,n))//"'"
            END DO
          END DO
        END DO
      END DO
    END DO
  END DO

  rcode = nf_close(ncid)
  call error_nc(section, rcode)

  IF (debg >= 100) PRINT *,'Memory loaded input variable. Value (dimN/2)=',                     &
    TRIM(matou(halfdim(dimMin(2)), halfdim(dimMin(3)), halfdim(dimMin(4)), halfdim(dimMin(5)),  &
      halfdim(dimMin(6))))

  RETURN
END SUBROUTINE fill_inputs_50char

SUBROUTINE fill_inputs_real(debg, ncs, Nncs, fvars, dimMin, matin)
! Subroutine to fill a kind-shape of input real fields

  USE module_constants
  USE module_gen_tools, ONLY: halfdim

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

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
  IF (debg >= 75) THEN
    PRINT *,'Filling matrices of dimension: ',UBOUND(matin)
    PRINT *,"Variable in file: '"//TRIM(ncs(fvars(1)))//"' with id:",fvars(2)
  END IF
  rcode = nf_open(TRIM(ncs(fvars(1))), 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_get_var_real ( ncid, fvars(2), matin )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_close(ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

  IF (debg >= 100) PRINT *,'Memory loaded input variable. Value (dimN/2)=',                     &
    matin(halfdim(dimMin(1)), halfdim(dimMin(2)), halfdim(dimMin(3)), halfdim(dimMin(4)),       &
      halfdim(dimMin(5)), halfdim(dimMin(6)))
  RETURN
  
END SUBROUTINE fill_inputs_real

SUBROUTINE gattribute_REALvalue(file, debg, attributename, attelement, value)
! Subroutine to obtain a real value from an attribute from a netCDF file

  USE module_gen_tools, ONLY: diag_fatal

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

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

  rcode = nf_open(file, 0, ncid)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)

  IF (debg >= 100) PRINT *,"Reading in fle '"//TRIM(file)//"'"
  
  PRINT *,"Attribute name: '"//TRIM(attributename)//"' "
  rcode = nf_inq_atttype(ncid, NF_GLOBAL, attributename, atttype)
  CALL error_nc(section, rcode)

! If attribute is not real (TYPE =5) stops
!!
  message = 'In '//TRIM(section)//" real attribute (type = 5) '"//TRIM(attributename)//              &
    "' is desired but it is of type: "//CHAR(48+atttype)
  IF (atttype /= 5) CALL diag_fatal(message)

! Looking for attribute length
!!
  rcode = nf_inq_attlen(ncid, NF_GLOBAL, attributename, attlen)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

! Allocating and getting value
!!
  IF (ALLOCATED(attrealvalues)) DEALLOCATE(attrealvalues)
  ALLOCATE (attrealvalues(attlen))
  rcode = nf_get_att_real(ncid, NF_GLOBAL, attributename, attrealvalues)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  IF (debg >= 75) PRINT *,"attribute: '"//TRIM(attributename)//' values: ',attrealvalues(1:attlen)

  rcode = nf_close(ncid)
  value=attrealvalues(attelement)
  IF (debg >= 75) PRINT *,'giving back value: ',value

  DEALLOCATE(attrealvalues)
  RETURN
END SUBROUTINE gattribute_REALvalue

SUBROUTINE gattribute_STRINGvalue(file, debg, attributename, attelement, value)
! Subroutine to obtain a string value from an attribute from a netCDF file

  USE module_gen_tools, ONLY: diag_fatal

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: file
  CHARACTER(LEN=50), INTENT(IN)                          :: attributename
  CHARACTER(LEN=250), INTENT(OUT)                        :: value
  INTEGER, INTENT(IN)                                    :: debg, attelement
!!! Local vars
  INTEGER                                                :: ncid, attid, attlen, atttype
  INTEGER                                                :: rcode
  CHARACTER(LEN=50)                                      :: section, attname
  INTEGER                                                :: ndims, nvars, ngatts, nunlimdimid
  CHARACTER(LEN=250), DIMENSION(:), ALLOCATABLE          :: attstringvalues
  CHARACTER(LEN=250)                                     :: message

!!!!!!!!!!!!!!! Variables
! file: netCDF file
! attributename: name of attribute
! attelement: element of values to given back
! value: values recoded in attribute (only att. element component)
! 

  section="'attribute_STRINGvalue'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_open(file, 0, ncid)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)

  IF (debg >= 100) PRINT *,"  Reading in fle '"//TRIM(file)//"'"
  
  PRINT *,"  Attribute name: '"//TRIM(attributename)//"' "
  rcode = nf_inq_atttype(ncid, NF_GLOBAL, attributename, atttype)
  CALL error_nc(section, rcode)

! If attribute is not string (TYPE =2) stops
!!
  message = '  In '//TRIM(section)//" string attribute (type = 2) '"//TRIM(attributename)//     &
    "' is desired but it is of type: "//CHAR(48+atttype)
  IF (atttype /= 2) CALL diag_fatal(message)

! Looking for attribute length
!!
  rcode = nf_inq_attlen(ncid, NF_GLOBAL, attributename, attlen)
  CALL error_nc(section, rcode)

! Allocating and getting value
!!
  IF (ALLOCATED(attstringvalues)) DEALLOCATE(attstringvalues)
  ALLOCATE (attstringvalues(attlen))
  rcode = nf_get_att_text(ncid, NF_GLOBAL, attributename, attstringvalues)
  CALL error_nc(section, rcode)
  IF (debg >= 75) PRINT *,"  attribute: '"//TRIM(attributename)//' values: ',                   &
    attstringvalues(1:attlen)

  rcode = nf_close(ncid)
  value=attstringvalues(attelement)
  IF (debg >= 75) PRINT *,"  giving back value: '"//TRIM(value)//"'"

  DEALLOCATE(attstringvalues)
  RETURN
END SUBROUTINE gattribute_STRINGvalue

SUBROUTINE nc_gatts(fileinf, debg) 
! Subroutine to print all global attributes of a netCDF file 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

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

  rcode = nf_open(fileinf, 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  IF (debg >= 100) THEN
    PRINT *,'File: '//TRIM(fileinf)
    DO iatt=1, ngatts
      rcode = nf_inq_attname(ncid, NF_GLOBAL, iatt, attname)
      rcode = nf_inq_atttype(ncid, NF_GLOBAL, attname, atttype)
      rcode = nf_inq_attlen(ncid, NF_GLOBAL, attname, attlen)     
      PRINT *,'     iatt:',iatt,'***'//TRIM(attname)//'*** type:',atttype,' length: ',attlen
      SELECT CASE(atttype)
      CASE(2)
        attcharvalues=''
        rcode = nf_get_att_text(ncid, NF_GLOBAL, attname, attcharvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,"          * value: '"//TRIM(attcharvalues)//"'"
      CASE(3)
        IF (ALLOCATED(attintvalues)) DEALLOCATE(attintvalues)
        ALLOCATE (attintvalues(attlen))
        rcode = nf_get_att_int1(ncid, NF_GLOBAL, TRIM(attname), attintvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,'          * values:',(attintvalues(j),' ,',j=1,attlen)
        DEALLOCATE(attintvalues)
      CASE(4)
        IF (ALLOCATED(attintvalues)) DEALLOCATE(attintvalues)
        ALLOCATE (attintvalues(attlen))
        rcode = nf_get_att_int(ncid, NF_GLOBAL, TRIM(attname), attintvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,'          * values:',(attintvalues(j),' ,',j=1,attlen)
        DEALLOCATE(attintvalues)
!      CASE(8)
!        IF (ALLOCATED(attlongintvalues)) DEALLOCATE(attlongintvalues)
!        ALLOCATE (attlongintvalues(attlen))
!        rcode = nf_get_att_int2(ncid, NF_GLOBAL, TRIM(attname), attlongintvalues)
!	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!        PRINT *,'          * values:',(attlongintvalues(j),' ,',j=1,attlen)
!        DEALLOCATE(attlongintvalues)
      CASE(5)
        IF (ALLOCATED(attrealvalues)) DEALLOCATE(attrealvalues)
        ALLOCATE (attrealvalues(attlen))
        rcode = nf_get_att_real(ncid, NF_GLOBAL, TRIM(attname), attrealvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,'          * values:',(attrealvalues(j),' ,',j=1,attlen)
        DEALLOCATE(attrealvalues)
      CASE(6)
        IF (ALLOCATED(attdoublevalues)) DEALLOCATE(attdoublevalues)
        ALLOCATE (attdoublevalues(attlen))
        rcode = nf_get_att_double(ncid, NF_GLOBAL, TRIM(attname), attdoublevalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,'          * values:',(attdoublevalues(j),' ,',j=1,attlen)
        DEALLOCATE(attdoublevalues)
      CASE DEFAULT
        PRINT *,'           ----- Not printable data type -----'
      END SELECT
    END DO
  ENDIF
  rcode = nf_close(ncid)

  RETURN
END SUBROUTINE nc_gatts

SUBROUTINE nc_dimensions(file, dbg, ndims, Xdimname, Ydimname, Zdimname, Tdimname, dimsval,      &
  dimsname, dx, dy, dz, dt) 
! Subroutine to obtain range of dimensions of a netCDF file

  USE module_constants
  
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

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
  
  rcode = nf_open(file, 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

  DO idim=1, ndims

    rcode = nf_inq_dim(ncid, idim, dimsname(idim), dimsval(idim))
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
  rcode = nf_close(ncid)
  RETURN

END SUBROUTINE nc_dimensions

INTEGER FUNCTION nc_last_iddim(debg, ncid)
! Function to give higest iddim of a netCDF file

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: debg, ncid

! Local
  INTEGER                                                 :: rcode, idim
  INTEGER                                                 :: ndims, nvars, ngatts, nunlimdimid
  CHARACTER(LEN=50)                                       :: section, dimname
  
  section="'nc_last_iddim'"
  
!!!!!!! Variables
! ncid: netCDF id
  
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  CALL error_nc(section, rcode)
  
  IF (debg >= 150) THEN
    PRINT *,' netCDF file has ',ndims, ' dimensions'
  ENDIF

  DO idim=1,ndims
    rcode = nf_inq_dimname (ncid, idim, dimname)
    CALL error_nc(section, rcode)  
    IF (debg >= 150 ) PRINT *,'var #',idim,' name: ',dimname
  END DO  

  nc_last_iddim = ndims

END FUNCTION nc_last_iddim

INTEGER FUNCTION nc_last_idvar(debg, ncid)
! Function to give higest idvar of a netCDF file

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: debg, ncid

! Local
  INTEGER                                                 :: rcode, ivar
  INTEGER                                                 :: ndims, nvars, ngatts, nunlimdimid
  CHARACTER(LEN=50)                                       :: section, varname
  
  section="'nc_last_idvar'"
  
!!!!!!! Variables
! ncid: netCDF id
  
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  CALL error_nc(section, rcode)
  
  IF (debg >= 150) THEN
    PRINT *,' netCDF file has ',nvars, ' variables'
  ENDIF

  DO ivar=1,nvars
    rcode = nf_inq_varname (ncid, ivar, varname)
    CALL error_nc(section, rcode)  
    IF (debg >= 150 ) PRINT *,'var #',ivar,' name: ',varname
  END DO  

  nc_last_idvar = nvars

END FUNCTION nc_last_idvar

SUBROUTINE nc_Ndim(fileinf, debg, ndims, nvars, ngatts, nunlimdimid) 
! Subroutine to obtain number of dimensions of a netCDF file 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

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

  rcode = nf_open(fileinf, 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  IF (debg >= 20) THEN
    PRINT *,'File: '//TRIM(fileinf)
    PRINT *,' INPUT file has = ',ndims, ' dimensions, '
    PRINT *,'                  ',nvars, ' variables, and '
    PRINT *,'                  ',ngatts,' global attributes '
    PRINT *,'  '
  ENDIF
  rcode = nf_close(ncid)

  RETURN
END SUBROUTINE nc_Ndim

SUBROUTINE search_dimensions(debg, ncs, Nnc, sdims, fdims)
! Subroutine to search a dimension from a given set of 'Nnc' netcCDF files 
!
! NOTE: Variable values are got from first file where it is found

  USE module_constants
  USE module_gen_tools, ONLY: diag_fatal

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

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

  IF (debg >= 75) PRINT *,"  Searching dimensions in "//TRIM(section)//"..."
  fdims=0
  files_loop: DO ifile=1, Nnc
    rcode = nf_open(TRIM(ncs(ifile)), 0, ncid)
    IF (debg >= 20) PRINT *,"  Reading in file: '"//TRIM(ncs(ifile))//"' ..."
    CALL error_nc(section, rcode)
    rcode = nf_inq_ndims(ncid, ncNdims)
    
    IF (ALLOCATED(ncdims)) DEALLOCATE(ncdims)
    ALLOCATE(ncdims(ncNdims))
    
    DO idim=1,ncNdims
      rcode = nf_inq_dimlen(ncid, idim, ncdims(idim))
    END DO

! Searching dimension
!!
    rcode = nf_inq_dimid(ncid, TRIM(sdims), iddim)
    CALL error_nc(section, rcode)
    IF (fdims(1) == 0 ) THEN
      fdims(1)=ifile
      fdims(2)=iddim
      rcode = nf_inq_dimlen (ncid, iddim, fdims(3))

      IF (debg >= 75) PRINT *,"  Dimension: '"//TRIM(sdims)//"' found in '"//TRIM(ncs(ifile))// &
        ' dim id:',fdims(2),' range: ', fdims(3)
    END IF
    
    rcode = nf_close(ncid)
    DEALLOCATE (ncdims)
  END DO files_loop

  IF (fdims(1)==0) PRINT *,TRIM(errmsg)//" dimension: '"//TRIM(sdims)//"' NOT found!"
  
  messg='  The dimension has not been not found !!'
  IF (fdims(1) == 0) CALL diag_fatal(messg)
  IF (debg >= 75) THEN
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

  USE module_constants
  USE module_gen_tools, ONLY: diag_fatal

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, Nnc, Nsvars
  CHARACTER(LEN=500), DIMENSION(Nnc), INTENT(IN)          :: ncs
  CHARACTER(LEN=50), DIMENSION(Nsvars), INTENT(IN)        :: svars
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

  IF (debg >= 75) THEN
    PRINT *,"  Searching ..."
    DO ivar=1, Nsvars
      PRINT *,ivar,"  '"//TRIM(svars(ivar))//"'"
    END DO
  
  END IF
  fvars=0
  dimfvars=1
  files_loop: DO ifile=1, Nnc
    rcode = nf_open(TRIM(ncs(ifile)), 0, ncid)
    IF (debg >= 20) PRINT *,"  Reading in file: '"//TRIM(ncs(ifile))//"' ..."
    IF (rcode /= 0) PRINT *, '  '//TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
    rcode = nf_inq_ndims(ncid, ncNdims)
    
    IF (ALLOCATED(ncdims)) DEALLOCATE(ncdims)
    ALLOCATE(ncdims(ncNdims))
    
    DO idim=1,ncNdims
      rcode = nf_inq_dimlen(ncid, idim, ncdims(idim))
    END DO

! Searching variables characteristics
!!
    DO ivar=1, Nsvars
      rcode = nf_inq_varid(ncid, TRIM(svars(ivar)), idvar)
      IF (rcode /= 0) PRINT *,'  '//TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
      IF (fvars(ivar,1) == 0 ) THEN
        fvars(ivar,1)=ifile
	fvars(ivar,2)=idvar
	rcode = nf_inq_varndims (ncid, idvar, Ndimfvar)
	rcode = nf_inq_vardimid (ncid, idvar, ndimsfvar)
	dimfvars(ivar,1:Ndimfvar) = ncdims(ndimsfvar(1:Ndimfvar))
	
        IF (debg >= 75) PRINT *,'  Variable # ',ivar,'/',Nsvars,": '"//TRIM(svars(ivar))//      &
	  "' found in '"//TRIM(ncs(ifile))//' var id:',fvars(ivar,2),' of dimensions: ',        &
          dimfvars(ivar,1:Ndimfvar)
      END IF
    END DO
    rcode = nf_close(ncid)
    DEALLOCATE (ncdims)
  END DO files_loop

  DO ivar=1, Nsvars
    IF (fvars(ivar,1)==0) PRINT *,'  '//TRIM(errmsg)//" variable: '"//TRIM(svars(ivar))//       &
      "' NOT found!"
  END DO
  
  messg='Some variables have not been not found !!'
  IF (.NOT.(ALL(fvars(:,1) /= 0))) CALL diag_fatal(messg)
  IF (debg >= 75) THEN
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
