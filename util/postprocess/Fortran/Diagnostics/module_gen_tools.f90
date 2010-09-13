MODULE module_gen_tools
  USE module_constants
  
  CONTAINS
! Subroutines/functions to make some general processes 
! GMS. UC: January 2010. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!! Subroutines/Functions
! all_diagnostic: Subroutine to obtain all diagnostic names from 'variables_diagnostics.inf'
! all_Ndiagnostic: Function to obtain how many diagnostic names are in 'variables_diagnostics.inf'
! diag_fatal: Subroutine to give fatal error with a message
! diagnostic_inf: Subroutine to read diagnostic variable information from 
!     'variables_diagnostics.inf' external ASCII file
! diagnostic_var_inf: Subroutine to read diagnostic variable information from 
!     'variables_diagnostics.inf' external ASCII file
! diagnostic_dim_inf: Subroutine to read diagnostic variable information from 
!     'dimensions_diagnostics.inf' external ASCII file
! diagnostic_inf_Ninvar: Subroutine to give number of input variables to compute varDIAG diagnostic
! fill_dimension_type: Function to fill a dimension type
! fill_variable_type: Function to fill a variable type
! give_Lstring: Function that gives a string of length 'Lstring' filling blanks after 'string' (up
!     to 250) 
! give1D_from6D: Subroutine to give a 1D vector from any dimension of a 6D matrix
! halfdim: Function to give the half value of a dimension
! Int_String: Function to convert an integer to a String
! only_number: Function to give a real number from any string with contiguous numbers (point 
!     included) 
! print_6Dhalfdim: Subroutine to print central value of a real 6D matrix
! search_variables: Subroutine to search variables from a given netcCDF file
! string_int: Function to transform a string to an integer value
! string_real: Subroutine to transform a string to a real value
! string_Intvalues: Subroutine to obtain integer values from a 'namelist' string separated by comas
! string_Realvalues: Subroutine to obtain real values from a 'namelist' string separated by comas
! string_values: Subroutine to obtain values from a 'namelist' string separated by comas
! number_values: Subroutine to obtain number of variables from a 'namelist' variable with coma    
!    separation

SUBROUTINE all_diagnostic(debg, Ndiag, vecDIAG)
! Subroutine to obtain all diagnostic names from 'variables_diagnostics.inf'

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                    :: debg, Ndiag
  CHARACTER(LEN=250), DIMENSION(Ndiag), INTENT(OUT)      :: vecDIAG
  
! Local variables
  INTEGER                                                :: iunit, ios, idiag, Llabel
  CHARACTER(LEN=50)                                      :: label, section
  LOGICAL                                                :: is_used

  section="'all_diagnostic'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

!  Read parameters from diagnostic variables information file 'variables_diagnostics.inf' 
  DO iunit=10,100
    INQUIRE(unit=iunit, opened=is_used)
    IF (.not. is_used) EXIT
  END DO
  OPEN(iunit, file='variables_diagnostics.inf', status='old', form='formatted', iostat=ios)
  IF ( ios /= 0 ) STOP "ERROR opening 'variables_diagnostics.inf'"

  vecDIAG=''
  idiag=0
  DO 
    READ(iunit,*,END=100)label
    Llabel=LEN_TRIM(label)
    IF (label(1:1)//label(Llabel:Llabel)=='**') THEN
      idiag=idiag+1
      vecDIAG(idiag)=label(2:Llabel-1)
    END IF
  END DO

 100 CONTINUE
  
  CLOSE(iunit)
  IF (debg >= 150) THEN
    PRINT *,'  Number of diagnostics: ',idiag
    DO idiag=1, Ndiag
      PRINT *,"  '"//TRIM(vecDIAG(idiag))//"'"
    END DO
  END IF 

END SUBROUTINE all_diagnostic

INTEGER FUNCTION all_Ndiagnostic(debg)
! Function to obtain how many diagnostic names are in 'variables_diagnostics.inf'

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                    :: debg
  
! Local variables
  INTEGER                                                :: iunit, ios, Llabel
  CHARACTER(LEN=50)                                      :: label, section
  LOGICAL                                                :: is_used

  section="'all_Ndiagnostic'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

!  Read parameters from diagnostic variables information file 'variables_diagnostics.inf' 
  DO iunit=10,100
    INQUIRE(unit=iunit, opened=is_used)
    IF (.not. is_used) EXIT
  END DO
  OPEN(iunit, file='variables_diagnostics.inf', status='old', form='formatted', iostat=ios)
  IF ( ios /= 0 ) STOP "ERROR opening 'variables_diagnostics.inf'"

  all_Ndiagnostic=0
  DO 
    READ(iunit,*,END=100)label
    Llabel=LEN_TRIM(label)
    IF (label(1:1)//label(Llabel:Llabel)=='**') THEN
      all_Ndiagnostic=all_Ndiagnostic+1
    END IF
  END DO

 100 CONTINUE
  CLOSE(iunit)  

  IF (debg >= 150) PRINT *,'  Number of diagnostics: ',all_Ndiagnostic

END FUNCTION all_Ndiagnostic

SUBROUTINE diag_fatal(msg)
! Subroutine to give fatal error with a message

  IMPLICIT NONE
  
  CHARACTER(LEN=250), INTENT(IN)                          :: msg
  
  PRINT *,msg
  STOP

END SUBROUTINE diag_fatal

SUBROUTINE diagnostic_inf(debg, varDIAG, Ninvar, varinnames, NdimDIAG, shapeDIAG, longdescDIAG, &
  stddescDIAG, unitsDIAG)
! Subroutine to read diagnostic variable information from 'variables_diagnostics.inf' external 
! ASCII file. File format:
!*varDIAG*
! Num_input_variables:  Ninvar 
! Input_variables:      varinnames (coma separated) 
! diagnostic_dim:       NdimDIAG 
! diagnostic_dim_shape: shapeDIAG (coma separated) 
! diagnostic_longdesc:  longdescDIAG 
! diagnostic_std_name:  stddescDIAG
! diagnostic_units:     unitsDIAG 
!    (blank line)
!*varDIAG*
! (...)

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)                          :: varDIAG
  INTEGER, INTENT(IN)                                    :: Ninvar
  INTEGER, INTENT(OUT)                                   :: NdimDIAG
  INTEGER, DIMENSION(6), INTENT(OUT)                     :: shapeDIAG
  CHARACTER(LEN=50), DIMENSION(Ninvar), INTENT(OUT)      :: varinnames
  CHARACTER(LEN=250), INTENT(OUT)                        :: longdescDIAG
  CHARACTER(LEN=50), INTENT(OUT)                         :: unitsDIAG, stddescDIAG
  INTEGER, INTENT(IN)                                    :: debg

! Local variables
  INTEGER                                                :: i, ilin
  INTEGER                                                :: iunit, ios
  INTEGER                                                :: Llabel, posvarDIAG
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label, section
  LOGICAL                                                :: is_used

!!!!!!!!!!!!!! Variables
!  varDIAG: variable diagnostic name
!  Ninvar: number of input variables necessary to compute diagnostic variable
!  varinnames: varable input names variables
!  NdimDIAG: number of dimension of diagnostic variable
!  shapeDIAG: order of id dimensions of diagnostic varible
!  longdescDIAG: long description of diagnostic variable
!  stddescDIAG: standard CF convection name
!  unitsDIAG: units of diagnostic variable
!  posvarDIAG: position of diagnostic variable inside information file

  section="'diagnostic_inf'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

!  Read parameters from diagnostic variables information file 'variables_diagnostics.inf' 
   DO iunit=10,100
     INQUIRE(unit=iunit, opened=is_used)
     IF (.not. is_used) EXIT
   END DO
   OPEN(iunit, file='variables_diagnostics.inf', status='old', form='formatted', iostat=ios)
   IF ( ios /= 0 ) STOP "ERROR opening 'variables_diagnostics.inf'"
   ilin=1
   posvarDIAG=0
   DO 
     READ(iunit,*,END=100)label
     Llabel=LEN_TRIM(label)
     IF (label(2:Llabel-1)==TRIM(varDIAG)) THEN
       posvarDIAG=ilin
       EXIT
     END IF
     ilin=ilin+1
   END DO

 100 CONTINUE

   IF (posvarDIAG == 0) THEN
     PRINT *,"Diagnostic variable '"//TRIM(varDIAG)//"' not found in 'variables_diagnostics.inf'"
     STOP
   END IF

   REWIND (iunit)

   DO ilin=1,posvarDIAG
     READ(iunit,*)car
   END DO
   shapeDIAG=1

   READ(iunit,*)car, car
   READ(iunit,*)car, (varinnames(i),i=1,Ninvar)
   READ(iunit,*)car, NdimDIAG
   READ(iunit,*)car, (shapeDIAG(i), i=1,NdimDIAG)
   READ(iunit,*)car, longdescDIAG
   READ(iunit,*)car, stddescDIAG   
   READ(iunit,*)car, unitsDIAG 

   CLOSE(iunit)

   IF (debg >= 75) THEN
     PRINT *,"Read information for '"//TRIM(varDIAG)//"' variable________"
     PRINT *,'  Number of necessary input variables:', Ninvar
     PRINT *,'  Name of input variables:', ('  '//TRIM(varinnames(i)//'  '),i=1,Ninvar)
     PRINT *,'  Number of dimensions:', NdimDIAG
     PRINT *,'  Shape of dimensions:', (shapeDIAG(i), i=1,NdimDIAG)
     PRINT *,'  Long description:', TRIM(longdescDIAG)
     PRINT *,'  CF-standard name:', TRIM(stddescDIAG)
     PRINT *,'  Units:', TRIM(unitsDIAG)
   END IF
  
END SUBROUTINE diagnostic_inf

SUBROUTINE diagnostic_var_inf(debg, varDIAG, varread)
! Subroutine to read diagnostic variable information from 'variables_diagnostics.inf' external 
! ASCII file. File format:
!*varDIAG*
! diagnostic_type:      (type of diagnostic)
! Num_input_variables:  (number of input variables) 
! Input_variables:      (names of input variable, coma separated) 
! diagnostic_rank:      (rank of diagnostic) 
! diagnostic_shape:     (shape of diagnostic according to 'namelist.dimension_outnames', coma
!    separated)  
! diagnostic_std_name:  (standard name)
! diagnostic_longdesc:  (long name)
! diagnostic_units:     (units)
! diagnostic_method:    (method of computation of diagnostic, change varDIAG if one desires each
!    method)
! diagnostic_constant:  (constant to use to compute diagnostic according to 'method')
! diagnostic_Noptions:  (number of options for diagnostic)
! diagnostic_options:   (values of options)
! diagnostic_coords:    (base coords of diagnostic, optional)
! diagnostic_formula:   (formula to be given as attribute of diagnostic, optional)
!    (blank line)
!*varDIAG*
! (...)

  USE module_types

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)                          :: varDIAG
  INTEGER, INTENT(IN)                                    :: debg
  TYPE(variabledef), INTENT(OUT)                         :: varread
  
! Local variables
  INTEGER                                                :: i, ilin
  INTEGER                                                :: iunit, ios
  INTEGER                                                :: Llabel, posvarDIAG
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label, section
  LOGICAL                                                :: is_used
  INTEGER                                                :: Ninvar
  INTEGER                                                :: NdimDIAG
  INTEGER, DIMENSION(:), ALLOCATABLE, TARGET, SAVE       :: shapeDIAG_targ, optionsDIAG_targ
  CHARACTER(LEN=250), DIMENSION(:), ALLOCATABLE, TARGET,                                        &
    SAVE                                                 :: invarnames_targ
  CHARACTER(LEN=250)                                     :: longdescDIAG
  CHARACTER(LEN=50)                                      :: unitsDIAG, stddescDIAG
  CHARACTER(LEN=3000)                                    :: READinvarnames

!!!!!!!!!!!!!! Variables
!  varDIAG: variable diagnostic name
!  varread: output variable

  section="'diagnostic_var_inf'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

!  Read parameters from diagnostic variables information file 'variables_diagnostics.inf' 
  DO iunit=10,100
    INQUIRE(unit=iunit, opened=is_used)
    IF (.not. is_used) EXIT
  END DO
  OPEN(iunit, file='variables_diagnostics.inf', status='old', form='formatted', iostat=ios)
  IF ( ios /= 0 ) STOP "ERROR opening 'variables_diagnostics.inf'"
  ilin=1
  posvarDIAG=0
  DO 
    READ(iunit,*,END=100)label
    Llabel=LEN_TRIM(label)
    IF (label(2:Llabel-1)==TRIM(varDIAG)) THEN
      posvarDIAG=ilin
      EXIT
    END IF
    ilin=ilin+1
  END DO

 100 CONTINUE

  IF (posvarDIAG == 0) THEN
    PRINT *,"Diagnostic variable '"//TRIM(varDIAG)//"' not found in 'variables_diagnostics.inf'"
    STOP
  END IF

  REWIND (iunit)

  DO ilin=1,posvarDIAG
    READ(iunit,*)car
  END DO
  varread%name=varDIAG
  READ(iunit,*)car, varread%type
  READ(iunit,*)car, varread%Ninvarnames
  READ(iunit,*)car, READinvarnames
  IF (ALLOCATED(invarnames_targ)) DEALLOCATE(invarnames_targ)
  ALLOCATE(invarnames_targ(varread%Ninvarnames))
!  READ(iunit,*)car, invarnames_targ
  CALL string_values(READinvarnames, debg, varread%Ninvarnames, invarnames_targ)
  IF (ASSOCIATED(varread%INvarnames)) NULLIFY(varread%INvarnames)
  ALLOCATE(varread%INvarnames(varread%Ninvarnames))
  varread%INvarnames=>invarnames_targ
  READ(iunit,*)car, varread%rank
  IF (ALLOCATED(shapeDIAG_targ)) DEALLOCATE(shapeDIAG_targ)
  ALLOCATE(shapeDIAG_targ(varread%rank))
  READ(iunit,*)car, shapeDIAG_targ
  IF (ASSOCIATED(varread%shape)) NULLIFY(varread%shape)
  ALLOCATE(varread%shape(varread%rank))
  varread%shape=>shapeDIAG_targ
  READ(iunit,*)car, varread%stdname
  READ(iunit,*)car, varread%lonname
  READ(iunit,*)car, varread%units
  READ(iunit,*)car, varread%method
  READ(iunit,*)car, varread%constant
  READ(iunit,*)car, varread%Noptions
  IF (ALLOCATED(optionsDIAG_targ)) DEALLOCATE(optionsDIAG_targ)
  ALLOCATE(optionsDIAG_targ(varread%Noptions))
  READ(iunit,*)car, optionsDIAG_targ
  IF (ASSOCIATED(varread%options)) NULLIFY(varread%options)
  ALLOCATE(varread%options(varread%Noptions))
  varread%options=>optionsDIAG_targ
  READ(iunit,*)car, varread%coords
  READ(iunit,*)car, varread%form

  CLOSE(iunit)

  IF (debg >= 75) THEN
    PRINT *,"  Read information for '"//TRIM(varDIAG)//"' variable________"
    IF (varread%type == 2) PRINT *,'  string variable'
    IF (varread%type == 4) PRINT *,'  integer variable'
    IF (varread%type == 5) PRINT *,'  real variable'
    PRINT *,'  Number of necessary input variables:', varread%NinVarnames
    PRINT *,'  Name of input variables:', ("'"//TRIM(varread%INvarnames(i))//"'", char(44),   &
      i=1, varread%NinVarnames)
    PRINT *,'  rank:', varread%rank
    PRINT *,'  Shape of dimensions:', (varread%shape(i), char(44), i=1,varread%rank)
    PRINT *,'  CF-standard name:', TRIM(varread%stdname)
    PRINT *,'  Long description:', TRIM(varread%lonname)
    PRINT *,'  Units:', TRIM(varread%units)
    PRINT *,'  Method:', TRIM(varread%method)
    PRINT *,'  constant:', varread%constant
    PRINT *,'  number of options for diagnostic: ',varread%Noptions
    PRINT *,'    values of options: ',varread%options
    PRINT *,'  coordinates:', TRIM(varread%coords)
    PRINT *,'  formula:', TRIM(varread%form)
  END IF
  
END SUBROUTINE diagnostic_var_inf

SUBROUTINE diagnostic_dim_inf(debg, dimDIAG0, dimid, diminf)
! Subroutine to read dimension information from 'dimensions_diagnostics.inf' external 
! ASCII file. '-' values mean NOVALUE. File format:
!*dimDIAG*
! dimtype               H/V/T (horizontal/vertical/time)
! dimaxis               (space axis to which it makes reference)
! dim_in_name           (dimension name in in put file; '-' no value)
! num_dimInVarnames     (number of input variables to compute dimension; '0' no variables)
! dim_in_varnames       (variables names, coma separated, to compute dimension) 
! method                (method to compute dimension)
!    direct: values are the same from dim_in_varnames [for 1D computations with dimension ids of
!      inputs from 'indimensions']
!    sumct: values are the same from dim_in_varnames plus a constant [same in 'direct']
!    prodct: values are the same from dim_in_varnames times a constant [same in 'direct']
!    sumall: values are the result of the sum of all [same in 'direct']
!    xxxxxx: specific for this dimension (xxxxx must have some sense in 'calc_method1D' (in
!       'module_gen_tools') or in 'compute_dimensions' for 'name' (in 'module_nc_tools') 
! constant              (constant value for method='constant')
! indimensions          (rank of input variables to 1D compute dimension values)
! dim_Noptions:         (number of options for method of dimension)
! dim_options:          (values of options)
! standard_name         (CF-1.4 standard name of dimension)
! long_name             (long name of dimension)
! units                 (units of dimension)
! Ndim_values           (number of fixed dimension values; '0' no values)
! dim_values            (fixed dimension values, coma separated; '-' no values)
!!!! Specific for dimtype=H
! dim_coords            (dimensions base of the dimension)
!!!! Specific for dimtype=V
! dim_positive          (sign of increase of vertical dimension)
! dim_formula           (text to appear as reference of formula of vertical dimension)
!!!! Specific for dimtype=T
! NOTE: Keep in mind to adjust "yearref" in 'module_constants.f90' as might appears in 'units'
!    (blank line)
!*dimDIAG*
! (...)

  USE module_constants
  USE module_types

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: debg, dimid
  TYPE(dimensiondef)                                     :: diminf
  CHARACTER(LEN=*), INTENT(IN)                           :: dimDIAG0

! Local variables
  INTEGER                                                :: i, ilin, idim
  INTEGER                                                :: iunit, ios
  INTEGER                                                :: Llabel, posDIM
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label, section, dimDIAG
  LOGICAL                                                :: is_used
  CHARACTER(LEN=250), DIMENSION(:), ALLOCATABLE, TARGET,                                       &
    SAVE                                                 :: dimInvarnames
  INTEGER, DIMENSION(:), ALLOCATABLE, TARGET, SAVE       :: dimindimensions, optionsDIM_targ
  REAL(KIND=Rhigh), DIMENSION(:), ALLOCATABLE, TARGET,                                         &
    SAVE                                                 :: dimfixvalues
  CHARACTER(LEN=3000)                                    :: readINvarnames, readINdimensions,  &
    readFIXvalues
  
!!!!!!!!!!!!!! Variables
! dimDIAG: dimension name to be search
! dimid: id of dimension to be read
! diminf: information of dimension (see definition in 'module_constants.f90')
! dimInvarnames: vector with variable names of input files to compute dimension
! dimfixvalues: vector with fixed values of dimension
! posDIM: position of beginning of decription of dimension in file
! readINvarnames: all read (as character string) input variables' name from file
! readINdimensions: all read (as character string) which dimension is wanted for 1D computations
! readFIXvalues: all read (as character string) input fixed values from file

  section="'diagnostic_dim_inf'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  IF (ASSOCIATED(diminf%INvarnames)) NULLIFY(diminf%INvarnames)
  IF (ASSOCIATED(diminf%indimensions)) NULLIFY(diminf%indimensions)
  IF (ASSOCIATED(diminf%values)) NULLIFY(diminf%values)

  dimDIAG=dimDIAG0

!  Read parameters from diagnostic dimensions information file 'dimensions_diagnostics.inf' 
!!
   DO iunit=10,100
     INQUIRE(unit=iunit, opened=is_used)
     IF (.not. is_used) EXIT
   END DO
   OPEN(iunit, file='dimensions_diagnostics.inf', status='old', form='formatted', iostat=ios)
   IF ( ios /= 0 ) STOP "ERROR opening 'dimensions_diagnostics.inf'"

! Looking for beginning of dimension description in file
!!
   IF (debg >= 150) PRINT *,"  searching dimension '"//TRIM(dimDIAG)//"'"
   ilin=1
   posDIM=0
   DO 
     READ(iunit,*,END=100)label
     Llabel=LEN_TRIM(label)
     IF (label(2:Llabel-1)==TRIM(dimDIAG)) THEN
       posDIM=ilin
       EXIT
     END IF
     ilin=ilin+1
   END DO

 100 CONTINUE

   IF (posDIM == 0) THEN
     PRINT *,"Diagnostic dimension '"//TRIM(dimDIAG)//"' not found in 'dimensions_diagnostics.inf'"
     STOP
   END IF

   REWIND (iunit)

   DO ilin=1,posDIM
     READ(iunit,*)car
   END DO

   diminf%name=dimDIAG
   diminf%id=dimid

   READ(iunit,*)car, diminf%type
   READ(iunit,*)car, diminf%axis
   READ(iunit,*)car, diminf%INname
   READ(iunit,*)car, diminf%NinVarnames
   IF (ALLOCATED(dimInvarnames)) DEALLOCATE(dimInvarnames)
   ALLOCATE(dimInvarnames(diminf%NinVarnames))
   IF (diminf%NinVarnames > 0) THEN
     READ(iunit,*)car, readINvarnames
     CALL string_values(readINvarnames, debg, diminf%NinVarnames, dimInvarnames)
   ELSE
     READ(iunit,*)car, car
     dimInvarnames='-'
   END IF
   IF (.NOT.(ASSOCIATED(diminf%INvarnames))) ALLOCATE(diminf%INvarnames(diminf%NinVarnames))
   diminf%INvarnames=>dimInvarnames
   READ(iunit,*)car, diminf%method
   READ(iunit,*)car, diminf%constant
   IF (ALLOCATED(dimindimensions)) DEALLOCATE(dimindimensions)
   ALLOCATE(dimindimensions(diminf%NinVarnames))
   READ(iunit,*)car, dimindimensions
   IF (.NOT.(ASSOCIATED(diminf%indimensions))) ALLOCATE(diminf%indimensions(diminf%NinVarnames))
   diminf%indimensions=>dimindimensions
   READ(iunit,*)car, diminf%Noptions
   IF (ALLOCATED(optionsDIM_targ)) DEALLOCATE(optionsDIM_targ)
   ALLOCATE(optionsDIM_targ(diminf%Noptions))
   READ(iunit,*)car, optionsDIM_targ
   IF (.NOT.(ASSOCIATED(diminf%options))) ALLOCATE(diminf%options(diminf%Noptions))
   diminf%options=>optionsDIM_targ
   READ(iunit,*)car, diminf%stdname
   READ(iunit,*)car, diminf%lonname
   READ(iunit,*)car, diminf%units
   READ(iunit,*)car, diminf%Nvalues
   IF (diminf%Nvalues > 0) THEN
     IF (ALLOCATED(dimfixvalues)) DEALLOCATE(dimfixvalues)
     ALLOCATE(dimfixvalues(diminf%Nvalues))
     READ(iunit,*)car, dimfixvalues
   ELSE
     IF (ALLOCATED(dimfixvalues)) DEALLOCATE(dimfixvalues)
     ALLOCATE(dimfixvalues(0))
     dimfixvalues=0
     READ(iunit,*)car, car
   END IF
   diminf%values=>dimfixvalues
   
! Specific horizontal dim, however is read for all dimensions. If it has a value it will be written
! as a dimension attribute (it might be helpful)
   READ(iunit,*)car, diminf%coords
   
! Specific vertical dim, however is read for all dimensions. If it has a value it will be written
! as a dimension attribute (it might be helpful)
   READ(iunit,*)car, diminf%positive
   READ(iunit,*)car, diminf%form
   
   CLOSE(iunit)

   IF (debg >= 75) THEN
     PRINT *,"Read information for '"//TRIM(diminf%name)//"' dimension________"
     PRINT *,'Dimension id: ',diminf%id
     PRINT *,'  Dimension type:', TRIM(diminf%type)
     PRINT *,'  Dimension axis:', TRIM(diminf%axis)
     IF (TRIM(diminf%INname) /= '-') THEN
       PRINT *,'  Dimension name in input files: ',TRIM(diminf%INname)
     ELSE
       PRINT *,'  New dimension. It does not exists in input files!'
     END IF
     IF (diminf%NinVarnames > 0) THEN
       PRINT *,'  Number of input variables to compute dimension: ',diminf%NinVarnames
       PRINT *,'  Name of input variables:', ('  '//TRIM(diminf%INvarnames(idim)//'  '),        &
         idim=1,diminf%NinVarnames)
     ELSE
       PRINT *,'  No input variables are needed to compute dimension!'
     END IF
     PRINT *,'  Dimension computation method: ',TRIM(diminf%method)
     PRINT *,'  Constant: ',diminf%constant
     PRINT *,'  Number of options of variable: ',diminf%Noptions,' values: ',diminf%options
     PRINT *,'  Dimensions of input variables: ',diminf%indimensions
     PRINT *,'  Dimension standard name: ',TRIM(diminf%stdname)
     PRINT *,'  Dimension long name: ',TRIM(diminf%lonname)
     PRINT *,'  Dimension units: ',TRIM(diminf%units)
     IF (diminf%Nvalues > 0) THEN
       PRINT *,'  Number of fixed values of dimension: ',diminf%Nvalues
       PRINT *,'  Fixed values of dimension: ',(diminf%values(idim), idim=1, diminf%Nvalues)
     END IF
! Specific horizontal dim
     IF (TRIM(diminf%coords) /= '-') PRINT *,'  Base coordinates of dimension: ',               &
       TRIM(diminf%coords)
   
! Specific vertical dim
     IF (TRIM(diminf%positive) /= '-') PRINT *,'  Sign of increase of dimension: ',             &
       TRIM(diminf%positive)
     IF (TRIM(diminf%form) /= '-') PRINT *,'  Formula of dimension: ',TRIM(diminf%form)

   END IF

   RETURN
END SUBROUTINE diagnostic_dim_inf

SUBROUTINE diagnostic_inf_Ninvar(varDIAG, debg, Ninvar)
! Subroutine to give number of input variables to copute varDIAG diagnostic from
!  'variables_diagnostics.inf' external ASCII file. File format:
!*varDIAG*
! Num_input_variables:  Ninvar
! Input_variables:      varinnames (coma separated)
! diagnostic_dim:       NdimDIAG
! diagnostic_dim_shape: shapeDIAG (coma separated)
! diagnostic_longdesc:  longdescDIAG
! diagnostic_units:     unitsDIAG
!    (blank line)
!*varDIAG*
! (...)

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)                          :: varDIAG
  INTEGER, INTENT(IN)                                    :: debg
  INTEGER, INTENT(OUT)                                   :: Ninvar
! Local variables
  INTEGER                                                :: iunit, ios, ilin
  INTEGER                                                :: posvarDIAG, Llabel
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label, section
  LOGICAL                                                :: is_used

  section="'diagnostic_inf_Ninvar'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

!  Read parameters from diagnostic variables information file 'variables_diagnostics.inf'
   DO iunit=10,100
     INQUIRE(unit=iunit, opened=is_used)
     IF (.not. is_used) EXIT
   END DO
   OPEN(iunit, file='variables_diagnostics.inf', status='old', form='formatted', iostat=ios)
   IF ( ios /= 0 ) STOP "ERROR opening 'variables_diagnostics.inf'"
   ilin=1
   posvarDIAG=0
   DO
     READ(iunit,*,END=100)label
     Llabel=LEN_TRIM(label)
     IF (label(2:Llabel-1)==TRIM(varDIAG)) THEN
       posvarDIAG=ilin
       EXIT
     END IF
     ilin=ilin+1
   END DO

 100 CONTINUE

   IF (posvarDIAG == 0) THEN
     PRINT *,"Diagnostic variable '"//TRIM(varDIAG)//"' not found in 'variables_diagnostics.inf'"
     STOP
   END IF

  REWIND (iunit)

   DO ilin=1,posvarDIAG
     READ(iunit,*)car
   END DO

   READ(iunit,*)car,Ninvar

   IF (debg >= 75) THEN
     PRINT *,"Read information for '"//TRIM(varDIAG)//"' variable________"
     PRINT *,' Number of necessary input variables:', Ninvar
   END IF
   close(iunit)

END SUBROUTINE diagnostic_inf_Ninvar

SUBROUTINE fill_dimension_type(debg, dimname0, dimid, dimtype, dimaxs, diminname0, dimrange,   &
  dimNinvars, diminvars0, dimmethod0, dimct, indim, Nopts, opts, dimstd0, dimlong0, dimu0,     &
  dimNval, dimval, dimcoord0, dimpos0, dimform0, dimfilled) 
! Function to fill a 'dimensiondef' type

!  USE module_constans, ONLY: dimension_type
  USE module_types

  IMPLICIT NONE

!  INCLUDE 'include/types.inc'

  INTEGER, INTENT(IN)                                     :: debg
  CHARACTER(LEN=*), INTENT(IN)                            :: dimname0
  INTEGER, INTENT(IN)                                     :: dimid
  CHARACTER(LEN=1), INTENT(IN)                            :: dimtype, dimaxs
  CHARACTER(LEN=*), INTENT(IN)                            :: diminname0
  INTEGER, INTENT(IN)                                     :: dimrange
  INTEGER, INTENT(IN)                                     :: dimNinvars
  CHARACTER(LEN=*), DIMENSION(dimNinvars), INTENT(IN)     :: diminvars0
  CHARACTER(LEN=*), INTENT(IN)                            :: dimmethod0
  REAL, INTENT(IN)                                        :: dimct
  INTEGER, DIMENSION(dimNinvars), INTENT(IN)              :: indim
  INTEGER, INTENT(IN)                                     :: Nopts
  INTEGER, DIMENSION(Nopts), INTENT(IN)                   :: opts
  CHARACTER(LEN=*), INTENT(IN)                            :: dimstd0
  CHARACTER(LEN=*), INTENT(IN)                            :: dimlong0
  CHARACTER(LEN=*), INTENT(IN)                            :: dimu0
  INTEGER, INTENT(IN)                                     :: dimNval
  REAL(KIND=Rhigh), DIMENSION(dimNval), INTENT(IN)        :: dimval
  CHARACTER(LEN=*), INTENT(IN)                            :: dimcoord0
  CHARACTER(LEN=*), INTENT(IN)                            :: dimpos0
  CHARACTER(LEN=*), INTENT(IN)                            :: dimform0
  TYPE(dimensiondef), INTENT(OUT)                         :: dimfilled  

! Local
  INTEGER                                                 :: idim
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250),DIMENSION(:),ALLOCATABLE,TARGET,SAVE :: diminvars_targ
  INTEGER, DIMENSION(:), ALLOCATABLE, TARGET, SAVE        :: indimvars_targ, opts_targ
  REAL(KIND=Rhigh), DIMENSION(:), ALLOCATABLE, TARGET,                                          &
    SAVE                                                  :: dimval_targ

  CHARACTER(LEN=50)                                       :: dimname
  CHARACTER(LEN=50)                                       :: diminname
  CHARACTER(LEN=250), DIMENSION(dimNinvars)               :: diminvars
  CHARACTER(LEN=50)                                       :: dimmethod
  CHARACTER(LEN=250)                                      :: dimstd
  CHARACTER(LEN=250)                                      :: dimlong
  CHARACTER(LEN=50)                                       :: dimu
  CHARACTER(LEN=250)                                      :: dimcoord
  CHARACTER(LEN=50)                                       :: dimpos
  CHARACTER(LEN=250)                                      :: dimform

!!!!!!! Variables
! dimfilled: dimension variable type filled with given values

  section="'fill_dimension_type'"
  IF (debg >= 100) PRINT *,'Section '//section//'... .. .'

  IF (ASSOCIATED(dimfilled%INvarnames)) NULLIFY(dimfilled%INvarnames)
  IF (ASSOCIATED(dimfilled%indimensions)) NULLIFY(dimfilled%indimensions)
  IF (ASSOCIATED(dimfilled%options)) NULLIFY(dimfilled%options)
  IF (ASSOCIATED(dimfilled%values)) NULLIFY(dimfilled%values)

  dimname=dimname0
  diminname=diminname0
  diminvars=diminvars0
  dimmethod=dimmethod0
  dimstd=dimstd0
  dimlong=dimlong0
  dimu=dimu0
  dimcoord=dimcoord0
  dimpos=dimpos0
  dimform=dimform0

! Dimensions of target vectors for pointers of 'dimensiondef' type
!!
  IF (ALLOCATED(diminvars_targ)) DEALLOCATE(diminvars_targ)
  ALLOCATE(diminvars_targ(dimNinvars))

  IF (ALLOCATED(indimvars_targ)) DEALLOCATE(indimvars_targ)
  ALLOCATE(indimvars_targ(dimNinvars))

  IF (ALLOCATED(opts_targ)) DEALLOCATE(opts_targ)
  ALLOCATE(opts_targ(Nopts))

  IF (ALLOCATED(dimval_targ)) DEALLOCATE(dimval_targ)
  ALLOCATE(dimval_targ(dimNval))

  diminvars_targ=diminvars
  indimvars_targ=indim
  opts_targ=opts
  dimval_targ=dimval

  dimfilled%name=dimname
  dimfilled%id=dimid
  dimfilled%type=dimtype
  dimfilled%axis=dimaxs  
  dimfilled%INname=diminname
  dimfilled%range=dimrange
  dimfilled%NinVarnames=dimNinvars
  IF (.NOT.(ASSOCIATED(dimfilled%INvarnames))) ALLOCATE(dimfilled%INvarnames(dimNinvars))
  dimfilled%INvarnames=>diminvars_targ
  dimfilled%method=dimmethod
  dimfilled%constant=dimct  
  IF (.NOT.(ASSOCIATED(dimfilled%indimensions))) ALLOCATE(dimfilled%indimensions(dimNinvars))
  dimfilled%indimensions=>indimvars_targ
  IF (.NOT.(ASSOCIATED(dimfilled%options))) ALLOCATE(dimfilled%options(Nopts))
  dimfilled%options=>opts_targ
  dimfilled%stdname=dimstd
  dimfilled%lonname=dimlong
  dimfilled%units=dimu
  dimfilled%Nvalues=dimNval
  IF (.NOT.(ASSOCIATED(dimfilled%values))) ALLOCATE(dimfilled%values(dimNval))
  dimfilled%values=>dimval_targ
  dimfilled%coords=dimcoord
  dimfilled%positive=dimpos
  dimfilled%form=dimform
   
   IF (debg >= 150) THEN
     PRINT *,"Filled '"//TRIM(dimfilled%name)//"' dimension________"
     PRINT *,'Dimension id: ',dimfilled%id
     PRINT *,'  Dimension type:', TRIM(dimfilled%type)
     PRINT *,'  Dimension axis:', TRIM(dimfilled%axis)
     IF (TRIM(dimfilled%INname) /= '-') THEN
       PRINT *,'  Dimension name in input files: ',TRIM(dimfilled%INname)
     ELSE
       PRINT *,'  New dimension. It does not exists in input files!'
     END IF
     IF (dimfilled%NinVarnames > 0) THEN
       PRINT *,'  Number of input variables to compute dimension: ',dimfilled%NinVarnames
       PRINT *,'  Name of input variables:', ('  '//TRIM(dimfilled%INvarnames(idim)//'  '),     &
         idim=1,dimfilled%NinVarnames)
     ELSE
       PRINT *,'  No input variables are needed to compute dimension!'
     END IF
     PRINT *,'  Dimension computation method: ',TRIM(dimfilled%method)
     PRINT *,'  Constant for the method: ',dimfilled%constant
     PRINT *,'  Number of options: ',dimfilled%Noptions,' values:', dimfilled%options
     PRINT *,'  Dimensions to use of input variables: ',dimfilled%indimensions
     PRINT *,'  Dimension standard name: ',TRIM(dimfilled%stdname)
     PRINT *,'  Dimension long name: ',TRIM(dimfilled%lonname)
     PRINT *,'  Dimension units: ',TRIM(dimfilled%units)
     PRINT *,'  Number of values of dimension: ',dimfilled%Nvalues
     PRINT *,'  Values of dimension: ',(dimfilled%values(idim), idim=1, dimfilled%Nvalues)
! Specific horizontal dim
     IF (TRIM(dimfilled%coords) /= '-') PRINT *,'  Base coordinates of dimension: ',            &
       TRIM(dimfilled%coords)
   
! Specific vertical dim
     IF (TRIM(dimfilled%positive) /= '-') PRINT *,'  Sign of increase of dimension: ',          &
       TRIM(dimfilled%positive)
     IF (TRIM(dimfilled%form) /= '-') PRINT *,'  Formula of dimension: ',TRIM(dimfilled%form)

   END IF
  RETURN
END SUBROUTINE fill_dimension_type

SUBROUTINE fill_variable_type(debg, varname0, varid, vartype, varNinnames, varINnames0,       &
  varrank, varshape, varstd0, varlon0, varu0, varmethod0, varct, varNops, varops, varcoords0,   &
  varform0, varfill)
  
! Subroutine to fill up a 'variabledef' type

  USE module_types

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN)                           :: varname0, varstd0, varlon0, varu0,  &
    varmethod0, varcoords0, varform0
  INTEGER, INTENT(IN)                                    :: debg, varid, vartype, varNinnames,  &
    varrank, varNops
  CHARACTER(LEN=*), DIMENSION(varNinnames), INTENT(IN)   :: varINnames0
  INTEGER, DIMENSION(varrank), INTENT(IN)                :: varshape
  INTEGER, DIMENSION(varNops), INTENT(IN)                :: varops
  REAL, INTENT(IN)                                       :: varct
  TYPE(variabledef), INTENT(OUT)                         :: varfill
  
! Local variables
  INTEGER                                                :: i
  CHARACTER(LEN=50)                                      :: section
  INTEGER                                                :: Ninvar
  INTEGER                                                :: NdimDIAG
  INTEGER, DIMENSION(:), ALLOCATABLE, TARGET, SAVE       :: shapeVAR_targ, optionsVAR_targ
  CHARACTER(LEN=250), DIMENSION(:), ALLOCATABLE, TARGET,                                        &
    SAVE                                                 :: innames_targ
  CHARACTER(LEN=250)                                     :: longDIAG, stdDIAG, crdDIAG, formDIAG
  CHARACTER(LEN=50)                                      :: nameDIAG, unitsDIAG, methDIAG

!!!!!!!!!!!!!! Variables
!  varDIAG: variable diagnostic name
!  varfill: output variable

  section="'fill_diagnostic_type'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  nameDIAG=varname0
  stdDIAG=varstd0
  longDIAG=varlon0
  unitsDIAG=varu0
  methDIAG=varmethod0
  crdDIAG=varcoords0
  formDIAG=varform0

  IF (ALLOCATED(innames_targ)) DEALLOCATE(innames_targ)
  ALLOCATE(innames_targ(varNinnames))

  IF (ALLOCATED(shapeVAR_targ)) DEALLOCATE(shapeVAR_targ)
  ALLOCATE(shapeVAR_targ(varrank))

  IF (ALLOCATED(optionsVAR_targ)) DEALLOCATE(optionsVAR_targ)
  ALLOCATE(optionsVAR_targ(varNops))

  varfill%name=nameDIAG
  varfill%id=varid
  varfill%type=vartype
  varfill%Ninvarnames=varNinnames
  IF (ASSOCIATED(varfill%INvarnames)) NULLIFY(varfill%INvarnames)
  ALLOCATE(varfill%INvarnames(varfill%Ninvarnames))
  varfill%INvarnames=>innames_targ
  varfill%rank=varrank
  IF (ASSOCIATED(varfill%shape)) NULLIFY(varfill%shape)
  ALLOCATE(varfill%shape(varfill%rank))
  varfill%shape=>shapeVAR_targ
  varfill%stdname=stdDIAG
  varfill%lonname=longDIAG
  varfill%units=unitsDIAG
  varfill%method=methDIAG
  varfill%constant=varct
  varfill%Noptions=varNops
  IF (ASSOCIATED(varfill%options)) NULLIFY(varfill%options)
  ALLOCATE(varfill%options(varfill%Noptions))
  varfill%options=>optionsVAR_targ
  varfill%coords=crdDIAG
  varfill%form=formDIAG

  IF (debg >= 75) THEN
    PRINT *,"  Read information for '"//TRIM(nameDIAG)//"' variable________"
    IF (varfill%type == 2) PRINT *,'  string variable'
    IF (varfill%type == 4) PRINT *,'  integer variable'
    IF (varfill%type == 5) PRINT *,'  real variable'
    PRINT *,'  Number of necessary input variables:', varfill%NinVarnames
    PRINT *,'  Name of input variables:', ("'"//TRIM(varfill%INvarnames(i))//"'", char(44),   &
      i=1, varfill%NinVarnames)
    PRINT *,'  rank:', varfill%rank
    PRINT *,'  Shape of dimensions:', (varfill%shape(i), char(44), i=1,varfill%rank)
    PRINT *,'  CF-standard name:', TRIM(varfill%stdname)
    PRINT *,'  Long description:', TRIM(varfill%lonname)
    PRINT *,'  Units:', TRIM(varfill%units)
    PRINT *,'  Method:', TRIM(varfill%method)
    PRINT *,'  constant:', varfill%constant
    PRINT *,'  number of options for diagnostic: ',varfill%Noptions, ' values: ',varfill%options
    PRINT *,'  coordinates:', TRIM(varfill%coords)
    PRINT *,'  formula:', TRIM(varfill%form)
  END IF

END SUBROUTINE fill_variable_type

CHARACTER(LEN=250) FUNCTION give_Lstring(debg, string)
! Function that gives a string of length filling blanks after 'string' (up to 250)

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg
  CHARACTER(LEN=*), INTENT(IN)                            :: string

! Local
  INTEGER                                                 :: Lstring, icar
  CHARACTER(LEN=50)                                       :: section
  
  section="'give_Lstring'"
  give_Lstring=' '

  IF (debg >= 150 ) PRINT *,'Section '//section//'... .. .'

  Lstring=LEN_TRIM(string)
  give_Lstring=TRIM(string)

  IF (debg >= 150 ) PRINT *,"**** '"//give_Lstring//"' ****"

END FUNCTION give_Lstring

SUBROUTINE give1D_from6D(debg, matrix, matrg, wtddim, othrval, vec)
! Subroutine to give a 1D vector from any dimension of a 6D matrix

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg, wtddim
  INTEGER, DIMENSION(6), INTENT(IN)                       :: matrg
  INTEGER, DIMENSION(5), INTENT(IN)                       :: othrval
  REAL, DIMENSION(matrg(1), matrg(2), matrg(3), matrg(4),                                       &
    matrg(5), matrg(6) )                                  :: matrix
  REAL, DIMENSION(matrg(wtddim)), INTENT(OUT)             :: vec
  
! Local
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: mssg
  
!!!!!!! Variables
! matrix: 6D matrix
! matrg: ranges of 6D 
! wtddim: wanted dimension
! othrval: values of other dimensions
! vec: vector with all the values of 'matrix' taking all range of dimension 'wtddim' and all the
!    rest at the first position
  
  section="'give1D_from6D'"
  IF (debg >= 150) PRINT *,'Section '//section//'... .. .'
  
  IF (wtddim > 6) THEN
    mssg = 'Desired dimension '//CHAR(48+wtddim)//' is larger than 6D !!'
    CALL diag_fatal(mssg)
  END IF
  
  SELECT CASE (wtddim)
    CASE (1)
      vec=matrix(:,othrval(1),othrval(2),othrval(3),othrval(4),othrval(5))
    CASE (2)
      vec=matrix(othrval(1),:,othrval(2),othrval(3),othrval(4),othrval(5))
    CASE (3)
      vec=matrix(othrval(1),othrval(2),:,othrval(3),othrval(4),othrval(5))
    CASE (4)
      vec=matrix(othrval(1),othrval(2),othrval(3),:,othrval(4),othrval(5))
    CASE (5)
      vec=matrix(othrval(1),othrval(2),othrval(3),othrval(4),:,othrval(5))
    CASE (6)
      vec=matrix(othrval(1),othrval(2),othrval(3),othrval(4),othrval(5),:)
  END SELECT
  
  IF (debg >= 150) PRINT *,'  Given values of dim ',wtddim,' : ',vec
  
END SUBROUTINE give1D_from6D

INTEGER FUNCTION halfdim(dim)
! Function to give the half value of a dimension

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: dim
  
  halfdim = dim/2
  IF (dim < 2) halfdim = 1     ! Assuming non-zero dimension range
  
END FUNCTION halfdim

CHARACTER(LEN=8) FUNCTION Int_String(debg, integer)
! Function to convert an integer to a String

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg, integer
  
! Local
  INTEGER                                                 :: ipot
  REAL                                                    :: pot, int_portion
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=32)                                       :: string
  
  section="'Int_String'"
  IF (debg >= 150) PRINT *,'Section '//section//'... .. .'

  Int_String=''  
  string=''
  int_portion=integer*1.
  DO ipot=1, 8
    pot=10.**(8.-ipot*1.)
    IF (pot <= integer) THEN
      string(ipot:ipot)=CHAR(INT(int_portion/pot)+48)
      int_portion=int_portion-INT(int_portion/pot)*1.*pot
    END IF
  END DO
  Int_String=ADJUSTL(string)
  
  IF (debg >= 150) PRINT *,'From integer: ',integer," string '"//TRIM(Int_String)//"' "

END FUNCTION Int_String

REAL FUNCTION only_number(debg, string0in)
! Function to give a real number from any string with contiguous numbers (point included)

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg
  CHARACTER(LEN=*), INTENT(IN)                            :: string0in

! Local
  INTEGER                                                 :: icar, Lstring, Lnewstring
  CHARACTER(LEN=50)                                       :: stringin, section, newstring
  LOGICAL                                                 :: point
  INTEGER                                                 :: firstnumber, lastnumber

!!!!!!! Variables
! string: string to obtain a number
! Lstring: length of 'string'
! [first/last]number: position of the first/last number in 'string'
! newstring: string obtained from initial string with desired chain of numbers (maybe with point)
! point: indicates if '.' has been found, so real number
! Lnewsting: length of 'newstring'

  section="'only_number'"
    
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
  point=.FALSE.
  stringin=string0in
  newstring=''

  Lstring=LEN_TRIM(stringin)
  IF (debg >= 150) PRINT *,'  Length of string: ',Lstring
  
! Looking for number of points
!!
  firstnumber=0
  DO icar=1, Lstring
    IF ((ICHAR(stringin(icar:icar)) >= 48 ) .AND. (ICHAR(stringin(icar:icar)) <= 57 )) THEN
      IF (firstnumber == 0) firstnumber=icar
      lastnumber=icar
    END IF
  END DO
  IF (debg >= 150) PRINT *,'First number: ',firstnumber,' last number: ',lastnumber
  
  Lnewstring=0
  DO icar=1, Lstring
    PRINT *,stringin(icar:icar)//': ',ICHAR(stringin(icar:icar))
    IF ((ICHAR(stringin(icar:icar)) >= 48 ) .AND. (ICHAR(stringin(icar:icar)) <= 57 )) THEN
      Lnewstring=Lnewstring+1
      newstring(Lnewstring:Lnewstring)=stringin(icar:icar)
    END IF
! Filling with point. If 'point' has not been previously found and it is at firstnumber +/- 1 
!   and/or lastnumber +/- 1
    IF ((ICHAR(stringin(icar:icar)) == 46) .AND. (.NOT.point) .AND. ((ABS(firstnumber - icar) ==&
      1) .OR. (ABS(lastnumber - icar) == 1))) THEN
      Lnewstring=Lnewstring+1
      newstring(Lnewstring:Lnewstring)=stringin(icar:icar)
      point=.TRUE.
    END IF
  END DO

  IF (point) THEN
    CALL string_real(debg, newstring, only_number)
  ELSE
    only_number=string_int(debg, newstring)*1.
  END IF

  IF (debg >= 150) PRINT *,"  From string '"//TRIM(stringin)//"' real number:", only_number

END FUNCTION only_number


REAL FUNCTION print_6Dhalfdim(mat, rgs)
! Subroutine to print central value of a real 6D matrix

  IMPLICIT NONE
  
  INTEGER, DIMENSION(6), INTENT(IN)                       :: rgs
  REAL, DIMENSION(rgs(1), rgs(2), rgs(3), rgs(4), rgs(5),                                       &
    rgs(6)), INTENT(IN)                                   :: mat
    
! Local
!  INTEGER                                                 :: halfdim

  print_6Dhalfdim=mat(halfdim(rgs(1)), halfdim(rgs(2)), halfdim(rgs(3)), halfdim(rgs(4)),       &
      halfdim(rgs(5)), halfdim(rgs(6)))

END FUNCTION print_6Dhalfdim

INTEGER FUNCTION string_int(debg, string)
! Function to transform a string to an integer value

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: debg
  CHARACTER(LEN=20), INTENT(IN)                          :: string

! Local
  INTEGER                                                :: i,j
  INTEGER                                                :: Lstring, point, Lpre, Laft
  CHARACTER(LEN=50)                                      :: section

!!!!!!!!!!!!!!!! Variables
! string: string with integer number
! Lstring: length of string

  section="'string_int'"  
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 150) PRINT *,"  String: '"//TRIM(string)//"'"

  Lstring=LEN_TRIM(string)

  string_int=0
  DO i=1,Lstring
    string_int=string_int+(IACHAR(string(i:i))-48)*10.**(Lstring-i)
  END DO 


  IF (debg >= 150) PRINT *,'  Integer:',string_int
  
  RETURN
END FUNCTION string_int

SUBROUTINE string_real(debg, string, value)
! Subroutine to transform a string to a real value

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: debg
  CHARACTER(LEN=20), INTENT(IN)                          :: string
  REAL, INTENT(OUT)                                      :: value       
! Local
  INTEGER                                                :: i,j
  INTEGER                                                :: Lstring, point, Lpre, Laft
  CHARACTER(LEN=50)                                      :: section

!!!!!!!!!!!!!!!! Variables
! string: string with real number
! value: real value 
! Lstring: length of string
! point: position of point
! Lpre: length of string previous to the point
! Laft: length of string after the point

  section="'string_real'"  
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 150) PRINT *,"  String: '"//TRIM(string)//"'"

  Lstring=LEN_TRIM(string)
  point=INDEX(string, '.')
  Lpre=point-1
  Laft=Lstring-(point+1)

  value=0.
  DO i=1,Lpre
    value=value+(IACHAR(string(i:i))-48)*10.**(Lpre-i)
  END DO 

  DO i=1,Laft
    value=value+(IACHAR(string(point+i:point+i))-48)*10.**(-i)
  END DO

!  PRINT *,'  String:',string,'Lpre:',Lpre,'Laft:',Laft,'Real:',value
  RETURN
END SUBROUTINE string_real 

SUBROUTINE string_Intvalues(debg, string, Nvalues, values)
! Subroutine to obtain real values from a 'namelist' string separated by comas

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: Nvalues, debg
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  INTEGER, DIMENSION(Nvalues), INTENT(OUT)               :: values
! Local
  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval, point
  CHARACTER(LEN=50)                                      :: section
  CHARACTER(LEN=20)                                      :: Svalues

  section="'string_Intvalues'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 150) PRINT *,"  string: '"//TRIM(string)//"'"

  Lstring = len_trim(string)

  ival=1
  jval=1
  DO i=1,Lstring
    IF (string(i:i) == ',') THEN
      eval=i-1
      Svalues=ADJUSTL(string(ival:eval))
      values(jval)=string_int(debg, Svalues)
      ival=i+1
      jval=jval+1
    END IF

  END DO

  Svalues=ADJUSTL(string(ival:Lstring))
  values(jval)=string_int(debg, Svalues)

  IF (debg >= 150) THEN
    PRINT *,'  Found values_______________'
    DO jval=1,Nvalues
      PRINT *,'  ',values(jval)
    END DO
  END IF
  RETURN
END SUBROUTINE string_Intvalues

SUBROUTINE string_Realvalues(debg, string, Nvalues, values)
! Subroutine to obtain real values from a 'namelist' string separated by comas

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: Nvalues, debg
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  REAL, DIMENSION(Nvalues), INTENT(OUT)                  :: values
! Local
  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval, point
  CHARACTER(LEN=50)                                      :: section
  CHARACTER(LEN=20)                                      :: Svalues

  section="'string_Realvalues'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 150) PRINT *,"  string: '"//TRIM(string)//"'"

  Lstring = len_trim(string)

  ival=1
  jval=1
  DO i=1,Lstring
    IF (string(i:i) == ',') THEN
      eval=i-1
      Svalues=ADJUSTL(string(ival:eval))
      CALL string_real(debg, Svalues, values(jval))
      ival=i+1
      jval=jval+1
    END IF

  END DO

  Svalues=ADJUSTL(string(ival:Lstring))
  CALL string_real(debg, Svalues, values(jval))

  IF (debg >= 150) THEN
    PRINT *,'  Found values_______________'
    DO jval=1,Nvalues
      PRINT *,'  ',values(jval)
    END DO
  END IF
  RETURN

END SUBROUTINE string_Realvalues

SUBROUTINE string_values(string, debg, Nvalues, values)
! Subroutine to obtain values from a 'namelist' string separated by comas

  IMPLICIT NONE 

  INTEGER, INTENT(IN)                                    :: Nvalues
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  CHARACTER(LEN=250), DIMENSION(Nvalues), INTENT(OUT)    :: values
  INTEGER, INTENT(IN)                                    :: debg
! Locals
  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval
  CHARACTER(LEN=50)                                      :: section

  section="'string_values'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  Lstring = LEN_TRIM(string)
  values=''

  ival=1
  jval=1
  DO i=1,Lstring
    IF (string(i:i) == ',') THEN
      eval=i-1
      values(jval)=ADJUSTL(string(ival:eval))
      ival=i+1
      jval=jval+1
    END IF
  
  END DO

  values(jval)=ADJUSTL(string(ival:Lstring))

  IF (debg >= 150) THEN
    PRINT *,'  Found', Nvalues,' values_______________'
    DO jval=1,Nvalues
      PRINT *,"  '"//TRIM(values(jval))//"'"
    END DO
  END IF
  RETURN

END SUBROUTINE string_values

SUBROUTINE number_values(string, debg, Nvalues)
! Subroutine to obtain number of variables from a 'namelist' variable with coma separation

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: debg
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  INTEGER, INTENT(OUT)                                   :: Nvalues
! local
  INTEGER                                                :: i, Lstring
  CHARACTER(LEN=50)                                      :: section
  
  section="'number_values'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  Lstring = len_trim(string)

  Nvalues=1

  DO i=1,Lstring

    IF (string(i:i) == ',') THEN
      Nvalues=Nvalues+1
    END IF
  END DO

  IF (debg >= 150) PRINT *,'  There are ',Nvalues,' values'
  RETURN

END SUBROUTINE number_values

END MODULE module_gen_tools
