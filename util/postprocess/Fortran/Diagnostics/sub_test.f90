PROGRAM sub_test
! Program to test functions & subroutines

  USE module_types

  IMPLICIT NONE

!  INCLUDE 'include/types.inc'

  INTEGER                                                :: i,j,k,l,m,n,ijk,idim
  INTEGER                                                :: debug
  INTEGER                                                :: dim1, dim2, dim3, dim4, dim5, dim6 
  INTEGER                                                :: wanteddim
  CHARACTER(LEN=50)                                      :: st, method
  CHARACTER(LEN=250)                                     :: give_Lstring, funsub
  INTEGER, DIMENSION(6)                                  :: dimranges
  REAL                                                   :: constant, print_6Dhalfdim
  REAL                                                   :: number, only_number
  REAL, DIMENSION(:), ALLOCATABLE                        :: mat1D
  REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE              :: matval, matnew

  INTEGER                                                :: Nwords
  CHARACTER(LEN=50)                                      :: word
  CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE           :: words
  LOGICAL                                                :: is_word_in
  TYPE(dimensiondef)                                     :: dimensionsearch
  TYPE(variabledef)                                      :: variableget

  funsub='diagnostic_var_inf'

  debug=150
  dim1=100
  dim2=80
  dim3=30
  dim4=8
  dim5=1
  dim6=1
  
  dimranges=(/dim1, dim2, dim3, dim4, dim5, dim6 /)
  
  IF (ALLOCATED(matval)) DEALLOCATE(matval)
  ALLOCATE(matval(dim1, dim2, dim3, dim4, dim5, dim6))
  
  DO i=1,dim1
    DO j=1,dim2
      DO k=1,dim3
        DO l=1,dim4
	  DO m=1,dim5
	    DO n=1,dim6
	      matval(i,j,k,l,m,n)=SQRT(((i*1.-1.)-dim1/2.)**2.+((j*1.-1.)-dim2/2.)**2.)+       &
	        10.*(k*1.-1.)+5.*(l*1.-1.)
	    END DO
	  END DO
	END DO
      END DO
    END DO
  END DO

  SELECT CASE (funsub)

!!  diagnostic_var_inf
!!!!
  CASE ('diagnostic_var_inf')
    word='CLT'
    CALL diagnostic_var_inf(debug, word, variableget)

!!  only_number
!!!!
  CASE ('only_number')
    word='....A150...f'
    number=only_number(debug, word)
    PRINT *,"From '"//TRIM(word)//"' :",number

!!  diagnostic_dim_inf
!!!!
  CASE ('diagnostic_dim_inf')
    CALL diagnostic_dim_inf(debug, 'lev', 3, dimensionsearch)
  
    PRINT *,"Read information for '"//TRIM(dimensionsearch%name)//"' dimension________"
    PRINT *,'Dimension id: ',dimensionsearch%id
    PRINT *,'  Dimension type:', TRIM(dimensionsearch%type)
    IF (TRIM(dimensionsearch%INname) /= '-') THEN
      PRINT *,'  Dimension name in input files: ',TRIM(dimensionsearch%INname)
    ELSE
      PRINT *,'  New dimension. It does not exists in input files!'
    END IF
    IF (dimensionsearch%NinVarnames > 0) THEN
      PRINT *,'  Number of input variables to compute dimension: ',dimensionsearch%NinVarnames
      PRINT *,'  Name of input variables:', ("'"//TRIM(dimensionsearch%INvarnames(idim))//"'",  &
        char(44), idim=1,dimensionsearch%NinVarnames)
    ELSE
      PRINT *,'  No input variables are needed to compute dimension!'
    END IF
    PRINT *,'  Dimension method: ',TRIM(dimensionsearch%method)
    PRINT *,'  Dimension constant ',dimensionsearch%constant
    PRINT *,'  Dimension in dimensions: ',dimensionsearch%indimensions
    PRINT *,'  Dimension standard name: ',TRIM(dimensionsearch%stdname)
    PRINT *,'  Dimension long name: ',TRIM(dimensionsearch%lonname)
    PRINT *,'  Dimension units: ',TRIM(dimensionsearch%units)
    IF (dimensionsearch%Nvalues > 0) THEN
      PRINT *,'  Number of values of dimension: ',dimensionsearch%Nvalues
      PRINT *,'  Values of dimension: ',(dimensionsearch%values(idim), idim=1,                  &
        dimensionsearch%Nvalues)
    END IF
! Specific horizontal dim
    IF (TRIM(dimensionsearch%coords) /= '-') PRINT *,'  Base coordinates of dimension: ',       &
      TRIM(dimensionsearch%coords)
   
! Specific vertical dim
    IF (TRIM(dimensionsearch%positive) /= '-') PRINT *,'  Sign of increase of dimension: ',     &
      TRIM(dimensionsearch%positive)
    IF (TRIM(dimensionsearch%form) /= '-') PRINT *,'  Formula of dimension: ',                  &
      TRIM(dimensionsearch%form)

!! calc_method_gen6D
!!!!
  CASE('calc_method_gen6D')

    constant=0.5
    method='sumall'
  
    IF (ALLOCATED(matnew)) DEALLOCATE(matnew)
    ALLOCATE(matnew(dim1, dim2, dim3, dim4, dim5, dim6))
  
    CALL calc_method_gen6D(debug, method,(/dim1,dim2,dim3,dim4,dim5,dim6/),2,(/matval,matval/),&
      constant, matnew)
  
    PRINT *,'matnew: ',print_6Dhalfdim(matnew, (/dim1,dim2,dim3,dim4,dim5,dim6 /))

!! calc_method1D
!!!!
  CASE ('calc_method1D')
  
    constant=0.5
    method='sumall'
  
    IF (ALLOCATED(mat1D)) DEALLOCATE(mat1D)
    ALLOCATE(mat1D(dim3))
  
    CALL calc_method1D(debug, method, dim3, 2, (/ matval(1,1,:,1,1,1), matval(1,1,:,1,1,1) /), &
      constant, mat1D)
  
    PRINT *,'mat1D: ',mat1D

!! give1D_from6D
!!!!
  CASE ('give1D_from6D')

   wanteddim=1

   IF (ALLOCATED(mat1D)) DEALLOCATE(mat1D)
   ALLOCATE(mat1D(dimranges(wanteddim)))
  
   CALL give1D_from6D(debug, matval, dimranges, wanteddim, mat1D)

!! give_Lstring
!!!!
  CASE ('give_Lstring')

    st='Hola Lluis'
  
    PRINT *,"**** '"//give_Lstring(150, st)//"' ****"

! This does not work!
    PRINT *,"12 char: '"//TRIM(give_Lstring(50,st))//"'"

! is_word_in
!!
  CASE ('is_word_in')
    Nwords=5
    IF (ALLOCATED(words)) DEALLOCATE(words)
    ALLOCATE(words(Nwords))

    words=(/ 'direct', 'sumct', 'prodct', 'sumall'/)
    word='sumall'

    IF (is_word_in(word, Nwords, words)) PRINT *,"'"//TRIM(word)//"' is in :",(TRIM(words(i)), &
      char(44), i=1, Nwords)

  DEALLOCATE(words)

  CASE DEFAULT
    PRINT *,"'"//TRIM(funsub)//"' does not exist !!!!"
  END SELECT
END PROGRAM sub_test

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
! diagnostic_coords:    (base coords of diagnostic, optional)
! diagnostic_formula:   (formula to be given as attribute of diagnostic, optional)
! diagnostic_Noptions:  (number of options for diagnostic)
! diagnostic_options:   (values of options)
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

   READ(iunit,*)car, varread%type
   READ(iunit,*)car, varread%Ninvarnames
   READ(iunit,*)car, READinvarnames
   IF (ALLOCATED(invarnames_targ)) DEALLOCATE(invarnames_targ)
   ALLOCATE(invarnames_targ(varread%Ninvarnames))
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
   READ(iunit,*)car, varread%coords
   READ(iunit,*)car, varread%form
   READ(iunit,*)car, varread%Noptions
   IF (ALLOCATED(optionsDIAG_targ)) DEALLOCATE(optionsDIAG_targ)
   ALLOCATE(optionsDIAG_targ(varread%Noptions))
   READ(iunit,*)car, optionsDIAG_targ
   IF (ASSOCIATED(varread%options)) NULLIFY(varread%options)
   ALLOCATE(varread%options(varread%Noptions))
   varread%options=>optionsDIAG_targ

   CLOSE(iunit)

   IF (debg >= 75) THEN
     PRINT *,"  Read information for '"//TRIM(varDIAG)//"' variable________"
     IF (varread%type == 2) PRINT *,'  string variable'
     IF (varread%type == 4) PRINT *,'  integer variable'
     IF (varread%type == 5) PRINT *,'  real variable'
     PRINT *,'  Number of necessary input variables:', varread%NinVarnames
     PRINT *,'  Name of input variables:', ("'"//TRIM(varread%INvarnames(i))//"'", char(44),  &
       i=1, varread%NinVarnames)
     PRINT *,'  rank:', varread%rank
     PRINT *,'  Shape of dimensions:', (varread%shape(i), char(44), i=1,varread%rank)
     PRINT *,'  CF-standard name:', TRIM(varread%stdname)
     PRINT *,'  Long description:', TRIM(varread%lonname)
     PRINT *,'  Units:', TRIM(varread%units)
     PRINT *,'  Method:', TRIM(varread%method)
     PRINT *,'  constant:', varread%constant
     PRINT *,'  coordinates:', TRIM(varread%coords)
     PRINT *,'  formula:', TRIM(varread%form)
     PRINT *,'  number of options for diagnostic: ',varread%Noptions
     PRINT *,'  values of options: ',varread%options
   END IF
  
END SUBROUTINE diagnostic_var_inf

REAL FUNCTION only_number(debg, string0in)
! Function to give a real number from any string with contiguous numbers (point included)

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg
  CHARACTER(LEN=*), INTENT(IN)                            :: string0in

! Local
  INTEGER                                                 :: icar, Lstring, Lnewstring
  INTEGER                                                 :: string_int
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
    PRINT *,icar,'|',stringin(icar:icar)//': ',ICHAR(stringin(icar:icar)), "'"//                         &
      TRIM(newstring(1:Lnewstring))//"'"
  END DO

  IF (point) THEN
    CALL string_real(debg, newstring, only_number)
  ELSE
    only_number=string_int(debg, newstring)*1.
  END IF

  IF (debg >= 150) PRINT *,"  From string '"//TRIM(stringin)//"' real number:", only_number

END FUNCTION only_number

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
!    direct: values are the same from dim_in_varnames [for num_dimInVarnames=1]
!    sumct: values are the same from dim_in_varnames plus a constant [for num_dimInVarnames=1]
!    prodct: values are the same from dim_in_varnames times a constant [for num_dimInVarnames=1]
!    sumall: values are the result of the sum of all [dim_in_varnames]
!    xxxxxx: specific for this dimension (xxxxx must have some sense in 'calc_method1D' (in
!       'module_gen_tools') or in 'compute_dimensions' for 'name' (in 'module_nc_tools') 
! constant              (constant value for method='constant')
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

!  INCLUDE 'include/types.inc'

  INTEGER, INTENT(IN)                                    :: debg, dimid
  TYPE(dimensiondef), INTENT(OUT)                        :: diminf
  CHARACTER(LEN=*), INTENT(IN)                           :: dimDIAG0

! Local variables
  INTEGER                                                :: i, ilin, idim
  INTEGER                                                :: iunit, ios
  INTEGER                                                :: Llabel, posDIM
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label, section, dimDIAG
  LOGICAL                                                :: is_used
  CHARACTER(LEN=250), DIMENSION(:), ALLOCATABLE, TARGET  :: dimInvarnames
  INTEGER, DIMENSION(:), ALLOCATABLE, TARGET             :: dimindimensions
  REAL, DIMENSION(:), ALLOCATABLE, TARGET                :: dimfixvalues
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
   PRINT *,'Que pasa neng?', TRIM(diminf%type)
   READ(iunit,*)car, diminf%axis
   PRINT *,'Que pasa neng? 0', TRIM(diminf%axis)
   READ(iunit,*)car, diminf%INname
   PRINT *,'Que pasa neng? 1 ', TRIM(diminf%INname)
   READ(iunit,*)car, diminf%NinVarnames
   PRINT *,'Que pasa neng? 2 ', diminf%NinVarnames
   IF (ALLOCATED(dimInvarnames)) DEALLOCATE(dimInvarnames)
   ALLOCATE(dimInvarnames(diminf%NinVarnames))
   READ(iunit,*)car, readINvarnames
   PRINT *,'Que pasa neng? 3 ', "'"//TRIM(readINvarnames)//"'"
   CALL string_values(readINvarnames, debg, diminf%NinVarnames, dimInvarnames)
   PRINT *,'Que pasa neng? 3b ', (TRIM(dimInvarnames(idim)), char(44), idim=1, diminf%NinVarnames)
   ALLOCATE(diminf%INvarnames(diminf%NinVarnames))
   diminf%INvarnames=>dimInvarnames
   PRINT *,'Que pasa neng? 4 ', (TRIM(diminf%INvarnames(idim)), char(44), idim=1, diminf%NinVarnames)
   PRINT *,'Ale hoop!'
   READ(iunit,*)car, diminf%method
   PRINT *,'Que pasa neng? 5 ', TRIM(diminf%method)
   READ(iunit,*)car, diminf%constant
   PRINT *,'Que pasa neng? 5 a', diminf%constant
   IF (ALLOCATED(dimindimensions)) DEALLOCATE(dimindimensions)
   ALLOCATE(dimindimensions(diminf%NinVarnames))
   READ(iunit,*)car, readINdimensions
   PRINT *,'Que pasa neng? 5 b ', TRIM(readINdimensions)
   CALL string_Intvalues(debg, readINdimensions, diminf%NinVarnames, dimindimensions)
   diminf%indimensions=>dimindimensions
   PRINT *,'Que pasa neng? 5 bb ',diminf%indimensions
   READ(iunit,*)car, diminf%stdname
   PRINT *,'Que pasa neng? 5 c', TRIM(diminf%stdname)
   READ(iunit,*)car, diminf%lonname
   PRINT *,'Que pasa neng? 6 ', TRIM(diminf%lonname)
   READ(iunit,*)car, diminf%units
   PRINT *,'Que pasa neng? 7 ', TRIM(diminf%units)
   READ(iunit,*)car, diminf%Nvalues
   PRINT *,'Que pasa neng? 8 ', diminf%Nvalues
   IF (ALLOCATED(dimfixvalues)) DEALLOCATE(dimfixvalues)
   ALLOCATE(dimfixvalues(diminf%Nvalues))
   READ(iunit,*)car, readFIXvalues
   PRINT *,'Que pasa neng? 9 ', TRIM(readFIXvalues)
   IF (TRIM(readFIXvalues) /= '-') THEN
     CALL string_Realvalues(debg, readFIXvalues, diminf%Nvalues, dimfixvalues)
   ELSE
     dimfixvalues=0.
   END IF
   diminf%values=>dimfixvalues
   PRINT *,'Que pasa neng? 10 ', diminf%values
   
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

REAL FUNCTION print_6Dhalfdim(mat, rgs)
! Subroutine to print central value of a real 6D matrix

!  USE module_gen_tools

  IMPLICIT NONE
  
  INTEGER, DIMENSION(6), INTENT(IN)                       :: rgs
  REAL, DIMENSION(rgs(1), rgs(2), rgs(3), rgs(4), rgs(5),                                       &
    rgs(6)), INTENT(IN)                                   :: mat
    
! Local
  INTEGER                                                 :: halfdim

  print_6Dhalfdim=mat(halfdim(rgs(1)), halfdim(rgs(2)), halfdim(rgs(3)), halfdim(rgs(4)),       &
      halfdim(rgs(5)), halfdim(rgs(6)))

END FUNCTION print_6Dhalfdim

SUBROUTINE calc_method_gen6D(debg, meth, rgs, Ninvalues, invalues, ct, vals)
! Subroutine to compute generic methods for 6D matrices of the same shape

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg, Ninvalues
  INTEGER, DIMENSION(6), INTENT(IN)                       :: rgs
  CHARACTER(LEN=50)                                       :: meth
  REAL, INTENT(IN)                                        :: ct
  REAL, DIMENSION(rgs(1), rgs(2), rgs(3), rgs(4), rgs(5),                                       &
    rgs(6), Ninvalues), INTENT(IN)                        :: invalues
  REAL, DIMENSION(rgs(1), rgs(2), rgs(3), rgs(4), rgs(5),                                       &
    rgs(6)), INTENT(OUT)                                  :: vals
  
! Local
  INTEGER                                                 :: ival, j
  INTEGER                                                 :: halfdim
  CHARACTER(LEN=50)                                       :: section, messg
  REAL                                                    :: print_6Dhalfdim
  
!!!!!!! Variables
! meth: method to compute
! rgs: ranges of input values
! Ninvalues: number of input values
! invalues: input values
! ct: constant for 'sumct' and 'prodct' methods
! vals: result of application of the method

  section="'calc_method_gen6D'"
  
  IF (debg >= 150) PRINT *,'Section '//section//'... .. .'
  vals=0.
  
  SELECT CASE (meth)
    CASE ('direct')
      vals=invalues(:,:,:,:,:,:,1)
    CASE ('sumct')
      vals=invalues(:,:,:,:,:,:,1)+ct
    CASE ('prodct')
      vals=invalues(:,:,:,:,:,:,1)*ct    
    CASE ('sumall')
      PRINT *,meth
      vals=SUM(invalues, DIM=7)
    CASE DEFAULT
      messg="  Giving 6D general method: '"//TRIM(meth)//"' is not defined!"
      CALL diag_fatal(messg)
  END SELECT
  
  IF (debg >= 150) THEN
    PRINT *,"  dim/2 values given by "//TRIM(meth)//"' method: "
    PRINT *,'    ',(invalues(halfdim(rgs(1)), halfdim(rgs(2)), halfdim(rgs(3)), halfdim(rgs(4)),&
      halfdim(rgs(5)), halfdim(rgs(6)),j), char(44), j=1, Ninvalues), '-->',                    &
      print_6Dhalfdim(vals, rgs)
  END IF
  
END SUBROUTINE calc_method_gen6D


SUBROUTINE calc_method1D(debg, meth, rg, Ninvalues, invalues, ct, vals)
! Subroutine to compute specific 1D method

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg, rg, Ninvalues
  CHARACTER(LEN=50)                                       :: meth
  REAL, INTENT(IN)                                        :: ct
  REAL, DIMENSION(rg, Ninvalues), INTENT(IN)              :: invalues
  REAL, DIMENSION(rg), INTENT(OUT)                        :: vals
  
! Local
  INTEGER                                                 :: ival, j
  CHARACTER(LEN=50)                                       :: section, messg
  
!!!!!!! Variables
! meth: method to compute
! rg: range of input values
! Ninvalues: number of input values
! invalues: input values
! ct: constant for 'sumct' and 'prodct' methods
! vals: result of application of the method

  section="'calc_method1D'"
  
  IF (debg >= 150) PRINT *,'Section '//section//'... .. .'
  
  SELECT CASE (meth)
    CASE ('direct')
      vals=invalues(:,1)
    CASE ('sumct')
      vals=invalues(:,1)+ct
    CASE ('prodct')
      vals=invalues(:,1)*ct    
    CASE ('sumall')
      vals=SUM(invalues, DIM=2)
    CASE DEFAULT
      messg="  Giving method: '"//TRIM(meth)//"' is not defined!"
      CALL diag_fatal(messg)
  END SELECT
  
  IF (debg >= 150) THEN
    PRINT *,"  Values given by "//TRIM(meth)//"' method: "
    DO ival=1, rg
      PRINT *,'    ',ival,(invalues(ival, j), char(44), j=1, Ninvalues), '-->', vals(ival)
    END DO
  END IF
  
END SUBROUTINE calc_method1D

SUBROUTINE give1D_from6D(debg, matrix, matrg, wtddim, vec)
! Subroutine to give a 1D vector from any dimension of a 6D matrix

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg, wtddim
  INTEGER, DIMENSION(6), INTENT(IN)                       :: matrg
  REAL, DIMENSION(matrg(1), matrg(2), matrg(3), matrg(4),                                       &
    matrg(5), matrg(6) )                                  :: matrix
  REAL, DIMENSION(matrg(wtddim)), INTENT(OUT)             :: vec
  
! Local
  CHARACTER(LEN=50)                                       :: section
  
  section="'give1D_from6D'"
  IF (debg >= 150) PRINT *,'Section '//section//'... .. .'
  
  SELECT CASE (wtddim)
    CASE (1)
      vec=matrix(:,1,1,1,1,1)
    CASE (2)
      vec=matrix(1,:,1,1,1,1)
    CASE (3)
      vec=matrix(1,1,:,1,1,1)
    CASE (4)
      vec=matrix(1,1,1,:,1,1)
    CASE (5)
      vec=matrix(1,1,1,1,:,1)
    CASE (6)
      vec=matrix(1,1,1,1,1,:)
  END SELECT
  
  IF (debg >= 150) PRINT *,'  Given values of dim ',wtddim,' : ',vec
  
END SUBROUTINE give1D_from6D

CHARACTER(LEN=250) FUNCTION give_Lstring(debg, string)
! Function that gives a string of length 'Lstring' filling blanks after 'string' (up to 250)

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg
  CHARACTER(LEN=*), INTENT(IN)                            :: string

! Local
  INTEGER                                                 :: icar, lenstring, Lstring
  CHARACTER(LEN=50)                                       :: section
  
  section="'give_Lstring'"

  IF (debg >= 150 ) PRINT *,'Section '//section//'... .. .'

  Lstring=LEN_TRIM(string)
  give_Lstring=TRIM(string)
  DO icar=lenstring+1, 250
    give_Lstring(icar:icar)=' '
  END DO

END FUNCTION give_Lstring

SUBROUTINE diag_fatal(msg)
! Subroutine to give fatal error with a message

  IMPLICIT NONE
  
  CHARACTER(LEN=250), INTENT(IN)                          :: msg
  
  PRINT *,msg
  STOP

END SUBROUTINE diag_fatal

INTEGER FUNCTION halfdim(dim)
! Function to give the half value of a dimension

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: dim
  
  halfdim = dim/2
  IF (dim < 2) halfdim = 1     ! Assuming non-zero dimension range
  
END FUNCTION halfdim

LOGICAL FUNCTION is_word_in(wd, Nwd, wds)
! Function to test whether 'wd' is within 'wds'

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: Nwd
  CHARACTER(LEN=50), INTENT(IN)                          :: wd
  CHARACTER(LEN=50), DIMENSION(Nwd), INTENT(IN)          :: wds

  is_word_in=ANY(wds==wd)

END FUNCTION is_word_in

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
  IF (debg >= 150) PRINT *,"String: '"//TRIM(string)//"'"

  Lstring=LEN_TRIM(string)

  string_int=0
  DO i=1,Lstring
    string_int=string_int+(IACHAR(string(i:i))-48)*10.**(Lstring-i)
  END DO 


  IF (debg >= 150) PRINT *,'Integer:',string_int
  
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
  IF (debg >= 150) PRINT *,"String: '"//TRIM(string)//"'"

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

!  PRINT *,'String:',string,'Lpre:',Lpre,'Laft:',Laft,'Real:',value
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
  INTEGER                                                :: Lstring, string_int
  INTEGER                                                :: ival, eval, point
  CHARACTER(LEN=50)                                      :: section
  CHARACTER(LEN=20)                                      :: Svalues

  section="'string_Intvalues'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 150) PRINT *,"string: '"//TRIM(string)//"'"

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
    PRINT *,'Found values_______________'
    DO jval=1,Nvalues
      PRINT *,values(jval)
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
  IF (debg >= 150) PRINT *,"string: '"//TRIM(string)//"'"

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
    PRINT *,'Found values_______________'
    DO jval=1,Nvalues
      PRINT *,values(jval)
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
    PRINT *,'Found values_______________'
    DO jval=1,Nvalues
      PRINT *,'************'//TRIM(values(jval))//'********'
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

!  PRINT *,'There are ',Nvalues,' values'
  RETURN

END SUBROUTINE number_values
