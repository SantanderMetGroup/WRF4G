! VARIABLE. Variable description
!!
    CASE ('VARIABLE')

      IF (dbg >= 75) PRINT *,'  Computing VARIABLE...'

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,4)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,1,:,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        method%variable%: SELECT CASE (diagcom%method)

          CASE ('method1')
	  
	  
	  CASE ('methodn')

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT method%variable%

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (2)
!         Text diagnostic
          rcode = nf_put_var_text (oid, jvar, Cdiagnostic3D)
          CALL error_nc(section, rcode)
        CASE (4)
!         integer diagnostic
          rcode = nf_put_var_real (oid, jvar, Idiagnostic3D)
          CALL error_nc(section, rcode)
        CASE (5)
!       Real diagnostic
          rcode = nf_put_var_real (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA)
