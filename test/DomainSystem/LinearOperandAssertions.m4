module LinearOperandAssertions
  
  USE_MACROS({FlowField})
  use LogModule
  use Global
  use TestUtilities
  use FlowFieldModule
  
  implicit none
  private
  public :: createAssertEqualLinearOperand_Scalar, &
       createAssertEqualLinearOperand_Vector
  
  character(*), parameter :: MOD_NAME = 'LinearOperandAssertions'
    
  type, extends(ImpureScalarLinearOperationInterface), abstract :: AssertEqual_ScalarType
     real(FLOAT) :: tolerance = TOL
  end type AssertEqual_ScalarType
  
  type, extends(ImpureVectorLinearOperationInterface), abstract :: AssertEqual_VectorType
     real(FLOAT) :: tolerance = TOL
  end type AssertEqual_VectorType

  
  m4_define({TYPEDEFS_ASSERTEQUALLINEAROPERAND}, {
  type, extends(AssertEqual_$3Type), abstract :: AssertEqual_$2_$3Type
     private
     real(FLOAT), dimension(:, :), pointer :: operand1 => null()
     real(FLOAT), dimension(:, :), pointer :: operand2 => null()
   contains
     procedure :: deinit => deinit_AssertEqual_$2_$3
     m4_ifelse($1, {}, {}, {procedure :: visit$1ForOperand1 => &
          visit$1ForOperand1_AssertEqual_$2_$3})
     m4_ifelse($1, {}, {}, {procedure :: visit$1ForOperand2 => &
          visit$1ForOperand2_AssertEqual_$2_$3})
     procedure :: getNumRows => getNumRows_AssertEqual_$2_$3
     procedure :: getNumColumns => getNumColumns_AssertEqual_$2_$3
     procedure :: check => check_AssertEqual_$2_$3
  end type AssertEqual_$2_$3Type

  
  type, extends(AssertEqual_$2_$3Type) :: AssertExactlyEqual_$2_$3Type
     logical :: allowZeroSum = .false. 
     logical :: allowDiffSizes = .false. 
   contains
     procedure :: perform => perform_AssertExactlyEqual_$2_$3
  end type AssertExactlyEqual_$2_$3Type

  
  type, extends(AssertEqual_$2_$3Type) :: AssertEqualWithPermutations_$2_$3Type
   contains
     procedure :: perform => perform_AssertEqualWithPermutations_$2_$3
  end type AssertEqualWithPermutations_$2_$3Type

  
  type, extends(AssertEqual_$2_$3Type) :: AssertEqualWithPermutations_$2Transposed_$3Type
   contains
     procedure :: perform => &
          perform_AssertEqualWithPermutations_$2Transposed_$3
  end type AssertEqualWithPermutations_$2Transposed_$3Type
  })

  ! The following classes can be used to check data embedded in various
  ! linear system elements without having to implement explicit accessors.
  ! The classes will compare two such elements via the visit?ForOperand1
  ! and visit?ForOperand2 methods.  The visit?ForResult method is not
  ! needed.  Calling 'performImpure' triggers the assertion.
  
  TYPEDEFS_ASSERTEQUALLINEAROPERAND({Matrix}, {A}, {Scalar})
  
  TYPEDEFS_ASSERTEQUALLINEAROPERAND({Matrix}, {A}, {Vector})
  
  TYPEDEFS_ASSERTEQUALLINEAROPERAND({NullSpace}, {Q2}, {Scalar})
  
  TYPEDEFS_ASSERTEQUALLINEAROPERAND({NullSpace}, {Q2}, {Vector})
  
  TYPEDEFS_ASSERTEQUALLINEAROPERAND({RHSColumn}, {B}, {Scalar})
  
  TYPEDEFS_ASSERTEQUALLINEAROPERAND({RHSColumn}, {B}, {Vector})
  
  TYPEDEFS_ASSERTEQUALLINEAROPERAND({SolutionColumn}, {X}, {Scalar})
  
  TYPEDEFS_ASSERTEQUALLINEAROPERAND({SolutionColumn}, {X}, {Vector})
  
  
contains

  m4_define({PROCEDUREBODY_CREATE_ASSERTEQUALLINEAROPERAND_SELECTVARIATION},
  {
       select case ( trim(variationName) )
       case ('ExactlyEqual')
          allocate( AssertExactlyEqual_$1_$2Type :: &
               impure$2LinearOperation, stat=stat )
       case ('EqualWithPermutations')
          allocate( AssertEqualWithPermutations_$1_$2Type :: &
               impure$2LinearOperation, stat=stat )
       case ('TransposedEqualWithPermutations')
          allocate( AssertEqualWithPermutations_$1Transposed_$2Type :: &
               impure$2LinearOperation, stat=stat )
       case ('EqualOrZeroSum')
          allocate( AssertExactlyEqual_$1_$2Type :: &
               impure$2LinearOperation, stat=stat )
          select type ( impure$2LinearOperation )
          type is (AssertExactlyEqual_$1_$2Type)
             impure$2LinearOperation%allowZeroSum = .true. 
          end select
       case ('EqualAndAllowDiffSizes')
          allocate( AssertExactlyEqual_$1_$2Type :: &
               impure$2LinearOperation, stat=stat )
          select type ( impure$2LinearOperation )
          type is (AssertExactlyEqual_$1_$2Type)
             impure$2LinearOperation%allowDiffSizes = .true. 
          end select
       case default
          call addEvent( FATAL, 'variationName not recognised.  Can &
               &have ''ExactlyEqual'', ''EqualWithPermutations'', &
               &''TransposedEqualWithPermutations'', ''EqualOrZeroSum'', &
               &or ''EqualAndAllowDiffSizes''; argument was '''//trim(&
               variationName)//'''.', log )
       end select})
       
  m4_define({PROCEDURE_CREATE_ASSERTEQUALLINEAROPERAND}, {
  subroutine createAssertEqualLinearOperand_$1( impure$1LinearOperation, &
       operandName, variationName, tolerance, log )
    class(Impure$1LinearOperationInterface), allocatable, &
         intent(inout) :: impure$1LinearOperation
    character(*), intent(in) :: operandName, variationName
    real(FLOAT), intent(in), optional :: tolerance
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createAssertEqualLinearOperand_$1', log )
    
    select case ( operandName )
    case ('A')
       PROCEDUREBODY_CREATE_ASSERTEQUALLINEAROPERAND_SELECTVARIATION({A},
       {$1})
    case ('Q2')
       PROCEDUREBODY_CREATE_ASSERTEQUALLINEAROPERAND_SELECTVARIATION({Q2},
       {$1})
    case ('B')
       PROCEDUREBODY_CREATE_ASSERTEQUALLINEAROPERAND_SELECTVARIATION({B},
       {$1})
    case ('X')
       PROCEDUREBODY_CREATE_ASSERTEQUALLINEAROPERAND_SELECTVARIATION({X},
       {$1})
    case default
       call addEvent( FATAL, 'operandName not recognised.  Can have &
            &''A'', ''Q2'', ''B'', or ''X''; argument was '''//trim(&
            operandName)//'''.', log )
    end select
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &impure$1LinearOperation.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    if ( present(tolerance) ) then
       select type ( impure$1LinearOperation )
          class is (AssertEqual_$1Type)
          impure$1LinearOperation%tolerance = tolerance
       end select
    end if

    call endSub( log )
  end subroutine createAssertEqualLinearOperand_$1
  })
  
  PROCEDURE_CREATE_ASSERTEQUALLINEAROPERAND({Scalar})
  
  PROCEDURE_CREATE_ASSERTEQUALLINEAROPERAND({Vector})

  
  m4_define({METHODDEF_ASSERTEQUALLINEAROPERAND_PERFORM_AEWP}, {
  subroutine perform_AssertEqualWithPermutations_$2$4_$3( obj, log )
    class(AssertEqualWithPermutations_$2$4_$3Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, j, j0
    logical, dimension(size(obj%operand1, $5)) :: checkList1
    logical, dimension(size(obj%operand2, $5)) :: checkList2
    real(FLOAT), dimension(size(obj%operand1, $6)) :: $7{}1
    real(FLOAT), dimension(size(obj%operand2, $6)) :: $7{}2
    real(FLOAT) :: rmsErrMin, rmsErr
    logical, dimension(2) :: vacancies
    call beginSub( MOD_NAME, &
         'perform_AssertEqualWithPermutations$4_$2_$3', log )

    ! automatically do an operand check
    call obj%check( vacancies, log )
    call assertFalse( any(vacancies), 'One or more operands have not &
         &been set.  Check echo file.' )
    if ( any(vacancies) ) then
       call endSub( log )
       return
    end if

    call assertEqual( size(checkList1), size(checkList2), &
         'Second operand has different number of $7s to the first.' )
    if ( size(checkList1) /= size(checkList2) ) then
       call endSub( log )
       return
    end if

    call assertFalse( size(checkList1) == 0, 'Operands have no $7s.' )
    if ( size(checkList1) == 0 ) then
       call endSub( log )
       return
    end if
    
    checkList1 = .false. 
    checkList2 = .false. 
    do i = 1, size(checkList1)
       do j = 1, size(checkList2)
          if ( checkList2(j) ) cycle
          $8
          if ( all( abs($7{}1 - $7{}2) <= obj%tolerance ) ) then
             checkList1(i) = .true.
             checkList2(j) = .true.
             exit
          end if
       end do
    end do
    
    call assertTrue( all(checkList1), 'Although ordering of the $7s &
         &is not important, there are '//str(count(.not. checkList1))//' &
         &that do not match.  Iterating over the remaining $7s from set #&
         &1, and matching $7s from set #2 with the best RMS error, &
         &yields the following comparisons:' )
    
    do i = 1, size(checkList1)
       if ( checkList1(i) ) cycle
       rmsErrMin = huge(rmsErrMin)
       
       do j = 1, size(checkList2)
          if ( checkList2(j) ) cycle
          $8
          rmsErr = sqrt(sum(($7{}1 - $7{}2)**2))
          if ( rmsErr < rmsErrMin ) then
             rmsErrMin = rmsErr
             j0 = j
          end if
       end do
       
       j = j0
       checkList1(i) = .true.
       checkList2(j) = .true.
       $8
       call assertEqual( $7{}1, $7{}2, obj%tolerance, '' )
    end do
    
    call endSub( log )
  end subroutine perform_AssertEqualWithPermutations_$2$4_$3
  })

  
  m4_define({METHODDEF_ASSERTEQUALLINEAROPERAND_VISIT}, {
  subroutine visit$2ForOperand$3_$1( obj, array, from )
    class($1Type), intent(inout) :: obj
    real(FLOAT), dimension(:, :), target, intent(inout) :: array
    integer, dimension(2), intent(in), optional :: from
    obj%operand$3 => array
  end subroutine visit$2ForOperand$3_$1
  })

  
  m4_define({METHODDEFS_ASSERTEQUALLINEAROPERAND}, {
  pure subroutine deinit_AssertEqual_$2_$3( obj, log )
    class(AssertEqual_$2_$3Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    obj%tolerance = TOL
  end subroutine deinit_AssertEqual_$2_$3

  
  METHODDEF_ASSERTEQUALLINEAROPERAND_VISIT({AssertEqual_$2_$3},
  {$1}, {1})
  
  METHODDEF_ASSERTEQUALLINEAROPERAND_VISIT({AssertEqual_$2_$3},
  {$1}, {2})
  
  function getNumRows_AssertEqual_$2_$3( obj )
    class(AssertEqual_$2_$3Type), intent(in) :: obj
    integer :: getNumRows_AssertEqual_$2_$3
    ! stub
    getNumRows_AssertEqual_$2_$3 = 0
  end function getNumRows_AssertEqual_$2_$3

  
  function getNumColumns_AssertEqual_$2_$3( obj )
    class(AssertEqual_$2_$3Type), intent(in) :: obj
    integer :: getNumColumns_AssertEqual_$2_$3
    ! stub
    getNumColumns_AssertEqual_$2_$3 = 0
  end function getNumColumns_AssertEqual_$2_$3

  
  pure subroutine check_AssertEqual_$2_$3( obj, vacancies, log )
    class(AssertEqual_$2_$3Type), intent(in) :: obj
    logical, dimension(:), intent(out), optional :: vacancies
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'check_AssertEqual_$2_$3', log )

    vacancies(1) = .not. associated(obj%operand1)
    vacancies(2) = .not. associated(obj%operand2)
    
    do i = 1, 2
       call addEvent( vacancies(i), WARNING, &
            'Operand '//str(i)//' has not been set.', log )
    end do
    call endSub( log )
  end subroutine check_AssertEqual_$2_$3
  
  
  subroutine perform_AssertExactlyEqual_$2_$3( obj, log )
    class(AssertExactlyEqual_$2_$3Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, j, m, n, stat
    logical, dimension(3) :: vacancies
    real(FLOAT) :: sign
    call beginSub( MOD_NAME, 'perform_AssertExactlyEqual_$2_$3', log )

    ! automatically do an operand check
    call obj%check( vacancies, log )
    call assertFalse( any(vacancies), 'One or more operands have not &
         &been set.  Check echo file.' )
    if ( any(vacancies) ) then
       call endSub( log )
       return
    end if

    if ( .not. obj%allowDiffSizes ) then
       call assertEqual( size(obj%operand1, 1), size(obj%operand2, 1), &
            'Second operand has different number of rows than first &
            &operand.' )
       if ( size(obj%operand1, 1) /= size(obj%operand2, 1) ) then
          call endSub( log )
          return
       end if
       
       call assertEqual( size(obj%operand1, 2), size(obj%operand2, 2), &
            'Second operand has different number of columns than first &
            &operand.' )
       if ( size(obj%operand1, 2) /= size(obj%operand2, 2) ) then
          call endSub( log )
          return
       end if
    end if

    call assertFalse( size(obj%operand1, 1) == 0, 'Operand 1 has no &
         &rows.' )
    if ( size(obj%operand1, 1) == 0 ) then
       call endSub( log )
       return
    end if
    call assertFalse( size(obj%operand2, 1) == 0, 'Operand 2 has no &
         &rows.' )
    if ( size(obj%operand2, 1) == 0 ) then
       call endSub( log )
       return
    end if
    call assertFalse( size(obj%operand1, 2) == 0, 'Operand 1 has no &
         &columns.' )
    if ( size(obj%operand1, 2) == 0 ) then
       call endSub( log )
       return
    end if
    call assertFalse( size(obj%operand2, 2) == 0, 'Operand 2 has no &
         &columns.' )
    if ( size(obj%operand2, 2) == 0 ) then
       call endSub( log )
       return
    end if

    m = min( size(obj%operand1, 1), size(obj%operand2, 1) )
    n = min( size(obj%operand1, 2), size(obj%operand2, 2) )
    
    ! with e.g. null space basis vectors, elements are expected to have
    ! the same magnitude because they are orthonormal.  However, one
    ! vector might be the negative of the other.  Allow for a second try
    ! with the one operand inverted.
    sign = 1._FLOAT
    if ( obj%allowZeroSum ) then
       if ( .not. all(abs(obj%operand2(1:m, 1:n) - obj%operand1(1:m, 1:&
            n)) < 1.E-5) ) then
          sign = -1._FLOAT
       end if
    end if

    do j = 1, n
       do i = 1, m
          call assertEqual( obj%operand1(i, j), sign*obj%operand2(i, j), &
               obj%tolerance, 'Discrepancy in element ('//str(i)//', '//&
               str(j)//').' )
       end do
    end do

    call endSub( log )
  end subroutine perform_AssertExactlyEqual_$2_$3


  METHODDEF_ASSERTEQUALLINEAROPERAND_PERFORM_AEWP($1, $2, $3,
  {}, {1}, {2}, {row}, {
          row1 = obj%operand1(j, :)
          row2 = obj%operand2(i, :)})

  METHODDEF_ASSERTEQUALLINEAROPERAND_PERFORM_AEWP($1, $2, $3,
  {Transposed}, {2}, {1}, {column}, {
          column1 = obj%operand1(:, j)
          column2 = obj%operand2(:, i)})
  })
  
  
  METHODDEFS_ASSERTEQUALLINEAROPERAND({Matrix}, {A}, {Scalar})
  
  METHODDEFS_ASSERTEQUALLINEAROPERAND({Matrix}, {A}, {Vector})
  
  METHODDEFS_ASSERTEQUALLINEAROPERAND({NullSpace}, {Q2}, {Scalar})
  
  METHODDEFS_ASSERTEQUALLINEAROPERAND({NullSpace}, {Q2}, {Vector})
  
  METHODDEFS_ASSERTEQUALLINEAROPERAND({RHSColumn}, {B}, {Scalar})
  
  METHODDEFS_ASSERTEQUALLINEAROPERAND({RHSColumn}, {B}, {Vector})
  
  METHODDEFS_ASSERTEQUALLINEAROPERAND({SolutionColumn}, {X}, {Scalar})
  
  METHODDEFS_ASSERTEQUALLINEAROPERAND({SolutionColumn}, {X}, {Vector})
  
  
end module LinearOperandAssertions
