
  !-------------------------------------------------------------------
  !- LinearOperands
  !-------------------------------------------------------------------

  MACRO({METHODLIST_LINEAROPERAND_CONCRETE}, {{
     procedure :: acceptAsOperand1 => &
          acceptAsOperand1_$1
     procedure :: acceptAsOperand2 => &
          acceptAsOperand2_$1
     procedure :: acceptAsOperand3 => &
          acceptAsOperand3_$1
     }})

     
  !-------------------------------------------------------------------
  !- MultiRowMatrix methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEF_MULTIROWMATRIX_ACCEPT}, {{
  subroutine acceptAsOperand$2_$1MultiRowMatrix( obj, &
       LWR($1LinearOperation) )
    class($1MultiRowMatrixType), intent(inout), target :: obj
    class($1LinearOperationInterface), intent(inout) :: &
         LWR($1LinearOperation)
    ! delegate to strategy
    call obj%operandBehaviour%acceptAsOperand$2( LWR($1LinearOperation), &
         obj, [obj%rowCounter+1, obj%columnCounter+1] )
  end subroutine acceptAsOperand$2_$1MultiRowMatrix
  }})
  
  MACRO({METHODDEFS_MULTIROWMATRIX_ACCEPT}, {{
  EXPAND({METHODDEF_MULTIROWMATRIX_ACCEPT($1, {1})})
  EXPAND({METHODDEF_MULTIROWMATRIX_ACCEPT($1, {2})})
  EXPAND({METHODDEF_MULTIROWMATRIX_ACCEPT($1, {3})})
  }})
  
 
  !-------------------------------------------------------------------
  !- SingleRowMatrix methods
  !-------------------------------------------------------------------
      
  MACRO({METHODDEF_SINGLEROWMATRIX_ACCEPT}, {{
  subroutine acceptAsOperand$2_$1SingleRowMatrix( obj, &
       LWR($1LinearOperation) )
    class($1SingleRowMatrixType), intent(inout), target :: obj
    class($1LinearOperationInterface), intent(inout) :: &
         LWR($1LinearOperation)
    ! delegate to strategy
    call obj%operandBehaviour%acceptAsOperand$2( LWR($1LinearOperation), &
         obj, [1, obj%columnCounter+1] )
  end subroutine acceptAsOperand$2_$1SingleRowMatrix
  }})
  
  MACRO({METHODDEFS_SINGLEROWMATRIX_ACCEPT}, {{
  EXPAND({METHODDEF_SINGLEROWMATRIX_ACCEPT($1, {1})})
  EXPAND({METHODDEF_SINGLEROWMATRIX_ACCEPT($1, {2})})
  EXPAND({METHODDEF_SINGLEROWMATRIX_ACCEPT($1, {3})})
  }})
  
 
  !-------------------------------------------------------------------
  !- ScalarMatrix OperandBehaviour methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEF_MATRIX_OPERANDBEHAVIOUR_ACCEPT}, {{
  subroutine acceptAsOperand$2_$1Matrix_$3Behaviour( obj, &
       LWR($1LinearOperation), LWR($1Matrix), from )
    class($1Matrix_$3BehaviourType), intent(in) :: obj
    class($1LinearOperationInterface), intent(inout) :: &
         LWR($1LinearOperation)
    class($1MatrixType), target, intent(inout) :: &
         LWR($1Matrix)
    integer, dimension(2), intent(in) :: from
    call LWR($1LinearOperation)%visitMatrixForOperand$2( &
         LWR($1Matrix)%rows $4 )
    if ( allocated(LWR($1Matrix)%Q2) ) &
         call LWR($1LinearOperation)%visitNullSpaceForOperand$2( &
         LWR($1Matrix)%Q2 $4 )
  end subroutine acceptAsOperand$2_$1Matrix_$3Behaviour
  }})


  MACRO({METHODDEFS_MATRIX_OPERANDBEHAVIOUR}, {{
  EXPAND({METHODDEF_MATRIX_OPERANDBEHAVIOUR_ACCEPT($1, {1}, 
    {SequentialSection}, {, from})})
  EXPAND({METHODDEF_MATRIX_OPERANDBEHAVIOUR_ACCEPT($1, {2}, 
    {SequentialSection}, {, from})})
  EXPAND({METHODDEF_MATRIX_OPERANDBEHAVIOUR_ACCEPT($1, {3}, 
    {SequentialSection}, {, from})})
  EXPAND({METHODDEF_MATRIX_OPERANDBEHAVIOUR_ACCEPT($1, {1}, 
    {WholeArray})})
  EXPAND({METHODDEF_MATRIX_OPERANDBEHAVIOUR_ACCEPT($1, {2}, 
    {WholeArray})})
  EXPAND({METHODDEF_MATRIX_OPERANDBEHAVIOUR_ACCEPT($1, {3}, 
    {WholeArray})})
  }})  
  

  !-------------------------------------------------------------------
  !- RHSColumn methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEF_CONCRETERHSCOLUMN_ACCEPT}, {{
  subroutine acceptAsOperand$2_$1RHSColumn( obj, &
       LWR($1LinearOperation) )
    class($1RHSColumnType), intent(inout), target :: obj
    class($1LinearOperationInterface), intent(inout) :: &
         LWR($1LinearOperation)
    ! delegate to strategy
    call obj%operandBehaviour%acceptAsOperand$2( LWR($1LinearOperation), &
         obj, [obj%elementCounter+1, 1] )
  end subroutine acceptAsOperand$2_$1RHSColumn
  }})
  
  MACRO({METHODDEFS_CONCRETERHSCOLUMN_ACCEPT}, {{
  EXPAND({METHODDEF_CONCRETERHSCOLUMN_ACCEPT($1, {1})})
  EXPAND({METHODDEF_CONCRETERHSCOLUMN_ACCEPT($1, {2})})
  EXPAND({METHODDEF_CONCRETERHSCOLUMN_ACCEPT($1, {3})})
  }})


 
  !-------------------------------------------------------------------
  !- RHSColumn OperandBehaviour methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEF_RHSCOLUMN_OPERANDBEHAVIOUR_ACCEPT}, {{
  subroutine acceptAsOperand$2_$1RHSColumn_$3Behaviour( obj, &
       LWR($1LinearOperation), LWR($1RHSColumn), from )
    class($1RHSColumn_$3BehaviourType), intent(in) :: obj
    class($1LinearOperationInterface), intent(inout) :: &
         LWR($1LinearOperation)
    class($1RHSColumnType), target, intent(inout) :: LWR($1RHSColumn)
    integer, dimension(2), intent(in) :: from
    call LWR($1LinearOperation)%visitRHSColumnForOperand$2( &
         LWR($1RHSColumn)%realValues $4 )
  end subroutine acceptAsOperand$2_$1RHSColumn_$3Behaviour
  }})

  MACRO({METHODDEFS_RHSCOLUMN_OPERANDBEHAVIOUR}, {{
  EXPAND({METHODDEF_RHSCOLUMN_OPERANDBEHAVIOUR_ACCEPT($1, {1}, 
    {SequentialSection}, {, from})})
  EXPAND({METHODDEF_RHSCOLUMN_OPERANDBEHAVIOUR_ACCEPT($1, {2}, 
    {SequentialSection}, {, from})})
  EXPAND({METHODDEF_RHSCOLUMN_OPERANDBEHAVIOUR_ACCEPT($1, {3}, 
    {SequentialSection}, {, from})})
  EXPAND({METHODDEF_RHSCOLUMN_OPERANDBEHAVIOUR_ACCEPT($1, {1}, 
    {WholeArray})})
  EXPAND({METHODDEF_RHSCOLUMN_OPERANDBEHAVIOUR_ACCEPT($1, {2}, 
    {WholeArray})})
  EXPAND({METHODDEF_RHSCOLUMN_OPERANDBEHAVIOUR_ACCEPT($1, {3}, 
    {WholeArray})})
  }})

  
  !-------------------------------------------------------------------
  !- LinearSystem methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEF_CONCRETELINEARSYSTEM_ACCEPT}, {{
  subroutine acceptAsOperand$2_$1LinearSystem( obj, &
       LWR($1)LinearOperation )
    class($1LinearSystemType), intent(inout), target :: obj
    class($1LinearOperationInterface), intent(inout) :: &
         LWR($1)LinearOperation $3
    
    ! descend through matrix and RHS
    call obj%LWR($1Matrix)%acceptAsOperand$2( LWR($1)LinearOperation )
    call obj%LWR($1RHSColumn)%acceptAsOperand$2( LWR($1)LinearOperation )

    ! also visit the solution column, and update counters
    call scalarLinearOperation%visitSolutionColumnForOperand$2( obj%&
         solution, [obj%solutionElementCounter + 1, 1] )
    obj%visitationCounter = obj%visitationCounter + 1
    obj%solutionElementCounter = obj%solutionElementCounter + obj%&
         LWR($1RHSColumn)%getNumElements(obj%visitationCounter)
    $4
  end subroutine acceptAsOperand$2_$1LinearSystem
  }})
  
 
  !-------------------------------------------------------------------
  !- LinearSystem OperandBehaviour methods
  !-------------------------------------------------------------------

  ! [redundant?  Consider deleting]
  
  MACRO({METHODDEF_LINEARSYSTEM_SEQUENTIALSECTIONBEHAVIOUR_ACCEPT}, {{
  subroutine acceptAs$2_$1LinearSystem_SequentialSectionBehaviour( obj, &
       LWR($1LinearOperation), values, from )
    class($1LinearSystem_SequentialSectionBehaviourType), target, intent(&
         inout) :: obj
    class($1LinearOperationInterface), intent(inout) :: &
         LWR($1LinearOperation)
    real(FLOAT), dimension(:, :), intent(in) :: values
    integer, dimension(2), intent(in) :: from
    call LWR($1LinearOperation)%visitLinearSystemFor$2( values, from )
    call LWR($1LinearOperation)%visitNullSpaceFor$2( values, from )
  end subroutine acceptAs$2_$1LinearSystem_SequentialSectionBehaviour
  }})

  MACRO({METHODDEF_LINEARSYSTEM_WHOLEARRAYBEHAVIOUR_ACCEPT}, {{
  subroutine acceptAs$2_$1LinearSystem_WholeArrayBehaviour( obj, &
       LWR($1LinearOperation), values, from )
    class($1LinearSystem_WholeArrayBehaviourType), target, intent(&
         inout) :: obj
    class($1LinearOperationInterface), intent(inout) :: &
         LWR($1LinearOperation)
    real(FLOAT), dimension(:, :), intent(in) :: values
    integer, dimension(2), intent(in) :: from
    call LWR($1LinearOperation)%visitLinearSystemFor$2( values )
    call LWR($1LinearOperation)%visitNullSpaceFor$2( values )
  end subroutine acceptAs$2_$1LinearSystem_WholeArrayBehaviour
  }})
