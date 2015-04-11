  
  !-------------------------------------------------------------------
  !- ProfileOperations
  !-------------------------------------------------------------------

  MACRO({METHODLIST_PROFILEOPERATION_CONCRETE}, {{
     m4_ifelse($2, {}, {}, {procedure :: visit$2ForOperand1 => &
          visit$2ForOperand1_$1})
     m4_ifelse($3, {}, {}, {procedure :: visit$3ForOperand2 => &
          visit$3ForOperand2_$1})
     m4_ifelse($4, {}, {}, {procedure :: visit$4ForOperand3 => &
          visit$4ForOperand3_$1})
     procedure :: perform => perform_$1
     procedure :: convert => convert_$1
     procedure :: setScaleFactor => setScaleFactor_$1
     }})
  
  MACRO({TYPEDEF_PROFILEOPERATION_ABSTRACT}, {{
  type, extends(Pure$1ProfileOperationInterface), abstract :: Abstract$1ProfileOperationType
     private
     class($1LinearSystemInterface), pointer :: LWR($1)LinearSystem
   contains
     procedure :: init => init_Abstract$1ProfileOperation
     procedure :: deinit => deinit_Abstract$1ProfileOperation
  end type Abstract$1ProfileOperationType
  }})
  
  
  MACRO({TYPEDEFS_APPENDREDUCEDSYSTEM}, {{
  type, extends(Abstract$2ProfileOperationType) :: Append$1Matrix_$2Type
     private
     type(A_Times_Q2_$2Type) :: A_times_Q2
   contains
     EXPAND({METHODLIST_PROFILEOPERATION_CONCRETE(
     {Append$1Matrix_$2},
     {$1}, {ExactlyConstr})})
  end type Append$1Matrix_$2Type
  
  
  type, extends(Abstract$2ProfileOperationType) :: Append$1RHS_$2Type
     private
     type(B_Minus_A_Times_X_$2Type) :: b_minus_A_times_x
   contains
     EXPAND({METHODLIST_PROFILEOPERATION_CONCRETE(
     {Append$1RHS_$2},
     {$1}, {$1}, {ExactlyConstr})})
  end type Append$1RHS_$2Type
  }})
  
  MACRO({TYPEDEF_APPENDFINALRHS}, {{
  type, extends(Abstract$1ProfileOperationType) :: AppendFinalRHS_$1Type
     private
     type(B_Minus_A_Times_X_$1Type) :: b_minus_A_times_x
   contains
     EXPAND({METHODLIST_PROFILEOPERATION_CONCRETE(
     {AppendFinalRHS_$1},
     {LeastSquares}, {LeastSquares}, {PolytpIntAve})})
  end type AppendFinalRHS_$1Type
  }})

  
  MACRO({TYPEDEF_FINALISE_PROFILE}, {{
  type, extends(Abstract$1ProfileOperationType) :: Finalise$2Profile_$1Type
     private
     type(X_Plus_Q2_Times_X_$1Type) :: x_plus_Q2_times_x
   contains
     EXPAND({METHODLIST_PROFILEOPERATION_CONCRETE(
     {Finalise$2Profile_$1},
     {$3}, {$3}, {$4})})
  end type Finalise$2Profile_$1Type
  }})

  
  !-------------------------------------------------------------------
  !- General ProfileOperation methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEF_PROFILEOPERATION_VISIT}, {{
  subroutine visit$3ForOperand$4_$2_$1( obj, LWR($1)LinearOperand )
    class($2_$1Type), intent(inout) :: obj
    class($1LinearOperandInterface), intent(inout) :: &
         LWR($1)LinearOperand
    
    ! the following call is the second of two double dispatches - a bit of
    ! a headf***, unfortunately.  Pass the embedded LinearOperation object
    ! to the passed-in LinearOperand.
    call LWR($1)LinearOperand%acceptAsOperand$4( obj%$5 )
  end subroutine visit$3ForOperand$4_$2_$1
  }})
  
  
  MACRO({METHODDEF_PROFILEOPERATION_PERFORM_APPEND_MATRIX}, {{
  pure subroutine perform_Append$2Matrix_$1( obj, log )
    class(Append$2Matrix_$1Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'perform_Append$2Matrix_$1', &
         log )

    ! store the result in the linear system associated with this object
    call obj%LWR($1)LinearSystem%setMatrixRows( obj%$3, log )

    call endSub( log )
  end subroutine perform_Append$2Matrix_$1
  }})
  
  
  MACRO({METHODDEF_PROFILEOPERATION_PERFORM_APPEND_RHS}, {{
  pure subroutine perform_Append$2RHS_$1( obj, log )
    class(Append$2RHS_$1Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    type($1LinOpBasedRHSGeneratorType) :: rhsg
    call beginSub( MOD_NAME, 'perform_Append$2RHS_$1', &
         log )

    ! store the result in the linear system associated with this object
    call rhsg%init( obj%$3 )
    call obj%LWR($1)LinearSystem%setRHSElements( rhsg, log )

    call endSub( log )
  end subroutine perform_Append$2RHS_$1
  }})

  MACRO({METHODDEF_PROFILEOPERATION_PERFORM_FINALISE_PROFILE}, {{
  pure subroutine perform_Finalise$2Profile_$1( obj, log )
    class(Finalise$2Profile_$1Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'perform_Finalise$2Profile_$1', &
         log )

    ! store the result in the linear system associated with this object
    call obj%LWR($1)LinearSystem%setSolutionElements( obj%$3, log )

    call endSub( log )
  end subroutine perform_Finalise$2Profile_$1
  }})


  MACRO({METHODDEF_PROFILEOPERATION_CONVERT}, {{
  pure subroutine convert_$2_$1( obj, &
       pure$1LinearOperation, log )
    class($2_$1Type), intent(inout) :: obj
    class(Pure$1LinearOperationInterface), allocatable, intent(inout) :: &
         pure$1LinearOperation
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'convert_$2_$1', &
         log )

    call obj%$3%clone( pure$1LinearOperation, log )

    call endSub( log )
  end subroutine convert_$2_$1
  }})

  MACRO({METHODDEF_PROFILEOPERATION_SETSCALEFACTOR}, {{
  pure subroutine setScaleFactor_$2_$1( obj, value )
    class($2_$1Type), intent(inout) :: obj
    real(FLOAT), intent(in) :: value
    call obj%$3%setScaleFactor( value )
  end subroutine setScaleFactor_$2_$1
  }})

  

  !-------------------------------------------------------------------
  !- Abstract$1ProfileOperation methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_PROFILEOPERATION_ABSTRACT}, {{
  subroutine init_Abstract$1ProfileOperation( obj, LWR($1)LinearSystem, &
       log )
    class(Abstract$1ProfileOperationType), intent(inout) :: obj
    class($1LinearSystemInterface), target, intent(in) :: &
         LWR($1)LinearSystem
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_Abstract$1ProfileOperation', log )

    obj%LWR($1)LinearSystem => LWR($1)LinearSystem
    
    call endSub( log )
  end subroutine init_Abstract$1ProfileOperation

  
  subroutine deinit_Abstract$1ProfileOperation( obj, log )
    class(Abstract$1ProfileOperationType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_Abstract$1ProfileOperation', log )

    nullify( obj%LWR($1)LinearSystem )
    
    call endSub( log )
  end subroutine deinit_Abstract$1ProfileOperation
  }})

    
  MACRO({METHODDEFS_APPENDREDUCEDSYSTEM_POLYTPINTAVE}, {{
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendPolytpIntAveMatrix}, {PolytpIntAve}, {1},
  {A_times_Q2})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendPolytpIntAveMatrix}, {ExactlyConstr}, {2},
  {A_times_Q2})})

  EXPAND({METHODDEF_PROFILEOPERATION_PERFORM_APPEND_MATRIX($1, 
  {PolytpIntAve}, {A_times_Q2})})

  EXPAND({METHODDEF_PROFILEOPERATION_CONVERT($1,
  {AppendPolytpIntAveMatrix}, {A_times_Q2})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_SETSCALEFACTOR($1,
  {AppendPolytpIntAveMatrix}, {A_times_Q2})})
  
  subroutine visitPolytpIntAveForOperand1_AppendPolytpIntAveRHS_$1( &
       obj, LWR($1)LinearOperand )
    class(AppendPolytpIntAveRHS_$1Type), intent(inout) :: obj
    class($1LinearOperandInterface), intent(inout) :: &
         LWR($1)LinearOperand

    ! Here is an anomaly compared to sibling methods.  As before, we want
    ! to execute bpia - Apia*x1exact, storing the result in the RHS of the
    ! linear system associated with this object.  However, bpia is taken
    ! from this ASSOCIATED system, not the passed in system.  The argument
    ! is ignored.  When this family of methods is repeatedly called, we
    ! should get the result bpia - sum( Apia_k * x1exact_k, k=1,n ), where
    ! k is the index to a primitive profile.
    call obj%LWR($1)LinearSystem%acceptAsOperand1( obj%b_minus_A_times_x )
  end subroutine visitPolytpIntAveForOperand1_AppendPolytpIntAveRHS_$1
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendPolytpIntAveRHS}, {PolytpIntAve}, {2},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendPolytpIntAveRHS}, {ExactlyConstr}, {3},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_PERFORM_APPEND_RHS($1, {PolytpIntAve},
  {b_minus_A_times_x})})

  EXPAND({METHODDEF_PROFILEOPERATION_CONVERT($1,
  {AppendPolytpIntAveRHS}, {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_SETSCALEFACTOR($1,
  {AppendPolytpIntAveRHS}, {b_minus_A_times_x})})
  }})
    

  MACRO({METHODDEFS_APPENDREDUCEDSYSTEM_LEASTSQUARES}, {{
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendLeastSquaresMatrix}, {LeastSquares}, {1},
  {A_times_Q2})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendLeastSquaresMatrix}, {ExactlyConstr}, {2},
  {A_times_Q2})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_PERFORM_APPEND_MATRIX($1, 
  {LeastSquares}, {A_times_Q2})})

  EXPAND({METHODDEF_PROFILEOPERATION_CONVERT($1,
  {AppendLeastSquaresMatrix}, {A_times_Q2})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_SETSCALEFACTOR($1,
  {AppendLeastSquaresMatrix}, {A_times_Q2})})

  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendLeastSquaresRHS}, {LeastSquares}, {1},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendLeastSquaresRHS}, {LeastSquares}, {2},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendLeastSquaresRHS}, {ExactlyConstr}, {3},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_PERFORM_APPEND_RHS($1, {LeastSquares},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_CONVERT($1,
  {AppendLeastSquaresRHS}, {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_SETSCALEFACTOR($1,
  {AppendLeastSquaresRHS}, {b_minus_A_times_x})})
  }})

  
  MACRO({METHODDEFS_APPENDFINALRHS}, {{
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendFinalRHS}, {LeastSquares}, {1},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendFinalRHS}, {LeastSquares}, {2},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {AppendFinalRHS}, {PolytpIntAve}, {3},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_PERFORM_APPEND_RHS($1, {Final},
  {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_CONVERT($1,
  {AppendFinalRHS}, {b_minus_A_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_SETSCALEFACTOR($1,
  {AppendFinalRHS}, {b_minus_A_times_x})})
  }})
  
  
  MACRO({METHODDEFS_FINALISE_PROFILE}, {{
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {Finalise$2Profile}, {$3}, {1},
  {x_plus_Q2_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1,
  {Finalise$2Profile}, {$3}, {2},
  {x_plus_Q2_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_VISIT($1, 
  {Finalise$2Profile}, {$4}, {3},
  {x_plus_Q2_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_PERFORM_FINALISE_PROFILE($1,
  {$2}, {x_plus_Q2_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_CONVERT($1, 
  {Finalise$2Profile}, {x_plus_Q2_times_x})})
  
  EXPAND({METHODDEF_PROFILEOPERATION_SETSCALEFACTOR($1, 
  {Finalise$2Profile}, {x_plus_Q2_times_x})})
  }})
