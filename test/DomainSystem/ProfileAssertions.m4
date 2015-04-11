module ProfileAssertions
  
  USE_MACROS({FiniteVolume})
  use LogModule
  use Global
  use TestUtilities
  use FlowFieldModule
  use FiniteVolumeModule
  
  implicit none
  private
  public :: createAssertEqualProfileComponent_Scalar, &
       createAssertEqualProfileComponent_Vector
  
  character(*), parameter :: MOD_NAME = 'ProfileAssertions'

  m4_define({TYPEDEF_ASSERTEQUALPROFILECOMPONENT_ABSTRACT}, {
  type, extends(Impure$1ProfileOperationInterface), abstract :: AssertEqual_$1Interface
     private
     class(Impure$1LinearOperationInterface), allocatable :: &
          impure$1LinearOperation
   contains
     procedure :: init => init_$1
     procedure :: deinit => deinit_$1
  end type AssertEqual_$1Interface
  })

  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_ABSTRACT({Scalar})
  
  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_ABSTRACT({Vector})
  
  
  m4_define({TYPEDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE}, {
  type, extends(AssertEqual_$2Interface) :: AssertEqual_$1_$2Type
     private
   contains
     procedure :: perform => perform_$1_$2
     m4_ifelse($1, {}, {}, {procedure :: visit$1ForOperand1 => &
          visit$1ForOperand1_$2})
     m4_ifelse($1, {}, {}, {procedure :: visit$1ForOperand2 => &
          visit$1ForOperand2_$2})
  end type AssertEqual_$1_$2Type
  })
  
  ! The following classes can be used to check data embedded in various
  ! linear system elements without having to implement explicit accessors.
  ! The classes will compare two such elements via the visit?ForOperand1
  ! and visit?ForOperand2 methods.  The visit?ForResult method is not
  ! needed.  Calling 'performImpure' triggers the assertion.
  
  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE({ExactlyConstr}, {Scalar})
  
  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE({LeastSquares}, {Scalar})
  
  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE({PolytpIntAve}, {Scalar})
  
  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE({Final}, {Scalar})

  
  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE({ExactlyConstr}, {Vector})
  
  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE({LeastSquares}, {Vector})
  
  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE({PolytpIntAve}, {Vector})
      
  TYPEDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE({Final}, {Vector})
  
contains

  m4_define({METHODDEFS_ASSERTEQUALPROFILECOMPONENT_ABSTRACT}, {
  subroutine init_$1( obj, impure$1LinearOperation, log )
    class(AssertEqual_$1Interface), intent(in) :: obj
    class(Impure$1LinearOperationInterface), allocatable, &
         intent(inout) :: impure$1LinearOperation
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_$1', log )

    EXPAND({INJECT({impure$1LinearOperation})})
    
    call endSub( log )
  end subroutine init_$1

  
  subroutine deinit_$1( obj, log )
    class(AssertEqual_$1Interface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinit_$1', log )

    call destroy( obj%impure$1LinearOperation, log )
    
    call endSub( log )
  end subroutine deinit_$1
  })

  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_ABSTRACT({Scalar})
  
  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_ABSTRACT({Vector})  

  
  m4_define({PROCEDURE_CREATE_ASSERTEQUALPROFILECOMPONENT_CONCRETE}, {
  subroutine createAssertEqualProfileComponent_$1( &
       impure$1ProfileOperation, componentName, &
       impure$1LinearOperation, log )
    class(Impure$1ProfileOperationInterface), allocatable, &
         intent(inout) :: impure$1ProfileOperation
    character(*), intent(in) :: componentName
    class(Impure$1LinearOperationInterface), allocatable, &
         intent(inout) :: impure$1LinearOperation
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createAssertEqualProfileComponent_$1', log )
    
    select case ( componentName )
    case ('ExactlyConstr')
       allocate( AssertEqual_ExactlyConstr_$1Type :: &
            impure$1ProfileOperation, stat=stat )
    case ('LeastSquares')
       allocate( AssertEqual_LeastSquares_$1Type :: &
            impure$1ProfileOperation, stat=stat )
    case ('PolytpIntAve')
       allocate( AssertEqual_PolytpIntAve_$1Type :: &
            impure$1ProfileOperation, stat=stat )
    case ('Final')
       allocate( AssertEqual_Final_$1Type :: &
            impure$1ProfileOperation, stat=stat )
    case default
       call addEvent( FATAL, 'componentName not recognised.  Can have &
            &''ExactlyConstr'', ''LeastSquares'', ''PolytpIntAve'', or &
            &''Final''; argument was '''//trim(componentName)//'''.', &
            log )
    end select
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &impure$1ProfileOperation.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    select type ( impure$1ProfileOperation )
    class is (AssertEqual_$1Interface)
       call impure$1ProfileOperation%init( impure$1LinearOperation, log )
    end select
    
    call endSub( log )
  end subroutine createAssertEqualProfileComponent_$1
  })
  
  PROCEDURE_CREATE_ASSERTEQUALPROFILECOMPONENT_CONCRETE({Scalar})
  
  PROCEDURE_CREATE_ASSERTEQUALPROFILECOMPONENT_CONCRETE({Vector})


  m4_define({METHODDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE_VISIT}, {
  subroutine visit$3ForOperand$4_$2( obj, &
       LWR($2)LinearOperand )
    class(AssertEqual_$1_$2Type), intent(inout) :: obj
    class($2LinearOperandInterface), intent(inout) :: &
         LWR($2)LinearOperand

    ! the following call is the second of two double dispatches - a bit of
    ! a headf***, unfortunately.  Pass the embedded LinearOperation object
    ! to the passed-in LinearOperand.
    call LWR($2)LinearOperand%acceptAsOperand$4( &
         obj%impure$2LinearOperation )
  end subroutine visit$3ForOperand$4_$2
  })
  
  m4_define({METHODDEFS_ASSERTEQUALPROFILECOMPONENT_CONCRETE}, {
  METHODDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE_VISIT({$1}, {$2},
  {$1}, {1})
  
  METHODDEF_ASSERTEQUALPROFILECOMPONENT_CONCRETE_VISIT({$1}, {$2},
  {$1}, {2})
  
  subroutine perform_$1_$2( obj, log )
    class(AssertEqual_$1_$2Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    logical, dimension(3) :: vacancies
    call beginSub( MOD_NAME, 'perform_$1_$2', log )
    
    call obj%impure$2LinearOperation%perform( log )
    
    call endSub( log )
  end subroutine perform_$1_$2})
  
  
  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_CONCRETE({ExactlyConstr}, {Scalar})
  
  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_CONCRETE({LeastSquares}, {Scalar})
  
  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_CONCRETE({PolytpIntAve}, {Scalar})
  
  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_CONCRETE({Final}, {Scalar})
  
  
  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_CONCRETE({ExactlyConstr}, {Vector})
  
  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_CONCRETE({LeastSquares}, {Vector})
  
  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_CONCRETE({PolytpIntAve}, {Vector})
  
  METHODDEFS_ASSERTEQUALPROFILECOMPONENT_CONCRETE({Final}, {Vector})
    
  
end module ProfileAssertions
