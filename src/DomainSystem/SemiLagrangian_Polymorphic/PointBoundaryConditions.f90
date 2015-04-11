module PointBoundaryConditionsModule
  
  use LogModule
  use Global
  use FlowFieldModule
  use SemiLagrangianModule
  use FiniteVolumeModule
  
  ! The following module commits the present module to appropriate
  ! implementations of collaborator interfaces.  For different
  ! implementations, use an alternative module implementing the same
  ! creation signatures.  The signatures are:
  !
  ! createZerothSpatialDerivative
  ! createFirstSpatialDerivative
  !
  use SpatialDerivativesModule
  
  implicit none
  private
  public :: createPointScalarBoundaryCondition, &
       createPointDirichletBoundaryCondition, &
       createPointNeumannBoundaryCondition, &
       createPointNoSlipBoundaryCondition
  
  character(*), parameter :: MOD_NAME = 'PointBoundaryConditionsModule'


  ! The following BC may represent a Dirichlet BC, in which case its
  ! supertype will hold a zeroth spatial derivative, or a Neumann BC,
  ! in which case the supertype will hold a first spatial derivative.
  ! In the latter case a Direction object is not stored, since the
  ! direction will be the face normal - computed as part of the
  ! geometry.
  
  type, extends(PointScalarBoundaryConditionInterface) :: PointScalarBoundaryConditionType
     private
     class(UndirectedSpatialDerivativeInterface), allocatable :: &
          undirectedSpatialDerivative
     real(FLOAT) :: value
   contains
     procedure :: init => init_Scalar
     procedure :: deinit => deinit_Scalar
     procedure :: clone => clone_Scalar
     procedure :: appendRows_Matrix => appendRows_Matrix_Scalar
     procedure :: appendRows_LinearSystem => &
          appendRows_LinearSystem_Scalar
     procedure :: getValue => getValue_Scalar
     procedure :: getValues => getValues_Scalar
  end type PointScalarBoundaryConditionType
  

  type, extends(PointVectorBoundaryConditionInterface) :: PointNoSlipBoundaryConditionType
     private
     class(UndirectedSpatialDerivativeInterface), allocatable :: &
          undirectedSpatialDerivative
     type(RealVectorType) :: value
   contains
     procedure :: init => init_NoSlip
     procedure :: deinit => deinit_NoSlip
     procedure :: clone => clone_NoSlip
     procedure :: appendRows_Matrix => appendRows_Matrix_NoSlip
     procedure :: appendRows_LinearSystem => &
          appendRows_LinearSystem_NoSlip
     procedure :: getValue => getValue_NoSlip
     procedure :: getValues => getValues_NoSlip
     procedure :: hasCoupledComponents => hasCoupledComponents_NoSlip
  end type PointNoSlipBoundaryConditionType

  
  
contains

  
  !-------------------------------------------------------------------
  !- PointScalarBoundaryCondition methods
  !-------------------------------------------------------------------
  
  subroutine createPointScalarBoundaryCondition( spbc, &
       scalarFlowVariable, undirectedSpatialDerivative, value, log )
    class(PointScalarBoundaryConditionInterface), allocatable, &
         intent(out) :: spbc
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(UndirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: undirectedSpatialDerivative
    real(FLOAT), intent(in) :: value
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createPointScalarBoundaryCondition', log )
    
    ! allocate
    if ( allocated(spbc) ) then
       call addEvent( FATAL, 'spbc already allocated.', log )
    else
       allocate( PointScalarBoundaryConditionType :: spbc, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &PointScalarBoundaryConditionType :: spbc.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( spbc )
    class is (PointScalarBoundaryConditionType)
       call spbc%init( scalarFlowVariable, undirectedSpatialDerivative, &
            value, log )
    end select
    call endSub( log )
  end subroutine createPointScalarBoundaryCondition
  
  
  ! creates a PointScalarBoundaryCondition with 1st spatial derivative
  subroutine createPointDirichletBoundaryCondition( spbc, &
       scalarFlowVariable, value, log )
    class(PointScalarBoundaryConditionInterface), allocatable, &
         intent(out) :: spbc
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    real(FLOAT), intent(in) :: value
    class(UndirectedSpatialDerivativeInterface), allocatable :: usd
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, &
         'createPointDirichletBoundaryCondition', log )
    
    call createZerothSpatialDerivative( usd, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call createPointScalarBoundaryCondition( spbc, scalarFlowVariable, &
         usd, value, log )
    
    call endSub( log )
  end subroutine createPointDirichletBoundaryCondition
  

  ! creates a PointScalarBoundaryCondition with 1st spatial derivative
  subroutine createPointNeumannBoundaryCondition( spbc, &
       scalarFlowVariable, value, log )
    class(PointScalarBoundaryConditionInterface), allocatable, &
         intent(out) :: spbc
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    real(FLOAT), intent(in) :: value
    class(UndirectedSpatialDerivativeInterface), allocatable :: usd
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createPointNeumannBoundaryCondition', &
         log )
    
    call createFirstSpatialDerivative( usd, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call createPointScalarBoundaryCondition( spbc, scalarFlowVariable, &
         usd, value, log )
  end subroutine createPointNeumannBoundaryCondition
  
  
  subroutine init_Scalar( obj, scalarFlowVariable, &
       undirectedSpatialDerivative, value, log )
    class(PointScalarBoundaryConditionType), intent(inout) :: obj
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(UndirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: undirectedSpatialDerivative
    real(FLOAT), intent(in) :: value
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_Scalar', log )

    obj%value = value

    call checkInjection( allocated(undirectedSpatialDerivative), &
         allocated(obj%undirectedSpatialDerivative), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( undirectedSpatialDerivative, obj%&
         undirectedSpatialDerivative )

    ! initialise the supertype
    call obj%initPointScalarBoundaryCondition( scalarFlowVariable, log )
    
    call endSub( log )
  end subroutine init_Scalar


  subroutine deinit_Scalar( obj, log )
    class(PointScalarBoundaryConditionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_Scalar', log )
    
    obj%value = 0._FLOAT
    
    ! deinitialise supertype
    call obj%deinitPointScalarBoundaryCondition( log )

    call endSub( log )
  end subroutine deinit_Scalar


  subroutine clone_Scalar( obj, tgt, log )
    class(PointScalarBoundaryConditionType), intent(in) :: obj
    class(PointScalarBoundaryConditionInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_Scalar', log )

    allocate( PointScalarBoundaryConditionType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &PointScalarBoundaryConditionType :: tgt.  STAT='//int2str(&
         stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    select type ( tgt )
    type is (PointScalarBoundaryConditionType)
       tgt%value = obj%value
    end select
    
    ! deinitialise supertype
    call tgt%initPointScalarBoundaryCondition( obj, log )
    
    call endSub( log )
  end subroutine clone_Scalar

  
  subroutine appendRows_Matrix_Scalar( obj, scalarMatrix, &
       boundaryStaticPointGroupPointer, log )
    class(PointScalarBoundaryConditionType), intent(in) :: obj
    class(ScalarMatrixInterface), intent(inout) :: &
         scalarMatrix
    class(BoundaryStaticPointGroupPointerType), intent(in) :: &
         boundaryStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_Matrix_Scalar', log )

    call scalarMatrix%setRows( boundaryStaticPointGroupPointer%&
         computeSubRows( obj%undirectedSpatialDerivative, &
         boundaryStaticPointGroupPointer%getNumPoints() ), log )
    
    call endSub( log )
  end subroutine appendRows_Matrix_Scalar

    
  subroutine appendRows_LinearSystem_Scalar( obj, scalarLinearSystem, &
       boundaryStaticPointGroupPointer, log )
    class(PointScalarBoundaryConditionType), intent(in) :: obj
    class(ScalarLinearSystemInterface), intent(inout) :: &
         scalarLinearSystem
    class(BoundaryStaticPointGroupPointerType), intent(in) :: &
         boundaryStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_LinearSystem_Scalar', log )

    call scalarLinearSystem%setMatrixRows( &
         boundaryStaticPointGroupPointer%computeSubRows( obj%&
         undirectedSpatialDerivative, boundaryStaticPointGroupPointer%&
         getNumPoints() ), log )

    call endSub( log )
  end subroutine appendRows_LinearSystem_Scalar

  
  elemental subroutine getValue_Scalar( obj, value )
    class(PointScalarBoundaryConditionType), intent(in) :: obj
    real(FLOAT), intent(out) :: value

    value = obj%value
  end subroutine getValue_Scalar

  
  pure subroutine getValues_Scalar( obj, values )
    class(PointScalarBoundaryConditionType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values

    values = obj%value
  end subroutine getValues_Scalar


  
  !-------------------------------------------------------------------
  !- PointNoSlipBoundaryCondition methods
  !-------------------------------------------------------------------

  subroutine createPointNoSlipBoundaryCondition( spbc, &
       vectorFlowVariable, value, log )
    class(PointVectorBoundaryConditionInterface), allocatable, intent(&
         out) :: spbc
    class(VectorFlowVariableInterface), target, intent(in) :: &
         vectorFlowVariable
    type(RealVectorType), intent(in) :: value
    class(LogType), intent(inout), optional :: log
    class(UndirectedSpatialDerivativeInterface), allocatable :: usd
    integer :: allocStat
    call beginSub( MOD_NAME, 'createPointNoSlipBoundaryCondition', log )
    
    ! allocate
    if ( allocated(spbc) ) then
       call addEvent( FATAL, 'spbc already allocated.', log )
    else
       allocate( PointNoSlipBoundaryConditionType :: spbc, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &PointNoSlipBoundaryConditionType :: spbc.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create helpers
    call createZerothSpatialDerivative( usd, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise
    select type ( spbc )
    class is (PointNoSlipBoundaryConditionType)
       call spbc%init( vectorFlowVariable, usd, value, log )
    end select
    call endSub( log )
  end subroutine createPointNoSlipBoundaryCondition


  subroutine init_NoSlip( obj, vectorFlowVariable, &
       undirectedSpatialDerivative, value, log )
    class(PointNoSlipBoundaryConditionType), intent(inout) :: obj
    class(VectorFlowVariableInterface), target, intent(in) :: &
         vectorFlowVariable
    class(UndirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: undirectedSpatialDerivative
    type(RealVectorType), intent(in) :: value
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_NoSlip', log )
    
    obj%value = value

    call checkInjection( allocated(undirectedSpatialDerivative), &
         allocated(obj%undirectedSpatialDerivative), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( undirectedSpatialDerivative, obj%&
         undirectedSpatialDerivative )

    ! initialise the supertype
    call obj%initPointVectorBoundaryCondition( vectorFlowVariable, log )
    
    call endSub( log )
  end subroutine init_NoSlip


  subroutine clone_NoSlip( obj, tgt, log )
    class(PointNoSlipBoundaryConditionType), intent(in) :: obj
    class(PointVectorBoundaryConditionInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_Scalar', log )

    allocate( PointNoSlipBoundaryConditionType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &PointNoSlipBoundaryConditionType :: tgt.  STAT='//int2str(&
         stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    select type ( tgt )
    type is (PointNoSlipBoundaryConditionType)
       tgt%value = obj%value
       call obj%undirectedSpatialDerivative%clone( &
            tgt%undirectedSpatialDerivative, log )
    end select
    
    ! deinitialise supertype
    call tgt%initPointVectorBoundaryCondition( obj, log )
    
    call endSub( log )
  end subroutine clone_NoSlip


  subroutine deinit_NoSlip( obj, log )
    class(PointNoSlipBoundaryConditionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_NoSlip', log )
    
    obj%value = 0._FLOAT

    ! deinitialise supertype
    call obj%deinitPointVectorBoundaryCondition( log )

    call endSub( log )
  end subroutine deinit_NoSlip
  
  
  subroutine appendRows_Matrix_NoSlip( obj, vectorMatrix, &
       boundaryStaticPointGroupPointer, log )
    class(PointNoSlipBoundaryConditionType), intent(in) :: obj
    class(VectorMatrixInterface), intent(inout) :: &
         vectorMatrix
    class(BoundaryStaticPointGroupPointerType), intent(in) :: &
         boundaryStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_Matrix_NoSlip', log )

    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine appendRows_Matrix_NoSlip

  
  subroutine appendRows_LinearSystem_NoSlip( obj, &
       vectorLinearSystem, boundaryStaticPointGroupPointer, log )
    class(PointNoSlipBoundaryConditionType), intent(in) :: obj
    class(VectorLinearSystemInterface), intent(inout) :: &
         vectorLinearSystem
    class(BoundaryStaticPointGroupPointerType), intent(in) :: &
         boundaryStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_LinearSystem_NoSlip', log )

    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine appendRows_LinearSystem_NoSlip


  elemental subroutine getValue_NoSlip( obj, value, componentIndex )
    class(PointNoSlipBoundaryConditionType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: componentIndex
    value = obj%value%getValue(componentIndex)
  end subroutine getValue_NoSlip


  pure subroutine getValues_NoSlip( obj, values, componentIndex )
    class(PointNoSlipBoundaryConditionType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: componentIndex
    values = obj%value%getValues()
  end subroutine getValues_NoSlip
  
  
  pure function hasCoupledComponents_NoSlip( obj )
    class(PointNoSlipBoundaryConditionType), intent(in) :: obj
    logical :: hasCoupledComponents_NoSlip
    hasCoupledComponents_NoSlip = .false.
  end function hasCoupledComponents_NoSlip

  
end module PointBoundaryConditionsModule
