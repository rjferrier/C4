module SemiLagrangian_Stubs
  
  use LogModule
  use Global
  use SemiLagrangianModule
  use FlowFieldModule
  use FlowField_Stubs
  
  implicit none
  private
  public :: createDeparturePointCollectionStub
  
  character(*), parameter :: MOD_NAME = 'SemiLagrangian_Stubs'

  type, extends(DeparturePointCollectionInterface) :: DeparturePointCollectionStub
     private
   contains
     procedure :: init => init_Departure
     procedure :: deinit => deinit_Departure
     procedure :: appendScalarMoments => &
          appendScalarMoments_Departure
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_Departure
     generic :: appendMoments => appendScalarMoments, &
          appendVelocityMoments
     procedure :: computeSubRows => &
          computeSubRows_Departure
     procedure :: initialiseScalarFields => &
          initialiseScalarFields_Departure
     procedure :: initialiseVectorFields => &
          initialiseVectorFields_Departure
  end type DeparturePointCollectionStub
  
contains


  subroutine createDeparturePointCollectionStub( &
       departurePointCollection, log )
    class(DeparturePointCollectionInterface), allocatable, intent(&
         inout) :: departurePointCollection
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createDeparturePointCollectionStub', log )
    
    ! allocate
    if ( allocated(departurePointCollection) ) then
       call addEvent( FATAL, 'departurePointCollection already &
            &allocated.', log )
    else
       allocate( DeparturePointCollectionStub :: &
            departurePointCollection, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &DeparturePointCollectionStub :: departurePointCollection.  &
            &STAT='//int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( departurePointCollection )
    class is (DeparturePointCollectionStub)
       call departurePointCollection%init( log )
    end select
    call endSub( log )
  end subroutine createDeparturePointCollectionStub

  
  subroutine init_Departure( obj, log )
    class(DeparturePointCollectionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_Departure', log )
    
    call endSub( log )
  end subroutine init_Departure

  
  subroutine deinit_Departure( obj, log )
    class(DeparturePointCollectionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_Departure', log )
    
    call endSub( log )
  end subroutine deinit_Departure
  

  subroutine appendScalarMoments_Departure( obj, scalarMomentGroupList, &
       polytopeIndex, log )
    class(DeparturePointCollectionStub), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    type(PointScalarMomentFieldListIteratorType) :: iterator
    class(PointScalarMomentFieldInterface), pointer :: psmf
    class(ScalarMomentGroupInterface), allocatable :: smg
    class(ScalarFlowVariableInterface), pointer :: fv, fvTgt
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, &
         'appendScalarMoments_Departure', log )
    
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine appendScalarMoments_Departure

  
  subroutine appendVelocityMoments_Departure( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class(DeparturePointCollectionStub), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    type(PointVectorMomentFieldListIteratorType) :: iterator
    class(PointVectorMomentFieldInterface), pointer :: pvmf
    class(VectorMomentGroupInterface), allocatable :: vmg
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_Departure', log )
    
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendVelocityMoments_Departure

  
  pure function computeSubRows_Departure( obj, &
       polytopeIndex, directedSpatialDerivative ) result ( row )
    class(DeparturePointCollectionStub), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    real(FLOAT), dimension(1, NCOEFS_ADV) :: row
    integer :: n
    
    row = 0._FLOAT    
  end function computeSubRows_Departure
  

  subroutine initialiseScalarFields_Departure( obj, &
       scalarFieldSpecifications, log )
    class(DeparturePointCollectionStub), intent(inout) :: obj
    type(ScalarFieldSpecificationListType), intent(in) :: &
         scalarFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'initialiseScalarFields_Departure', log )

    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine initialiseScalarFields_Departure


  subroutine initialiseVectorFields_Departure( obj, vectorFieldSpecifications, log )
    class(DeparturePointCollectionStub), intent(inout) :: obj
    type(VectorFieldSpecificationListType), intent(in) :: &
         vectorFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'initialiseVectorFields_Departure', log )
    
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine initialiseVectorFields_Departure


end module SemiLagrangian_Stubs
