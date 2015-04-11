HEADER()
module DomainModule
  
  use LogModule
  use Global
  use SemiLagrangianModule
  use FiniteVolumeModule
  use FlowFieldModule
  
  implicit none
  private
  public :: destroy
  
  character(*), parameter :: MOD_NAME = 'DomainModule'

  
  interface destroy
     module procedure destroyInteriorRegion
     module procedure destroyBoundaryRegion
  end interface
  
  !-------------------------------------------------------------------
  !- Regions
  !-------------------------------------------------------------------

  ! this used to be a concrete instance held by the module, but its
  ! mutable state turned out to be incompatible with pure procedures.
  ! So we let one of the clients allocate a concrete instance: the 
  ! pointer below then serves as a global point of access.
  type(IDManagerType) :: regionIDManager
  
  type, abstract, public :: RegionInterface
     integer :: ID
     type(IDManagerType), pointer :: IDManager
     character(20) :: name
   contains
     procedure(r_di), deferred :: deinit
     procedure :: initRegion
     procedure :: deinitRegion
     procedure :: sameAs => sameAs_Region
     procedure :: describe => describe_Region
  end type RegionInterface


  type, extends(RegionInterface), abstract, public :: InteriorRegionInterface
     private
   contains
     procedure(ir_di), deferred :: deinit
     procedure :: initInteriorRegion
     procedure :: deinitInteriorRegion
  end type InteriorRegionInterface

  
  type, extends(RegionInterface), abstract, public :: BoundaryRegionInterface
     private
     ! note that we have exactly one velocity BC but multiple scalar
     ! BCs
     class(PointVectorBoundaryConditionInterface), allocatable :: &
          pointVelocityBoundaryCondition
     type(PointScalarBoundaryConditionListType) :: &
          pointScalarBoundaryConditionList
   contains
     procedure(br_di), deferred :: deinit
     procedure(br_gfn), deferred :: getFaceNormals
     procedure :: initBoundaryRegion
     procedure :: deinitBoundaryRegion
     procedure :: getPointVelocityBoundaryCondition
     procedure :: getPointScalarBoundaryConditionList
     generic :: getBoundaryConditions => &
          getPointVelocityBoundaryCondition, &
          getPointScalarBoundaryConditionList
  end type BoundaryRegionInterface

  

  !-------------------------------------------------------------------
  !- abstract interfaces
  !-------------------------------------------------------------------

  
  ! Region signatures
  abstract interface
     subroutine r_di( obj, log )
       import RegionInterface, LogType
       class(RegionInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine r_di
  end interface
  
  ! InteriorRegion signatures
  abstract interface
     subroutine ir_di( obj, log )
       import InteriorRegionInterface, LogType
       class(InteriorRegionInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ir_di
  end interface
  
  ! BoundaryRegion signatures
  abstract interface
     subroutine br_di( obj, log )
       import BoundaryRegionInterface, LogType
       class(BoundaryRegionInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine br_di
     
!!$     subroutine br_gfn( obj, directions, &
!!$          boundaryStaticPointGroupPointer, log )
!!$       import BoundaryRegionInterface, DirectionInterface, &
!!$            BoundaryStaticPointGroupPointerType, LogType
!!$       class(BoundaryRegionInterface), intent(in) :: obj
!!$       class(DirectionInterface), dimension(:), allocatable, intent(&
!!$            out) :: directions
!!$       class(BoundaryStaticPointGroupPointerType), intent(in) :: &
!!$            boundaryStaticPointGroupPointer
!!$       class(LogType), intent(inout), optional :: log
!!$     end subroutine br_gfn
     
     pure subroutine br_gfn( obj, directions, &
          boundaryStaticPointCollection, polytopeIndex, log )
       import BoundaryRegionInterface, DirectionInterface, &
            BoundaryStaticPointCollectionInterface, LogType
       class(BoundaryRegionInterface), intent(in) :: obj
       class(DirectionInterface), dimension(:), allocatable, intent(&
            out) :: directions
       class(BoundaryStaticPointCollectionInterface), intent(in) :: &
            boundaryStaticPointCollection
       integer, intent(in) :: polytopeIndex
       class(LogType), intent(inout), optional :: log
     end subroutine br_gfn
  end interface

  
contains
  
  !-------------------------------------------------------------------
  !- Region methods
  !-------------------------------------------------------------------
  
  subroutine initRegion( obj, name, log )
    class(RegionInterface), intent(inout) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'initRegion', log )

!!$    ! initialise the global IDManager pointer system
!!$    if ( .not. associated(regionIDManagerPtr) ) then
!!$       allocate( obj%IDManager, stat=stat )
!!$       call addEvent( stat/=0, FATAL, 'Problem allocating obj%&
!!$            &IDManager.  STAT='//int2str(stat), log )
!!$       if ( checkSub(FATAL, log) ) then
!!$          call endSub( log )
!!$          return
!!$       end if
!!$       regionIDManagerPtr => obj%IDManager
!!$    else
!!$       obj%IDManager => regionIDManagerPtr
!!$    end if
       
    call regionIDManager%requestID( obj%ID, log )
    obj%name = name

    call endSub( log )
  end subroutine initRegion

  
  subroutine deinitRegion( obj, log )
    class(RegionInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitRegion', log )
    
    call regionIDManager%returnID( obj%ID )
    obj%name = ''

    ! deinitialise the global IDManager pointer system
    ! TK
       
    call endSub( log )
  end subroutine deinitRegion

 
  pure function describe_Region( obj )
    class(RegionInterface), intent(in) :: obj
    character(60) :: describe_Region
    describe_Region = trim(obj%name)//' (region '//str(obj%ID)//')'
  end function describe_Region

  
  pure function sameAs_Region( obj, r )
    class(RegionInterface), intent(in) :: obj
    class(RegionInterface), intent(in) :: r
    logical :: sameAs_Region
    sameAs_Region = ( obj%ID == r%ID )
  end function sameAs_Region

  

  !-------------------------------------------------------------------
  !- InteriorRegion methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY({InteriorRegion})})

  subroutine initInteriorRegion( obj, name, log )
    class(InteriorRegionInterface), intent(inout) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initInteriorRegion', log )

    call obj%initRegion( name, log )
    
    call endSub( log )
  end subroutine initInteriorRegion

  
  subroutine deinitInteriorRegion( obj, log )
    class(InteriorRegionInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitInteriorRegion', log )

    call obj%deinitRegion( log )

    call endSub( log )
  end subroutine deinitInteriorRegion

  

  !-------------------------------------------------------------------
  !- BoundaryRegion methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY({BoundaryRegion})})
  
  subroutine initBoundaryRegion( obj, name, &
       pointVelocityBoundaryCondition, &
       pointScalarBoundaryConditionList, log )
    class(BoundaryRegionInterface), intent(inout) :: obj
    character(*), intent(in) :: name
    class(PointVectorBoundaryConditionInterface), allocatable, &
         intent(inout) :: pointVelocityBoundaryCondition
    type(PointScalarBoundaryConditionListType), intent(inout) :: &
         pointScalarBoundaryConditionList
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initBoundaryRegion', log )

    EXPAND({INJECT({pointVelocityBoundaryCondition})})
    
    call obj%pointScalarBoundaryConditionList%takeNodes( &
         pointScalarBoundaryConditionList, log )
    call obj%initRegion( name, log )    
    
    call endSub( log )
  end subroutine initBoundaryRegion

  
  subroutine deinitBoundaryRegion( obj, log )
    class(BoundaryRegionInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitBoundaryRegion', log )
    
    call obj%deinitRegion( log )

    call destroy( obj%pointVelocityBoundaryCondition, log )
    call obj%pointScalarBoundaryConditionList%deinit( log )

    call endSub( log )
  end subroutine deinitBoundaryRegion


  subroutine getPointScalarBoundaryConditionList( obj, &
       pointScalarBoundaryConditionList )
    class(BoundaryRegionInterface), target, intent(in) :: obj
    type(PointScalarBoundaryConditionListType), pointer, &
         intent(out) :: pointScalarBoundaryConditionList
    pointScalarBoundaryConditionList => obj%&
         pointScalarBoundaryConditionList
  end subroutine getPointScalarBoundaryConditionList

  
  subroutine getPointVelocityBoundaryCondition( obj, &
       pointVelocityBoundaryCondition )
    class(BoundaryRegionInterface), target, intent(in) :: obj
    class(PointVectorBoundaryConditionInterface), pointer, &
         intent(out) :: pointVelocityBoundaryCondition
    pointVelocityBoundaryCondition => obj%&
         pointVelocityBoundaryCondition
  end subroutine getPointVelocityBoundaryCondition

  
end module DomainModule
