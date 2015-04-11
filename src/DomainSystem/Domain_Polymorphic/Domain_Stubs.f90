module Domain_Stubs
  
  use LogModule
  use Global
  use DomainModule
  use FlowFieldModule
  use SemiLagrangianModule
  
  
  implicit none
  private
  public :: createInteriorRegion, createBoundaryRegion
  
  character(*), parameter :: MOD_NAME = 'Domain_Stubs'

  
  !-------------------------------------------------------------------
  !- Regions
  !-------------------------------------------------------------------
  
  
  type, extends(BoundaryRegionInterface), public :: BoundaryRegionStub
     private
   contains
     procedure :: init => init_BoundaryRegion
     procedure :: deinit => deinit_BoundaryRegion
     procedure :: getFaceNormals => getFaceNormals_BoundaryRegion
  end type BoundaryRegionStub

  
  type, extends(InteriorRegionInterface), public :: InteriorRegionStub
     private
   contains
     procedure :: init => init_InteriorRegion
     procedure :: deinit => deinit_InteriorRegion
  end type InteriorRegionStub
  
  
contains

  
  !-------------------------------------------------------------------
  !- InteriorRegion methods
  !-------------------------------------------------------------------
  
  subroutine createInteriorRegion( ir, name, log )
    class(InteriorRegionInterface), allocatable, intent(out) :: ir
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createInteriorRegion', log )
    
    ! allocate
    if ( allocated(ir) ) then
       call addEvent( FATAL, 'ir already allocated.', log )
    else
       allocate( InteriorRegionStub :: ir, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &InteriorRegionStub :: ir.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( ir )
    class is (InteriorRegionStub)
       call ir%init( name, log )
    end select
    call endSub( log )
  end subroutine createInteriorRegion

  
  subroutine init_InteriorRegion( obj, name, log )
    class(InteriorRegionStub), intent(inout) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_InteriorRegion', log )

    call obj%initInteriorRegion( name, log )
    
    call endSub( log )
  end subroutine init_InteriorRegion

  
  subroutine deinit_InteriorRegion( obj, log )
    class(InteriorRegionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_InteriorRegion', log )
    
    call obj%deinitInteriorRegion( log )

    call endSub( log )
  end subroutine deinit_InteriorRegion

  
  !-------------------------------------------------------------------
  !- BoundaryRegion methods
  !-------------------------------------------------------------------

  subroutine createBoundaryRegion( br, name, &
       pointVelocityBoundaryCondition, &
       pointScalarBoundaryConditionList, log )
    class(BoundaryRegionInterface), allocatable, intent(out) :: br
    character(*), intent(in) :: name
    class(PointVectorBoundaryConditionInterface), allocatable, &
         intent(inout) :: pointVelocityBoundaryCondition
    type(PointScalarBoundaryConditionListType), intent(inout) :: &
         pointScalarBoundaryConditionList
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createBoundaryRegion', log )
    
    ! allocate
    if ( allocated(br) ) then
       call addEvent( FATAL, 'br already allocated.', log )
    else
       allocate( BoundaryRegionStub :: br, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &BoundaryRegionStub :: br.  STAT='//int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( br )
    class is (BoundaryRegionStub)
       call br%init( name, pointVelocityBoundaryCondition, &
            pointScalarBoundaryConditionList, log )
    end select
    call endSub( log )
  end subroutine createBoundaryRegion

  
  subroutine init_BoundaryRegion( obj, name, &
       pointVelocityBoundaryCondition, &
       pointScalarBoundaryConditionList, log )
    class(BoundaryRegionStub), intent(inout) :: obj
    character(*), intent(in) :: name
    class(PointVectorBoundaryConditionInterface), allocatable, &
         intent(inout) :: pointVelocityBoundaryCondition
    type(PointScalarBoundaryConditionListType), intent(inout) :: &
         pointScalarBoundaryConditionList
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_BoundaryRegion', log )
    
    call obj%initBoundaryRegion( name, &
       pointVelocityBoundaryCondition, &
       pointScalarBoundaryConditionList, log )
    
    call endSub( log )
  end subroutine init_BoundaryRegion

  
  subroutine deinit_BoundaryRegion( obj, log )
    class(BoundaryRegionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_BoundaryRegion', log )
    
    call obj%deinitBoundaryRegion( log )

    call endSub( log )
  end subroutine deinit_BoundaryRegion

  
  pure subroutine getFaceNormals_BoundaryRegion( obj, directions, &
       boundaryStaticPointCollection, polytopeIndex, log )
    class(BoundaryRegionStub), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(&
         out) :: directions
    class(BoundaryStaticPointCollectionInterface), intent(in) :: &
         boundaryStaticPointCollection
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getFaceNormals_BoundaryRegion', &
         log )
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine getFaceNormals_BoundaryRegion

!!$  subroutine getFaceNormals_BoundaryRegion( obj, directions, &
!!$       boundaryStaticPointGroupPointer, log )
!!$    class(BoundaryRegionStub), intent(in) :: obj
!!$    class(DirectionInterface), dimension(:), allocatable, intent(&
!!$         out) :: directions
!!$    type(BoundaryStaticPointGroupPointerType), intent(in) :: &
!!$         boundaryStaticPointGroupPointer
!!$    class(LogType), intent(inout), optional :: log
!!$    call beginSub( MOD_NAME, 'getFaceNormals_BoundaryRegion', &
!!$         log )
!!$    call addEvent( WARNING, 'This stub method should be overridden.' )
!!$    
!!$    call endSub( log )
!!$  end subroutine getFaceNormals_BoundaryRegion

end module Domain_Stubs
