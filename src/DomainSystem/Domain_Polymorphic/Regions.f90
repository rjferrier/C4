module RegionsModule
  
  use LogModule
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FlowFieldModule
  use DomainModule
  
  implicit none
  private
  public :: createFlatBoundaryRegion, createInteriorRegion
  
  character(*), parameter :: MOD_NAME = 'RegionsModule'

  
  type, extends(BoundaryRegionInterface) :: FlatBoundaryRegionType
     private
     class(DirectionInterface), allocatable :: faceNormal
   contains
     procedure :: init => init_FlatBoundaryRegion
     procedure :: deinit => deinit_FlatBoundaryRegion
     procedure :: getFaceNormals => getFaceNormals_FlatBoundaryRegion
  end type FlatBoundaryRegionType

  
  type, extends(InteriorRegionInterface) :: InteriorRegionType
     private
   contains
     procedure :: init => init_InteriorRegion
     procedure :: deinit => deinit_InteriorRegion

  end type InteriorRegionType

  
  
contains

  
  !-------------------------------------------------------------------
  !- FlatBoundaryRegion methods
  !-------------------------------------------------------------------
  

  subroutine createFlatBoundaryRegion( br, name, &
       pointVelocityBoundaryCondition, &
       pointScalarBoundaryConditionList, faceNormal, log )
    class(BoundaryRegionInterface), allocatable, intent(out) :: br
    character(*), intent(in) :: name
    class(PointVectorBoundaryConditionInterface), allocatable, &
         intent(inout) :: pointVelocityBoundaryCondition
    type(PointScalarBoundaryConditionListType), intent(inout) :: &
         pointScalarBoundaryConditionList
    class(DirectionInterface), allocatable, intent(inout) :: &
         faceNormal
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createFlatBoundaryRegion', log )
    
    ! allocate
    if ( allocated(br) ) then
       call addEvent( FATAL, 'br already allocated.', log )
    else
       allocate( FlatBoundaryRegionType :: br, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &FlatBoundaryRegionType :: br.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( br )
    class is (FlatBoundaryRegionType)
       call br%init( name, pointVelocityBoundaryCondition, &
            pointScalarBoundaryConditionList, faceNormal, log )
    end select
    call endSub( log )
  end subroutine createFlatBoundaryRegion

  
  subroutine init_FlatBoundaryRegion( obj, name, &
       pointVelocityBoundaryCondition, &
       pointScalarBoundaryConditionList, faceNormal, log )
    class(FlatBoundaryRegionType), intent(inout) :: obj
    character(*), intent(in) :: name
    class(PointVectorBoundaryConditionInterface), allocatable, &
         intent(inout) :: pointVelocityBoundaryCondition
    type(PointScalarBoundaryConditionListType), intent(inout) :: &
         pointScalarBoundaryConditionList
    class(DirectionInterface), allocatable, intent(inout) :: &
         faceNormal
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_FlatBoundaryRegion', log )

    call checkInjection( allocated(faceNormal), allocated(obj%&
         faceNormal), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( faceNormal, obj%faceNormal )
    
    call obj%initBoundaryRegion( name, &
       pointVelocityBoundaryCondition, &
       pointScalarBoundaryConditionList, log )
    
    call endSub( log )
  end subroutine init_FlatBoundaryRegion

    
  subroutine deinit_FlatBoundaryRegion( obj, log )
    class(FlatBoundaryRegionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_FlatBoundaryRegion', log )

    call obj%deinitBoundaryRegion( log )

    call destroy( obj%faceNormal, log )

    call endSub( log )
  end subroutine deinit_FlatBoundaryRegion


  pure subroutine getFaceNormals_FlatBoundaryRegion( obj, directions, &
       boundaryStaticPointCollection, polytopeIndex, log )
    class(FlatBoundaryRegionType), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(&
         out) :: directions
    class(BoundaryStaticPointCollectionInterface), intent(in) :: &
         boundaryStaticPointCollection
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getFaceNormals_FlatBoundaryRegion', &
         log )

    call obj%faceNormal%clone( directions, &
         boundaryStaticPointCollection%getNumPointsPerPolytope(), log )
    
    call endSub( log )
  end subroutine getFaceNormals_FlatBoundaryRegion

  

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
       allocate( InteriorRegionType :: ir, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &InteriorRegionType :: ir.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( ir )
    class is (InteriorRegionType)
       call ir%init( name, log )
    end select
    call endSub( log )
  end subroutine createInteriorRegion

  
  subroutine init_InteriorRegion( obj, name, log )
    class(InteriorRegionType), intent(inout) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_InteriorRegion', log )

    call obj%initInteriorRegion( name, log )
    
    call endSub( log )
  end subroutine init_InteriorRegion

    
  subroutine deinit_InteriorRegion( obj, log )
    class(InteriorRegionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_InteriorRegion', log )

    call obj%deinitInteriorRegion( log )

    call endSub( log )
  end subroutine deinit_InteriorRegion



end module RegionsModule
