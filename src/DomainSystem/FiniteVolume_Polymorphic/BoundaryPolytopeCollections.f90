module BoundaryPolytopeCollections
  
  use FiniteVolumeModule
  use Global
  
  implicit none
  private 

  character(*), parameter :: MOD_NAME = 'BoundaryPolytopeCollections'


  !---------------------------------------------------------------------
  !- temporary
  !---------------------------------------------------------------------

  ! provisional interface so that BoundaryPolytopes can link to a
  ! BoundaryCondition
  type, abstract :: BoundaryCondition
     
  end type BoundaryCondition


  !---------------------------------------------------------------------
  !- top level public types
  !---------------------------------------------------------------------
  
  type, extends(PolytopeCollectionDecorator), abstract :: BoundaryPolytopeCollection
     private
     class(BoundaryCondition), pointer :: bc
     
  end type BoundaryPolytopeCollection


  !---------------------------------------------------------------------
  !- BoundaryPolytopeCollection implementations
  !---------------------------------------------------------------------
  

  
  !---------------------------------------------------------------------
  !- creator/destructor
  !---------------------------------------------------------------------

  type, public :: BoundaryPolytopeFactory
   contains
     private
     procedure, nopass :: createBoundaryPointArray
     procedure, nopass :: createBoundarySimplexArray
     procedure, nopass :: createBoundaryPointGrid
     procedure, nopass :: createBoundaryOrthotopeGrid
     generic, public :: create => createBoundaryPointArray, &
          createBoundarySimplexArray, createBoundaryPointGrid, &
          createBoundaryOrthotopeGrid
     procedure, nopass :: destroy => destroyBoundaryPolytopeCollection
  end type BoundaryPolytopeFactory


contains

  
  !---------------------------------------------------------------------
  !- BoundaryPolytopeFactory procedures
  !---------------------------------------------------------------------

  subroutine createBoundarySimplexArray( pc, polytopeDim, ppArray, bc, &
       error )
    class(PolytopeCollection), allocatable, intent(inout) :: pc
    integer, intent(in) :: polytopeDim
    type(PolytopePointer), dimension(:), intent(in) :: ppArray
    class(BoundaryCondition), intent(inout) :: bc
    type(ErrorComposite), intent(out) :: error
    type(ErrorComposite) :: subError
    integer :: allocStat
    call error%init( MOD_NAME, 'createBoundarySimplexArray' )

    allocate( BoundarySimplexArray :: pc, stat=allocStat )
    select type ( pc )
    type is (BoundarySimplexArray)
       call error%addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &BoundarySimplexArray type.  STAT='//int2str(allocStat) )
       if ( allocStat/=0 ) return

       ! after successully allocating the decorator, create the host
       ! component
       call factory%create( pc%pc, polytopeDim, ppArray, subError )
       call error%add( subError )
       if ( error%getSeverity()>=FATAL ) return
       
       ! now initialise the decorations
       call pc%init( bc, subError )
       call error%add( subError )
    end select
  end subroutine createBoundarySimplexArray


  ! TBC
  
  
  subroutine createBoundaryPointArray( pc, coords, bc, error )
    class(PolytopeCollection), allocatable, intent(inout) :: pc
    real(FLOAT), dimension(:), intent(in) :: coords
    class(BoundaryCondition), intent(inout) :: bc
    type(ErrorComposite), intent(out) :: error
    type(ErrorComposite) :: subError
    integer :: allocStat
    call error%init( MOD_NAME, 'createBoundaryPointArray' )

    allocate( BoundaryPointArray :: pc, stat=allocStat )
    select type ( pc )
    type is (BoundaryPointArray)
       call error%add( allocStat/=0, FATAL, 'Problem allocating &
            &BoundaryPointArray type.  STAT='//int2str(allocStat) )
       if ( allocStat/=0 ) return

       call pc%init( coords, subError )
       call error%add( subError )
    end select
  end subroutine createBoundaryPointArray
  

  subroutine createBoundaryOrthotopeGrid( pc, pcpArrayInt, &
       pcpArrayExt, bc, error )
    class(PolytopeGrid), allocatable, intent(inout) :: pc
    type(PolytopeCollectionPointer), dimension(NDIM), intent(in) :: &
         pcpArrayInt
    type(PolytopeCollectionPointer), dimension(NDIM*2), intent(in), &
         optional :: pcpArrayExt
    type(ErrorComposite), intent(out) :: error
    type(ErrorComposite) :: subError
    integer :: allocStat
    call error%init( MOD_NAME, 'createBoundaryOrthotopeGrid' )

    allocate( OrthotopeGrid :: pc, stat=allocStat )
    select type ( pc )
    type is (OrthotopeGrid)
       call error%add( allocStat/=0, FATAL, 'Problem allocating &
            &OrthotopeGrid type.  STAT='//int2str(allocStat) )
       if ( allocStat/=0 ) return
       
       call pc%init( pcpArrayInt, pcpArrayExt, error )
       call error%add( subError )
    end select
  end subroutine createBoundaryOrthotopeGrid
  

  subroutine createBoundaryPointGrid( pc, pointGridSize, minCoords, &
       maxCoords, bc, error )
    class(PolytopeCollection), allocatable, intent(inout) :: pc
    integer, dimension(NDIM), intent(in) :: pointGridSize
    real(FLOAT), dimension(NDIM), intent(in) :: minCoords, maxCoords
    class(BoundaryCondition), intent(inout) :: bc
    type(ErrorComposite), intent(out) :: error
    type(ErrorComposite) :: subError
    integer :: allocStat
    call error%init( MOD_NAME, 'createBoundaryPointGrid' )

    allocate( PointGrid :: pc, stat=allocStat )
    select type ( pc )
    type is (PointGrid)
       call error%add( allocStat/=0, FATAL, 'Problem allocating &
            &PointGrid type.  STAT='//int2str(allocStat) )
       if ( allocStat/=0 ) return

       call pc%init( pointGridSize, minCoords, maxCoords, subError )
       call error%add( subError )
    end select
  end subroutine createBoundaryPointGrid


  subroutine destroyBoundaryPolytopeCollection( pc, error )
    class(PolytopeCollection), allocatable, intent(inout) :: pc
    type(ErrorComposite), intent(out) :: error
    type(ErrorComposite) :: subError
    integer :: deallocStat
    call error%init( MOD_NAME, 'destroyPolytopeCollection' )
    if ( .not. allocated(pc) ) then
       call error%add( deallocStat/=0, WARNING, 'PolytopeCollection &
            &already deallocated.' )
    else
       call pc%deinit( subError )
       call error%addSub( subError )
       deallocate( pc, stat=deallocStat )
       call error%add( deallocStat/=0, WARNING, 'Problem deallocating &
            &PolytopeCollection.  STAT='//int2str(deallocStat) )
    end if
  end subroutine destroyPolytopeCollection
  
    
  
  
  
end module BoundaryPolytopeCollections
