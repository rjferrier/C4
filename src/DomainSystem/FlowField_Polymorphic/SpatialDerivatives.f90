module SpatialDerivativesModule
  
  use LogModule
  use Global
  use FlowFieldModule
  
  ! The following module commits the present module to appropriate
  ! implementations of collaborator interfaces.  For different
  ! implementations, use an alternative module implementing the same
  ! creation signatures.  The signatures are:
  !
  ! createCartesianDirection
  ! createObliqueDirection
  !
  use DirectionsModule
  
  implicit none
  private
  public :: createZerothSpatialDerivative, &
       createFirstSpatialDerivative, spreadCoords, spreadPowers
  
  character(*), parameter :: MOD_NAME = 'SpatialDerivativesModule'


  interface createZerothSpatialDerivative
     module procedure createUndirectedZerothSpatialDerivative
     module procedure createDirectedZerothSpatialDerivative
  end interface

  interface createFirstSpatialDerivative
     module procedure createUndirectedFirstSpatialDerivative
     module procedure createCartesianFirstSpatialDerivative
     module procedure createObliqueFirstSpatialDerivative
  end interface

  
  !-------------------------------------------------------------------
  !- main types
  !-------------------------------------------------------------------

  ! Here we derive from both the Undirected and Directed variants of
  ! SpatialDerivativeInterface.  The latter has more information in
  ! the form of a Direction object.
  
  type, extends(UndirectedSpatialDerivativeInterface), public :: UndirectedZerothSpatialDerivativeType
     private
   contains
     procedure :: deinit => deinit_UndirectedZeroth
     procedure :: clone => clone_UndirectedZeroth
     procedure :: computeSubRowComponentsFromRawCoords => &
          computeSubRowComponentsFromRawCoords_UndirectedZeroth
     procedure :: computeSubRows => computeSubRows_UndirectedZeroth
     procedure :: sameAs => sameAs_UndirectedZeroth
  end type UndirectedZerothSpatialDerivativeType

  
  type, extends(DirectedSpatialDerivativeInterface), public :: DirectedZerothSpatialDerivativeType
     private
   contains
!!$     procedure :: init => init_DirectedZeroth
     procedure :: deinit => deinit_DirectedZeroth
     procedure :: clone => clone_DirectedZeroth
     procedure :: computeSubRowComponentsFromRawCoords => &
          computeSubRowComponentsFromRawCoords_DirectedZeroth
     procedure :: computeSubRows => computeSubRows_DirectedZeroth
     procedure :: sameAs => sameAs_DirectedZeroth
  end type DirectedZerothSpatialDerivativeType
  

  type, extends(UndirectedSpatialDerivativeInterface), public :: UndirectedFirstSpatialDerivativeType
     private
   contains
     procedure :: deinit => deinit_UndirectedFirst
     procedure :: clone => clone_UndirectedFirst
     procedure :: computeSubRowComponentsFromRawCoords => &
          computeSubRowComponentsFromRawCoords_UndirectedFirst
     procedure :: computeSubRows => computeSubRows_UndirectedFirst
     procedure :: sameAs => sameAs_UndirectedFirst
  end type UndirectedFirstSpatialDerivativeType


  type, extends(DirectedSpatialDerivativeInterface), public :: DirectedFirstSpatialDerivativeType
     private
     class(DirectionInterface), allocatable :: direction
   contains
     procedure :: init => init_DirectedFirst
     procedure :: deinit => deinit_DirectedFirst
     procedure :: clone => clone_DirectedFirst
     procedure :: computeSubRowComponentsFromRawCoords => &
          computeSubRowComponentsFromRawCoords_DirectedFirst
     procedure :: computeSubRows => computeSubRows_DirectedFirst
     procedure :: sameAs => sameAs_DirectedFirst
  end type DirectedFirstSpatialDerivativeType

  
  
contains

  
  !-------------------------------------------------------------------
  !- common functions
  !-------------------------------------------------------------------

  pure function spreadCoords( coords, nCoefs )
    ! expands coords into the 3rd dimension for manipulation by
    ! computeComponentRows.  See notes in the abstract interface for
    ! computeComponentRows regarding the the various array shapes.
    real(FLOAT), dimension(:, :), intent(in) :: coords
    integer, intent(in) :: nCoefs
    real(FLOAT), dimension(NDIM, size(coords, 2), nCoefs) :: &
         spreadCoords

    spreadCoords = spread(coords, dim=3, ncopies=nCoefs)
  end function spreadCoords


  pure function spreadPowers( powers, nPoints )
    ! expands powers into the 3rd dimension for manipulation by
    ! computeComponentRows.  See notes in the abstract interface for
    ! computeComponentRows regarding the the various array shapes.
    integer, dimension(:, :), intent(in) :: powers
    integer, intent(in) :: nPoints
    integer, dimension(NDIM, nPoints, size(powers, 2)) :: &
         spreadPowers

    spreadPowers = spread(powers, dim=2, ncopies=nPoints)
  end function spreadPowers

  
  pure function computeSubRowComponentsFromRawCoords_Zeroth( coords, &
       undiffPowers ) result ( rowComponents )
    real(FLOAT), dimension(:, :), intent(in) :: coords
    integer, dimension(:, :), intent(in) :: undiffPowers
    real(FLOAT), dimension(NDIM, size(coords, 2), size(&
         undiffPowers, 2)) :: rowComponents
    
    ! compute x**px, y**py, etc.
    rowComponents = &
         spreadCoords(coords, size(undiffPowers, 2)) ** &
         spreadPowers(undiffPowers, size(coords, 2))
  end function computeSubRowComponentsFromRawCoords_Zeroth

  
  pure function computeSubRowComponentsFromRawCoords_First( coords, &
       undiffPowers ) result ( rowComponents )
    ! beware, differing array shapes: see notes in abstract interface
    real(FLOAT), dimension(:, :), intent(in) :: coords
    integer, dimension(:, :), intent(in) :: undiffPowers
    real(FLOAT), dimension(NDIM, size(coords, 2), size(&
         undiffPowers, 2)) :: rowComponents
    integer, dimension(NDIM, NDIM, size(coords, 2), size(undiffPowers, &
         2)) :: p
    real(FLOAT), dimension(NDIM, NDIM, size(coords, 2), size(&
         undiffPowers, 2)) :: c
    integer :: i

    ! we need to compute the group
    ! [px*x**max(0, px-1) *    y**py           *    z**pz          , 
    !     x**px           * py*y**max(0, py-1) *    z**pz          ,
    !     x**px           *    y**py           * pz*z**max(0, pz-1)].

    ! to do this, we can spread the various arrays (i) to match each other
    ! and (ii) into a temporary dimension with distance NDIM
    p(1, :, :, :) = spreadPowers( undiffPowers, size(coords, 2) )
    c(1, :, :, :) = spreadCoords( coords, size(undiffPowers, 2) )
    p = spread( p(1, :, :, :), dim=1, ncopies=NDIM )
    c = spread( c(1, :, :, :), dim=1, ncopies=NDIM )

!!$    ! initialise rowComponents with the premultiplying px, py, and pz
!!$    rowComponents = real( p(:, :, :, 1), FLOAT )
    
    ! looking at the NDIM*NDIM section of the spread powers array, modify
    ! the powers along the diagonal
    forall ( i = 1:NDIM )
       p(i, i, :, :) = max( 0, p(i, i, :, :) - 1 )
    end forall

    ! pre-empt the wrong evaluation of c**p = 0.0 when c = 0.0 and p = 0.
    ! We require c**p = 1.0 when p = 0, so modify c to a safe value.
    where ( p == 0 ) c = 1._FLOAT

!!$    ! now we can operate, reducing the dimension-4 arrays to dimension-3
!!$    rowComponents = rowComponents * product( c**p, dim=4 )
    
    ! now we can operate, reducing the dimension-4 arrays to dimension-3
    c = c**p
    rowComponents = product( c, dim=2 )
    rowComponents = real( spreadPowers(undiffPowers, size(coords, 2)), &
         FLOAT ) * rowComponents
    
  end function computeSubRowComponentsFromRawCoords_First
  

  !-------------------------------------------------------------------
  !- UndirectedZerothSpatialDerivative methods
  !-------------------------------------------------------------------

  subroutine createUndirectedZerothSpatialDerivative( usd, log )
    class(UndirectedSpatialDerivativeInterface), allocatable, intent(&
         out) :: usd
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, &
         'createUndirectedZerothSpatialDerivative', log )
    
    ! allocate
    if ( allocated(usd) ) then
       call addEvent( FATAL, 'usd already allocated.', log )
    else
       allocate( UndirectedZerothSpatialDerivativeType :: usd, stat=&
            allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &UndirectedZerothSpatialDerivativeType :: usd.  STAT='//&
            int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! nothing to initialise
    call endSub( log )
  end subroutine createUndirectedZerothSpatialDerivative
  
  
  subroutine deinit_UndirectedZeroth( obj, log )
    class(UndirectedZerothSpatialDerivativeType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_UndirectedZeroth', log )

    ! nothing to deinitialise
    call endSub( log )
  end subroutine deinit_UndirectedZeroth
  
  
  subroutine clone_UndirectedZeroth( obj, tgt, log )
    class(UndirectedZerothSpatialDerivativeType), intent(in) :: obj
    class(UndirectedSpatialDerivativeInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_UndirectedZeroth', log )

    allocate( UndirectedZerothSpatialDerivativeType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &UndirectedZerothSpatialDerivativeType :: tgt.  STAT='//int2str(&
         stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! nothing to copy
    call endSub( log )
  end subroutine clone_UndirectedZeroth

  
  pure function computeSubRowComponentsFromRawCoords_UndirectedZeroth( obj, &
       coords, undiffPowers ) result ( rowComponents )
    class(UndirectedZerothSpatialDerivativeType), intent(in) :: obj
    ! beware, differing array shapes: see notes in abstract interface
    real(FLOAT), dimension(:, :), intent(in) :: coords
    integer, dimension(:, :), intent(in) :: undiffPowers
    real(FLOAT), dimension(NDIM, size(coords, 2), size(&
         undiffPowers, 2)) :: rowComponents

    rowComponents = computeSubRowComponentsFromRawCoords_Zeroth( coords, &
         undiffPowers )
  end function computeSubRowComponentsFromRawCoords_UndirectedZeroth

  
  pure function computeSubRows_UndirectedZeroth( &
       obj, rowComponents, directions ) result ( rows )
    class(UndirectedZerothSpatialDerivativeType), intent(in) :: obj
    real(FLOAT), dimension(:, :, :), intent(in) :: rowComponents
    class(DirectionInterface), dimension(:), intent(in) :: &
         directions
    real(FLOAT), dimension(size(rowComponents, 2), size(&
         rowComponents, 3)) :: rows
    
    ! compute x**px * y**py * etc.  (direction doesn't do a thing.)
    rows = product(rowComponents, dim=1)
  end function computeSubRows_UndirectedZeroth

  
  pure function sameAs_UndirectedZeroth( obj, &
       undirectedSpatialDerivative )
    class(UndirectedZerothSpatialDerivativeType), intent(in) :: obj
    class(UndirectedSpatialDerivativeInterface), intent(in) :: &
         undirectedSpatialDerivative
    logical :: sameAs_UndirectedZeroth
    select type ( undirectedSpatialDerivative )
    type is (UndirectedZerothSpatialDerivativeType)
       sameAs_UndirectedZeroth = .true. 
    class default
       sameAs_UndirectedZeroth = .false.
    end select
  end function sameAs_UndirectedZeroth

  
  !-------------------------------------------------------------------
  !- DirectedZerothSpatialDerivative methods
  !-------------------------------------------------------------------

  subroutine createDirectedZerothSpatialDerivative( dsd, log )
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         out) :: dsd
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createDirectedZerothSpatialDerivative', log )
    
    ! allocate
    if ( allocated(dsd) ) then
       call addEvent( FATAL, 'dsd already allocated.', log )
    else
       allocate( DirectedZerothSpatialDerivativeType :: dsd, stat=&
            allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &DirectedZerothSpatialDerivativeType :: dsd.  STAT='//&
            int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! nothing to initialise
    call endSub( log )
  end subroutine createDirectedZerothSpatialDerivative
  
  
  subroutine deinit_DirectedZeroth( obj, log )
    class(DirectedZerothSpatialDerivativeType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_DirectedZeroth', log )

    call endSub( log )
  end subroutine deinit_DirectedZeroth
  
  
  subroutine clone_DirectedZeroth( obj, tgt, log )
    class(DirectedZerothSpatialDerivativeType), intent(in) :: obj
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_DirectedZeroth', log )

    allocate( DirectedZerothSpatialDerivativeType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &DirectedZerothSpatialDerivativeType :: tgt.  STAT='//&
         int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! nothing to copy
    call endSub( log )
  end subroutine clone_DirectedZeroth

  
  pure function computeSubRowComponentsFromRawCoords_DirectedZeroth( &
       obj, coords, undiffPowers ) result ( rowComponents )
    class(DirectedZerothSpatialDerivativeType), intent(in) :: obj
    ! beware, differing array shapes: see notes in abstract interface
    real(FLOAT), dimension(:, :), intent(in) :: coords
    integer, dimension(:, :), intent(in) :: undiffPowers
    real(FLOAT), dimension(NDIM, size(coords, 2), size(&
         undiffPowers, 2)) :: rowComponents

    ! call common function
    rowComponents = computeSubRowComponentsFromRawCoords_Zeroth( coords, &
         undiffPowers )
  end function computeSubRowComponentsFromRawCoords_DirectedZeroth

  
  pure function computeSubRows_DirectedZeroth( &
       obj, rowComponents ) result ( rows )
    class(DirectedZerothSpatialDerivativeType), intent(in) :: obj
    real(FLOAT), dimension(:, :, :), intent(in) :: rowComponents
    real(FLOAT), dimension(size(rowComponents, 2), size(&
         rowComponents, 3)) :: rows
    
    ! compute x**px * y**py * etc.
    rows = product(rowComponents, dim=1)
  end function computeSubRows_DirectedZeroth


  pure function sameAs_DirectedZeroth( obj, directedSpatialDerivative )
    class(DirectedZerothSpatialDerivativeType), intent(in) :: obj
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    logical :: sameAs_DirectedZeroth
    select type ( directedSpatialDerivative )
    type is (DirectedZerothSpatialDerivativeType)
       sameAs_DirectedZeroth = .true. 
    class default
       sameAs_DirectedZeroth = .false.
    end select
  end function sameAs_DirectedZeroth


  !-------------------------------------------------------------------
  !- UndirectedFirstSpatialDerivative methods
  !-------------------------------------------------------------------

  subroutine createUndirectedFirstSpatialDerivative( ufsd, log )
    class(UndirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: ufsd
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, &
         'createUndirectedFirstSpatialDerivative', log )
    
    ! allocate
    if ( allocated(ufsd) ) then
       call addEvent( FATAL, 'ufsd already allocated.', log )
    else
       allocate( UndirectedFirstSpatialDerivativeType :: ufsd, stat=&
            allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &UndirectedFirstSpatialDerivativeType :: ufsd.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! nothing to initialise
    call endSub( log )
  end subroutine createUndirectedFirstSpatialDerivative

  
  subroutine deinit_UndirectedFirst( obj, log )
    class(UndirectedFirstSpatialDerivativeType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_UndirectedFirst', log )

    ! nothing to deinitialise
    call endSub( log )
  end subroutine deinit_UndirectedFirst
  

  subroutine clone_UndirectedFirst( obj, tgt, log )
    class(UndirectedFirstSpatialDerivativeType), intent(in) :: obj
    class(UndirectedSpatialDerivativeInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_UndirectedFirst', log )

    allocate( UndirectedFirstSpatialDerivativeType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &UndirectedFirstSpatialDerivativeType :: tgt.  STAT='//&
         int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    call endSub( log )
  end subroutine clone_UndirectedFirst

  
  pure function computeSubRowComponentsFromRawCoords_UndirectedFirst( &
       obj, coords, undiffPowers ) result ( rowComponents )
    class(UndirectedFirstSpatialDerivativeType), intent(in) :: obj
    ! beware, differing array shapes: see notes in abstract interface
    real(FLOAT), dimension(:, :), intent(in) :: coords
    integer, dimension(:, :), intent(in) :: undiffPowers
    real(FLOAT), dimension(NDIM, size(coords, 2), size(&
         undiffPowers, 2)) :: rowComponents

    ! call common function
    rowComponents = computeSubRowComponentsFromRawCoords_First( coords, &
         undiffPowers )
  end function computeSubRowComponentsFromRawCoords_UndirectedFirst
 
  
  pure function computeSubRows_UndirectedFirst( obj, &
       rowComponents, directions ) result ( rows )
    class(UndirectedFirstSpatialDerivativeType), intent(in) :: obj
    real(FLOAT), dimension(:, :, :), intent(in) :: rowComponents
    class(DirectionInterface), dimension(:), intent(in) :: directions
    real(FLOAT), dimension(size(rowComponents, 2), size(&
         rowComponents, 3)) :: rows
    
    ! here we discriminate according to direction
    rows = dot_product( directions, rowComponents )

  end function computeSubRows_UndirectedFirst

  
  pure function sameAs_UndirectedFirst( obj, undirectedSpatialDerivative )
    class(UndirectedFirstSpatialDerivativeType), intent(in) :: obj
    class(UndirectedSpatialDerivativeInterface), intent(in) :: &
         undirectedSpatialDerivative
    logical :: sameAs_UndirectedFirst
    select type ( undirectedSpatialDerivative )
    type is (UndirectedFirstSpatialDerivativeType)
       sameAs_UndirectedFirst = .true. 
    class default
       sameAs_UndirectedFirst = .false.
    end select
  end function sameAs_UndirectedFirst

  
  !-------------------------------------------------------------------
  !- DirectedFirstSpatialDerivative methods
  !-------------------------------------------------------------------

  subroutine createCartesianFirstSpatialDerivative( dfsd, dimIndex, &
       log )
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: dfsd
    integer, intent(in) :: dimIndex
    class(LogType), intent(inout), optional :: log
    class(DirectionInterface), allocatable :: direction
    integer :: allocStat
    call beginSub( MOD_NAME, &
         'createCartesianFirstSpatialDerivative', log )
    
    ! create components to be injected
    call createCartesianDirection( direction, dimIndex, log )

    ! allocate main object
    if ( allocated(dfsd) ) then
       call addEvent( FATAL, 'dfsd already allocated.', log )
    else
       allocate( DirectedFirstSpatialDerivativeType :: dfsd, stat=&
            allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &DirectedFirstSpatialDerivativeType :: dfsd.  STAT='&
	    //int2str(allocStat), log )
    end if
    
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( dfsd )
    class is (DirectedFirstSpatialDerivativeType)
       call dfsd%init( direction, log )
    end select
    
    call endSub( log )
  end subroutine createCartesianFirstSpatialDerivative

  
  subroutine createObliqueFirstSpatialDerivative( dfsd, dirVector, &
       log )
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: dfsd
    type(RealVectorType), intent(in) :: dirVector
    class(LogType), intent(inout), optional :: log
    class(DirectionInterface), allocatable :: direction
    integer :: allocStat
    call beginSub( MOD_NAME, &
         'createObliqueFirstSpatialDerivative', log )
    
    ! create components to be injected
    call createObliqueDirection( direction, dirVector, log )

    ! allocate main object
    if ( allocated(dfsd) ) then
       call addEvent( FATAL, 'dfsd already allocated.', log )
    else
       allocate( DirectedFirstSpatialDerivativeType :: dfsd, stat=&
            allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &DirectedFirstSpatialDerivativeType :: dfsd.  STAT='&
	    //int2str(allocStat), log )
    end if
    
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( dfsd )
    class is (DirectedFirstSpatialDerivativeType)
       call dfsd%init( direction, log )
    end select
    
    call endSub( log )
  end subroutine createObliqueFirstSpatialDerivative
  
  
  subroutine init_DirectedFirst( obj, direction, log )
    class(DirectedFirstSpatialDerivativeType), intent(out) :: obj
    class(DirectionInterface), allocatable, intent(inout) :: direction
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_DirectedFirst', log )
    
    call checkInjection( allocated(direction), allocated(&
         obj%direction), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( direction, obj%direction )
    
    call endSub( log )
  end subroutine init_DirectedFirst

  
  subroutine deinit_DirectedFirst( obj, log )
    class(DirectedFirstSpatialDerivativeType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_DirectedFirst', log )

    call destroy( obj%direction, log )
    
    call endSub( log )
  end subroutine deinit_DirectedFirst
  
  
  subroutine clone_DirectedFirst( obj, tgt, log )
    class(DirectedFirstSpatialDerivativeType), intent(in) :: obj
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_DirectedFirst', log )

    allocate( DirectedFirstSpatialDerivativeType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &DirectedFirstSpatialDerivativeType :: tgt.  STAT='//int2str(&
         stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    select type ( tgt )
    type is (DirectedFirstSpatialDerivativeType)
       call obj%direction%clone( tgt%direction )
    end select
    
    call endSub( log )
  end subroutine clone_DirectedFirst
  
  
  pure function computeSubRowComponentsFromRawCoords_DirectedFirst( obj, &
       coords, undiffPowers ) result ( rowComponents )
    class(DirectedFirstSpatialDerivativeType), intent(in) :: obj
    ! beware, differing array shapes: see notes in abstract interface
    real(FLOAT), dimension(:, :), intent(in) :: coords
    integer, dimension(:, :), intent(in) :: undiffPowers
    real(FLOAT), dimension(NDIM, size(coords, 2), size(&
         undiffPowers, 2)) :: rowComponents
    integer, dimension(NDIM, size(coords, 2), size(undiffPowers, &
         2)) :: spreadPowers

    ! call common function
    rowComponents = computeSubRowComponentsFromRawCoords_First( coords, &
         undiffPowers )
  end function computeSubRowComponentsFromRawCoords_DirectedFirst

  
  pure function computeSubRows_DirectedFirst( obj, &
       rowComponents ) result ( rows )
    class(DirectedFirstSpatialDerivativeType), intent(in) :: obj
    real(FLOAT), dimension(:, :, :), intent(in) :: rowComponents
    real(FLOAT), dimension(size(rowComponents, 2), size(&
         rowComponents, 3)) :: rows
    
    ! here we discriminate according to direction
    rows = dot_product( obj%direction, rowComponents )
  end function computeSubRows_DirectedFirst

  
  pure function sameAs_DirectedFirst( obj, directedSpatialDerivative )
    class(DirectedFirstSpatialDerivativeType), intent(in) :: obj
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    logical :: sameAs_DirectedFirst
    select type ( directedSpatialDerivative )
    type is (DirectedFirstSpatialDerivativeType)
       sameAs_DirectedFirst = obj%direction%sameAs( &
            directedSpatialDerivative%direction )
    class default
       sameAs_DirectedFirst = .false.
    end select
  end function sameAs_DirectedFirst


  
end module SpatialDerivativesModule
