module PolytopeCollectionsModule
  
  USE_MACROS({PolytopeCollections})
  use Global
  use LogModule
  use FiniteVolumeModule
  use SemiLagrangianModule
  use DomainModule
  use FlowFieldModule

  ! placeholder for a module that points to any one of the variants in
  ! the folder of the same name.  The module should not be created
  ! within the source tree as it may interfere with automated tests
  ! that define their own variants.
  use PolytopeCollections_DomainDim
  
  implicit none
  private
  public :: createPointArray, createSimplexArray, createPointGrid, &
       createOrthotopeGrid
  
  character(*), parameter :: MOD_NAME = 'PolytopeCollectionsModule'

  
  !-------------------------------------------------------------------
  !- SuperPolytopesManagers
  !-------------------------------------------------------------------

  type, abstract :: SuperPolytopesManagerInterface
     type(PolytopePointerSetType), dimension(:), allocatable :: &
          superPolytopes
   contains
     procedure :: initSuperPolytopesManager
     procedure :: deinitSuperPolytopesManager
  end type SuperPolytopesManagerInterface

  
  type, extends(SuperPolytopesManagerInterface) :: ArraySuperPolytopesManagerType
   contains
     procedure :: init => initArraySPM
     procedure :: deinit => deinitArraySPM
     procedure :: collectSuperPolytopes => &
          collectSuperPolytopes_ArraySPM
  end type ArraySuperPolytopesManagerType

  
  type, extends(SuperPolytopesManagerInterface) :: GridSuperPolytopesManagerType
     type(PolytopeGridPointerType), dimension(NDIM) :: interiors
     type(ModifiedGridInfoType) :: modifiedGridInfo
   contains
     procedure :: initGridSPM
     procedure :: deinitGridSPM
     procedure :: collectSuperPolytopes => &
          collectSuperPolytopes_GridSPM
  end type GridSuperPolytopesManagerType

  
  !-------------------------------------------------------------------
  !- PolytopeArray extensions 
  !-------------------------------------------------------------------

  type, extends(PolytopeArrayInterface) :: PointArrayType
     private
     EXPAND({DATALIST_POLYTOPECOLLECTION_WITHEXTENSIONS({Point},
     {Array})})
     type(RealVectorType), dimension(:), allocatable :: coords
     integer :: globalPointIndexOffset
     
   contains
     EXPAND({METHODLIST_POLYTOPECOLLECTION({Point}, {Array})})
     procedure :: init => initPointArray
     procedure :: deinit => deinitPointArray
     procedure :: pointToSubPolytopeArray => &
          pointToSubPolytopeArray_PointArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_PointArray
     procedure :: collectSuperPolytopes => &
          collectSuperPolytopes_PointArray
     procedure :: getCentroid => getCentroid_PointArray
     procedure :: getSize => getSize_PointArray
     procedure :: isAssociatedWithRegion => &
          isAssociatedWithRegion_PointArray
     procedure :: writePointLocations => &
          writePointLocations_PointArray
     procedure :: writePointConnectivity => &
          writePointConnectivity_PointArray
     procedure :: appendConnection => appendConnection_PointArray
  end type PointArrayType

  
  type, extends(PolytopeArrayInterface) :: SimplexArrayType
     private
     EXPAND({DATALIST_POLYTOPECOLLECTION_WITHEXTENSIONS({Simplex},
     {Array})})
     ! dimensions correspond to (1) boundaries on each simplex and (2)
     ! simplicies
     type(PolytopePointerType), dimension(:, :), allocatable :: &
          boundaries
     class(PolytopeArrayInterface), pointer :: subPolytopeArray => &
          null()
     
     ! (optional) storage
     type(RealVectorType), dimension(:), allocatable :: centroids
   contains
     EXPAND({METHODLIST_POLYTOPECOLLECTION({Simplex}, {Array})})
     procedure :: init => initSimplexArray
     procedure :: deinit => deinitSimplexArray
     procedure :: pointToSubPolytopeArray => &
          pointToSubPolytopeArray_SimplexArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_SimplexArray
     procedure :: collectSuperPolytopes => &
          collectSuperPolytopes_SimplexArray
     procedure :: getCentroid => getCentroid_SimplexArray
     procedure :: getSize => getSize_SimplexArray
     procedure :: isAssociatedWithRegion => &
          isAssociatedWithRegion_SimplexArray
     procedure :: writePointLocations => &
          writePointLocations_SimplexArray
     procedure :: writePointConnectivity => &
          writePointConnectivity_SimplexArray
     procedure :: appendConnection => appendConnection_SimplexArray
     procedure, private :: computeCentroid => &
          computeCentroid_SimplexArray
  end type SimplexArrayType
      
  

  !-------------------------------------------------------------------
  !- PolytopeGrid extensions 
  !-------------------------------------------------------------------

  type, extends(PolytopeGridInterface) :: PointGridType
     private
     EXPAND({DATALIST_POLYTOPECOLLECTION_WITHEXTENSIONS({Simplex},
     {Array})})
     integer :: globalPointIndexOffset
     ! can have smaller rank than NDIM, but static allocation is
     ! preferred
     
   contains
     EXPAND({METHODLIST_POLYTOPECOLLECTION({Point}, {Grid})})
     procedure :: init => initPointGrid
     procedure :: collectSubPolytopes => collectSubPolytopes_PointGrid
     procedure :: collectSuperPolytopes => &
          collectSuperPolytopes_PointGrid
     procedure :: getCentroid => getCentroid_PointGrid
     procedure :: getSize => getSize_PointGrid
     procedure :: isAssociatedWithRegion => &
          isAssociatedWithRegion_PointGrid
     procedure :: breakUp => breakUp_PointGrid
     procedure :: getCartesianCode => getCartesianCode_PointGrid
     procedure :: deinit => deinitPointGrid
     procedure :: pointToComplex => pointToComplex_PointGrid
     procedure :: writePointLocations => &
          writePointLocations_PointGrid
     procedure :: writePointConnectivity => &
          writePointConnectivity_PointGrid
     procedure :: appendConnection => appendConnection_PointGrid
  end type PointGridType
  

  type, extends(PolytopeGridInterface) :: UnboundedOrthotopeGridType
     private
     EXPAND({DATALIST_POLYTOPECOLLECTION_WITHEXTENSIONS(
     {UnboundedOrthotope}, {Grid})})
     ! if there are exterior grids, the min/max coordinates are not
     ! necessarily those of the core PointGrid.  So store grid
     ! parameters independently
     type(PolytopeGridPointerType), dimension(NDIM) :: &
          constituents
     class(PolytopeComplexInterface), dimension(:), pointer :: &
          simplicialComplexes
     type(ModifiedGridInfoType) :: cutInfo
   contains
     EXPAND({METHODLIST_POLYTOPECOLLECTION({UnboundedOrthotope}, {Grid})})
     procedure :: init => initUnboundedOrthotopeGrid
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_UnboundedOrthotopeGrid
     procedure :: collectSuperPolytopes => &
          collectSuperPolytopes_UnboundedOrthotopeGrid
     procedure :: getCentroid => getCentroid_UnboundedOrthotopeGrid
     procedure :: getSize => getSize_UnboundedOrthotopeGrid
     procedure :: isAssociatedWithRegion => &
          isAssociatedWithRegion_UnboundedOrthotopeGrid
     procedure :: breakUp => breakUp_UnboundedOrthotopeGrid
     procedure :: getCartesianCode => &
          getCartesianCode_UnboundedOrthotopeGrid
     procedure :: deinit => deinitUnboundedOrthotopeGrid
     procedure :: pointToComplex => pointToComplex_UnboundedOrthotopeGrid
     procedure :: writePointLocations => &
          writePointLocations_UnboundedOrthotopeGrid
     procedure :: writePointConnectivity => &
          writePointConnectivity_UnboundedOrthotopeGrid
     procedure :: appendConnection => &
          appendConnection_UnboundedOrthotopeGrid
  end type UnboundedOrthotopeGridType

  
contains


  ! n.b. input array size is the total number of elements being
  ! supplied, not the eventual length of the unstructured array.
  subroutine checkInputArraySize( inputArraySize, nBins, log )
    integer, intent(in) :: inputArraySize, nBins
    class(LogType), intent(inout), optional :: log
    integer :: x
    call beginSub( MOD_NAME, 'checkInputArraySize', log )

    x = mod( inputArraySize, nBins )
    call addEvent( x/=0, FATAL, 'Input array is not reshapeable into &
         &nBins*nEntities.  Remainder: '//int2str(x), log )
    call endSub( log )
  end subroutine checkInputArraySize


  !-------------------------------------------------------------------
  !- SuperPolytopesManager procedures
  !-------------------------------------------------------------------


  subroutine initSuperPolytopesManager( obj, arraySize, log )
    class(SuperPolytopesManagerInterface), intent(out) :: obj
    integer, intent(in) :: arraySize
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'initSuperPolytopesManager', log )
    
    allocate( obj%superPolytopes(arraySize), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &obj%superPolytopes(arraySize).  STAT='//int2str(&
         allocStat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call endSub( log )
  end subroutine initSuperPolytopesManager

  
  subroutine deinitSuperPolytopesManager( obj, log )
    class(SuperPolytopesManagerInterface), intent(out) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, n, deallocStat
    call beginSub( MOD_NAME, 'deinitSuperPolytopesManager', log )

    ! compiler complains about use of FORALL here
    do i = 1, size(obj%superPolytopes)
       call obj%superPolytopes(i)%deinit( log )
    end do
    
    deallocate( obj%superPolytopes, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating &
         &obj%superPolytopes.  STAT='//int2str(deallocStat), log )
    
    call endSub( log )
  end subroutine deinitSuperPolytopesManager


  subroutine setSuperPolytopePointerSet( obj, index, pps, log, height )
    class(SuperPolytopesManagerInterface), intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, 'setSuperPolytopePointerSet', log )

    
    
    
    call endSub( log )
  end subroutine setSuperPolytopePointerSet
  

  
  !-------------------------------------------------------------------
  !- ArraySuperPolytopesManager procedures
  !-------------------------------------------------------------------

  subroutine initArraySPM( obj, arraySize, log )
    class(ArraySuperPolytopesManagerType), intent(out) :: obj
    integer, intent(in) :: arraySize
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'initArraySPM', log )
    
    call obj%initSuperPolytopesManager( arraySize, log )
    
    call endSub( log )
  end subroutine initArraySPM

  
  subroutine deinitArraySPM( obj, log )
    class(ArraySuperPolytopesManagerType), intent(out) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, n, deallocStat
    call beginSub( MOD_NAME, 'deinitArraySPM', log )

    call obj%deinitSuperPolytopesManager( log )
    
    call endSub( log )
  end subroutine deinitArraySPM


  subroutine setSuperPolytopePointerSet_ArraySPM( obj, index, pps, log, height )
    class(ArraySuperPolytopesManagerType), intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, 'setSuperPolytopePointerSet_ArraySPM', log )

    
    
    call endSub( log )
  end subroutine setSuperPolytopePointerSet_ArraySPM

  
  pure recursive subroutine collectSuperPolytopes_ArraySPM( obj, index, pps, log, height )
    class(ArraySuperPolytopesManagerType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, 'collectSuperPolytopes_ArraySPM', log )

    ! 
    
    call endSub( log )
  end subroutine collectSuperPolytopes_ArraySPM
  

  
  !-------------------------------------------------------------------
  !- GridSuperPolytopesManager procedures
  !-------------------------------------------------------------------

  subroutine initGridSPM( obj, gridShape, log )
    class(GridSuperPolytopesManagerType), intent(out) :: obj
    type(IntVectorType), intent(in) :: gridShape
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'initGridSPM', log )

    call obj%initSuperPolytopesManager( product(gridShape), log )
    
    call endSub( log )
  end subroutine initGridSPM

  
  subroutine deinitGridSPM( obj, log )
    class(GridSuperPolytopesManagerType), intent(out) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, n, deallocStat
    call beginSub( MOD_NAME, 'deinitGridSPM', log )
    
    ! compiler complains about use of FORALL here
    do i = 1, size(obj%superPolytopes)
       call obj%superPolytopes(i)%deinit( log )
    end do
    
    deallocate( obj%superPolytopes, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating &
         &obj%superPolytopes.  STAT='//int2str(deallocStat), log )
    
    call endSub( log )
  end subroutine deinitGridSPM


  subroutine setSuperPolytopePointerSet_GridSPM( obj, index, pps, log, height )
    class(GridSuperPolytopesManagerType), intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, 'setSuperPolytopePointerSet_GridSPM', log )

    
    
    call endSub( log )
  end subroutine setSuperPolytopePointerSet_GridSPM

  
  pure recursive subroutine collectSuperPolytopes_GridSPM( obj, index, pps, log, height )
    class(GridSuperPolytopesManagerType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, 'collectSuperPolytopes_GridSPM', log )

    ! 
    
    call endSub( log )
  end subroutine collectSuperPolytopes_GridSPM

   
  !-------------------------------------------------------------------
  !- PointArrayType procedures
  !-------------------------------------------------------------------

  subroutine createPointArray( pa, coords, log )
    class(PolytopeArrayInterface), allocatable, intent(out) :: pa
    real(FLOAT), dimension(:), intent(in) :: coords
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createPointArray', log )
    
    ! allocate
    if ( allocated(pa) ) then
       call addEvent( FATAL, 'pa already allocated.', log )
    else
       allocate( PointArrayType :: pa, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &PointArrayType :: pa.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( pa )
    class is (PointArrayType)
       call pa%init( coords, log )
    end select
    call endSub( log )
  end subroutine createPointArray


  subroutine initPointArray( obj, coords, log )
    class(PointArrayType), intent(out) :: obj
    real(FLOAT), dimension(:), intent(in) :: coords
    class(LogType), intent(inout), optional :: log
    integer :: allocStat, nPoints, iPoint, i, iDim
    integer, parameter :: POLYTOPE_DIM = 0
    call beginSub( MOD_NAME, 'initPointArray', log )
    
    ! check input
    call checkInputArraySize( size(coords), NDIM, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! after successful inputs check, allocate
    nPoints = size(coords)/NDIM
    allocate( obj%coords(nPoints), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &coordinates array.  STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

!!$    ! set this subclass' components by putting in the coordinates
!!$    do iPoint = 1, nPoints
!!$       i = (iPoint - 1)*NDIM + 1
!!$       obj%coords(iPoint) = coords( i:i+NDIM )
!!$    end do

    ! alternative setting 
    iPoint = 1
    iDim = 1
    do i = 1, size(coords)
       call obj%coords(iPoint)%setElement( iDim, coords(i) )
       iDim = iDim + 1
       if ( iDim > NDIM ) then
          iDim = 1
          iPoint = iPoint + 1
       end if
    end do

    ! local components
    obj%globalPointIndexOffset = 0
    
    ! initialise the parent type
    call obj%initPolytopeArray( nPoints, POLYTOPE_DIM, log )
    
    call endSub( log )
  end subroutine initPointArray
  
  

  subroutine deinitPointArray( obj, log )
    class(PointArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinitPointArray', log )
    
    ! inherited components
    call obj%deinitPolytopeArray( log )    
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! components unique to this class
    if ( allocated(obj%coords) ) then
       deallocate( obj%coords, stat=deallocStat )
       call addEvent( deallocStat/=0, WARNING, 'Problem &
            &deallocating coordinates array.  STAT='//int2str(&
            deallocStat), log )
    else
       call addEvent( .true., ADVICE, 'Coordinates array &
            &already deallocated.', log )
    end if

    obj%globalPointIndexOffset = 0
    call destroy( obj%departurePoints, log )
    
    call endSub( log )
  end subroutine deinitPointArray
  

  pure recursive subroutine collectSubPolytopes_PointArray( obj, &
       index, pps, log, depth )
    class(PointArrayType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    ! no more subpolytopes.
  end subroutine collectSubPolytopes_PointArray
  

  pure recursive subroutine collectSuperPolytopes_PointArray( obj, &
       index, pps, log, height )
    class(PointArrayType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, 'collectSuperPolytopes_PointArray', log )
    ! delegate
    call obj%superPolytopesManager%collectSuperPolytopes( index, &
         pps, log, height )
    call endSub( log )
  end subroutine collectSuperPolytopes_PointArray
  
  
  subroutine pointToSubPolytopeArray_PointArray( obj, ptr )
    class(PointArrayType), intent(in) :: obj
    class(PolytopeArrayInterface), pointer, intent(out) :: ptr
    ! no sub-arrays
    nullify(ptr)
  end subroutine pointToSubPolytopeArray_PointArray
  

  pure recursive subroutine getCentroid_PointArray( obj, index, &
       coords, log )
    class(PointArrayType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out) :: coords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCentroid_PointArray', log )

    call addEvent( index > size(obj%coords), FATAL, 'Index exceeds &
         &size of coordinates array in '//trim(obj%describe())//'.  &
         &index = '//str(index)//'; size(obj%coords) = '//str(&
         coords)//'.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! just get the point's coordinates 
    coords = obj%coords(index)

    call endSub( log )
  end subroutine getCentroid_PointArray
  

  pure subroutine getSize_PointArray( obj, index, size, log )
    class(PointArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    real(FLOAT), intent(out) :: size
    class(LogType), intent(inout), optional :: log
    size = 0._FLOAT
  end subroutine getSize_PointArray
    
  
  pure function isAssociatedWithRegion_PointArray( obj, region )
    class(PointArrayType), intent(in) :: obj
    class(RegionInterface), intent(in) :: region
    logical :: isAssociatedWithRegion_PointArray
    
    isAssociatedWithRegion_PointArray = .false.
    
  end function isAssociatedWithRegion_PointArray

  
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_WRITEPOINTS({Array})})
  
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_DEPARTUREPOINTS({Point}, {Array})})

  EXPAND({METHODDEFS_POLYTOPECOLLECTION_APPENDMOMENTS({Point}, {Array})})
  
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_NOTAPPLICABLE({Point},
  {Array}, {Face})})


  EXPAND({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_NOTAPPLICABLE({Point},
  {Array}, {Cell})})

  
  !-------------------------------------------------------------------
  !- SimplexArrayType procedures
  !-------------------------------------------------------------------

  subroutine createSimplexArray( sa, polytopeDim, ppArray, &
       subPolytopeArray, log )
    class(PolytopeArrayInterface), allocatable, intent(out) :: sa
    integer, intent(in) :: polytopeDim
    type(PolytopePointerType), dimension(:), intent(in) :: ppArray
    class(PolytopeArrayInterface), target, intent(in), optional :: &
         subPolytopeArray
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createSimplexArray', log )
    
    ! allocate
    if ( allocated(sa) ) then
       call addEvent( FATAL, 'sa already allocated.', log )
    else
       allocate( SimplexArrayType :: sa, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &SimplexArrayType :: sa.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( sa )
    class is (SimplexArrayType)
       call sa%init( polytopeDim, ppArray, subPolytopeArray, log )
    end select
    call endSub( log )
  end subroutine createSimplexArray

  
  subroutine initSimplexArray( obj, polytopeDim, ppArray, &
       subPolytopeArray, log )
    class(SimplexArrayType), intent(out) :: obj
    integer, intent(in) :: polytopeDim
    type(PolytopePointerType), dimension(:), intent(in) :: ppArray
    class(PolytopeArrayInterface), target, intent(in), optional :: &
         subPolytopeArray
    class(LogType), intent(inout), optional :: log
    integer :: allocStat, nBoundaries, nSimplicies, i, j
    type(SizeVectorType) :: tableSize
    call beginSub( MOD_NAME, 'initSimplexArray', log )
    
    ! check inputs
    nBoundaries = polytopeDim + 1
    call checkInputArraySize( size(ppArray), nBoundaries, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! after successful inputs check, allocate
    nSimplicies = size(ppArray)/nBoundaries
    allocate( obj%boundaries(nBoundaries, nSimplicies), stat=&
         allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &elements of bounding subpolytopes array.  STAT='//int2str(&
         allocStat), log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if
    
   ! loop over simplicies putting in the boundaries.  This cannot be
    ! made into an elemental procedure and/or done via the reshape
    ! intrinsic due to the presence of pointer components.
    do j = 1, nSimplicies
       do i = 1, nBoundaries
          tableSize = vector( [nBoundaries, nSimplicies] )
          obj%boundaries(i, j) = ppArray( tableSize%address2index( &
               vector([i, j]) ) )
       end do
    end do

    ! link to the associated subPolytopeArray needed for building
    ! simplicial complexes.  The subPolytopeArray is the PolytopeArray
    ! containing bounding elements that are (1) one dimension less than
    ! the object's elements and (2) not coincident with any orthogonal
    ! Cartesian gridlines.  If more than one such array exists, this
    ! object needs to be divided further.
    if ( present(subPolytopeArray) ) &
         obj%subPolytopeArray => subPolytopeArray
    
    ! initialise the parent type
    call obj%initPolytopeArray( nSimplicies, polytopeDim, log )
    call endSub( log )
  end subroutine initSimplexArray

     
  subroutine deinitSimplexArray( obj, log )
    class(SimplexArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinitSimplexArray', log )
    
    ! inherited components
    call obj%deinitPolytopeArray( log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

    ! components unique to this class
    if ( allocated(obj%boundaries) ) then
       call obj%boundaries%deinit()
       deallocate( obj%boundaries, stat=deallocStat )
       call addEvent( deallocStat/=0, WARNING, 'Problem &
            &deallocating PolytopePointerType array representing the &
            &simplex boundaries.  STAT='//int2str(deallocStat), log )
    else
       call addEvent( .true., ADVICE, 'obj%boundaries already &
            &deallocated.', log )
    end if
    
    nullify(obj%subPolytopeArray)

    deallocate( obj%centroids, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating obj%&
         &centroids.  STAT='//int2str(deallocStat), log )

    call destroy( obj%departurePoints, log )

    if ( allocated(obj%faceExtension) ) then
       call destroy( obj%faceExtension, log )
    end if

    if ( allocated(obj%cellExtension) ) then
       call destroy( obj%cellExtension, log )
    end if
    call endSub( log )
    
  end subroutine deinitSimplexArray

  
  pure recursive subroutine collectSubPolytopes_SimplexArray( &
       obj, index, pps, log, depth )
    class(SimplexArrayType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    integer :: iBdry, height
    type(PolytopePointerType), pointer :: bdry
    call beginSub( MOD_NAME, 'collectSubPolytopes_SimplexArray', log )
    
    ! loop over the boundaries for this simplex
    do iBdry = 1, obj%getPolytopeDim() + 1
       bdry => obj%boundaries(iBdry, index)
       if ( present(depth) ) then
          call pps%addRecursively( bdry, log, -depth )
       else
          call pps%addRecursively( bdry, log )
       end if
    end do
    
    call endSub( log )
  end subroutine collectSubPolytopes_SimplexArray

  
  pure recursive subroutine collectSuperPolytopes_SimplexArray( obj, &
       index, pps, log, height )
    class(SimplexArrayType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, 'collectSuperPolytopes_SimplexArray', log )
    ! delegate
    call obj%superPolytopesManager%collectSuperPolytopes( index, &
         pps, log, height )
    call endSub( log )
  end subroutine collectSuperPolytopes_SimplexArray


  subroutine pointToSubPolytopeArray_SimplexArray( obj, ptr )
    class(SimplexArrayType), intent(in) :: obj
    class(PolytopeArrayInterface), pointer, intent(out) :: ptr
    ptr => obj%subPolytopeArray
  end subroutine pointToSubPolytopeArray_SimplexArray
  

  function getSubPolytopeArray_SimplexArray( obj )
    class(SimplexArrayType), intent(in) :: obj
    class(PolytopeArrayInterface), pointer :: &
         getSubPolytopeArray_SimplexArray
    getSubPolytopeArray_SimplexArray => obj%subPolytopeArray
  end function getSubPolytopeArray_SimplexArray


  pure recursive subroutine computeCentroid_SimplexArray( obj, &
       index, coords, log )
    class(SimplexArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    type(RealVectorType) :: subCoords
    type(PolytopePointerSetType) :: pps
    type(PolytopePointerSetIteratorType) :: iterator
    type(PolytopePointerType), pointer :: pp
    integer :: n
    logical, parameter :: DEBUG_MODE = .true.
    call beginSub( MOD_NAME, 'computeCentroid_SimplexArray', log )

    ! first gather all bounding polytopes
    call obj%collectSubPolytopes( index, pps, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! filter all points in the set to give us bounding vertices
    call pps%filter( 0, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! loop over the set adding point coordinate contributions
    ! according to the simplex centroid formula C = 1/n*sum( P_i, i=1,
    ! n )
    n = pps%size()
    coords = 0._FLOAT
    call iterator%init( pps )
    call iterator%first( pp )
    do
       if ( iterator%isDone() ) exit
       
       ! the point's coordinates are accessed through getCentroid
       ! again.
       call pp%getCentroid( subCoords )
       if ( checkSub(FATAL, log) ) then
         call endSub( log )
         return
       end if
       
       coords = coords + subCoords
       call iterator%next( pp )
    end do
    coords = coords/real(n, FLOAT)

    call pps%deinit( log )
    call endSub( log )
  end subroutine computeCentroid_SimplexArray


  pure subroutine getCentroid_SimplexArray( obj, index, coords, &
       log )
    class(SimplexArrayType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCentroid_SimplexArray', log )
    
    ! centroid may not necessarily have been computed previously
    call addEvent( .not. allocated(obj%centroids), FATAL, 'Centroids not &
         &available.  Try initialising this type again with &
         &centroidsNeeded argument set to .true.', log )
    if ( .not. allocated(obj%centroids) ) then
       call endSub( log )
       return
    end if
    
    coords = obj%centroids(index)
    
    call endSub( log )
  end subroutine getCentroid_SimplexArray
    

  pure subroutine getSize_SimplexArray( obj, index, size, log )
    class(SimplexArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    real(FLOAT), intent(out) :: size
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerSetType) :: pps
    type(PolytopePointerSetIteratorType) :: iterator
    type(PolytopePointerType), pointer :: pp
    type(RealVectorType) :: x
    type(RealVectorType), dimension(obj%getPolytopeDim()) :: edgeVectors
    integer :: i, n, stat
    call beginSub( MOD_NAME, 'getSize_SimplexArray', log )

    ! first gather all bounding polytopes
    call obj%collectSubPolytopes( index, pps, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! filter all points in the set to give us bounding vertices
    call pps%filter( 0, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
       
    ! use the first point's coordinates as the origin
    call iterator%init( pps )
    call iterator%first( pp )
    call pp%getCentroid( x )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    n = obj%getPolytopeDim()
    forall ( i = 1:n )
       edgeVectors(i) = x
    end forall

    ! n points should remain.  Compute edge vectors
    call iterator%next( pp )
    do i = 1, n
       if ( iterator%isDone() ) exit
       
       call pp%getCentroid( x )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
       edgeVectors(i) = x - edgeVectors(i)
       
       call iterator%next( pp )
    end do
    call addEvent( i < n, FATAL, 'Loop over points ended prematurely', &
         log )

    ! the formula to use will depend on the simplex dimension
    select case ( n )
    case ( 1 )
       size = edgeVectors(1)%magnitude()
    case ( 2 )
       size = edgeVectors(1)%crossProductMagnitude( edgeVectors(2) ) / 2
    case( 3 )
!!$       size = edgeVectors(1)%scalarTripleProduct( edgeVectors(2), &
!!$            edgeVectors(3) ) / 6
       call addEvent( FATAL, 'Not yet implemented for NDIM=3.', log )
    end select
    
    call pps%deinit( log )
    call endSub( log )
  end subroutine getSize_SimplexArray

  
  pure function isAssociatedWithRegion_SimplexArray( obj, region )
    class(SimplexArrayType), intent(in) :: obj
    class(RegionInterface), intent(in) :: region
    logical :: isAssociatedWithRegion_SimplexArray
    
    isAssociatedWithRegion_SimplexArray = .false.
    
  end function isAssociatedWithRegion_SimplexArray
  

  subroutine writePointLocations_SimplexArray( obj, stringList, &
       nPointsRunning, precision, indent, log )
    class(SimplexArrayType), intent(inout) :: obj
    type(StringListType), intent(inout) :: stringList
    integer, intent(inout) :: nPointsRunning
    integer, intent(in), optional :: precision, indent
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'writePointLocations_SimplexArray', log )
    
    ! nothing to do
    call addEvent( ADVICE, 'Not applicable.', log )
    
    call endSub( log )
  end subroutine writePointLocations_SimplexArray
  
  
  subroutine writePointConnectivity_SimplexArray( obj, connectivityList, &
       offsetList, nPolytopesRunning, startFromZero, indent, log )
    class(SimplexArrayType), intent(inout) :: obj
    type(StringListType), intent(inout) :: connectivityList, offsetList
    integer, intent(inout) :: nPolytopesRunning
    logical, intent(in) :: startFromZero
    integer, intent(in), optional :: indent
    class(LogType), intent(inout), optional :: log
    integer :: i, i1, i2, iostat, t
    type(StringListIteratorType) :: sli
    type(StringType) :: s
    call beginSub( MOD_NAME, 'writePointConnectivity_SimplexArray', log )
    
    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine writePointConnectivity_SimplexArray
  

  pure subroutine appendConnection_SimplexArray( obj, string, &
       polytopeIndex, startFromZero, indent )
    class(SimplexArrayType), intent(in) :: obj
    character(*), intent(inout) :: string
    integer, intent(in)  :: polytopeIndex
    logical, intent(in)  :: startFromZero
    integer, intent(in), optional :: indent
    ! n/a
  end subroutine appendConnection_SimplexArray
  

  EXPAND({METHODDEFS_POLYTOPECOLLECTION_DEPARTUREPOINTS({Simplex},
  {Array})})

  EXPAND({METHODDEFS_POLYTOPECOLLECTION_APPENDMOMENTS_WITHEXTENSIONS(
  {Simplex}, {Array})})
  

  EXPAND({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_POSITIVE({Simplex},
  {Array}, {Face}, {
    integer :: i}, {
    if ( .not. allocated(obj%centroids) ) then
       ! we now need to store centroids because the SimplexLocating
       ! algorithm uses them in the dot product test
       allocate( obj%centroids(obj%getNumPolytopes()), stat=&
            stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &elements of obj%centroids.  STAT='//int2str(stat), log )
       if ( checkSub(FATAL, log) ) then
         call endSub( log )
         return
       end if

       do i = 1, obj%getNumPolytopes()
          call obj%computeCentroid(i, obj%centroids(i), log )
       end do
    end if})})

    
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_POSITIVE({Simplex},
  {Array}, {Cell})})

  
  !-------------------------------------------------------------------
  !- PointGridType procedures
  !-------------------------------------------------------------------

  subroutine createPointGrid( pg, pointGridSize, minCoords, &
       maxCoords, log )
    class(PolytopeGridInterface), allocatable, intent(out) :: pg
    type(IntVectorType), intent(in) :: pointGridSize
    type(RealVectorType), intent(in) :: minCoords, maxCoords
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createPointGrid', log )
    
    ! allocate
    if ( allocated(pg) ) then
       call addEvent( FATAL, 'pg already allocated.', log )
    else
       allocate( PointGridType :: pg, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &PointGridType :: pg.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( pg )
    class is (PointGridType)
       call pg%init( pointGridSize, minCoords, &
            maxCoords, log )
    end select
    call endSub( log )
  end subroutine createPointGrid

  
  subroutine initPointGrid( obj, pointGridSize, minCoords, &
       maxCoords, log )
    class(PointGridType), intent(out) :: obj
    type(IntVectorType), intent(in) :: pointGridSize
    type(RealVectorType), intent(in) :: minCoords, maxCoords
    class(LogType), intent(inout), optional :: log
    type(GridParametersType) :: gridParams
    integer, parameter :: POLYTOPE_DIM = 0
    call beginSub( MOD_NAME, 'initPointGrid', log )
    
    ! set this subclass' components
    obj%globalPointIndexOffset = 0

    gridParams%size = pointGridSize
    gridParams%extents(1) = minCoords
    gridParams%extents(2) = maxCoords
    gridParams%spacing = (maxCoords - minCoords) / real(&
         pointGridSize - 1)
    
    ! initialise the inherited components
    call obj%initPolytopeGrid( gridParams, POLYTOPE_DIM, log )
    call endSub( log )
  end subroutine initPointGrid

  
  subroutine deinitPointGrid( obj, log )
    class(PointGridType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitPointGrid', log )

    ! inherited components
    call obj%deinitPolytopeGrid( log )    
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

    obj%globalPointIndexOffset = 0
    call destroy( obj%departurePoints, log )

    call endSub( log )
  end subroutine deinitPointGrid
    

  pure recursive subroutine collectSubPolytopes_PointGrid( obj, &
       index, pps, log, depth )
    class(PointGridType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    type(PolytopePointerType) :: pp
    ! no more subpolytopes.
  end subroutine collectSubPolytopes_PointGrid
    
  
  pure recursive subroutine collectSuperPolytopes_PointGrid( obj, &
       index, pps, log, height )
    class(PointGridType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    type(PolytopePointerType) :: pp
    call beginSub( MOD_NAME, 'collectSuperPolytopes_PointGrid', log )
    ! delegate
    call obj%superPolytopesManager%collectSuperPolytopes( index, &
         pps, log, height )
    call endSub( log )
  end subroutine collectSuperPolytopes_PointGrid
  
  
  pure recursive subroutine getCentroid_PointGrid( obj, index, &
       coords, log )
    class(PointGridType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out) :: coords
    class(LogType), intent(inout), optional :: log
    type(GridParametersType) :: gp

    gp = obj%getGridParameters()
    
    ! just work out the point's coordinates 
    coords = gp%extents(1) + gp%spacing*real( &
         obj%convertToGridAddress(index) - 1 )

  end subroutine getCentroid_PointGrid
      

  pure subroutine getSize_PointGrid( obj, index, size, log )
    class(PointGridType), intent(inout) :: obj
    integer, intent(in) :: index
    real(FLOAT), intent(out) :: size
    class(LogType), intent(inout), optional :: log
    ! tk
    size = 0._FLOAT
  end subroutine getSize_PointGrid

  
  pure function isAssociatedWithRegion_PointGrid( obj, region )
    class(PointGridType), intent(in) :: obj
    class(RegionInterface), intent(in) :: region
    logical :: isAssociatedWithRegion_PointGrid
    
    isAssociatedWithRegion_PointGrid = .false.
    
  end function isAssociatedWithRegion_PointGrid

  
  subroutine breakUp_PointGrid( obj, gridAddresses, &
       polytopeComplexes, log )
    class(PointGridType), intent(inout) :: obj
    type(IntVectorType), dimension(:), intent(in) :: gridAddresses
    class(PolytopeComplexInterface), dimension(:), target, &
         intent(in) :: polytopeComplexes
    class(LogType), intent(inout), optional :: log
    ! nothing to do
  end subroutine breakUp_PointGrid
  
  
  pure subroutine pointToComplex_PointGrid( obj, gridAddress, &
       polytopeComplex, log )
    class(PointGridType), intent(inout) :: obj
    type(IntVectorType), intent(in) :: gridAddress
    class(PolytopeComplexInterface), pointer, intent(out) :: &
         polytopeComplex
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'pointToComplex_PointGrid', log )

    ! nothing to do
    nullify(polytopeComplex)
    call endSub( log )
  end subroutine pointToComplex_PointGrid
  

  pure function getCartesianCode_PointGrid( obj ) result ( &
       cartesianCode )
    class(PointGridType), intent(in) :: obj
    type(LogicalVectorType) :: cartesianCode
    
    cartesianCode = .false.
  end function getCartesianCode_PointGrid

  
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_WRITEPOINTS({Grid})})
    
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_DEPARTUREPOINTS({Point}, {Grid})})

  EXPAND({METHODDEFS_POLYTOPECOLLECTION_APPENDMOMENTS({Point}, {Grid})})
  
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_NOTAPPLICABLE({Point},
  {Grid}, {Face})})

  EXPAND({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_NOTAPPLICABLE({Point},
  {Grid}, {Cell})})

  
  !-------------------------------------------------------------------
  !- UnboundedOrthotopeGridType procedures 
  !-------------------------------------------------------------------

  subroutine createOrthotopeGrid( og, constituentPolytopeGrids, log )
    class(PolytopeGridInterface), allocatable, intent(out) :: og
    type(PolytopeGridPointerType), dimension(NDIM), intent(in) :: &
         constituentPolytopeGrids
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createOrthotopeGrid', log )
    
    ! allocate
    if ( allocated(og) ) then
       call addEvent( FATAL, 'og already allocated.', log )
    else
       allocate( UnboundedOrthotopeGridType :: og, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &UnboundedOrthotopeGridType :: og.  STAT='//int2str(stat), &
            log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( og )
    class is (UnboundedOrthotopeGridType)
       call og%init( constituentPolytopeGrids, log )
    end select
    call endSub( log )
  end subroutine createOrthotopeGrid

  
  subroutine initUnboundedOrthotopeGrid( obj, constituentPolytopeGrids, &
       log, exteriorPolytopeGrids )
    class(UnboundedOrthotopeGridType), intent(out) :: obj
    type(PolytopeGridPointerType), dimension(NDIM), intent(in) :: &
         constituentPolytopeGrids
    class(LogType), intent(inout), optional :: log
    type(PolytopeGridPointerType), dimension(NDIM, 2), intent(in), &
         optional :: exteriorPolytopeGrids
    type(GridParametersType) :: gridParams, gp
    type(IntVectorType) :: gridSize
    real(FLOAT) :: val
    integer :: i, j, ii, polytopeDim, allocStat
    logical :: gridSizeIsSet
    type(PolytopeGridPointerType) :: nullGridPtr
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, 'initUnboundedOrthotopeGrid', log )
    
    ! establish the grid size.  Note that this is not simply the
    ! domain grid size; it will depend on the type of elements and
    ! whether this is the domain interior or a Cartesian boundary.
    gridSizeIsSet = .false. 
    do i = 1, NDIM
       if ( .not. constituentPolytopeGrids(i)%associated() ) cycle
       gp = constituentPolytopeGrids(i)%pg%getGridParameters()
       if ( .not. gridSizeIsSet ) then
          gridParams%size = gp%size-unitVector(i)
          gridSizeIsSet = .true.
       else
          call addEvent( any(gp%size-unitVector(&
               i) /= gridParams%size), FATAL, 'Constituent orthotope &
               &grid sizes are in conflict.  grid size #'//str(i-&
               1)//' = ('//str(gridParams%size+unitVector(&
               i))//') --> ('//str(gridParams%size)//'); grid size #'//&
               str(i)//' = ('//str(gp%size)//') --> ('//str(gp%size-&
               unitVector(i))//');', log )
       end if
    end do
    call addEvent( .not. gridSizeIsSet, FATAL, 'Grid size was &
         &not set.', log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if
    
    ! if okay to this point, set this subclass' components
    obj%constituents = constituentPolytopeGrids

    ! set other grid parameters
    do j = 1, 2
       do i = 1, NDIM
          
          ! first the extents
          if ( obj%constituents(i)%associated() ) then
             ! if no exterior, use constituent grid extents
             gp = obj%constituents(i)%pg%getGridParameters()
             val = gp%extents(j)%getValue(i)
             
          else
             ! the grid may be infinitesimal in dimension i, in
             ! which case any min/max coords can be used
             do ii = 1, NDIM
                if ( obj%constituents(ii)%associated() ) then
                   gp = obj%constituents(ii)%pg%getGridParameters()
                   val = gp%extents(j)%getValue(i)
                   exit
                end if
             end do
          end if
          call gridParams%extents(j)%setElement(i, val)
          
       end do
    end do
 
    ! now the spacing.  any of the constituents can be used.
    do i = 1, NDIM
       do ii = 1, NDIM
          if ( obj%constituents(ii)%associated() ) then
             gp = obj%constituents(ii)%pg%getGridParameters()
             val = gp%spacing%getValue(i)
             exit
          end if
       end do
       call gridParams%spacing%setElement(i, val)
    end do
    
    ! initialise the n-dimensional structured data
    call obj%cutInfo%init( gridParams%size, log )
    
    ! initialise the inherited components
    polytopeDim = count( constituentPolytopeGrids%associated() )
    call obj%initPolytopeGrid( gridParams, polytopeDim, log )

    call addEvent( DEBUG_MODE, ADVICE, 'Initialised &
         &a grid of size ('//str(gridParams%size)//') from constituent &
         &grids ('//str(constituentPolytopeGrids%associated())//')', log )
    call endSub( log )
  end subroutine initUnboundedOrthotopeGrid

  
  subroutine deinitUnboundedOrthotopeGrid( obj, log )
    class(UnboundedOrthotopeGridType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    type(PolytopeGridPointerType) :: nullGridPtr
    integer :: deallocStat
    call beginSub( MOD_NAME, &
         'deinitUnboundedOrthotopeGrid', log )
    
    ! inherited components
    call obj%deinitPolytopeGrid( log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

    ! components unique to this class
    obj%constituents = nullGridPtr

    call obj%cutInfo%deinit( log )
    
    call destroy( obj%departurePoints, log )

    if ( allocated(obj%faceExtension) ) then
       call destroy( obj%faceExtension, log )
    end if
    
    if ( allocated(obj%cellExtension) ) then
       call destroy( obj%cellExtension, log )
    end if
    call endSub( log )
  end subroutine deinitUnboundedOrthotopeGrid
  
  
  pure recursive subroutine collectSubPolytopes_UnboundedOrthotopeGrid( &
       obj, index, pps, log, depth )
    class(UnboundedOrthotopeGridType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    type(IntVectorType) :: gridAddress
    integer :: iDim, iPole
    class(PolytopeGridInterface), pointer :: pg
    type(PolytopePointerType) :: pp0, pp1
    call beginSub( MOD_NAME, &
         'collectSubPolytopes_UnboundedOrthotopeGrid', log )
    
    gridAddress = obj%convertToGridAddress( index )
    
    ! recurse over any resident simplicial complex?  No, not yet.
    ! Just the orthotope boundaries for now.  Also make sure that the
    ! presence of exterior boundaries fails tests, because they will
    ! affect relative grid addresses.
    do iDim = 1, NDIM
       if ( .not. obj%constituents(iDim)%associated() ) cycle
       pg => obj%constituents(iDim)%pg
       
       ! two boundaries exist in this dimension: one forward, one
       ! backward.
       call pp0%init( pg, gridAddress )
       call pp1%init( pg, gridAddress + unitVector(iDim) )
       
       call pps%addRecursively( pp0, log, depth )
       call pps%addRecursively( pp1, log, depth )
    end do
    
    call endSub( log )
  end subroutine collectSubPolytopes_UnboundedOrthotopeGrid


  pure recursive subroutine collectSuperPolytopes_UnboundedOrthotopeGrid( obj, &
       index, pps, log, height )
    class(UnboundedOrthotopeGridType), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    type(PolytopePointerType) :: pp
    call beginSub( MOD_NAME, 'collectSuperPolytopes_UnboundedOrthotopeGrid', log )
    ! delegate
    call obj%superPolytopesManager%collectSuperPolytopes( index, &
         pps, log, height )
    call endSub( log )
  end subroutine collectSuperPolytopes_UnboundedOrthotopeGrid


  pure recursive subroutine getCentroid_UnboundedOrthotopeGrid( obj, &
       index, coords, log )
    class(UnboundedOrthotopeGridType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out) :: coords
    class(LogType), intent(inout), optional :: log
    type(GridParametersType) :: gp
    type(IntVectorType) :: a
    real(FLOAT), dimension(NDIM) :: correction
    call beginSub( MOD_NAME, 'getCentroid_UnboundedOrthotopeGrid', log )

    a = obj%convertToGridAddress( index )
    gp = obj%getGridParameters()

    ! any deficiency in the Cartesian space will require us to project the
    ! otherwise full-dimensional centroid coordinates onto the appropriate
    ! plane(s).  In other words, the centroid coordinates are offset by
    ! 0.5*spacing in the respective direction(s).
    where ( obj%constituents%associated() )
       correction = 0.5_FLOAT
    elsewhere
       correction = 1._FLOAT
    end where

    coords = gp%extents(1) + ( real(a) - vector(correction) )*gp%spacing
    
    call endSub( log )
  end subroutine getCentroid_UnboundedOrthotopeGrid
      

  pure subroutine getSize_UnboundedOrthotopeGrid( obj, index, size, log )
    class(UnboundedOrthotopeGridType), intent(inout) :: obj
    integer, intent(in) :: index
    real(FLOAT), intent(out) :: size
    class(LogType), intent(inout), optional :: log
    type(GridParametersType) :: gp
    gp = obj%getGridParameters()
    size = product( gp%spacing%getValues(), &
         mask=obj%constituents%associated() )
  end subroutine getSize_UnboundedOrthotopeGrid
  
  
  pure function isAssociatedWithRegion_UnboundedOrthotopeGrid( obj, region )
    class(UnboundedOrthotopeGridType), intent(in) :: obj
    class(RegionInterface), intent(in) :: region
    logical :: isAssociatedWithRegion_UnboundedOrthotopeGrid
    
    isAssociatedWithRegion_UnboundedOrthotopeGrid = .false.
    
  end function isAssociatedWithRegion_UnboundedOrthotopeGrid

  
  subroutine breakUp_UnboundedOrthotopeGrid( obj, gridAddresses, &
       polytopeComplexes, log )
    class(UnboundedOrthotopeGridType), intent(inout) :: obj
    type(IntVectorType), dimension(:), intent(in) :: gridAddresses
    class(PolytopeComplexInterface), dimension(:), target, &
         intent(in) :: polytopeComplexes
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, &
         'breakUpUnboundedOrthotopeGrid', log )
    
    do i = 1, size(gridAddresses)
       call obj%cutInfo%addIndex( gridAddresses(i), i )
    end do
    obj%simplicialComplexes => polytopeComplexes
    
    call endSub( log )
  end subroutine breakUp_UnboundedOrthotopeGrid
  
  
  pure subroutine pointToComplex_UnboundedOrthotopeGrid( obj, &
       gridAddress, polytopeComplex, log )
    class(UnboundedOrthotopeGridType), intent(inout) :: obj
    type(IntVectorType), intent(in) :: gridAddress
    class(PolytopeComplexInterface), pointer, intent(out) :: &
         polytopeComplex
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'pointToComplex_UnboundedOrthotopeGrid', log )

    i = obj%cutInfo%getIndex( gridAddress )
    if ( i > 0 ) then
       polytopeComplex => obj%simplicialComplexes(i)
    else
       nullify(polytopeComplex)
    end if
    
    call endSub( log )
  end subroutine pointToComplex_UnboundedOrthotopeGrid
  

  pure function getCartesianCode_UnboundedOrthotopeGrid( obj ) result ( &
       cartesianCode )
    class(UnboundedOrthotopeGridType), intent(in) :: obj
    type(LogicalVectorType) :: cartesianCode

    cartesianCode = vector( obj%constituents%associated() )
  end function getCartesianCode_UnboundedOrthotopeGrid

  
  subroutine writePointLocations_UnboundedOrthotopeGrid( obj, &
       stringList, nPointsRunning, precision, indent, log )
    class(UnboundedOrthotopeGridType), intent(inout) :: obj
    type(StringListType), intent(inout) :: stringList
    integer, intent(inout) :: nPointsRunning
    integer, intent(in), optional :: precision, indent
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'writePointLocations_UnboundedOrthotopeGrid', log )
    
    ! nothing to do
    call addEvent( ADVICE, 'Not applicable.', log )

    call endSub( log )
  end subroutine writePointLocations_UnboundedOrthotopeGrid

  
  subroutine writePointConnectivity_UnboundedOrthotopeGrid( obj, &
       connectivityList, offsetList, nPolytopesRunning, startFromZero, &
       indent, log )
    class(UnboundedOrthotopeGridType), intent(inout) :: obj
    type(StringListType), intent(inout) :: connectivityList, offsetList
    integer, intent(inout) :: nPolytopesRunning
    logical, intent(in) :: startFromZero
    integer, intent(in), optional :: indent
    class(LogType), intent(inout), optional :: log
    integer :: i, i1, i2, iostat, t
    type(StringListIteratorType) :: sli
    type(StringType) :: s
    call beginSub( MOD_NAME, &
         'writePointConnectivity_UnboundedOrthotopeGrid', log )

    call addEvent( FATAL, 'Not implemented yet.', log )
!!$    ! delegate to ...
!!$    call obj%cutInfo%writePointConnectivity( obj, connectivityList, &
!!$         offsetList, nPolytopesRunning, startFromZero, indent, log )
       
    call endSub( log )
  end subroutine writePointConnectivity_UnboundedOrthotopeGrid


  pure subroutine appendConnection_UnboundedOrthotopeGrid( obj, string, &
       polytopeIndex, startFromZero, indent )
    class(UnboundedOrthotopeGridType), intent(in) :: obj
    character(*), intent(inout) :: string
    integer, intent(in)  :: polytopeIndex
    logical, intent(in)  :: startFromZero
    integer, intent(in), optional :: indent
    ! n/a
  end subroutine appendConnection_UnboundedOrthotopeGrid

  
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_DEPARTUREPOINTS(
  {UnboundedOrthotope}, {Grid})})

  EXPAND({METHODDEFS_POLYTOPECOLLECTION_APPENDMOMENTS_WITHEXTENSIONS(
  {UnboundedOrthotope}, {Grid})})

  EXPAND({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_POSITIVE(
  {UnboundedOrthotope}, {Grid}, {Face})})
  
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_POSITIVE(
  {UnboundedOrthotope}, {Grid}, {Cell})})

  
end module PolytopeCollectionsModule

