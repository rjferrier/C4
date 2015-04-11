module PolytopeComplexesModule

  use LogModule
  use Global
  use FiniteVolumeModule
  
  implicit none
  private
  public :: createUnmixedSimplicialComplex, &
       createUnmixedSimplicialComplexArray
  
  character(*), parameter :: MOD_NAME = 'PolytopeComplexesModule'

  !-------------------------------------------------------------------
  !- PolytopeComplexInterface implementations
  !-------------------------------------------------------------------
  
  ! helper class for UnmixedSimplicialComplexType.  It does NOT need
  ! to implement PolytopeArrayInterface (yet).
  type :: PolytopeSubArrayType
     class(PolytopeArrayInterface), pointer :: polytopeArray
     integer, dimension(:), allocatable :: indices
   contains
     procedure :: initPolytopeSubArray_fromIndices
     procedure :: initPolytopeSubArray_fromSet
     generic :: init => initPolytopeSubArray_fromIndices, &
          initPolytopeSubArray_fromSet
     procedure :: size => size_PolytopeSubArray
     procedure :: collectSubPolytopes
     procedure :: deinit => deinitPolytopeSubArray
  end type PolytopeSubArrayType


  ! the following is a simplicial complex whose elements conform to the
  ! following rule:
  ! 1. Discard all Cartesian elements, including unstructured elements
  !    coincident with Cartesian gridlines.  They will not be needed
  !    in the simplex-locating test.
  ! 2. Separate elements according to dimension.
  ! 3. For each dimension, the set of elements must all belong to the
  !    same PolytopeArray.
  type, extends(PolytopeComplexInterface), public :: UnmixedSimplicialComplexType
     private
     ! the first element in this array corresponds to a sub-array of
     ! simplicies the same dimension as the complex.  Subsequent
     ! elements correspond to decreasing dimensions until we reach a
     ! sub-array of points.
     type(PolytopeSubArrayType), dimension(:), allocatable :: &
          polytopeSubArrays
     class(FaceComplexInterface), allocatable :: faceExtension
     class(CellComplexInterface), allocatable :: cellExtension
   contains
     ! for init/deinit, have two points of access each: one for
     ! undecorated (init) and one that subclasses can call (init...)
     procedure :: initUnmixedSimplicialComplex
     procedure :: init => initUnmixedSimplicialComplex
     procedure :: getPolytopes_asArray => getUnmixedSimplicies_asArray
     procedure :: getPolytopes_asSet => getUnmixedSimplicies_asSet
     procedure :: deinitUnmixedSimplicialComplex
     procedure :: deinit => deinitUnmixedSimplicialComplex
     
     ! extension methods
     procedure :: setExtension_Faces => &
          setExtension_Faces_UnmixedSimplicialComplex
     procedure :: setExtension_Cells => &
          setExtension_Cells_UnmixedSimplicialComplex
     procedure :: getComplexExtension_Faces => &
          getComplexExtension_Faces_UnmixedSimplicialComplex
     procedure :: getComplexExtension_Cells => &
          getComplexExtension_Cells_UnmixedSimplicialComplex
  end type UnmixedSimplicialComplexType

  

contains
  
  !-------------------------------------------------------------------
  !- PolytopeSubArray methods
  !-------------------------------------------------------------------

  subroutine initPolytopeSubArray_fromIndices( obj, polytopeArray, &
       indices, log )
    class(PolytopeSubArrayType), intent(out) :: obj
    class(PolytopeArrayInterface), intent(in), target :: polytopeArray
    integer, dimension(:), intent(in) :: indices
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, &
         'initPolytopeSubArray_fromIndices', log )
    
    obj%polytopeArray => polytopeArray

    allocate( obj%indices(size(indices)), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &elements of obj%indices.  STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

    obj%indices = indices
    
    call endSub( log ) 
  end subroutine initPolytopeSubArray_fromIndices
  
  
  subroutine initPolytopeSubArray_fromSet( obj, polytopeArray, &
       polytopePointerSet, log )
    class(PolytopeSubArrayType), intent(out) :: obj
    class(PolytopeArrayInterface), intent(in), target :: polytopeArray
    type(PolytopePointerSetType), intent(inout) :: polytopePointerSet
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'initPolytopeSubArray_fromSet', log )
    
    obj%polytopeArray => polytopeArray

    call polytopePointerSet%query( polytopeArray, obj%indices, &
         log )
    
    call endSub( log ) 
  end subroutine initPolytopeSubArray_fromSet
  
  
  pure function size_PolytopeSubArray( obj )
    class(PolytopeSubArrayType), intent(in) :: obj
    integer :: size_PolytopeSubArray
    size_PolytopeSubArray = size( obj%indices )
  end function size_PolytopeSubArray
 
  
  ! this does a sweep over all subdimensional polytopes connected to
  ! the elements in a PolytopeSubArray.  However, more sophisticated
  ! discrimination of the members of a SimplicialComplex is not
  ! addressed here.
  subroutine collectSubPolytopes( obj, pps, log )
    class(PolytopeSubArrayType), intent(inout) :: obj
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer :: iSub
    type(PolytopePointerType) :: pp
    call beginSub( MOD_NAME, &
         'collectSubPolytopes', log )
    
    ! loop over polytopes in the subarray
    do iSub = 1, obj%size()
       pp = ptr( obj%polytopeArray, obj%indices(iSub) )
       ! delegate the collection operation to each of these in turn.
       ! Duplicates are conveniently taken care of as part of the
       ! method.
       call pp%collectSubPolytopes( pps, log )
    end do

    call endSub( log ) 
  end subroutine collectSubPolytopes


 
  subroutine deinitPolytopeSubArray( obj, log )
    class(PolytopeSubArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'deinitPolytopeSubArray', log )
    
    call endSub( log ) 
  end subroutine deinitPolytopeSubArray
  
  
  !-------------------------------------------------------------------
  !- UnmixedSimplicialComplex methods
  !-------------------------------------------------------------------

  !> Creates multiple PXs.
  !
  !> The extra argument, nIndiciesPerComplex, specifies how the
  !> indicies are to be divided up between the complexes.  There are
  !> the same number of complexes as elements in this argument, and
  !> the contents should add up to the length in indicies.
  subroutine createUnmixedSimplicialComplexArray( usxa, &
       polytopeArray, indicies, nIndiciesPerComplex, log )
    class(PolytopeComplexInterface), dimension(:), allocatable, &
         intent(out) :: usxa
    class(PolytopeArrayInterface), intent(in) :: &
         polytopeArray
    integer, dimension(:), intent(in) :: indicies
    integer, dimension(:), intent(in) :: nIndiciesPerComplex
    class(LogType), intent(inout), optional :: log
    integer :: allocStat, nComplexes, iComplex, i1, i2
    call beginSub( MOD_NAME, &
         'createUnmixedSimplicialComplexArray', log )
    
    ! inputs check
    call addEvent( sum(&
         nIndiciesPerComplex) /= size(indicies), FATAL, 'The &
         &elements in nIndiciesPerComplex should add up to the &
         &number of elements in indicies.', log )
    
    ! allocation
    nComplexes = size( nIndiciesPerComplex )
    allocate( UnmixedSimplicialComplexType :: usxa(nComplexes), &
         stat=allocStat )
    call addEvent( allocStat/=0, FATAL, &
         'Problem allocating elements '//str(nComplexes)//'and &
         &type (UnmixedSimplicialComplexType) of usxa.  STAT='//&
         int2str(allocStat), log )
    
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

    select type ( usxa )
    class is (UnmixedSimplicialComplexType)
       i2 = 0
       do iComplex = 1, nComplexes
          i1 = i2 + 1
          i2 = i1 + nIndiciesPerComplex( iComplex ) - 1
          call usxa(iComplex)%init( polytopeArray, indicies([i1:&
               i2]), log )
       end do
    end select

    call endSub( log ) 
  end subroutine createUnmixedSimplicialComplexArray
  
  
  subroutine createUnmixedSimplicialComplex( usx, polytopeArray, &
       topSimplexIndices, log )
    class(PolytopeComplexInterface), allocatable, intent(out) :: usx
    class(PolytopeArrayInterface), intent(in), target :: polytopeArray
    integer, dimension(:), intent(in) :: topSimplexIndices
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createUnmixedSimplicialComplex', log )
    
    ! allocate
    if ( allocated(usx) ) then
       call addEvent( FATAL, 'usx already allocated.', log )
    else
       allocate( UnmixedSimplicialComplexType :: usx, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &UnmixedSimplicialComplexType :: usx.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( usx )
    class is (UnmixedSimplicialComplexType)
       call usx%init( polytopeArray, topSimplexIndices, log )
    end select
    call endSub( log )
  end subroutine createUnmixedSimplicialComplex

  
  !> initialisation routine.
  !
  !> the arguments refer to simplicies at the topmost level;
  !> polytopeArray is the collection that the simplicies belong to,
  !> while topSimplexIndices specify their addresses in the
  !> collection.
  subroutine initUnmixedSimplicialComplex( obj, polytopeArray, &
       topSimplexIndices, log )
    class(UnmixedSimplicialComplexType), intent(out) :: obj
    class(PolytopeArrayInterface), intent(in), target :: polytopeArray
    integer, dimension(:), intent(in) :: topSimplexIndices
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerSetType) :: pps
    class(PolytopeArrayInterface), pointer :: pa
    integer :: allocStat, objDim, nLevels, i 
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, &
         'initUnmixedSimplicialComplex', log )
    
    ! infer the number of levels.  This is the number of dimensions
    ! plus one (because we count the 0th-dimensional simplicies).
    nLevels = polytopeArray%getPolytopeDim() + 1
    
    allocate( obj%polytopeSubArrays(nLevels), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &elements of obj%polytopeSubArrays.  STAT='//int2str(&
         allocStat), log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

    ! initialise the top level simplicies
    pa => polytopeArray
    call obj%polytopeSubArrays(1)%init( pa, topSimplexIndices, &
         log )
    
    ! indiscriminately collect all subpolytopes belonging to the
    ! top level simplicies.
    call obj%polytopeSubArrays(1)%collectSubPolytopes( pps, log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if
       
    ! go through the remaining levels populating the corresponding
    ! elements of obj%polytopeSubArrays.  Be careful -
    ! subPolytopeArray and polytopeSubArray are different things
    ! (have not thought of better names yet)
    i = 2
    do
       call pa%pointToSubPolytopeArray( pa )
       if ( i > nLevels .or. .not. associated(pa) ) exit
       
       call obj%polytopeSubArrays(i)%init( pa, pps, log )
       if ( checkSub(FATAL, log) ) then
         call endSub( log )
         return
       end if
       i = i + 1
    end do
    call addEvent( i < nLevels, WARNING, 'Loop through the &
         &subPolytopeCollections exited early', log )
    call addEvent( DEBUG_MODE, ADVICE, 'Loop through the &
         &subPolytopeCollections exited at i = '//int2str(i), log )
    
    call pps%deinit( log )

    call endSub( log ) 
  end subroutine initUnmixedSimplicialComplex

  
  subroutine getUnmixedSimplicies_asArray( obj, depth, &
       ppArray, log )
    class(UnmixedSimplicialComplexType), intent(in), target :: obj
    integer, intent(in) :: depth
    type(PolytopePointerType), dimension(:), allocatable, &
         intent(out) :: ppArray
    class(LogType), intent(inout), optional :: log
    type(PolytopeSubArrayType), pointer :: psa
    integer :: i, allocStat
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, 'getUnmixedSimplicies_asArray', log )
    
    psa => obj%polytopeSubArrays(depth + 1)

    allocate( ppArray(psa%size()), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &elements of ppArray.  STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if
    
    call addEvent( DEBUG_MODE, ADVICE, 'Members of set:', log )
    do i = 1, psa%size()
       ppArray(i) = ptr( psa%polytopeArray, psa%indices(i) )
       call addEvent( DEBUG_MODE, ADVICE, ppArray(i)%describe(), &
            log )
    end do

    call endSub( log ) 
  end subroutine getUnmixedSimplicies_asArray
  

  
  subroutine getUnmixedSimplicies_asSet( obj, depth, ppSet, &
       log )
    class(UnmixedSimplicialComplexType), intent(in), target :: obj
    integer, intent(in) :: depth
    type(PolytopePointerSetType), intent(out) :: ppSet
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerType), dimension(:), allocatable :: &
         ppArray
    integer :: i, deallocStat
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, &
         'getUnmixedSimplicies_asSet', log )
    
    ! part-delegate to the other getPolytopes method
    call obj%getPolytopes( depth, ppArray, log )
    
    call ppSet%init( ppArray, log )

    deallocate( ppArray, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem &
         &allocating elements of ppArray.  STAT='//int2str(&
         deallocStat), log )
    
    call endSub( log ) 
  end subroutine getUnmixedSimplicies_asSet


  
  subroutine deinitUnmixedSimplicialComplex( obj, log )
    class(UnmixedSimplicialComplexType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat, i
    call beginSub( MOD_NAME, 'deinitUnmixedSimplicialComplex', log )
    
    do i = 1, size(obj%polytopeSubArrays)
       call obj%polytopeSubArrays(i)%deinit( log )
    end do

    deallocate( obj%polytopeSubArrays, stat=deallocStat )
    call addEvent( deallocStat/=0, FATAL, 'Problem &
         &deallocating elements of obj%polytopeSubArrays.  STAT='//&
         int2str(deallocStat), log )

    call destroy( obj%cellExtension, log )
    call destroy( obj%faceExtension, log )
    
    call endSub( log ) 
  end subroutine deinitUnmixedSimplicialComplex

    

  subroutine setExtension_Faces_UnmixedSimplicialComplex( obj, &
       faceComplex, log )
    class(UnmixedSimplicialComplexType), intent(inout) :: obj
    class(FaceComplexInterface), allocatable, intent(inout) :: &
         faceComplex
    class(LogType), intent(inout), optional :: log
    integer :: i, allocStat
    call beginSub( MOD_NAME, &
         'createFaceExtension_UnmixedSimplicialComplex', log )
    
    call addEvent( allocated(obj%faceExtension), FATAL, 'Face &
         &extension already exists.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call checkInjection( allocated(faceComplex), &
         allocated(obj%faceExtension), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( faceComplex, obj%faceExtension )

    call endSub( log )
  end subroutine setExtension_Faces_UnmixedSimplicialComplex

  
  subroutine setExtension_Cells_UnmixedSimplicialComplex( obj, cellComplex, log )
    class(UnmixedSimplicialComplexType), intent(inout) :: obj
    class(CellComplexInterface), allocatable, intent(inout) :: cellComplex
    class(LogType), intent(inout), optional :: log
    integer :: i, allocStat
    call beginSub( MOD_NAME, 'createCellExtension_UnmixedSimplicialComplex', log )

    call addEvent( allocated(obj%cellExtension), FATAL, 'Cell &
         &extension already exists.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call checkInjection( allocated(cellComplex), &
         allocated(obj%cellExtension), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( cellComplex, obj%cellExtension )

    call endSub( log )
  end subroutine setExtension_Cells_UnmixedSimplicialComplex


  pure subroutine getComplexExtension_Faces_UnmixedSimplicialComplex( obj, &
       faceComplex, log )
    class(UnmixedSimplicialComplexType), intent(inout), target :: obj
    class(FaceComplexInterface), pointer, intent(inout) :: &
         faceComplex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'getComplexExtension_Faces_UnmixedSimplicialComplex', log )

    if ( allocated(obj%faceExtension) ) then
       faceComplex => obj%faceExtension
    end if
    
    call endSub( log )
  end subroutine getComplexExtension_Faces_UnmixedSimplicialComplex

  
  pure subroutine getComplexExtension_Cells_UnmixedSimplicialComplex( obj, &
       cellComplex, log )
    class(UnmixedSimplicialComplexType), intent(inout), target :: obj
    class(CellComplexInterface), pointer, intent(inout) :: &
         cellComplex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'getComplexExtension_Cells_UnmixedSimplicialComplex', log )

    if ( allocated(obj%cellExtension) ) then
       cellComplex => obj%cellExtension
    end if

    call endSub( log )
  end subroutine getComplexExtension_Cells_UnmixedSimplicialComplex
  
  
 
end module PolytopeComplexesModule
