HEADER()
module CellCollectionsModule

  USE_MACROS({IntegrableElements})
  use Global
  use LogModule
  use FiniteVolumeModule
  use FlowFieldModule

  implicit none
  private
  public :: attachCellArray, attachCellGrid, attachCellComplex, &
       createInteriorIntAveScalarMomentGroup, &
       createInteriorIntAveVectorMomentGroup
  
  character(*), parameter :: MOD_NAME = &
       'CellCollectionsModule'
  
  integer, parameter :: CELL_DEPTH = 0, FACE_DEPTH = 1

  integer :: i
  integer, parameter :: MAX_CELLS_PER_COMPLEX = 10
  integer, dimension(MAX_CELLS_PER_COMPLEX), parameter  :: &
       CELL_INDEX_VECTOR = [(i, i=1, MAX_CELLS_PER_COMPLEX)]

  logical, parameter :: SAFE_MODE = .true.
  
  
  !-------------------------------------------------------------------
  !- InteriorIntAveMoments
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_SCALARINTAVEMOMENT_CONCRETE(Interior, MomentField)})

  EXPAND({TYPEDEF_VECTORINTAVEMOMENT_CONCRETE(Interior, MomentField)})

  
  !-------------------------------------------------------------------
  !- CellArray
  !-------------------------------------------------------------------
  
  EXPAND({TYPEDEF_CELLCOLLECTION_CONCRETE({Array}, {}, {
     procedure :: initialiseScalarFields => &
          initialiseScalarFields_CellArray
     procedure :: initialiseVectorFields => &
          initialiseVectorFields_CellArray})})
  
  !-------------------------------------------------------------------
  !- CellGrid
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_CELLCOLLECTION_CONCRETE({Grid},
     {
     class(IntAveVectorMomentFieldInterface), allocatable :: &
          intAveVelocityMomentField
     type(IntAveScalarMomentFieldListType) :: &
          intAveScalarMomentFieldList},
     {
     procedure :: locate => locate_CellGrid
     procedure :: writeCellData => writeCellData_CellGrid
     procedure :: coords2address
     procedure :: initialiseScalarFields => &
          initialiseScalarFields_CellGrid
     procedure :: initialiseVectorFields => &
          initialiseVectorFields_CellGrid})})
  
  !-------------------------------------------------------------------
  !- CellComplex
  !-------------------------------------------------------------------

  type, extends(CellComplexInterface), public :: CellComplexType
     private
     ! we define our complex as mapping to an element in some collection.
     ! This allows us to defer certain properties of the complex to a
     ! large, possibly structured collection of otherwise primitive
     ! elements.  The following pointer object represents the placeholder
     ! element.
     type(CellPointerType) :: parentCell
     
     ! in the next two matrices, rows correspond to cells and columns
     ! to faces.  The connectivityMatrix is self-explanatory; the
     ! positionalMatrix describes whether each cell lies in the same
     ! direction as each face normal or not.
     type(PolytopePointerType), dimension(:), allocatable :: cellPolytopes
     type(PolytopePointerType), dimension(:), allocatable :: facePolytopes
     ! cellPolytopes/facePolytopes are distinct from cells/faces in that
     ! the former are polytope pointers with corresponding cell/face
     ! extensions.  The latter are the extensions.
     type(CellPointerType), dimension(:), allocatable :: cells
     type(FacePointerType), dimension(:), allocatable :: faces
     logical, dimension(:, :), allocatable :: connectivity
     logical, dimension(:, :), allocatable :: positions
     logical, dimension(:, :), allocatable :: working
   contains
     procedure :: init => init_CellComplex
     procedure :: deinit => deinit_CellComplex
     procedure :: locate => locate_CellComplex
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_CellComplex
     procedure :: appendScalarMoments => appendScalarMoments_CellComplex
     procedure :: initialiseScalarFields => &
          initialiseScalarFields_CellComplex
     procedure :: initialiseVectorFields => &
          initialiseVectorFields_CellComplex
     procedure, private  :: createSimplexLocatingMatrices
  end type CellComplexType

  
contains  
  
  !-------------------------------------------------------------------
  !- ScalarInteriorIntAveMoment methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_INTAVESCALARMOMENT(Interior, MomentField)})
  
  elemental subroutine getValue_InteriorIntAveScalarMomentGroup( obj, &
       value, momentIndex )
    class(InteriorIntAveScalarMomentGroupType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: momentIndex
    integer :: i
    i = obj%interiorGaussianPointGroupPointer%getPolytopeIndex()
    value = obj%intAveScalarMomentField%getValue( momentIndex )
  end subroutine getValue_InteriorIntAveScalarMomentGroup
  

  pure subroutine getValues_InteriorIntAveScalarMomentGroup( obj, values )
    class(InteriorIntAveScalarMomentGroupType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer :: i
    i = obj%interiorGaussianPointGroupPointer%getPolytopeIndex()
    call obj%intAveScalarMomentField%getValues( values, i )
  end subroutine getValues_InteriorIntAveScalarMomentGroup
  

  !-------------------------------------------------------------------
  !- VectorInteriorIntAveMoment methods
  !-------------------------------------------------------------------
  
  EXPAND({METHODDEFS_INTAVEVECTORMOMENT(Interior, MomentField,
  {, obj%hasCoupledComponents})})
  
  elemental subroutine getValue_InteriorIntAveVectorMomentGroup( obj, &
       value, momentIndex, componentIndex )
    class(InteriorIntAveVectorMomentGroupType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: momentIndex
    integer, intent(in) :: componentIndex

    ! tk
    value = 0._FLOAT
  end subroutine getValue_InteriorIntAveVectorMomentGroup
  

  pure subroutine getValues_InteriorIntAveVectorMomentGroup( obj, &
       values, componentIndex )
    class(InteriorIntAveVectorMomentGroupType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: componentIndex

    ! tk
    values = 0._FLOAT
  end subroutine getValues_InteriorIntAveVectorMomentGroup

  
  !-------------------------------------------------------------------
  !- CellArray methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_ATTACH_CELLCOLLECTION({Array}, {}, {
    type(ScalarMomentGroupListType) :: smgList
    class(CellArrayInterface), pointer :: cellExt}, 
    {call polytopeArray%getArrayExtension( cellExt )
    call cellArray%appendScalarMoments( smgList, 1, log )
    call cellExt%appendScalarMoments( smgList, 1, log )})})
  
  EXPAND({METHODDEF_CELLCOLLECTION_INIT({Array})})
  
  EXPAND({METHODDEF_CELLCOLLECTION_DEINIT({Array})})
  
  EXPAND({METHODDEF_CELLCOLLECTION_COMPUTESUBROW({Array})})

  
  subroutine appendVelocityMoments_CellArray( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class(CellArrayType), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_CellArray', log )

    ! unstructured primitive cells do not hold flow information in this
    ! implementation.  Do nothing.
    
    call endSub( log )
  end subroutine appendVelocityMoments_CellArray

  
  subroutine appendScalarMoments_CellArray( obj, scalarMomentGroupList, &
       polytopeIndex, log )
    class(CellArrayType), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendScalarMoments_CellArray', log )

    ! unstructured primitive cells do not hold flow information in this
    ! implementation.  Do nothing.
    
    call endSub( log )
  end subroutine appendScalarMoments_CellArray
  

  subroutine initialiseScalarFields_CellArray( obj, &
       scalarFieldSpecifications, log )
    class(CellArrayType), intent(inout) :: obj
    type(ScalarFieldSpecificationListType), intent(in) :: &
         scalarFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initialiseScalarFields_CellArray', log )

    call addEvent( ADVICE, 'CellArrays do not hold flow information in &
         &this implementation.  Nothing to do.', log )
    
    call endSub( log )
  end subroutine initialiseScalarFields_CellArray
  

  subroutine initialiseVectorFields_CellArray( obj, &
       vectorFieldSpecifications, log )
    class(CellArrayType), intent(inout) :: obj
    type(VectorFieldSpecificationListType), intent(in) :: &
         vectorFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initialiseVectorFields_CellArray', log )
    
    call addEvent( ADVICE, 'CellArrays do not hold flow information in &
         &this implementation.  Nothing to do.', log )

    call endSub( log )
  end subroutine initialiseVectorFields_CellArray

  
  !-------------------------------------------------------------------
  !- CellGrid methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_ATTACH_CELLCOLLECTION({Grid},
    {intAveVelocityMomentField, intAveScalarMomentFieldList, }, {
    class(IntAveVectorMomentFieldInterface), allocatable, intent(&
         inout) :: intAveVelocityMomentField
    type(IntAveScalarMomentFieldListType), intent(inout) :: &
         intAveScalarMomentFieldList})})

    
  EXPAND({METHODDEF_CELLCOLLECTION_INIT({Grid},
    {intAveVelocityMomentField, intAveScalarMomentFieldList, }, {
    class(IntAveVectorMomentFieldInterface), allocatable, intent(&
         inout) :: intAveVelocityMomentField
    type(IntAveScalarMomentFieldListType), intent(inout) :: &
         intAveScalarMomentFieldList},
    {EXPAND({INJECT({intAveVelocityMomentField})})
    call obj%intAveScalarMomentFieldList%&
         takeNodes( intAveScalarMomentFieldList, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if})})
    
  
  EXPAND({METHODDEF_CELLCOLLECTION_DEINIT({Grid}, {}, {},
    {call destroy( obj%intAveVelocityMomentField, log )
    call obj%intAveScalarMomentFieldList%deinit( log )})})
  
  EXPAND({METHODDEF_CELLCOLLECTION_COMPUTESUBROW({Grid})})
  
  subroutine createIntAveVelocityMoment_CellGrid( obj, velocityMoment, &
       polytopeIndex, log )
    class(CellGridType), intent(inout) :: obj
    class(VectorMomentGroupInterface), allocatable, intent(inout) :: &
         velocityMoment
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    integer :: stat
    class(IntAveVectorMomentFieldInterface), pointer :: viamf
    class(FlowVariableInterface), pointer :: fv
    call beginSub( MOD_NAME, 'createIntAveVectorMoment_CellGrid', log )
    
    ! the CellGrid has a one-to-one relationship with a velocity
    ! IntAveVectorMomentField, so no iteration or flow variable
    ! matching will be needed.
    
    ! delegate straightforwardly to the appropriate creation
    ! procedure residing in the same module
    call createInteriorIntAveVectorMomentGroup( velocityMoment, obj%&
         interiorGaussianPointCollection, polytopeIndex, obj%&
         intAveVelocityMomentField, .true., log )
    
    call endSub( log )
  end subroutine createIntAveVelocityMoment_CellGrid
  
  
  EXPAND({
  METHODDEF_INTEGRABLEELEMENTCOLLECTION_CONCRETE_CREATEINTAVEMOMENT(
  {CellGrid}, {MomentField}, {Scalar}, {Interior}, {.true.})})
  
    
  elemental subroutine locate_CellGrid( obj, coords, pp, log )
    class(CellGridType), intent(inout) :: obj
    type(RealVectorType), intent(in) :: coords
    type(PolytopePointerType), intent(out) :: pp
    class(LogType), intent(inout), optional :: log
    class(PolytopeGridInterface), pointer :: pg
    class(PolytopeComplexInterface), pointer :: px
    type(IntVectorType) :: gridAddress, relationToGrid
    class(CellComplexInterface), pointer :: cx
    type(CellPointerType) :: cp
    call beginSub( MOD_NAME, 'locate_CellGrid', log )
    
    call obj%convert( coords, gridAddress, relationToGrid )
    if ( .not. all(relationToGrid == 0) ) then
       ! if outside the grid, return null pp and leave early
       call pp%deinit( )
       call endSub( log )
       return
    end if
    
    call obj%getGridOwner( pg )
    call pg%pointToComplex( gridAddress, px, log )
    if ( associated(px) ) then
       call px%getComplexExtension( cx, log )

       if ( .not. associated(cx) ) then
          ! assume the cell extension is already attached
          call addEvent( FATAL, 'There should be a cell extension &
               &attached to px.', log )
!!$          call cp%init_pure( obj, pg%convertToIndex(gridAddress) )
!!$          call attachCellComplex( px, cp, log )
!!$          call px%getComplexExtension( cx, log )
       end if
       
       call cx%locate( coords, pp, log )
    else
       call pp%init( pg, gridAddress )
    end if
    
    call endSub( log )
  end subroutine locate_CellGrid

  
  subroutine writeCellData_CellGrid( obj, stringList, log )
    class(CellGridType), intent(inout) :: obj
    type(StringListType), intent(in) :: stringList 
    class(LogType), intent(inout), optional :: log
    integer :: fID
    class(PolytopeGridInterface), pointer :: pg
    call beginSub( MOD_NAME, 'writeCellData_CellGrid', log )

    ! recover the associated polytope grid
    call obj%getGridOwner( pg )
    
    ! loop over all cells in the grid
    do i = 1, pg%getNumPolytopes()

       ! is the cell uncut, cut, or blanked out (solid)?

       ! should we just delegate cell topography to pg?
       

    end do
    
    call endSub( log )    
  end subroutine writeCellData_CellGrid

  
!!$  subroutine appendVelocityMoments_CellGrid( obj, &
!!$       velocityMomentGroupList, polytopeIndex, log )
!!$    class(CellGridType), intent(inout) :: obj
!!$    type(VectorMomentGroupListType), intent(inout) :: &
!!$         velocityMomentGroupList
!!$    integer, intent(in) :: polytopeIndex
!!$    class(LogType), intent(inout), optional :: log
!!$    integer :: allocStat
!!$    class(VectorMomentGroupInterface), allocatable :: vmg
!!$    logical, parameter :: DEBUG_MODE = .false.
!!$    call beginSub( MOD_NAME, &
!!$         'appendVelocityMoments_CellGrid', log )
!!$
!!$    ! there is just one velocity field owned by the cells, so no iteration
!!$    ! over a list is necessary.  Nor is matching of flow variables
!!$    ! required.  Only one VectorMomentGroup needs to be created.
!!$    call createInteriorIntAveVectorMoment( vmg, obj%&
!!$         interiorGaussianPointCollection, polytopeIndex, obj%&
!!$         intAveVelocityMomentField, .true., log )
!!$    if ( checkSub(FATAL, log) ) then
!!$       call endSub( log )
!!$       return
!!$    end if
!!$       
!!$    ! append to the list
!!$    call velocityMomentGroupList%append( vmg, log )
!!$    if ( checkSub(FATAL, log) ) then
!!$       call endSub( log )
!!$       return
!!$    end if
!!$
!!$    call endSub( log )
!!$  end subroutine appendVelocityMoments_CellGrid

  EXPAND({METHODDEF_APPENDMOMENTS({CellGrid}, {}, {Interior}, 
  {MomentField}, {IntAve}, {obj%interiorGaussianPointCollection}, 
  {Scalar}, {.true.})})

  EXPAND({METHODDEF_APPENDVELOCITYMOMENTS_SINGLESOURCE(
  {CellGrid}, {}, {Interior}, 
  {MomentField}, {IntAve}, {obj%interiorGaussianPointCollection},
  {.true.})})
  
  
  elemental subroutine coords2address( obj, coords, gridAddress, &
       relationToGrid )
    class(CellGridType), intent(inout) :: obj
    type(RealVectorType), intent(in) :: coords
    type(IntVectorType), intent(out) :: gridAddress
    ! the next argument is an optional output describing the point's
    ! location relative to the grid in each dimension.  0 means it
    ! falls inside; -1, it falls short; +1, it falls long.
    type(IntVectorType), intent(out), optional :: relationToGrid
    type(RealVectorType) :: work
    class(PolytopeGridInterface), pointer :: pg
    type(LogicalVectorType) :: isBelowMinExtent, isAboveMaxExtent
    type(IntVectorType) :: i1, i2
    type(GridParametersType) :: gp

    call obj%getGridOwner( pg )
    gp = pg%getGridParameters()
    work = ( coords - gp%extents(1) )/gp%spacing
    gridAddress = ceiling( work%getValues() )

    if ( present(relationToGrid) ) then
       i1 = -1
       i2 = 1
       call i1%setMask( coords < gp%extents(1) )
       call i2%setMask( coords > gp%extents(2) )
       ! can do a masked addition.  
       relationToGrid = i1 + i2
    end if
  end subroutine coords2address
  

  subroutine initialiseScalarFields_CellGrid( obj, &
       scalarFieldSpecifications, log )
    class(CellGridType), intent(inout) :: obj
    type(ScalarFieldSpecificationListType), intent(in) :: &
         scalarFieldSpecifications
    class(LogType), intent(inout), optional :: log
    type(RealVectorType), dimension(:, :), allocatable :: r
    real(FLOAT), dimension(:), allocatable :: v
    integer :: i, stat
    type(ScalarFieldSpecificationListIteratorType) :: sfsli
    class(ScalarFieldSpecificationInterface), pointer :: sfs
    type(IntAveScalarMomentFieldListIteratorType) :: iasmfli
    class(IntAveScalarMomentFieldInterface), pointer :: iasmf
    call beginSub( MOD_NAME, 'initialiseScalarFields_CellGrid', log )

    ! get all point coordinates
    call obj%interiorGaussianPointCollection%createPositionsMatrix( r, &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! from this matrix, allocate a values matrix
    allocate( v(size(r)), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating v(size(r)).  &
         &STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise list iterators
    call sfsli%init( scalarFieldSpecifications )
    call iasmfli%init( obj%intAveScalarMomentFieldList )
    
    ! loop over int ave point fields
    call iasmfli%first( iasmf )
    do
       if ( iasmfli%isDone() ) exit
       
       ! loop over specs
       call sfsli%first( sfs )
       do
          if ( sfsli%isDone() ) exit

          ! if suitable spec, apply it.  Note that we have to convert the
          ! point values to int-ave values using gaussian weights
          if ( sfs%isAssociatedWith(iasmf) ) then
             call sfs%getValues( v, pack(r, .true.) )
             call iasmf%setField( obj%interiorGaussianPointCollection%&
                  convertToPolytopeValues(&
                  reshape(v, [size(r, 1), size(r, 2)])) )
          end if
          
          call sfsli%next( sfs )
       end do
       
       call iasmfli%next( iasmf )
    end do

    deallocate( r, v, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating r, v.  &
         &STAT='//int2str(stat), log )

    call endSub( log )
  end subroutine initialiseScalarFields_CellGrid
  

  subroutine initialiseVectorFields_CellGrid( obj, &
       vectorFieldSpecifications, log )
    class(CellGridType), intent(inout) :: obj
    type(VectorFieldSpecificationListType), intent(in) :: &
         vectorFieldSpecifications
    class(LogType), intent(inout), optional :: log
    type(RealVectorType), dimension(:, :), allocatable :: r
    type(RealVectorType), dimension(:), allocatable :: v
    integer :: i, stat
    type(VectorFieldSpecificationListIteratorType) :: vfsli
    class(VectorFieldSpecificationInterface), pointer :: vfs
    call beginSub( MOD_NAME, 'initialiseVectorFields_CellGrid', log )

    ! get all point coordinates
    call obj%interiorGaussianPointCollection%createPositionsMatrix( r, &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! from this matrix, allocate a values matrix
    allocate( v(size(r)), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating v(size(r, 1), &
         &size(r, 2)).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise list iterator
    call vfsli%init( vectorFieldSpecifications )
    
    ! only one embedded velocity field.  Loop over specs
    call vfsli%first( vfs )
    do
       if ( vfsli%isDone() ) exit

       ! if suitable spec, apply it
       if ( vfs%isAssociatedWith(obj%intAveVelocityMomentField) ) then
          call vfs%getValues( v, pack(r, .true.) )
          call obj%intAveVelocityMomentField%setField( v )
       end if

       call vfsli%next( vfs )
    end do

    deallocate( r, v, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating r, v.  &
         &STAT='//int2str(stat), log )
    
    call endSub( log )
  end subroutine initialiseVectorFields_CellGrid

  
  !-------------------------------------------------------------------
  !- CellComplex methods
  !-------------------------------------------------------------------

  m4_divert(-1)
  m4_define({ARGLINE}, {parentCell, })
  m4_define({ARGLIST}, {
    class(CellPointerType), intent(in) :: parentCell})
  m4_divert(0)

  EXPAND({PROCEDURE_ATTACH({CellComplex}, {CellComplex},
  {PolytopeComplex}, {ARGLINE}, {ARGLIST})})

  subroutine init_CellComplex( obj, polytopeComplex, ARGLINE log )
    class(CellComplexType), intent(out) :: obj
    class(PolytopeComplexInterface), intent(in), target :: polytopeComplex
    ARGLIST
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_CellComplex', log )
    
    ! compute components
    call obj%parentCell%init( parentCell )
    call polytopeComplex%getPolytopes( CELL_DEPTH, obj%cellPolytopes, &
         log )
    call polytopeComplex%getPolytopes( FACE_DEPTH, obj%facePolytopes, &
         log )

    call create( obj%cells, obj%cellPolytopes, log  )
    call create( obj%faces, obj%facePolytopes, log  )
    call addEvent( checkSub(WARNING, log), FATAL, 'Something is wrong, e&
         &.g. one or more extensions do not exist.  Cannot proceed with &
         &building the simplex-locating matricies.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call obj%createSimplexLocatingMatrices( polytopeComplex, log )
    
    ! init extension superclass
    call obj%initExtension( polytopeComplex, log )
    call endSub( log )
  end subroutine init_CellComplex

  
  subroutine deinit_CellComplex( obj, log )
    class(CellComplexType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinit_CellComplex', log )

    ! deinit extension superclass
    call obj%deinitExtension( log )
    
    call obj%parentCell%deinit( log )
    call destroy( obj%cellPolytopes, log )
    call destroy( obj%facePolytopes, log )
    call destroy( obj%cells, log )
    call destroy( obj%faces, log )

    deallocate( obj%connectivity, obj%positions, &
         obj%working, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating obj%&
         &connectivity, obj%positions, obj%working.  &
         &STAT='//int2str(stat), log )
    
    call endSub( log )
  end subroutine deinit_CellComplex

  
  subroutine createSimplexLocatingMatrices( obj, polytopeComplex, log )
    class(CellComplexType), intent(inout) :: obj
    class(PolytopeComplexInterface), intent(in) :: polytopeComplex
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerSetType) :: cellFacePolytopes
    type(PolytopePointerSetIteratorType) :: iterator
    type(PolytopePointerType), pointer :: pp
    type(RealVectorType) :: coords
    integer :: i, j, m, n, allocStat
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, 'createSimplexLocatingMatrices', &
         log )
    
    call addEvent( .not. (allocated(obj%cellPolytopes) .and. &
         allocated(obj%facePolytopes)), FATAL, 'Require the creation of &
         &obj%cellPolytopes and obj%facePolytopes as a prerequisite.', log )
    if ( checkSub(WARNING, log) ) then
       call endSub( log )
       return
    end if
    
    
    ! allocate matrix components
    m = size(obj%cellPolytopes)
    n = size(obj%facePolytopes)
    allocate( obj%connectivity(m, n), obj%positions(m, n), &
         obj%working(m, n), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &elements of obj%connectivity, obj%positions and/or &
         &obj%working.  STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

    ! reset connectivity matrix.  However, we do not care about the
    ! default state of the positions matrix
    obj%connectivity = .false. 

    ! loop over input cellPolytopes
    do i = 1, m
       
       ! get set of facePolytopes bordering the cell
       call obj%cellPolytopes(i)%collectSubPolytopes( cellFacePolytopes, &
            log, 1 )
       
       if ( DEBUG_MODE ) then
          call iterator%init( cellFacePolytopes )
          call addEvent( DEBUG_MODE, ADVICE, 'Inspecting cell: '//&
               obj%cellPolytopes(i)%describe(), log )
          call addEvent( DEBUG_MODE, ADVICE, '   boundaries are (&
               &number = '//str(cellFacePolytopes%size())//'): ', log )
          call iterator%first( pp )
          do
             if ( iterator%isDone() ) exit
             call addEvent( DEBUG_MODE, ADVICE, '      '//&
                  pp%describe(), log )
             call iterator%next( pp )
          end do
       end if
       
       ! loop over input facePolytopes 
       do j = 1, n
          call addEvent( DEBUG_MODE, ADVICE, '   Inspecting &
               &face: '//obj%facePolytopes(j)%describe(), log )

          ! does the face match a cell face?
          if ( cellFacePolytopes%match(obj%facePolytopes(j)) > 0 ) then

             ! then there is a connection
             obj%connectivity(i, j) = .true.
             
             ! use the dot product test to determine position
             call obj%cellPolytopes(i)%getCentroid( coords )
             call obj%faces(j)%findSide( coords, obj%positions(i, j), &
                  log )
          end if
       end do

       ! clean up facePolytopes bordering the cell
       call cellFacePolytopes%deinit( log )
    end do

    call addEvent( DEBUG_MODE, ADVICE, 'connectivity = ', log )
    do i = 1, m
       call addEvent( DEBUG_MODE, ADVICE, '    '//&
            str(obj%connectivity(i, :)), log )
    end do
    call addEvent( DEBUG_MODE, ADVICE, 'positions = ', log )
    do i = 1, m
       call addEvent( DEBUG_MODE, ADVICE, '    '//&
            str(obj%positions(i, :)), log )
    end do
    
    call endSub( log )
  end subroutine createSimplexLocatingMatrices

  
  elemental subroutine locate_CellComplex( obj, coords, pp, &
       log )
    class(CellComplexType), intent(inout) :: obj
    type(RealVectorType), intent(in) :: coords
    type(PolytopePointerType), intent(out) :: pp
    class(LogType), intent(inout), optional :: log
    integer :: i, m
    logical, parameter :: DEBUG_MODE = .true.
    logical, dimension(MAX_CELLS_PER_COMPLEX) :: mask
    call beginSub( MOD_NAME, 'locate_CellComplex', log )
    m = size(obj%cellPolytopes)
    
    ! find the input point's position wrt each face
    call obj%faces%findSide( coords, obj%working(1, :) )
    call addEvent( DEBUG_MODE, ADVICE, 'findSide result = '//str(&
         obj%working(1, :)), log )
    
    ! do some matrix manipulations
    obj%working = spread( obj%working(1, :), 1, size(obj%cellPolytopes) )
    obj%working = (obj%positions == obj%working)
    where ( .not. obj%connectivity ) obj%working = .true. 

    call addEvent( DEBUG_MODE, ADVICE, 'working matrix = ', log )
    do i = 1, m
       call addEvent( DEBUG_MODE, ADVICE, '    '//&
            str(obj%working(i, :)), log )
    end do

    mask = .false.
    mask(1:m) = all(obj%working, 2)
    
    if ( DEBUG_MODE ) then
       call addEvent( count(mask) > 1, FATAL, 'Expected mask to have &
            &one .true. element, but there are '//str(count(mask))//'.', &
            log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
    end if
    
    i = sum( CELL_INDEX_VECTOR, mask=mask )
    if ( i > 0 ) pp = obj%cellPolytopes(i)

    call addEvent( DEBUG_MODE, ADVICE, 'i = '//str(i), log )
    call addEvent( DEBUG_MODE, ADVICE, 'cell = '//obj%cellPolytopes(i)&
         %describe(), log )
    
    call endSub( log )
  end subroutine locate_CellComplex

  
  subroutine appendVelocityMoments_CellComplex( obj, &
       velocityMomentGroupList, log )
    class(CellComplexType), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_CellComplex', log )

    call obj%parentCell%appendVelocityMoments( velocityMomentGroupList, &
         log )
    
    call endSub( log )
  end subroutine appendVelocityMoments_CellComplex

  
  subroutine appendScalarMoments_CellComplex( obj, &
       scalarMomentGroupList, log )
    class(CellComplexType), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendScalarMoments_CellComplex', log )

    call obj%parentCell%appendScalarMoments( scalarMomentGroupList, &
         log )
    
    call endSub( log )
  end subroutine appendScalarMoments_CellComplex
    

  subroutine initialiseScalarFields_CellComplex( obj, &
       scalarFieldSpecifications, log )
    class(CellComplexType), intent(inout) :: obj
    type(ScalarFieldSpecificationListType), intent(in) :: &
         scalarFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initialiseScalarFields_CellComplex', log )
    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine initialiseScalarFields_CellComplex
  

  subroutine initialiseVectorFields_CellComplex( obj, &
       vectorFieldSpecifications, log )
    class(CellComplexType), intent(inout) :: obj
    type(VectorFieldSpecificationListType), intent(in) :: &
         vectorFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initialiseVectorFields_CellComplex', log )
    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine initialiseVectorFields_CellComplex


end module CellCollectionsModule
