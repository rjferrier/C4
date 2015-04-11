module FiniteVolume_Stubs

  use TestUtilities
  use Global
  use FlowFieldModule
  use FlowField_Stubs
  use FiniteVolumeModule
  use SemiLagrangianModule

  use LogModule
  
  implicit none
  private

  character(*), parameter :: MOD_NAME = 'FiniteVolume_Stubs'
  

  type, extends(PolytopeArrayInterface), public :: PolytopeArrayStub
     private
   contains
     procedure :: deinit => deinitArray
     procedure :: pointToSubPolytopeArray => &
          pointToSubPolytopeArray_Array
     procedure :: collectSubPolytopes => collectSubPolytopes_Array
     procedure :: collectSuperPolytopes => &
          collectSuperPolytopes_Array
     procedure :: getCentroid => getCentroid_Array
     procedure :: getSize => getSize_Array
     procedure :: injectDeparturePoints => injectDeparturePoints_Array
     procedure :: extractDeparturePoints => extractDeparturePoints_Array
     procedure :: appendVelocityMoments => appendVelocityMoments_Array
     procedure :: appendScalarMoments => appendScalarMoments_Array
     procedure :: setExtension_Faces => setExtension_Faces_Array
     procedure :: setExtension_Cells => setExtension_Cells_Array
     procedure :: getCollectionExtension_Faces => &
          getCollectionExtension_Faces_Array
     procedure :: getCollectionExtension_Cells => &
          getCollectionExtension_Cells_Array
     procedure :: getArrayExtension_Faces => &
          getArrayExtension_Faces_Array
     procedure :: getArrayExtension_Cells => &
          getArrayExtension_Cells_Array
     procedure :: writePointLocations => writePointLocations_Array
     procedure :: writePointConnectivity => &
          writePointConnectivity_Array
     procedure :: appendConnection => appendConnection_Array
  end type PolytopeArrayStub
      

  type, extends(PolytopeGridInterface), public :: PolytopeGridStub
     private
     ! extensions
     class(FaceCollectionInterface), allocatable :: faceExtension
     class(CellCollectionInterface), allocatable :: cellExtension
   contains
     procedure :: deinit => deinitGrid
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_Grid
     procedure :: collectSuperPolytopes => &
          collectSuperPolytopes_Grid
     procedure :: getCentroid => getCentroid_Grid
     procedure :: getSize => getSize_Grid
     procedure :: injectDeparturePoints => injectDeparturePoints_Grid
     procedure :: extractDeparturePoints => extractDeparturePoints_Grid
     procedure :: appendVelocityMoments => appendVelocityMoments_Grid
     procedure :: appendScalarMoments => appendScalarMoments_Grid
     procedure :: breakUp => breakUp_Grid
     procedure :: pointToComplex => pointToComplex_Grid
     procedure :: getCartesianCode => getCartesianCode_Grid
     procedure :: setExtension_Faces => setExtension_Faces_Grid
     procedure :: setExtension_Cells => setExtension_Cells_Grid
     procedure :: getCollectionExtension_Cells => &
          getCollectionExtension_Cells_Grid
     procedure :: getCollectionExtension_Faces => &
          getCollectionExtension_Faces_Grid
     procedure :: getGridExtension_Faces => getGridExtension_Faces_Grid
     procedure :: getGridExtension_Cells => getGridExtension_Cells_Grid
     procedure :: writePointLocations => writePointLocations_Grid
     procedure :: writePointConnectivity => &
          writePointConnectivity_Grid
     procedure :: appendConnection => appendConnection_Grid
  end type PolytopeGridStub


  type, extends(FaceArrayInterface), public :: FaceArrayStub
     private
   contains
     procedure :: deinit => deinit_FaceArray
     procedure :: getFaceNormal => getFaceNormal_FaceArray
     procedure :: findSide => findSide_FaceArray
     procedure :: computeSubRow => computeSubRow_FaceArray
     procedure :: appendScalarMoments => &
          appendScalarMoments_FaceArray
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_FaceArray
  end type FaceArrayStub

  
  type, extends(CellArrayInterface), public :: CellArrayStub
     private
   contains
     procedure :: deinit => deinit_CellArray
     procedure :: computeSubRow => computeSubRow_CellArray
     procedure :: appendScalarMoments => &
          appendScalarMoments_CellArray
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_CellArray
     procedure :: initialiseScalarFields => &
          initialiseScalarFields_CellArray
     procedure :: initialiseVectorFields => &
          initialiseVectorFields_CellArray
  end type CellArrayStub

  
contains

  !-------------------------------------------------------------------
  !- Array procedures
  !-------------------------------------------------------------------
  
  subroutine initArray( obj, polytopeDim, ppArray, subPolytopeArray, &
       log )
    class(PolytopeArrayStub), intent(out) :: obj
    integer, intent(in) :: polytopeDim
    type(PolytopePointerType), dimension(:), intent(in) :: ppArray
    class(PolytopeArrayInterface), target, intent(in) :: &
         subPolytopeArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )
  end subroutine initArray

     
  subroutine deinitArray( obj, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinitArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )    
  end subroutine deinitArray

  
  pure recursive subroutine collectSuperPolytopes_Array( &
       obj, index, pps, log, height )
    class(PolytopeArrayStub), intent(inout), target :: obj
    integer, intent(in) :: index
    type(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, 'collectSuperPolytopes_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )    
  end subroutine collectSuperPolytopes_Array

  
  pure recursive subroutine collectSubPolytopes_Array( &
       obj, index, pps, log, depth )
    class(PolytopeArrayStub), intent(inout), target :: obj
    integer, intent(in) :: index
    type(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    call beginSub( MOD_NAME, 'collectSubPolytopes_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )    
  end subroutine collectSubPolytopes_Array


  subroutine pointToSubPolytopeArray_Array( obj, ptr )
    class(PolytopeArrayStub), intent(in) :: obj
    class(PolytopeArrayInterface), pointer, intent(out) :: ptr
    nullify(ptr)
  end subroutine pointToSubPolytopeArray_Array


  function getSubPolytopeArray_Array( obj )
    class(PolytopeArrayStub), intent(in) :: obj
    class(PolytopeArrayInterface), pointer :: &
         getSubPolytopeArray_Array
    nullify(getSubPolytopeArray_Array)
  end function getSubPolytopeArray_Array


  pure recursive subroutine computeCentroid_Array( obj, &
       index, coords, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    integer, intent(in) :: index
    real(FLOAT), dimension(NDIM), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'computeCentroid_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    coords = 0._FLOAT
    call endSub( log )    
  end subroutine computeCentroid_Array


  pure subroutine getCentroid_Array( obj, index, coords, log )
    class(PolytopeArrayStub), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCentroid_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    coords = 0._FLOAT
    call endSub( log )    
  end subroutine getCentroid_Array
      

  pure subroutine getSize_Array( obj, index, size, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    integer, intent(in) :: index
    real(FLOAT), intent(out) :: size
    class(LogType), intent(inout), optional :: log
    ! tk
    size = 0._FLOAT
  end subroutine getSize_Array

  
  subroutine injectDeparturePoints_Array( obj, departurePoints, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    class(DeparturePointCollectionInterface), allocatable, intent(&
         inout) :: departurePoints
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'injectDeparturePoints_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine injectDeparturePoints_Array


  subroutine extractDeparturePoints_Array( obj, departurePoints, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    class(DeparturePointCollectionInterface), allocatable, intent(&
         inout) :: departurePoints
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'extractDeparturePoints_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine extractDeparturePoints_Array


  subroutine appendVelocityMoments_Array( obj, velocityMomentGroupList, &
       polytopeIndex, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendVelocityMoments_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine appendVelocityMoments_Array

  
  subroutine appendScalarMoments_Array( obj, scalarMomentGroupList, &
       polytopeIndex, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendScalarMoments_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine appendScalarMoments_Array

  
  subroutine setExtension_Faces_Array( obj, faceArray, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    class(FaceArrayInterface), allocatable, intent(inout) :: faceArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'setExtension_Faces_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )
  end subroutine setExtension_Faces_Array

  
  subroutine setExtension_Cells_Array( obj, cellArray, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    class(CellArrayInterface), allocatable, intent(inout) :: cellArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'setExtension_Cells_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )
  end subroutine setExtension_Cells_Array
  
  
  pure subroutine getCollectionExtension_Faces_Array( obj, &
       faceCollection, log )
    class(PolytopeArrayStub), intent(inout), target :: obj
    class(FaceCollectionInterface), pointer, intent(inout) :: &
         faceCollection
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCollectionExtension_Faces_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    nullify(faceCollection)
    call endSub( log )    
  end subroutine getCollectionExtension_Faces_Array

  
  pure subroutine getCollectionExtension_Cells_Array( obj, &
       cellCollection, log )
    class(PolytopeArrayStub), target, intent(inout) :: obj
    class(CellCollectionInterface), pointer, intent(inout) :: &
         cellCollection
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCollectionExtension_Cells_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    nullify(cellCollection)
    call endSub( log )    
  end subroutine getCollectionExtension_Cells_Array
  
  
  pure subroutine getArrayExtension_Faces_Array( obj, faceArray, log )
    class(PolytopeArrayStub), target, intent(inout) :: obj
    class(FaceArrayInterface), pointer, intent(inout) :: faceArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getArrayExtension_Faces_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    nullify(faceArray)
    call endSub( log )    
  end subroutine getArrayExtension_Faces_Array

  
  pure subroutine getArrayExtension_Cells_Array( obj, cellArray, log )
    class(PolytopeArrayStub), target, intent(inout) :: obj
    class(CellArrayInterface), pointer, intent(inout) :: cellArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getArrayExtension_Cells_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    nullify(cellArray)
    call endSub( log )    
  end subroutine getArrayExtension_Cells_Array
  
  
  subroutine writePointLocations_Array( obj, stringList, nPointsRunning, &
       precision, indent, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    type(StringListType), intent(inout) :: stringList
    integer, intent(inout) :: nPointsRunning
    integer, intent(in), optional :: precision, indent
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'writePointLocations_Array', log )
    
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine writePointLocations_Array

  
  subroutine writePointConnectivity_Array( obj, connectivityList, &
       offsetList, nPolytopesRunning, startFromZero, indent, log )
    class(PolytopeArrayStub), intent(inout) :: obj
    type(StringListType), intent(inout) :: connectivityList, offsetList
    integer, intent(inout) :: nPolytopesRunning
    logical, intent(in) :: startFromZero
    integer, intent(in), optional :: indent
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'writePointConnectivity_Array', log )

    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
       
    call endSub( log )
  end subroutine writePointConnectivity_Array


  pure subroutine appendConnection_Array( obj, string, &
       polytopeIndex, startFromZero, indent )
    class(PolytopeArrayStub), intent(in) :: obj
    character(*), intent(inout) :: string
    integer, intent(in)  :: polytopeIndex
    logical, intent(in)  :: startFromZero
    integer, intent(in), optional :: indent
    ! n/a
  end subroutine appendConnection_Array


  !-------------------------------------------------------------------
  !- Grid procedures 
  !-------------------------------------------------------------------

  subroutine initGrid( obj, interiorPolytopeGrids, log, &
       exteriorPolytopeGrids )
    class(PolytopeGridStub), intent(out) :: obj
    type(PolytopeGridPointerType), dimension(NDIM), intent(in) :: &
         interiorPolytopeGrids
    class(LogType), intent(inout), optional :: log
    type(PolytopeGridPointerType), dimension(NDIM, 2), intent(in), &
         optional :: exteriorPolytopeGrids
    call beginSub( MOD_NAME, 'initGrid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )    
  end subroutine initGrid

  
  subroutine deinitGrid( obj, log )
    class(PolytopeGridStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitGrid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )    
  end subroutine deinitGrid

  
  pure recursive subroutine collectSubPolytopes_Grid( &
       obj, index, pps, log, depth )
    class(PolytopeGridStub), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    call beginSub( MOD_NAME, 'collectSubPolytopes_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )    
  end subroutine collectSubPolytopes_Grid

  
  pure recursive subroutine collectSuperPolytopes_Grid( &
       obj, index, pps, log, height )
    class(PolytopeGridStub), intent(inout), target :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, 'collectSuperPolytopes_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )    
  end subroutine collectSuperPolytopes_Grid


  pure subroutine getCentroid_Grid( obj, index, &
       coords, log )
    class(PolytopeGridStub), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out) :: coords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCentroid_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )    
  end subroutine getCentroid_Grid

      
  pure subroutine getSize_Grid( obj, index, size, log )
    class(PolytopeGridStub), intent(inout) :: obj
    integer, intent(in) :: index
    real(FLOAT), intent(out) :: size
    class(LogType), intent(inout), optional :: log
    ! tk
    size = 0._FLOAT
  end subroutine getSize_Grid

  
  subroutine injectDeparturePoints_Grid( obj, departurePoints, log )
    class(PolytopeGridStub), intent(inout) :: obj
    class(DeparturePointCollectionInterface), allocatable, intent(&
         inout) :: departurePoints
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'injectDeparturePoints_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine injectDeparturePoints_Grid


  subroutine extractDeparturePoints_Grid( obj, departurePoints, log )
    class(PolytopeGridStub), intent(inout) :: obj
    class(DeparturePointCollectionInterface), allocatable, intent(&
         inout) :: departurePoints
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'extractDeparturePoints_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine extractDeparturePoints_Grid


  subroutine appendVelocityMoments_Grid( obj, velocityMomentGroupList, &
       polytopeIndex, log )
    class(PolytopeGridStub), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendVelocityMoments_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine appendVelocityMoments_Grid

  
  subroutine appendScalarMoments_Grid( obj, scalarMomentGroupList, &
       polytopeIndex, log )
    class(PolytopeGridStub), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendScalarMoments_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine appendScalarMoments_Grid

  
  subroutine breakUp_Grid( obj, gridAddresses, polytopeComplexes, &
       log )
    class(PolytopeGridStub), intent(inout) :: obj
    type(IntVectorType), dimension(:), intent(in) :: gridAddresses
    class(PolytopeComplexInterface), dimension(:), target, &
         intent(in) :: polytopeComplexes
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'breakUp_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )    
  end subroutine breakUp_Grid
    
  
  pure subroutine pointToComplex_Grid( obj, gridAddress, &
       polytopeComplex, log )
    class(PolytopeGridStub), intent(inout) :: obj
    type(IntVectorType), intent(in) :: gridAddress
    class(PolytopeComplexInterface), pointer, intent(out) :: &
         polytopeComplex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'pointToComplex_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    nullify(polytopeComplex)
    call endSub( log )    
  end subroutine pointToComplex_Grid


  pure function getCartesianCode_Grid( obj ) result ( cartesianCode )
    class(PolytopeGridStub), intent(in) :: obj
    type(LogicalVectorType) :: cartesianCode

    cartesianCode = .false.
  end function getCartesianCode_Grid


  subroutine setExtension_Faces_Grid( obj, faceGrid, log )
    class(PolytopeGridStub), intent(inout) :: obj
    class(FaceGridInterface), allocatable, intent(inout) :: faceGrid
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'setExtension_Faces_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )
  end subroutine setExtension_Faces_Grid

  
  subroutine setExtension_Cells_Grid( obj, cellGrid, log )
    class(PolytopeGridStub), intent(inout) :: obj
    class(CellGridInterface), allocatable, intent(inout) :: cellGrid
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'setExtension_Cells_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    call endSub( log )
  end subroutine setExtension_Cells_Grid
  
  
  pure subroutine getCollectionExtension_Faces_Grid( obj, faceCollection, &
       log )
    class(PolytopeGridStub), target, intent(inout) :: obj
    class(FaceCollectionInterface), pointer, intent(inout) :: &
         faceCollection
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCollectionExtension_Faces_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    nullify(faceCollection)
    call endSub( log )
  end subroutine getCollectionExtension_Faces_Grid

  
  pure subroutine getCollectionExtension_Cells_Grid( obj, cellCollection, &
       log )
    class(PolytopeGridStub), target, intent(inout) :: obj
    class(CellCollectionInterface), pointer, intent(inout) :: &
         cellCollection
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCollectionExtension_Cells_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    nullify(cellCollection)
    call endSub( log )    
  end subroutine getCollectionExtension_Cells_Grid

  
  pure subroutine getGridExtension_Faces_Grid( obj, faceGrid, log )
    class(PolytopeGridStub), target, intent(inout) :: obj
    class(FaceGridInterface), pointer, intent(inout) :: faceGrid
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getGridExtension_Faces_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    nullify(faceGrid)
    call endSub( log )    
  end subroutine getGridExtension_Faces_Grid

  
  pure subroutine getGridExtension_Cells_Grid( obj, cellGrid, log )
    class(PolytopeGridStub), target, intent(inout) :: obj
    class(CellGridInterface), pointer, intent(inout) :: cellGrid
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getGridExtension_Cells_Grid', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    nullify(cellGrid)
    call endSub( log )    
  end subroutine getGridExtension_Cells_Grid
  
  
  subroutine writePointLocations_Grid( obj, stringList, nPointsRunning, &
       precision, indent, log )
    class(PolytopeGridStub), intent(inout) :: obj
    type(StringListType), intent(inout) :: stringList
    integer, intent(inout) :: nPointsRunning
    integer, intent(in), optional :: precision, indent
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'writePointLocations_Grid', log )
    
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    call endSub( log )
  end subroutine writePointLocations_Grid

  
  subroutine writePointConnectivity_Grid( obj, connectivityList, &
       offsetList, nPolytopesRunning, startFromZero, indent, log )
    class(PolytopeGridStub), intent(inout) :: obj
    type(StringListType), intent(inout) :: connectivityList, offsetList
    integer, intent(inout) :: nPolytopesRunning
    logical, intent(in) :: startFromZero
    integer, intent(in), optional :: indent
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'writePointConnectivity_Grid', log )

    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
       
    call endSub( log )
  end subroutine writePointConnectivity_Grid


  pure subroutine appendConnection_Grid( obj, string, &
       polytopeIndex, startFromZero, indent )
    class(PolytopeGridStub), intent(in) :: obj
    character(*), intent(inout) :: string
    integer, intent(in)  :: polytopeIndex
    logical, intent(in)  :: startFromZero
    integer, intent(in), optional :: indent
    ! n/a
  end subroutine appendConnection_Grid
  

  !-------------------------------------------------------------------
  !- FaceArray methods
  !-------------------------------------------------------------------
  
  subroutine deinit_FaceArray( obj, log )
    class(FaceArrayStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_FaceArray', log )

    ! deinit extension superclass
    call obj%deinitExtension( log )
    
    call endSub( log )
  end subroutine deinit_FaceArray

  
  pure subroutine getFaceNormal_FaceArray( obj, index, vector, log )
    class(FaceArrayStub), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out) :: vector
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getFaceNormal_FaceArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    vector = 0._FLOAT
    
    call endSub( log )
  end subroutine getFaceNormal_FaceArray

  
  elemental subroutine findSide_FaceArray( obj, index, coords, side, &
       log )
    class(FaceArrayStub), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(in) :: coords
    logical, intent(out) :: side
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'findSide_FaceArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )

    side = .false.

    call endSub( log )
  end subroutine findSide_FaceArray


  pure function computeSubRow_FaceArray( obj, polytopeIndex, &
       directedSpatialDerivative ) result ( row )
    class(FaceArrayStub), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    real(FLOAT), dimension(1, NCOEFS_ADV) :: row

    row = 0._FLOAT
  end function computeSubRow_FaceArray

  
  subroutine appendScalarMoments_FaceArray( obj, &
       scalarMomentGroupList, polytopeIndex, log )
    class(FaceArrayStub), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendScalarMoments_FaceArray', log )
    
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    
    call endSub( log )
  end subroutine appendScalarMoments_FaceArray


  subroutine appendVelocityMoments_FaceArray( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class(FaceArrayStub), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_FaceArray', log )
    
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    
    call endSub( log )
  end subroutine appendVelocityMoments_FaceArray  

  
  !-------------------------------------------------------------------
  !- CellArray methods
  !-------------------------------------------------------------------
  
  subroutine deinit_CellArray( obj, log )
    class(CellArrayStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_CellArray', log )

    ! deinit extension superclass
    call obj%deinitExtension( log )
    
    call endSub( log )
  end subroutine deinit_CellArray

  
  pure function computeSubRow_CellArray( obj, polytopeIndex, &
       directedSpatialDerivative ) result ( row )
    class(CellArrayStub), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    real(FLOAT), dimension(1, NCOEFS_ADV) :: row

    row = 0._FLOAT
  end function computeSubRow_CellArray

  
  subroutine appendScalarMoments_CellArray( obj, &
       scalarMomentGroupList, polytopeIndex, log )
    class(CellArrayStub), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendScalarMoments_CellArray', log )
    
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    
    call endSub( log )
  end subroutine appendScalarMoments_CellArray


  subroutine appendVelocityMoments_CellArray( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class(CellArrayStub), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_CellArray', log )
    
    call addEvent( WARNING, 'This stub method should be overridden.', &
         log )
    
    call endSub( log )
  end subroutine appendVelocityMoments_CellArray

  
  subroutine initialiseScalarFields_CellArray( obj, &
       scalarFieldSpecifications, log )
    class(CellArrayStub), intent(inout) :: obj
    type(ScalarFieldSpecificationListType), intent(in) :: &
         scalarFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initialiseScalarFields_CellArray', log )
    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine initialiseScalarFields_CellArray
  

  subroutine initialiseVectorFields_CellArray( obj, &
       vectorFieldSpecifications, log )
    class(CellArrayStub), intent(inout) :: obj
    type(VectorFieldSpecificationListType), intent(in) :: &
         vectorFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initialiseVectorFields_CellArray', log )
    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine initialiseVectorFields_CellArray
  
end module FiniteVolume_Stubs
