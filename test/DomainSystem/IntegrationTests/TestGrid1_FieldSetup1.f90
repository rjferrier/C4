! This populates the PolytopeCollections generated in TestGrid1 with
! scalar moment fields, along with the appropriate arrival/gaussian points
! and cell/face extensions.
module TestGrid1_FieldSetup1

  use TestGrid1, setUp_DNU => setUp, tearDown_DNU => tearDown
  
  use TestUtilities
  use Global
  use DomainModule
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FlowFieldModule

  ! systems under test
  use PolytopeComplexesModule
  use CellCollectionsModule
  use FaceCollectionsModule
  use PolytopeComplexesModule
  use InteriorArrivalPointsModule
  use PointMomentFieldsModule
  use InteriorGaussianPointsModule
  use IntAveMomentFieldsModule
  use CellCollectionsModule
  use FaceCollectionsModule
  use FieldSpecificationsModule

  ! helpers
  use RegionsModule
  use PointArrangementsModule
  use SemiLagrangian_Stubs
  use SpatialDerivativesModule
  use FlowField_Stubs
  use DirectionsModule
  

  implicit none

  character(*), parameter, private :: MOD_NAME = 'TestGrid1_FieldSetup1'

  class(InteriorRegionInterface), allocatable :: interiorRegion
  
  class(FaceGridInterface), pointer :: xLineFaceGrid, yLineFaceGrid
  class(FaceArrayInterface), pointer :: xLineFaceArray, yLineFaceArray, &
       freeLineFaceArray
  class(CellGridInterface), pointer :: rectangleCellGrid
  class(CellArrayInterface), pointer :: triangleCellArray
  
  class(PolytopeComplexInterface), dimension(:), allocatable :: &
       cutRectangles
  class(PolytopeComplexInterface), dimension(:), allocatable :: &
       cutXLines, cutYLines
  
  class(CellComplexInterface), dimension(:), allocatable :: &
       cutRectangleCells
  class(FaceComplexInterface), dimension(:), allocatable :: &
       cutXLineFaces, cutYLineFaces

  
  ! for the tests only
  class(ScalarFlowVariableInterface), allocatable, target :: scalar
  class(VectorFlowVariableInterface), allocatable, target :: velocity

    
contains

  !-------------------------------------------------------------------
  !- fixture setup/teardown
  !-------------------------------------------------------------------

  subroutine setUp_TestGrid1_FieldSetup1( coordsMin, coordsMax, &
       resolution, velocitySpecification, scalarSpecifications, log )
    type(RealVectorType), intent(in) :: coordsMin, coordsMax
    type(IntVectorType), intent(in) :: resolution
    type(RealVectorType), intent(in) :: velocitySpecification
    class(ScalarFieldSpecificationListType), intent(in) :: &
         scalarSpecifications
    class(LogType), intent(inout) :: log
    integer :: i, j, n, nxl, nyl, nr, mq, nq, iq, jq, h, v, t, k, stat
    type(IntVectorType), dimension(:), allocatable :: xLineCutAddresses, &
         yLineCutAddresses, rectangleCutAddresses
    type(IntVectorType) :: nullAddress
    type(LogicalVectorType) :: l
    call beginSub( MOD_NAME, 'setUp_TestGrid1_FieldSetup1', log )
    
    ! set up the geometry
    call setUp_TestGrid1( coordsMin, coordsMax, resolution, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create a region
    call createInteriorRegion( interiorRegion, 'InteriorRegion', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! decorate the polytopes
    call setUp_TestGrid1_FieldSetup1_structuredPoints( pointGrid, &
         velocitySpecification, scalarSpecifications, log )
    call setUp_TestGrid1_FieldSetup1_structuredLines( xLineGrid, &
         xLineFaceGrid, velocitySpecification, scalarSpecifications, log )
    call setUp_TestGrid1_FieldSetup1_structuredLines( yLineGrid, &
         yLineFaceGrid, velocitySpecification, scalarSpecifications, log )
    call setUp_TestGrid1_FieldSetup1_structuredRectangles( &
         rectangleGrid, rectangleCellGrid, velocitySpecification, &
         scalarSpecifications, log )
    call setUp_TestGrid1_FieldSetup1_unstructuredPoints( xPointArray, &
         velocitySpecification, scalarSpecifications, log )
    call setUp_TestGrid1_FieldSetup1_unstructuredPoints( yPointArray, &
         velocitySpecification, scalarSpecifications, log )
    call setUp_TestGrid1_FieldSetup1_unstructuredLines( xLineArray, &
         xLineFaceArray, velocitySpecification, scalarSpecifications, &
         log )
    call setUp_TestGrid1_FieldSetup1_unstructuredLines( yLineArray, &
         yLineFaceArray, velocitySpecification, scalarSpecifications, &
         log )
    call setUp_TestGrid1_FieldSetup1_unstructuredLines( freeLineArray, &
         freeLineFaceArray, velocitySpecification, scalarSpecifications, &
         log )
    call setUp_TestGrid1_FieldSetup1_unstructuredTriangles( &
         triangleArray, triangleCellArray, velocitySpecification, &
         scalarSpecifications, log )
    
    
    ! create simplicial complexes
    nxl = xLineArray%getNumPolytopes()/2
    nyl = yLineArray%getNumPolytopes()/2
    nr = product( resolution )
    
    call createUnmixedSimplicialComplexArray( cutXLines, &
         xLineArray, [(i, i=1, 2*nxl)], [(2, i=1, nxl)], log )
    call createUnmixedSimplicialComplexArray( cutYLines, &
         yLineArray, [(i, i=1, 2*nyl)], [(2, i=1, nyl)], log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call createUnmixedSimplicialComplexArray( cutRectangles, &
         triangleArray, [(i, i=1, 4*nr)], [(4, i=1, nr)], log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! implement as cuts in the grid.  First allocate some arrays for
    ! addresses.
    allocate( xLineCutAddresses(nxl), yLineCutAddresses(nyl), &
         rectangleCutAddresses(nr), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating xLineCutAddresses(&
         &nxl), yLineCutAddresses(nyl), rectangleCutAddresses(nr).  &
         &STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! loop over cell quadruples setting addresses and decorating complexes
    ! as cells/faces
    mq = resolution%getValue(1)/2
    nq = resolution%getValue(2)/2
    do jq = 1, nq
       do iq = 1, mq
          ! reference indicies for unstructured polytopes, divided to
          ! get an indicies for the corresponding complexes
          h = getIndex( 4, iq, jq, mq, nq, nExtraNorth=2 )/2
          v = getIndex( 4, iq, jq, mq, nq, nExtraEast=2 )/2
          t = getIndex( 16, iq, jq, mq, nq )/4
          
          ! lower left hand address of this cell quadruple
          i = 2*(iq - 1) + 1
          j = 2*(jq - 1) + 1

          ! record address of each complex.  Decorate as a cell or face.
          ! Remember, we don't need to keep track of the cell/face
          ! extensions - clients can recover pointers to them later.
          
          xLineCutAddresses(h+1) = vector([i+1, j])
          k = xLineGrid%convertToIndex( vector([i+1, j]) )
          call attachInteriorFaceComplex( cutXLines(h+1), ptr(&
               xLineFaceGrid, k), log )
          
          xLineCutAddresses(h+2) = vector([i, j+1])
          k = xLineGrid%convertToIndex( vector([i, j+1]) )
          call attachInteriorFaceComplex( cutXLines(h+2), ptr(&
               xLineFaceGrid, k), log )
          
          yLineCutAddresses(v+1) = vector([i, j+1])
          k = yLineGrid%convertToIndex( vector([i, j+1]) )
          call attachInteriorFaceComplex( cutYLines(v+1), ptr(&
               yLineFaceGrid, k), log )
          
          yLineCutAddresses(v+2) = vector([i+1, j])
          k = yLineGrid%convertToIndex( vector([i+1, j]) )
          call attachInteriorFaceComplex( cutYLines(v+2), ptr(&
               yLineFaceGrid, k), log )
          
          rectangleCutAddresses(t+1) = vector([i, j])
          k = rectangleGrid%convertToIndex( vector([i, j]) )
          call attachCellComplex( cutRectangles(t+1), ptr(&
               rectangleCellGrid, k), log )
          
          rectangleCutAddresses(t+2) = vector([i+1, j])
          k = rectangleGrid%convertToIndex( vector([i+1, j]) )
          call attachCellComplex( cutRectangles(t+2), ptr(&
               rectangleCellGrid, k), log )
          
          rectangleCutAddresses(t+3) = vector([i, j+1])
          k = rectangleGrid%convertToIndex( vector([i, j+1]) )
          call attachCellComplex( cutRectangles(t+3), ptr(&
               rectangleCellGrid, k), log )
          
          rectangleCutAddresses(t+4) = vector([i+1, j+1])
          k = rectangleGrid%convertToIndex( vector([i+1, j+1]) )
          call attachCellComplex( cutRectangles(t+4), ptr(&
               rectangleCellGrid, k), log )
          
          ! at the end of each column of cells are two more H lines
          if ( jq == nq ) then
             xLineCutAddresses(h+3) = vector([i+1, j+2])
          end if
       end do
       
       ! at the end of each row of cells are two more V lines
       yLineCutAddresses(v+3) = vector([i+2, j+1])
    end do

    ! complete?
    do i = 1, size(xLineCutAddresses)
       l = xLineCutAddresses(i) == nullAddress
       call addEvent( all(l), FATAL, 'Element '//str(i)//' of &
            &xLineCutAddresses is still empty.', log )
    end do
    do i = 1, size(yLineCutAddresses)
       l = yLineCutAddresses(i) == nullAddress
       call addEvent( all(l), FATAL, 'Element '//str(i)//' of &
            &yLineCutAddresses is still empty.', log )
    end do
    do i = 1, size(rectangleCutAddresses)
       l = rectangleCutAddresses(i) == nullAddress
       call addEvent( all(l), FATAL, 'Element '//str(i)//' of &
            &rectangleCutAddresses is still empty.', log )
    end do
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
       
    call xLineGrid%breakUp( xLineCutAddresses, cutXLines, log )
    call yLineGrid%breakUp( yLineCutAddresses, cutYLines, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call rectangleGrid%breakUp( rectangleCutAddresses, cutRectangles, &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call endSub( log )
  end subroutine setUp_TestGrid1_FieldSetup1


  subroutine tearDown_TestGrid1_FieldSetup1( log )
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'tearDown_TestGrid1_FieldSetup1', log )

    call destroy( cutRectangles, log )
    call destroy( cutXLines, log )
    call destroy( cutYLines, log )

    nullify(rectangleCellGrid)
    nullify(triangleCellArray)
    nullify(xLineFaceGrid)
    nullify(yLineFaceGrid)
    nullify(xLineFaceArray)
    nullify(yLineFaceArray)
    nullify(freeLineFaceArray)

    call destroy( interiorRegion, log )
    
    ! tear down the geometry
    call teardown_TestGrid1( log )
    
    call endSub( log )
  end subroutine tearDown_TestGrid1_FieldSetup1
  

  subroutine setUp_TestGrid1_FieldSetup1_structuredPoints( pointGrid, &
       velocitySpecification, scalarSpecifications, log )
    class(PolytopeGridInterface), intent(inout) :: pointGrid
    type(RealVectorType), intent(in) :: velocitySpecification
    class(ScalarFieldSpecificationListType), intent(in) :: &
         scalarSpecifications
    class(LogType), intent(inout), optional :: log
    type(PointScalarMomentFieldListType) :: psmfList
    type(PointVectorMomentFieldListType) :: pvmfList
    class(PointScalarMomentFieldInterface), allocatable :: psmf
    class(PointVectorMomentFieldInterface), allocatable :: pvmf
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: ispc
    class(DeparturePointCollectionInterface), allocatable :: dpc
    integer :: i
    call beginSub( MOD_NAME, &
         'setUp_TestGrid1_FieldSetup1_structuredPoints', log )
    
    ! --- Arrival points ---

    ! point arrangement
    call createGridPointArrangement( pa, pointGrid, 1, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! make a list of moment fields associated with the scalar.  We require
    ! 0th and 1st spatial derivatives on interior verticies.
    do i = 1, 3
       select case ( i )
       case (1)
          call createZerothSpatialDerivative( dsd, log )
       case (2)
          call createFirstSpatialDerivative( dsd, 1, log )
       case (3)
          call createFirstSpatialDerivative( dsd, 2, log )
       end select
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if

       call createPointScalarMomentGrid( psmf, scalar, dsd, &
            pointGrid, pa, log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if

       ! append to the list
       call psmfList%append( psmf, log )
    end do
    call assertEqual( 3, psmfList%size(), 'psmfList should be three &
         &nodes long.' )
    
    ! make a dummy vector field to act as the velocity field.  It also
    ! needs to be added to a list (because the total velocity field
    ! could be further be made up by velocity gradients)
    call createZerothSpatialDerivative( dsd, log )
    call createPointVectorMomentGrid( pvmf, velocity, dsd, pointGrid, &
         pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    ! the velocity spec is primitive, so we can apply a blanket
    ! initialisation now
    call pvmf%setField( velocitySpecification )
    call pvmfList%append( pvmf, log )
    call assertEqual( 1, pvmfList%size(), 'pvmfList should be one &
         &node long.' )
    
    ! create the points, initialise scalar field(s) and inject the
    ! points into the appropriate PolytopeCollection
    call createInteriorArrivalPointCollection( ispc, pa, interiorRegion, &
         log )
    call createInteriorDeparturePointCollection( dpc, ispc, &
         .true., pvmfList, psmfList, log )
    call dpc%initialiseScalarFields( scalarSpecifications, log )
    call pointGrid%injectDeparturePoints( dpc, log )
      
    call endSub( log )
  end subroutine setUp_TestGrid1_FieldSetup1_structuredPoints
  
  
  subroutine setUp_TestGrid1_FieldSetup1_unstructuredPoints( pointArray, &
       velocitySpecification, scalarSpecifications, log )
    class(PolytopeArrayInterface), intent(inout) :: pointArray
    type(RealVectorType), intent(in) :: velocitySpecification
    class(ScalarFieldSpecificationListType), intent(in) :: &
         scalarSpecifications
    class(LogType), intent(inout), optional :: log
    type(PointScalarMomentFieldListType) :: psmfList
    type(PointVectorMomentFieldListType) :: pvmfList
    class(PointScalarMomentFieldInterface), allocatable :: psmf
    class(PointVectorMomentFieldInterface), allocatable :: pvmf
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: ispc
    class(DeparturePointCollectionInterface), allocatable :: dpc
    integer :: i
    call beginSub( MOD_NAME, &
         'setUp_TestGrid1_FieldSetup1_unstructuredPoints', log )
    
    ! --- Arrival points ---
    
    ! point arrangement
    call createArrayPointArrangement( pa, pointArray, 1, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! make a list of moment fields associated with the scalar.  We require
    ! 0th and 1st spatial derivatives on interior verticies.
    do i = 1, 3
       select case ( i )
       case (1)
          call createZerothSpatialDerivative( dsd, log )
       case (2)
          call createFirstSpatialDerivative( dsd, 1, log )
       case (3)
          call createFirstSpatialDerivative( dsd, 2, log )
       end select
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if

       call createPointScalarMomentArray( psmf, scalar, dsd, &
            pointArray, pa, log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if

       ! append to the list
       call psmfList%append( psmf, log )
    end do
    call assertEqual( 3, psmfList%size(), 'psmfList should be three &
         &nodes long.' )
    
    ! make a dummy vector field to act as the velocity field.  It also
    ! needs to be added to a list (because the total velocity field
    ! could be further be made up by velocity gradients)
    call createZerothSpatialDerivative( dsd, log )
    call createPointVectorMomentArray( pvmf, velocity, dsd, pointArray, &
         pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    ! the velocity spec is primitive, so we can apply a blanket
    ! initialisation now
    call pvmf%setField( velocitySpecification )
    call pvmfList%append( pvmf, log )
    call assertEqual( 1, pvmfList%size(), 'pvmfList should be one &
         &node long.' )
    
    ! create the points, initialise scalar field(s) and inject the
    ! points into the appropriate PolytopeCollection
    call createInteriorArrivalPointCollection( ispc, pa, interiorRegion, &
         log )
    call createInteriorDeparturePointCollection( dpc, ispc, &
         .true., pvmfList, psmfList, log )
    call dpc%initialiseScalarFields( scalarSpecifications, log )
    call pointArray%injectDeparturePoints( dpc, log )
      
    call endSub( log )
  end subroutine setUp_TestGrid1_FieldSetup1_unstructuredPoints


  subroutine setUp_TestGrid1_FieldSetup1_structuredLines( lineGrid, &
       lineFaceGrid, velocitySpecification, scalarSpecifications, log )
    class(PolytopeGridInterface), intent(inout) :: lineGrid
    class(FaceGridInterface), pointer, intent(inout) :: lineFaceGrid
    type(RealVectorType), intent(in) :: velocitySpecification
    class(ScalarFieldSpecificationListType), intent(in) :: &
         scalarSpecifications
    class(LogType), intent(inout), optional :: log
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: ispc
    integer :: i
    call beginSub( MOD_NAME, &
         'setUp_TestGrid1_FieldSetup1_structuredLines', log )

    ! no arrival points on the lines

    ! --- Gaussian points ---

    ! create the point arrangement
    call createGridPointArrangement( pa, lineGrid, 3, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the gaussian points
    call createInteriorGaussianPointCollection( ispc, interiorRegion, &
         pa, log )
    
    ! attach the face extension and get access to it
    call attachInteriorFaceGrid( lineGrid, ispc, log )
    call lineGrid%getGridExtension( lineFaceGrid, log )
      
    call endSub( log )
  end subroutine setUp_TestGrid1_FieldSetup1_structuredLines


  subroutine setUp_TestGrid1_FieldSetup1_unstructuredLines( lineArray, &
       lineFaceArray, velocitySpecification, scalarSpecifications, log )
    class(PolytopeArrayInterface), intent(inout) :: lineArray
    class(FaceArrayInterface), pointer, intent(inout) :: lineFaceArray
    type(RealVectorType), intent(in) :: velocitySpecification
    class(ScalarFieldSpecificationListType), intent(in) :: &
         scalarSpecifications
    class(LogType), intent(inout), optional :: log
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: ispc
    integer :: i
    call beginSub( MOD_NAME, &
         'setUp_TestGrid1_FieldSetup1_unstructuredLines', log )

    ! no arrival points on the lines

    ! --- Gaussian points ---

    ! create the point arrangement
    call createArrayPointArrangement( pa, lineArray, 3, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the gaussian points
    call createInteriorGaussianPointCollection( ispc, interiorRegion, &
         pa, log )
    
    ! attach the face extension and get access to it
    call attachInteriorFaceArray( lineArray, ispc, log )
    call lineArray%getArrayExtension( lineFaceArray, log )
    
    call endSub( log )
  end subroutine setUp_TestGrid1_FieldSetup1_unstructuredLines
  
  
  subroutine setUp_TestGrid1_FieldSetup1_structuredRectangles( &
       rectangleGrid, rectangleCellGrid, velocitySpecification, &
       scalarSpecifications, log )
    class(PolytopeGridInterface), intent(inout) :: rectangleGrid
    class(CellGridInterface), pointer, intent(inout) :: rectangleCellGrid
    type(RealVectorType), intent(in) :: velocitySpecification
    class(ScalarFieldSpecificationListType), intent(in) :: &
         scalarSpecifications
    class(LogType), intent(inout), optional :: log
    type(IntAveScalarMomentFieldListType) :: iasmfList
    type(IntAveVectorMomentFieldListType) :: iavmfList
    class(IntAveScalarMomentFieldInterface), allocatable :: iasmf
    class(IntAveVectorMomentFieldInterface), allocatable :: iavmf
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: ispc
    integer :: i
    call beginSub( MOD_NAME, &
         'setUp_TestGrid1_FieldSetup1_structuredRectangles', log )

    ! no arrival points on the rectangles

    ! --- Gaussian points ---

    ! create the point arrangement
    call createGridPointArrangement( pa, rectangleGrid, 3, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! make a one-node list of moment fields.  We require 0th spatial
    ! derivatives.
    call createZerothSpatialDerivative( dsd, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    call createIntAveScalarMomentGrid( iasmf, scalar, dsd, &
         rectangleGrid, pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! append to the list
    call iasmfList%append( iasmf, log )
    call assertEqual( 1, iasmfList%size(), 'iasmfList should be one node &
         &long.' )
    
    ! make a dummy vector field to act as the velocity field.  We do not
    ! append it to a list because we expect just the zeroth derivative.
    call createZerothSpatialDerivative( dsd, log )
    call createIntAveVectorMomentGrid( iavmf, velocity, dsd, &
         rectangleGrid, pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    ! the velocity spec is primitive, so we can apply a blanket
    ! initialisation now
    call iavmf%setField( velocitySpecification )

    ! create the gaussian points.  These do not hold moment fields; cells
    ! do.  Nevertheless the creation of moment fields must precede the
    ! creation of gaussian points, because in the latter the
    ! PointArrangement gets injected and becomes unreachable.
    call createInteriorGaussianPointCollection( ispc, interiorRegion, &
         pa, log )
    
    ! attach the cell extension and get access to it
    call attachCellGrid( rectangleGrid, ispc, iavmf, iasmfList, log )
    call rectangleGrid%getGridExtension( rectangleCellGrid, log )

    ! don't forget to ask the cells to initialise their fields
    call rectangleCellGrid%initialiseScalarFields( scalarSpecifications, &
         log )
    
    call endSub( log )
  end subroutine setUp_TestGrid1_FieldSetup1_structuredRectangles
  
  
  subroutine setUp_TestGrid1_FieldSetup1_unstructuredTriangles( &
       triangleArray, triangleCellArray, velocitySpecification, &
       scalarSpecifications, log )
    class(PolytopeArrayInterface), intent(inout) :: triangleArray
    class(CellArrayInterface), pointer, intent(inout) :: triangleCellArray
    type(RealVectorType), intent(in) :: velocitySpecification
    class(ScalarFieldSpecificationListType), intent(in) :: &
         scalarSpecifications
    class(LogType), intent(inout), optional :: log
    type(PointScalarMomentFieldListType) :: psmfList
    type(PointVectorMomentFieldListType) :: pvmfList
    class(PointScalarMomentFieldInterface), allocatable :: psmf
    class(PointVectorMomentFieldInterface), allocatable :: pvmf
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: ispc
    class(DeparturePointCollectionInterface), allocatable :: dpc
    integer :: i
    call beginSub( MOD_NAME, &
         'setUp_TestGrid1_FieldSetup1_unstructuredTriangles', log )
    
    ! --- Arrival points ---

    ! create the point arrangement
    call createArrayPointArrangement( pa, triangleArray, 1, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! make a one-node list of moment fields.  We require 0th spatial
    ! derivatives.
    call createZerothSpatialDerivative( dsd, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call createPointScalarMomentArray( psmf, scalar, dsd, &
         triangleArray, pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! append to the list
    call psmfList%append( psmf, log )
    call assertEqual( 1, psmfList%size(), 'psmfList should be one &
         &node long.' )
    
    ! make a dummy vector field to act as the velocity field
    call createZerothSpatialDerivative( dsd, log )
    call createPointVectorMomentArray( pvmf, velocity, dsd, &
         triangleArray, pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    ! the velocity spec is primitive, so we can apply a blanket
    ! initialisation now
    call pvmf%setField( velocitySpecification )
    call pvmfList%append( pvmf, log )
    call assertEqual( 1, pvmfList%size(), 'pvmfList should be one &
         &node long.' )
    
    ! create the points, initialise scalar field(s) and inject the points
    ! into the appropriate PolytopeCollection.  Note the .false. argument
    ! denoting least squares treatment
    call createInteriorArrivalPointCollection( ispc, pa, interiorRegion, &
         log )
    call createInteriorDeparturePointCollection( dpc, ispc, .false., &
         pvmfList, psmfList, log )
    call dpc%initialiseScalarFields( scalarSpecifications, log )
    call triangleArray%injectDeparturePoints( dpc, log )
    
    
    ! --- Gaussian points ---

    ! create the point arrangement
    call createArrayPointArrangement( pa, triangleArray, 3, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the gaussian points
    call createInteriorGaussianPointCollection( ispc, interiorRegion, &
         pa, log )
    
    ! attach the cell extension and get access to it
    call attachCellArray( triangleArray, ispc, log )
    call triangleArray%getArrayExtension( triangleCellArray, log )
    
    ! no fields exist here, so don't bother calling initialiseScalarFields

    call endSub( log )
  end subroutine setUp_TestGrid1_FieldSetup1_unstructuredTriangles


  !-------------------------------------------------------------------
  !- self tests
  !-------------------------------------------------------------------
  
  subroutine setUp
    class(ScalarFieldSpecificationInterface), allocatable :: sfs
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    type(ScalarFieldSpecificationListType) :: sfsl
    call log%init( MOD_NAME, 'setup' )

    call createScalarFlowVariableStub( scalar, 'passiveScalar', log )
    call createVectorFlowVariableStub( velocity, 'velocity', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call createZerothSpatialDerivative( dsd, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call createUniformScalarFieldSpecification( sfs, scalar, dsd, &
         0._FLOAT, log )
    call sfsl%append( sfs, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call setUp_TestGrid1_FieldSetup1( vector([0._FLOAT, 0._FLOAT]), &
         vector([18._FLOAT, 12._FLOAT]), vector([6, 4]), &
         vector([0._FLOAT, 0._FLOAT]), sfsl, log )

  end subroutine setUp

  
  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )
    
    call tearDown_TestGrid1_FieldSetup1( log )

    call destroy( scalar, log )
    call destroy( velocity, log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown


!!$  subroutine testTestGrid1_FieldSetup1
!!$    if ( log%test(MOD_NAME, 'testTestGrid1_FieldSetup1', FATAL) ) return
!!$    
!!$  end subroutine testTestGrid1_FieldSetup1

  
end module TestGrid1_FieldSetup1
