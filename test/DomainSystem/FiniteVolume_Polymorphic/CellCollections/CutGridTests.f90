module CutGridTests

  use TestUtilities
  use Global
  use FiniteVolumeModule
  use FlowFieldModule

  ! dynamic modules under test
  use PolytopeCollectionsModule
  use PolytopeComplexesModule
  use CellCollectionsModule
  use FaceCollectionsModule
  use PolytopeComplexesModule

  ! this module defines the test geometry
  use SmallStepGrid

  implicit none
  
  character(*), parameter, private :: MOD_NAME = &
       'CutGridTests'
  
  type(PolytopePointerType) :: pp
  type(PolytopePointerSetType) :: g_boundaries, D_boundaries
  type(TestLogType) :: log

  class(PolytopeComplexInterface), dimension(:), allocatable :: &
       cutRectangles
  class(PolytopeComplexInterface), dimension(:), allocatable :: &
       cutXLines, cutYLines

  class(InteriorStaticPointCollectionInterface), allocatable :: igpc
  class(IntAveVectorMomentFieldInterface), allocatable :: viamf
  type(IntAveScalarMomentFieldListType) :: siamfList
  
  class(CellGridInterface), pointer :: cellRectangleGrid
  
  
contains

  !-------------------------------------------------------------------
  !- fixture setup/teardown
  !-------------------------------------------------------------------

  subroutine setUp
    class(IntAveVectorMomentFieldInterface), allocatable :: iavmf
    type(IntAveScalarMomentFieldListType) :: iasmfList
    call log%init( MOD_NAME, 'setup' )
    
    ! set up the geometry
    call setUp_SmallStepGrid( log )
    if ( log%test(FATAL) ) return
    
    ! make extensions
    call attachInteriorFaceGrid( xLineGrid, igpc, log )
    call attachInteriorFaceGrid( yLineGrid, igpc, log )
    call attachInteriorFaceArray( xLineArray, igpc, log )
    call attachInteriorFaceArray( yLineArray, igpc, log )
    call attachInteriorFaceArray( freeLineArray, igpc, log )
    if ( log%test(FATAL) ) return

    ! iavmf and iasmfList are dummy arguments here
    call attachCellGrid( rectangleGrid, igpc, iavmf, iasmfList, log )
    call attachCellArray( triangleArray, igpc, log )
    if ( log%test(FATAL) ) return
    
    ! we will need to manipulate the rectangle extension
    call rectangleGrid%getGridExtension( cellRectangleGrid, log )
    
    ! create simplicial complexes
    call createUnmixedSimplicialComplexArray( cutXLines, &
         xLineArray, [1], [1], log )
    if ( log%test(FATAL) ) return
    call createUnmixedSimplicialComplexArray( cutYLines, &
         yLineArray, [1], [1], log )
    if ( log%test(FATAL) ) return
    call createUnmixedSimplicialComplexArray( cutRectangles, &
         triangleArray, [1:4], [4], log )
    if ( log%test(FATAL) ) return
    
    ! use these to represent cuts in the grid
    call xLineGrid%breakUp( [vector([3, 1])], cutXLines, log )
    if ( log%test(FATAL) ) return
    call yLineGrid%breakUp( [vector([4, 1])], cutYLines, log )
    if ( log%test(FATAL) ) return
    call rectangleGrid%breakUp( [vector([3, 1])], cutRectangles, &
         log )
    if ( log%test(FATAL) ) return
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )
    
    ! destroy simplicial complexes
    call destroy( cutXLines, log )
    call destroy( cutYLines, log )
    call destroy( cutRectangles, log )
    
    ! tear down the geometry
    call tearDown_SmallStepGrid( log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown


  subroutine testGetExtents_RectangleGrid
    type(GridParametersType) :: gp
    if ( log%test(MOD_NAME, 'testGetExtents_RectangleGrid', &
         FATAL) ) return
    
    gp = rectangleGrid%getGridParameters()
    call assertEqual( 0._FLOAT, gp%extents(1)%getValue(1), &
         TOL, 'Failed to return correct xMin.' )
    call assertEqual( 0._FLOAT, gp%extents(1)%getValue(2), &
         TOL, 'Failed to return correct yMin.' )
    call assertEqual( 1.5_FLOAT, gp%extents(2)%getValue(1), &
         TOL, 'Failed to return correct xMax.' )
    call assertEqual( 0.8_FLOAT, gp%extents(2)%getValue(2), &
         TOL, 'Failed to return correct yMax.' )
  end subroutine testGetExtents_RectangleGrid
  
  
  subroutine testGetSpacing_RectangleGrid
    type(GridParametersType) :: gp
    if ( log%test(MOD_NAME, 'testGetSpacing_RectangleGrid', &
         FATAL) ) return
    
    gp = rectangleGrid%getGridParameters()
    call assertEqual( 0.5_FLOAT, gp%spacing%getValue(1), &
         TOL, 'Failed to return correct dx.' )
    call assertEqual( 0.4_FLOAT, gp%spacing%getValue(2), &
         TOL, 'Failed to return correct dy.' )
  end subroutine testGetSpacing_RectangleGrid

  
  subroutine testConvertCoordsToGridAddress_inCells
    type(IntVectorType) :: gridAddress
    if ( log%test(MOD_NAME, &
         'testConvertCoordsToGridAddress_inCells', FATAL) ) return
    
    call cellRectangleGrid%convert( vector([1.1_FLOAT, 0.1_FLOAT]), &
         gridAddress )
    call assertEqual( [3, 1], gridAddress%getValues(), 'Failure to &
         &convert coordinates to correct grid address.' )
  end subroutine testConvertCoordsToGridAddress_inCells
    

  subroutine testLocateTriangleD_inGrid
    type(PolytopePointerType) :: actual, expected
    if ( log%test(MOD_NAME, &
         'testLocateTriangleD_inComplex', FATAL) ) return

    call cellRectangleGrid%locate( vector([1.1_FLOAT, &
         0.1_FLOAT]), actual, log )
    if ( log%test(FATAL) ) return

    expected = ptr(triangleArray, 4)
    call assertTrue( actual%sameAs( expected ), 'Should point &
         &to '//trim(expected%describe())//'; returned '//trim(&
         actual%describe())//'.' )
  end subroutine testLocateTriangleD_inGrid
  

  subroutine testLocateRectangle21_inGrid
    type(PolytopePointerType) :: actual, expected
    if ( log%test(MOD_NAME, &
         'testLocateRectangle21_inGrid', FATAL) ) return

    call cellRectangleGrid%locate( vector([0.9_FLOAT, &
         0.1_FLOAT]), actual, log )
    if ( log%test(FATAL) ) return

    expected = ptr(rectangleGrid, [2, 1])
    call assertTrue( actual%sameAs( expected ), 'Should point &
         &to '//trim(expected%describe())//'; returned '//trim(&
         actual%describe())//'.' )
  end subroutine testLocateRectangle21_inGrid

  
  subroutine testCoordsAreInsideGrid
    type(IntVectorType) :: gridAddress
    type(IntVectorType) :: relationToGrid
    if ( log%test(MOD_NAME, 'testCoordsAreInsideGrid', FATAL) ) return
    
    call cellRectangleGrid%convert( vector([1.1_FLOAT, 0.1_FLOAT]), &
         gridAddress, relationToGrid )
    call assertEqual( [0, 0], relationToGrid%getValues(), 'Failure &
         &to confirm that coords are inside grid.' )
    call assertTrue( all(relationToGrid == 0), 'Failure' )
  end subroutine testCoordsAreInsideGrid

  
  subroutine testCoordsAreOutsideGrid_BeyondXMax
    type(IntVectorType) :: gridAddress
    type(IntVectorType) :: relationToGrid
    if ( log%test(MOD_NAME, &
         'testCoordsAreOutsideGrid_BeyondXMax', FATAL) ) return
    
    call cellRectangleGrid%convert( vector([1.6_FLOAT, 0.1_FLOAT]), &
         gridAddress, relationToGrid )
    call assertEqual( [1, 0], relationToGrid%getValues(), 'Failure &
         &to confirm that xMax (and only xMax) has been exceeded.' )
  end subroutine testCoordsAreOutsideGrid_BeyondXMax

  
  subroutine testCoordsAreOutsideGrid_BeyondXMinAndYMax
    type(IntVectorType) :: gridAddress
    type(IntVectorType) :: relationToGrid
    if ( log%test(MOD_NAME, &
         'testCoordsAreOutsideGrid_BeyondXMinAndYMax', FATAL) ) &
         return
    
    call cellRectangleGrid%convert( vector([-0.1_FLOAT, 1.1_FLOAT]), &
         gridAddress, relationToGrid )
    call assertEqual( [-1, 1], relationToGrid%getValues(), 'Failure &
         &to confirm that coords have fallen short of xMin and &
         &exceeded yMax.' )
  end subroutine testCoordsAreOutsideGrid_BeyondXMinAndYMax


  subroutine testLocateNull_outsideGrid
    type(PolytopePointerType) :: actual, expected
    if ( log%test(MOD_NAME, 'testLocateNull_outsideGrid', FATAL) ) &
         return
    
    call cellRectangleGrid%locate( vector([1.6_FLOAT, &
         0.1_FLOAT]), actual, log ) 
    if ( log%test(FATAL) ) return

    call expected%deinit()
    call assertTrue( actual%sameAs( expected ), 'Should point &
         &to '//trim(expected%describe())//'; returned '//trim(&
         actual%describe())//'.' )
  end subroutine testLocateNull_outsideGrid
  

end module CutGridTests
