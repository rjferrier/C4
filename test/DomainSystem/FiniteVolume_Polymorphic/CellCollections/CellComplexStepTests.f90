module CellComplexStepTests
  use TestUtilities
  use Global

  ! We are going to shortcut the creation of mock polytopes, so this
  ! is not technically a unit test module yet.
  use FiniteVolumeModule
  use FlowFieldModule
  use FaceCollectionsModule
  use CellCollectionsModule
  use PolytopeComplexesModule

  ! the following module defines the test geometry, which relies upon
  ! Polytope classes.
  use SmallStepGrid

  implicit none

  character(*), parameter, private :: MOD_NAME = &
       'CellComplexStepTests'

  class(PolytopeComplexInterface), allocatable :: triangleComplex
  type(PolytopePointerSetType) :: expectedPtps, actualPtps
  type(TestLogType) :: log
  
  class(CellComplexInterface), pointer :: cellTriangleComplex
  type(CellPointerType) :: dummyCellPointer
  
  class(InteriorStaticPointCollectionInterface), allocatable :: igpc
  class(IntAveVectorMomentFieldInterface), allocatable :: viamf
  type(IntAveScalarMomentFieldListType) :: siamfList
  
contains
  

  subroutine setUp
    call log%init( MOD_NAME, 'setup' )
    
    ! set up the geometry
    call setUp_SmallStepGrid( log )
    if ( log%test(FATAL) ) return

    ! make extensions.  The supporting components are empty, not 
    ! stubs, so beware of segmentation faults.
    call attachInteriorFaceArray( xLineArray, igpc, log )
    call attachInteriorFaceArray( yLineArray, igpc, log )
    call attachInteriorFaceArray( freeLineArray, igpc, log )

    ! make stub components to go with the cell extension
    call attachCellArray( triangleArray, igpc, log )
    if ( log%test(FATAL) ) return
    
    ! triangleArray contains exactly the contents of our test
    ! simplicial complex.  Use for the definition.
    call createUnmixedSimplicialComplex( triangleComplex, &
         triangleArray, [1:4], log )
    if ( log%test(FATAL) ) return

    ! make extension for the complex
    call attachCellComplex( triangleComplex, dummyCellPointer, log )
    if ( log%test(FATAL) ) return

    ! and get access to the last extension
    call triangleComplex%getComplexExtension( cellTriangleComplex, &
         log )
    
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )
        
    call destroy( triangleComplex, log )
    call expectedPtps%deinit( log )
    
    ! tear down the geometry
    call tearDown_SmallStepGrid( log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown



  subroutine testLocateTriangleD_inComplex
    type(PolytopePointerType) :: actual, expected
    if ( log%test(MOD_NAME, &
         'testLocateTriangleD_inComplex', FATAL) ) return

    call cellTriangleComplex%locate( vector([1.1_FLOAT, 0.1_FLOAT]), &
         actual, log )
    if ( log%test(FATAL) ) return

    expected = ptr(triangleArray, 4)
    call assertTrue( actual%sameAs( expected ), 'Should point to '//&
         trim(expected%describe())//'; returned '//trim(&
         actual%describe())//'.' )
    
  end subroutine testLocateTriangleD_inComplex


  subroutine testLocateTriangleC_inComplex
    type(PolytopePointerType) :: actual, expected
    if ( log%test(MOD_NAME, &
         'testLocateTriangleC_inComplex', FATAL) ) return

    call cellTriangleComplex%locate( vector([1.2_FLOAT, 0.1_FLOAT]), &
         actual, log )
    if ( log%test(FATAL) ) return

    expected = ptr(triangleArray, 3)
    call assertTrue( actual%sameAs( expected ), 'Should point to '//&
         trim(expected%describe())//'; returned '//trim(&
         actual%describe())//'.' )
    
  end subroutine testLocateTriangleC_inComplex

  
  subroutine testLocateNullWithinStep_inComplex
    type(PolytopePointerType) :: actual, expected
    if ( log%test(MOD_NAME, &
         'testLocateNullWithinStep_inComplex', FATAL) ) return
!!$
!!$    call cellTriangleComplex%locate( vector([1.3_FLOAT, &
!!$         0.05_FLOAT]), actual, log )
!!$    if ( log%test(FATAL) ) return
!!$    
!!$    call expected%deinit()
!!$    call assertTrue( actual%sameAs( expected ), 'Should point to '//&
!!$         trim(expected%describe())//'; returned '//trim(&
!!$         actual%describe())//'.' )
    
  end subroutine testLocateNullWithinStep_inComplex

end module CellComplexStepTests
 
 
