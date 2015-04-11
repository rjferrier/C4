module ScalarTriangularProfileTests
  ! this test will emulate numerical experiments carried out in Matlab.
  ! The following values will be stored at the various points.  The
  ! 1./.1/.2 on the lower left hand corner refers to zeroth, x- and y-
  ! gradient respectively.  The cell integrated average is 0.7.
  !
  !      |   .5
  !    4 +   +--
  !      |   |  \--+.5
  !      +   |      \---
  !      |   +.8        \---.5
  !    2 +   |      +.9     \+--  
  !      |   |                  \---
  !    1 +   +-----------+-----------+ 
  !      | 1./.1/.2     .8            .5
  !   ---+---+---+---+---+---+---+---+--
  !      |   1       3               7
  !
  ! The normal vector of the sloping face is [1, 2]/sqrt(5).
  ! We will go anticlockwise when ordering the various elements.


  use TestUtilities
  use Global
  use FiniteVolumeModule
  use FlowFieldModule
  use SemiLagrangianModule
  use DomainModule
  
  ! systems under test
  use ProfilesModule
  use InteriorArrivalPointsModule
  use PointMomentFieldsModule
  use BoundaryArrivalPointsModule
  use PointBoundaryConditionsModule
  use InteriorGaussianPointsModule
  use IntAveMomentFieldsModule
  use CellCollectionsModule
  use PureProfileOperationsModule
  use PureLinearOperationsModule

  ! helpers
  use RegionsModule
  use PolytopeCollectionsModule
  use PointArrangementsModule
  use SemiLagrangian_Stubs
  use SpatialDerivativesModule
  use FlowField_Stubs
  use DirectionsModule
  use LinearSystemsModule
  use LinearOperandAssertions
  use ProfileAssertions
  use FiniteVolume_Stubs
  
  implicit none
  
  character(*), parameter :: MOD_NAME = 'ScalarTriangularProfileTests'

  ! systems under test
  class(ScalarProfileInterface), dimension(:), allocatable, target :: &
       profileArray
  class(ScalarProfileInterface), pointer :: profile
  class(ScalarFlowVariableInterface), allocatable, target :: sfv1
  class(VectorFlowVariableInterface), allocatable, target :: vfv
  class(CellArrayInterface), pointer :: cellExtension
  
  ! helpers
  class(BoundaryRegionInterface), allocatable :: boundaryRegion
  class(InteriorRegionInterface), allocatable :: interiorRegion
  class(PolytopeArrayInterface), allocatable :: interiorPointArray, &
       boundaryPointArray, interiorLineArray, boundaryLineArray, &
       triangleArray
  type(TestLogType) :: log
  
  class(ImpureScalarLinearOperationInterface), allocatable :: &
       linOpAssert
  class(ImpureScalarProfileOperationInterface), allocatable :: &
       profileAssert

  
  
contains

  
  subroutine setUp
    class(DirectionInterface), allocatable :: d
    type(PointScalarBoundaryConditionListType) :: spbcList
    class(PointScalarBoundaryConditionInterface), allocatable :: spbc
    class(PointVectorBoundaryConditionInterface), allocatable :: vpbc
    type(PolytopePointerSetType) :: pps
    type(CellPointerType) :: cp
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    type(RealVectorType) :: r
    call log%init( MOD_NAME, 'setup' )
        
    ! create flow variables 
    call createScalarFlowVariableStub( sfv1, 'var1', log )
    call createVectorFlowVariableStub( vfv, 'velocity', log )
    if ( log%test(FATAL) ) return

    ! create boundary conditions.  Make a one-node list of scalar
    ! boundary conditions.  We will choose Dirichlet BCs.  Choose a
    ! value of 0.5.  We will be choosing unique values for the various
    ! moments in order to make the system easie to debug.
    call createPointDirichletBoundaryCondition( spbc, sfv1, 0.5_FLOAT, &
         log )
    if ( log%test(FATAL) ) return
    call spbcList%append( spbc, log )
    
    ! do the same for the velocity.  Make a no-slip condition.
    call createPointNoSlipBoundaryCondition( vpbc, vfv, vector(), &
         log )
    if ( log%test(FATAL) ) return
    
    ! create regions.  Inlining the creation of a temporary unit vector in
    ! createObliqueDirection's argument list causes a runtime bug;
    ! set this vector explicitly.
    call r%setElement(1, 1._FLOAT)
    call r%setElement(2, 2._FLOAT)
    call r%normalise()
    call createObliqueDirection( d, r, log )
    call createFlatBoundaryRegion( boundaryRegion, 'BoundaryRegion', &
         vpbc, spbcList, d, log )
    call createInteriorRegion( interiorRegion, 'InteriorRegion', log )
    if ( log%test(FATAL) ) return
    
    ! create points 
    call setUp_InteriorVerticies( log )
    call setUp_BoundaryVerticies( log )
    call setUp_InteriorLines( log )
    call setUp_BoundaryLines( log )
    call setUp_InteriorTriangle( log )
    if ( log%test(FATAL) ) return

    call cp%init( cellExtension, 1 )
    ! create ONE profile.  This needs to be in the form of an array as we
    ! want to wrap it with a ComplexProfile later, but we can recover the
    ! single profile using a pointer.
    call createZerothSpatialDerivative( dsd, log )
    call allocScalarSimplexProfileArray( profileArray, 1, log )
    if ( log%test(FATAL) ) return
    call initScalarSimplexProfileArrayElement( profileArray, 1, sfv1, &
         cp, dsd, NCOEFS_ADV, log )
    if ( log%test(FATAL) ) return
    profile => profileArray(1)
    
    ! get the profile to build its matricies (but do not process them
    ! until the tests need to)
    call profile%buildSystems( log )
    if ( log%test(FATAL) ) return

  end subroutine setUp
  
  
  subroutine tearDown
    integer :: deallocStat
    call log%reinit( MOD_NAME, 'teardown' )

    call destroy( profileAssert, log )
    call destroy( linOpAssert, log )
    
    nullify(profile)
    call destroy( profileArray, log )

    call destroy( triangleArray, log )
    call destroy( boundaryLineArray, log )
    call destroy( interiorLineArray, log )
    call destroy( boundaryPointArray, log )
    call destroy( interiorPointArray, log )
    
    call destroy( boundaryRegion, log )
    call destroy( interiorRegion, log )

    call destroy( vfv, log )
    call destroy( sfv1, log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown

  
  
  !-------------------------------------------------------------------
  !- extended setup routines
  !-------------------------------------------------------------------
  
  subroutine setUp_InteriorVerticies( log )
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
    call beginSub( MOD_NAME, 'setUp_InteriorVerticies', log )

    ! create the polytope array
    call createPointArray( interiorPointArray, [1._FLOAT, 1._FLOAT], log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the point arrangement
    call createArrayPointArrangement( pa, interiorPointArray, &
         1, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! make a list of moment fields.  We require both 0th and 1st
    ! spatial derivatives.
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

       call createPointScalarMomentArray( psmf, sfv1, dsd, &
            interiorPointArray, pa, log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if

       ! assign 1 for the 0th sp deriv and 0.1/0.2 for the 1st.
       select case ( i )
       case (1)
          call psmf%setField( [1._FLOAT] )
       case (2)
          call psmf%setField( [0.1_FLOAT] )
       case (3)
          call psmf%setField( [0.2_FLOAT] )
       end select
       call psmfList%append( psmf, log )
    end do
    call assertEqual( 3, psmfList%size(), 'psmfList should be three &
         &nodes long.' )

    ! make a dummy vector field to act as the velocity field.  It also
    ! needs to be added to a list (because the total velocity field
    ! could be further be made up by velocity gradients)
    call createZerothSpatialDerivative( dsd, log )
    call createPointVectorMomentArray( pvmf, vfv, dsd, &
         interiorPointArray, pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call pvmfList%append( pvmf, log )
    call assertEqual( 1, pvmfList%size(), 'pvmfList should be one &
         &node long.' )

    ! create the points and inject into the appropriate PolytopeCollection
    call createInteriorArrivalPointCollection( ispc, pa, interiorRegion, &
         log )
    call createInteriorDeparturePointCollection( dpc, ispc, .true., &
         pvmfList, psmfList, log )
    call interiorPointArray%injectDeparturePoints( dpc, log )

    call endSub( log )
  end subroutine setUp_InteriorVerticies


  subroutine setUp_BoundaryVerticies( log )
    class(LogType), intent(inout), optional :: log
    class(UndirectedSpatialDerivativeInterface), allocatable :: usd
    class(PointArrangementInterface), allocatable :: pa
    class(BoundaryStaticPointCollectionInterface), allocatable :: bspc
    class(DeparturePointCollectionInterface), allocatable :: dpc
    integer :: i
    call beginSub( MOD_NAME, 'setUp_BoundaryVerticies', log )
    
    ! create the polytope array
    call createPointArray( boundaryPointArray, [7._FLOAT, 1._FLOAT, &
         1._FLOAT, 4._FLOAT], log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the point arrangement
    call createArrayPointArrangement( pa, boundaryPointArray, 1, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the points and inject into the appropriate PolytopeCollection
    call createBoundaryArrivalPointCollection( bspc, pa, boundaryRegion, &
         log )
    call createBoundaryDeparturePointCollection( dpc, bspc, .true., &
         boundaryRegion, log )
    call boundaryPointArray%injectDeparturePoints( dpc, log )

    call endSub( log )
  end subroutine setUp_BoundaryVerticies


  subroutine setUp_InteriorLines( log )
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
    call beginSub( MOD_NAME, 'setUp_InteriorLines', log )

    ! create the polytope array
    call createSimplexArray( interiorLineArray, 1, [ptr(&
         interiorPointArray, 1), ptr(boundaryPointArray, 1), ptr(&
         interiorPointArray, 1), ptr(boundaryPointArray, 2)], &
         log=log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the point arrangement
    call createArrayPointArrangement( pa, interiorLineArray, 1, log )
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

    call createPointScalarMomentArray( psmf, sfv1, dsd, &
         interiorLineArray, pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! assign 0.8 for the 0th sp deriv
    call psmf%setField( [0.8_FLOAT, 0.8_FLOAT] )
    call psmfList%append( psmf, log )
    call assertEqual( 1, psmfList%size(), 'psmfList should be one &
         &node long.' )

    ! make a dummy vector field to act as the velocity field
    call createZerothSpatialDerivative( dsd, log )
    call createPointVectorMomentArray( pvmf, vfv, dsd, &
         interiorLineArray, pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call pvmfList%append( pvmf, log )
    call assertEqual( 1, pvmfList%size(), 'pvmfList should be one &
         &node long.' )

    ! create the points and inject into the appropriate PolytopeCollection
    call createInteriorArrivalPointCollection( ispc, pa, interiorRegion, &
         log )
    call createInteriorDeparturePointCollection( dpc, ispc, .true., &
         pvmfList, psmfList, log )
    call interiorLineArray%injectDeparturePoints( dpc, log )

    call endSub( log )
  end subroutine setUp_InteriorLines


  subroutine setUp_BoundaryLines( log )
    class(LogType), intent(inout), optional :: log
    type(PointScalarBoundaryConditionListType) :: spbcList
    class(PointScalarBoundaryConditionInterface), allocatable :: spbc
    class(PointVectorBoundaryConditionInterface), allocatable :: vpbc
    class(PointArrangementInterface), allocatable :: pa
    class(BoundaryStaticPointCollectionInterface), allocatable :: bspc
    class(DeparturePointCollectionInterface), allocatable :: dpc
    integer :: i
    call beginSub( MOD_NAME, 'setUp_BoundaryLines', log )

    ! create the polytope array
    call createSimplexArray( boundaryLineArray, 1, [ptr(&
         boundaryPointArray, 1), ptr(boundaryPointArray, 2)], &
         log=log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the point arrangement
    call createArrayPointArrangement( pa, boundaryLineArray, 3, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    call createBoundaryArrivalPointCollection( bspc, pa, boundaryRegion, &
         log )
    call createBoundaryDeparturePointCollection( dpc, bspc, .true., &
         boundaryRegion, log )
    call boundaryLineArray%injectDeparturePoints( dpc, log )

    call endSub( log )
  end subroutine setUp_BoundaryLines


  subroutine setUp_InteriorTriangle( log )
    class(LogType), intent(inout), optional :: log
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: iapc
    class(StaticPointCollectionInterface), allocatable :: apc
    type(PointScalarMomentFieldListType) :: psmfList
    type(PointVectorMomentFieldListType) :: pvmfList
    class(PointScalarMomentFieldInterface), allocatable :: psmf
    class(PointVectorMomentFieldInterface), allocatable :: pvmf
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    type(IntAveScalarMomentFieldListType) :: siamfList
    class(IntAveScalarMomentFieldInterface), allocatable :: siamf
    class(IntAveVectorMomentFieldInterface), allocatable :: viamf
    class(InteriorStaticPointCollectionInterface), allocatable :: ispc
    class(DeparturePointCollectionInterface), allocatable :: dpc
    integer :: i
    call beginSub( MOD_NAME, 'setUp_InteriorTriangle', log )
    
    ! create the polytope array
    call createSimplexArray( triangleArray, 2, [ptr(interiorLineArray, &
         1), ptr(boundaryLineArray, 1), ptr(interiorLineArray, 2)], &
         log=log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
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
    
    call createPointScalarMomentArray( psmf, sfv1, dsd, &
         triangleArray, pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! assign 0.9 for the 0th sp deriv
    call psmf%setField( [0.9_FLOAT] )
    call psmfList%append( psmf, log )
    call assertEqual( 1, psmfList%size(), 'psmfList should be one &
         &node long.' )

    ! make a dummy vector field to act as the velocity field
    call createZerothSpatialDerivative( dsd, log )
    call createPointVectorMomentArray( pvmf, vfv, dsd, triangleArray, &
         pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call pvmfList%append( pvmf, log )
    call assertEqual( 1, pvmfList%size(), 'pvmfList should be one node &
         &long.' )
    
    ! create the points and inject into the appropriate PolytopeCollection
    ! note the .false. argument denoting least squares treatment
    call createInteriorArrivalPointCollection( ispc, pa, interiorRegion, &
         log )
    call createInteriorDeparturePointCollection( dpc, ispc, .false., &
         pvmfList, psmfList, log )
    call triangleArray%injectDeparturePoints( dpc, log )


    ! --- Gaussian points ---

    ! create the point arrangement
    call createArrayPointArrangement( pa, triangleArray, 3, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! unlike arrival points, gaussian points do not hold moment fields;
    ! rather it is cells that hold both gaussian points and moment fields.
    ! Create the gaussian points first.
    call createInteriorGaussianPointCollection( ispc, interiorRegion, &
         pa, log )

    ! attach the cell extension and get access to it
    call attachCellArray( triangleArray, ispc, log )
    call triangleArray%getArrayExtension( cellExtension, &
         log )

    call endSub( log )
  end subroutine setUp_InteriorTriangle

  
  !-------------------------------------------------------------------
  !- helper(s)
  !-------------------------------------------------------------------
  
  subroutine createOneElementLinearSystem( sls, log )
    class(ScalarLinearSystemInterface), allocatable, intent(inout) :: sls
    class(LogType), intent(inout), optional :: log
    class(ScalarMatrixInterface), allocatable :: A
    class(ScalarRHSColumnInterface), allocatable :: b
    type(ScalarLinOpBasedRHSGeneratorType) :: rhsg
    call beginSub( MOD_NAME, 'createOneElementLinearSystem', log )

    ! create a linear system
    call createScalarMultiRowMatrix( A, 1, 1, log=log )
    call createScalarRHSColumn( b, 1, 1, rhsg, log=log )
    call createScalarLinearSystem( sls, A, b, log=log )

    call endSub( log )
  end subroutine createOneElementLinearSystem

  
  subroutine setOneElementLinearSystem( sls, profOp, log, selector )
    class(ScalarLinearSystemInterface), allocatable, intent(inout) :: sls
    class(PureScalarProfileOperationInterface), intent(inout) :: profOp
    class(LogType), intent(inout), optional :: log
    character(3), intent(in), optional :: selector
    class(ScalarMatrixInterface), allocatable :: A
    class(ScalarRHSColumnInterface), allocatable :: b
    class(PureScalarLinearOperationInterface), allocatable :: linOp
    logical :: m, r
    call beginSub( MOD_NAME, 'setOneElementLinearSystem', log )

    ! determine the scope of this subroutine
    if ( present(selector) ) then
       select case ( selector )
       case ('mat')
          ! matrix only
          m = .true. 
          r = .false.
       case ('rhs')
          ! rhs only
          m = .false. 
          r = .true. 
       end select
    else
       ! both
       m = .true.
       r = .true. 
    end if
    
    ! at this level, we don't know what kind of linear operation is needed
    ! to set the Matrix and RHSColumn (via RHSGenerator) elements.  But
    ! the profile operation does.  Use it to create the needed linear
    ! operation.  The profile operation should have been initialised
    ! beforehand.
    call profOp%convert( linOp, log )
    if (m) call sls%setMatrixRows( linOp, log )
    if (r) call sls%setRHSElements( linOp, log )

    ! the above initialisations were based on cloning linOp, so clean up
    ! is necessary
    call destroy( linOp, log )
    
    call endSub( log )
  end subroutine setOneElementLinearSystem

  
  !-------------------------------------------------------------------
  !- the tests
  !-------------------------------------------------------------------
  
  subroutine testNumEquations
    if ( log%test(MOD_NAME, 'testNumEquations', FATAL) ) &
         return

    call assertEqual( 9, profile%getNumEquations('E'), 'Should have &
         &nine exactly constrained rows due to this many point moments &
         &on the edges and corners.  The point moment in the middle of &
         &the triangle should be least squares-treated.' )
    
    call assertEqual( 1, profile%getNumEquations('L'), 'Should have &
         &one least squares-treated row owing to the point moment in &
         &the middle of the triangle.' )
  end subroutine testNumEquations
  

  subroutine testExactlyConstrainedMatrix
    real(FLOAT), dimension(10, 9) :: expected
    if ( log%test(MOD_NAME, 'testExactlyConstrainedMatrix', FATAL) ) &
         return

    ! note transposition
    expected = reshape([&
         1.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 0.00000000000000_FLOAT, &
         0.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         7.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         4.00000000000000_FLOAT, 5.73205080756888_FLOAT, &
         2.26794919243112_FLOAT, 1.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 0.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 49.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 16.00000000000000_FLOAT, &
         32.85640646055101_FLOAT, 5.14359353944898_FLOAT, &
         1.00000000000000_FLOAT, 2.00000000000000_FLOAT, &
         0.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         343.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         64.00000000000000_FLOAT, 188.33459118601269_FLOAT, &
         11.66540881398725_FLOAT, 1.00000000000000_FLOAT, &
         3.00000000000000_FLOAT, 0.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         4.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         1.63397459621556_FLOAT, 3.36602540378444_FLOAT, &
         2.50000000000000_FLOAT, 0.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         7.00000000000000_FLOAT, 4.00000000000000_FLOAT, &
         4.00000000000000_FLOAT, 9.36602540378444_FLOAT, &
         7.63397459621556_FLOAT, 2.50000000000000_FLOAT, &
         1.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 49.00000000000000_FLOAT, &
         4.00000000000000_FLOAT, 16.00000000000000_FLOAT, &
         53.68653347947320_FLOAT, 17.31346652052678_FLOAT, &
         2.50000000000000_FLOAT, 2.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 16.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 2.66987298107781_FLOAT, &
         11.33012701892219_FLOAT, 6.25000000000000_FLOAT, &
         0.00000000000000_FLOAT, 2.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 7.00000000000000_FLOAT, &
         16.00000000000000_FLOAT, 4.00000000000000_FLOAT, &
         15.30384757729336_FLOAT, 25.69615242270662_FLOAT, &
         6.25000000000000_FLOAT, 1.00000000000000_FLOAT, &
         2.00000000000000_FLOAT, 1.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 64.00000000000000_FLOAT, &
         1.00000000000000_FLOAT, 4.36250462620345_FLOAT, &
         38.13749537379655_FLOAT, 15.62500000000000_FLOAT, &
         0.00000000000000_FLOAT, 3.00000000000000_FLOAT ], &
         shape=[10,9], order=[2,1] )

    ! create a Profile assertion object.  This has a nested LinearOperand
    ! assertion which needs to be (a) created with the expected values
    ! array and (b) injected into the Profile assertion object
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'A', &
         'TransposedEqualWithPermutations', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitMatrixForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'ExactlyConstr', linOpAssert, log )
    if ( log%test(FATAL) ) return

    ! link in and execute the assertion object
    call profile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return

  end subroutine testExactlyConstrainedMatrix

  
  subroutine testNullSpace
    real(FLOAT), dimension(NCOEFS_ADV, 1) :: expected
    if ( log%test(MOD_NAME, 'testNullSpace', FATAL) ) return

    call profile%factoriseSystems( log )
    if ( log%test(FATAL) ) return

    ! for verification matlab code see testNullSpace.m in the same folder
    expected = reshape([&
          0.42146361521176_FLOAT, &
          -0.46829290579085_FLOAT, &
          0.04682929057908_FLOAT, &
          0.00000000000000_FLOAT, &
          -0.51512219636993_FLOAT, &
          0.56195148694902_FLOAT, &
          -0.04682929057909_FLOAT, &
          0.09365858115817_FLOAT, &
          -0.09365858115817_FLOAT, &
          0.00000000000000_FLOAT], [NCOEFS_ADV, 1] )

    ! the expected and actual null space basis vectors will have elements
    ! of the same magnitude because they are orthonormal.  However, one
    ! vector might be the negative of the other.  Allow for this.
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'Q2', &
         'EqualOrZeroSum', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitNullSpaceForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'ExactlyConstr', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call profile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return

  end subroutine testNullSpace


  subroutine testExactlyConstrainedRHS
    real(FLOAT), dimension(9, 1) :: expected
    if ( log%test(MOD_NAME, 'testExactlyConstrainedRHS', FATAL) ) &
         return

    ! solving the profile updates the RHS
    call profile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return

    expected = reshape([&
         1.00000000000000_FLOAT, &
         0.50000000000000_FLOAT, &
         0.50000000000000_FLOAT, &
         0.80000000000000_FLOAT, &
         0.50000000000000_FLOAT, &
         0.50000000000000_FLOAT, &
         0.80000000000000_FLOAT, &
         0.10000000000000_FLOAT, &
         0.20000000000000_FLOAT], [9, 1])

    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'B', &
         'EqualWithPermutations', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitRHSColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'ExactlyConstr', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call profile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testExactlyConstrainedRHS

  
  subroutine testParticularSolution
    real(FLOAT), dimension(NCOEFS_ADV, 1) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testParticularSolution', FATAL) ) &
         return

    call profile%factoriseSystems( log )
    if ( log%test(FATAL) ) return

    ! solving the profile with no input free variables computes an
    ! internal particular solution based on the current moment values
    call profile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return

    ! for verification matlab code see ScalarTriangularProfileVerification.m
    expected = reshape([&
          0.48486842105263_FLOAT, &
          -0.00818713450292_FLOAT, &
          -0.07390350877193_FLOAT, &
          0.00833333333333_FLOAT, &
          0.71849415204678_FLOAT, &
          0.35760233918129_FLOAT, &
          -0.03165204678363_FLOAT, &
          -0.45891812865497_FLOAT, &
          -0.06330409356725_FLOAT, &
          0.06666666666667_FLOAT], [NCOEFS_ADV, 1])

    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'X', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitSolutionColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'ExactlyConstr', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call profile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testParticularSolution

  
  ! [2012-07-26: removed testProfileOpAndLinSysCoInitialisation.  This
  ! compared two methods of 'updating' a standalone LinearSystem: (i)
  ! calling perform() on some ProfileOperation, and (ii) calling refresh()
  ! on the LinearSystem.  But what we really mean by 'updating' is
  ! refreshing the RHSColumn.  The chosen ProfileOperation was a matrix
  ! operation that failed because the matrix rows had already been set
  ! using setOneElementLinearSystem.  This was switched to a RHSColumn
  ! operation, but that didn't work because the operation involved setting
  ! RHSGenerators rather than refreshing the real values.  Fine, then; we
  ! don't need the test.  ProfileOperation's perform() and LinearSystem's
  ! refresh() do different things.
  

  subroutine testReducedPointMomentMatrix
    real(FLOAT), dimension(1, 1) :: expected
    class(ScalarLinearSystemInterface), target, allocatable :: sls
    type(AppendLeastSquaresMatrix_ScalarType) :: profOp
    if ( log%test(MOD_NAME, 'testReducedPointMomentMatrix', &
         FATAL) ) return

    call profile%factoriseSystems( log )
    if ( log%test(FATAL) ) return
    call profile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return

    call createOneElementLinearSystem( sls, log )
    if ( log%test(FATAL) ) return

    ! initialise the profile operation object and finalise the linear system
    call profOp%init( sls, log )
    call profile%accept( profOp )
    call setOneElementLinearSystem( sls, profOp, log )
    if ( log%test(FATAL) ) return

    ! calling sls%refresh() is not needed because that method applies to
    ! the RHS.
    
    ! calculated in ScalarTriangularProfileVerification.m
    expected(1, 1) = 0.18731716231634_FLOAT

    ! this time we are comparing linear system components, not profile
    ! components.  No need to wrap linOpAssertion.
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'A', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitMatrixForOperand1( expected )
    call sls%acceptAsOperand2( linOpAssert )
    call linOpAssert%perform( log=log )

    call destroy( sls, log )
    if ( log%test(FATAL) ) return
  end subroutine testReducedPointMomentMatrix

  
  subroutine testReducedPointMomentRHS
    real(FLOAT), dimension(1, 1) :: expected
    class(ScalarLinearSystemInterface), target, allocatable :: sls
    type(AppendLeastSquaresRHS_ScalarType) :: profOp
    if ( log%test(MOD_NAME, 'testReducedPointMomentRHS', &
         FATAL) ) return
    
    call profile%factoriseSystems( log )
    if ( log%test(FATAL) ) return
    call profile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    call createOneElementLinearSystem( sls, log )
    if ( log%test(FATAL) ) return

    ! initialise the profile operation object and finalise the linear
    ! system
    call profOp%init( sls, log )
    call profile%accept( profOp )
    call setOneElementLinearSystem( sls, profOp, log )
    if ( log%test(FATAL) ) return

    ! update the linear system, i.e. execute the linear/profile operations
    call sls%solve()

    ! calculated in ScalarTriangularProfileVerification.m
    expected(1, 1) = -0.07105263157894_FLOAT
    
    ! as above, no need to wrap linOpAssertion
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'B', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitRHSColumnForOperand1( expected )
    call sls%acceptAsOperand2( linOpAssert )
    call linOpAssert%perform( log=log )
    
    call destroy( sls, log )
    if ( log%test(FATAL) ) return
  end subroutine testReducedPointMomentRHS

  
  subroutine testReducedCellIntAveRow
    real(FLOAT), dimension(1, 1) :: expected
    class(ScalarLinearSystemInterface), target, allocatable :: sls
    type(AppendPolytpIntAveMatrix_ScalarType) :: profOp
    if ( log%test(MOD_NAME, 'testReducedCellIntAveRow', &
         FATAL) ) return

    call profile%factoriseSystems( log )
    if ( log%test(FATAL) ) return
    call profile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return

    call createOneElementLinearSystem( sls, log )
    if ( log%test(FATAL) ) return

    ! initialise the profile operation object and finalise the linear system
    call profOp%init( sls, log )
    call profile%accept( profOp )
    call setOneElementLinearSystem( sls, profOp, log )
    if ( log%test(FATAL) ) return

    ! calling sls%refresh() is not needed because that method applies to
    ! the RHS.
    
    ! calculated in ScalarTriangularProfileVerification.m
    expected(1, 1) = 0.08429272304235_FLOAT
    
    ! as above, no need to wrap linOpAssertion
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'A', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitMatrixForOperand1( expected )
    call sls%acceptAsOperand2( linOpAssert )
    call linOpAssert%perform( log=log )

    call destroy( sls, log )
    if ( log%test(FATAL) ) return
  end subroutine testReducedCellIntAveRow

  
  subroutine testReducedCellIntAveRHS
    real(FLOAT), dimension(1, 1) :: expected
    class(ScalarLinearSystemInterface), target, allocatable :: sls
    type(AppendPolytpIntAveRHS_ScalarType) :: profOp
    type(PolytopePointerType) :: pp
    type(CellPointerType) :: cp
    type(ScalarMomentGroupListType) :: smgList
    class(ScalarMomentGroupInterface), pointer :: smg
    if ( log%test(MOD_NAME, 'testReducedCellIntAveRHS', &
         FATAL) ) return

    call profile%factoriseSystems( log )
    if ( log%test(FATAL) ) return
    call profile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    call createOneElementLinearSystem( sls, log )
    if ( log%test(FATAL) ) return
    
    ! cell arrays do not carry integrated average moments in this
    ! implementation, so we will set the desired initial 0.7 manually.
    call sls%setRHSElements( [0.7_FLOAT], log )

    ! If we were to do it the proper way, the routine would be:
    ! call smgList%setFlowVariable( sfv1 )
    ! pp = ptr(triangleArray, 1)
    ! call pp%getPointerExtension( cp )
    ! call cp%appendScalarMoments( smgList, log )
    ! if ( log%test(FATAL) ) return
    ! call smgList%find( smg, 1, log )
    ! ! check for empty node
    ! if ( log%test(WARNING) ) return
    ! call smg%appendEquations( sls, log )
    ! if ( log%test(FATAL) ) return
    ! ! update value by calling solve
    ! call sls%solve()
    
    ! we are about to modify linear system components, so reset 
    ! internal row and element counters beforehand
    call sls%prepareToReset()
    
    ! initialise the profile operation object and finalise the linear system
    call profOp%init( sls, log )
    call profile%accept( profOp )
    call setOneElementLinearSystem( sls, profOp, log )
    if ( log%test(FATAL) ) return

    ! update the linear system, i.e. execute the linear/profile operations
    call sls%solve()
    
    ! calculated in ScalarTriangularProfileVerification.m
    expected(1, 1) = -0.11697368421052_FLOAT

    ! as above, no need to wrap linOpAssertion
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'B', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitRHSColumnForOperand1( expected )
    call sls%acceptAsOperand2( linOpAssert )
    call linOpAssert%perform( log=log )

    call destroy( sls, log )
    if ( log%test(FATAL) ) return
  end subroutine testReducedCellIntAveRHS

  
  subroutine testSolve
    class(ScalarLinearSystemInterface), target, allocatable :: sls
    real(FLOAT), dimension(NCOEFS_ADV, 1) :: expected
    type(AppendPolytpIntAveMatrix_ScalarType) :: profOp_mat
    type(AppendPolytpIntAveRHS_ScalarType) :: profOp_rhs
    if ( log%test(MOD_NAME, 'testSolve', FATAL) ) return

    call profile%factoriseSystems( log )
    if ( log%test(FATAL) ) return
    call profile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return

    call createOneElementLinearSystem( sls, log )
    if ( log%test(FATAL) ) return
    
    ! cell arrays do not carry integrated average moments in this
    ! implementation, so we will set the desired initial 0.7 manually.
    call sls%setRHSElements( [0.7_FLOAT], log )

    ! we are about to modify linear system components, so reset 
    ! internal row and element counters beforehand
    call sls%prepareToReset()
    
    ! initialise the profile operation object dealing with the matrix
    call profOp_mat%init( sls, log )
    ! the third operand in the accept sequence is the simplex-to-complex
    ! volume fraction - not relevant here
    call profile%accept( profOp_mat )
    call setOneElementLinearSystem( sls, profOp_mat, log, 'mat' )
    if ( log%test(FATAL) ) return

    ! repeat for the RHS
    call profOp_rhs%init( sls, log )
    call profile%accept( profOp_rhs )
    call setOneElementLinearSystem( sls, profOp_rhs, log, 'rhs' )
    if ( log%test(FATAL) ) return

    ! since sls should have scalars for its matrix and RHS, the solution
    ! will be trivial, a scalar.  Nevertheless, go through the motions of
    ! factorising and solving.
    call sls%factorise( log )
    if ( log%test(FATAL) ) return
    call sls%solve( log=log )
    if ( log%test(FATAL) ) return

    ! the solution now within sls is the free variable needed to complete
    ! the solution of the profile under test.  We need to tell the profile
    ! this before performing finalisation.
    call profile%visit( sls )
    call profile%finalise( log )
    if ( log%test(FATAL) ) return

    ! calculated in ScalarTriangularProfileVerification.m
    expected = reshape([&
          -0.09999999999998_FLOAT, &
          0.64166666666664_FLOAT, &
          -0.13888888888889_FLOAT, &
          0.00833333333333_FLOAT, &
          1.43333333333330_FLOAT, &
          -0.42222222222219_FLOAT, &
          0.03333333333333_FLOAT, &
          -0.58888888888888_FLOAT, &
          0.06666666666666_FLOAT, &
          0.06666666666667_FLOAT ], [NCOEFS_ADV, 1])
    
    ! create a Profile assertion object.  This has a nested LinearOperand
    ! assertion which needs to be (a) created with the expected values
    ! array and (b) injected into the Profile assertion object
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'X', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitSolutionColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'ExactlyConstr', linOpAssert, log )
    if ( log%test(FATAL) ) return

    ! link in and execute the assertion object
    call profile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return

    call destroy( sls, log )
    if ( log%test(FATAL) ) return
  end subroutine testSolve


  subroutine testSolveAsComplexProfile
    class(ScalarProfileInterface), allocatable :: complexProfile
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    type(GridParametersType) :: gp
    type(PointArrangementStub) :: pa
    type(PolytopeGridStub) :: pg
    type(InteriorStaticPointCollectionStub) :: isp
    class(IntAveScalarMomentFieldInterface), allocatable, target :: iasmf
    class(ScalarMomentGroupInterface), allocatable :: smg
    real(FLOAT), dimension(NCOEFS_ADV, 1) :: expected
    if ( log%test(MOD_NAME, 'testSolveAsComplexProfile', FATAL) ) &
         return

    ! create a moment grid to represent the cell integrated average.  Most
    ! of its components can have minimal initialisations; we are only
    ! interested in storing the value of 0.7.
    call createZerothSpatialDerivative( dsd, log )
    if ( log%test(FATAL) ) return
    
    call gp%init( sizeVector([1, 1]), [vector([1._FLOAT, 1._FLOAT]), &
         vector([7._FLOAT, 4._FLOAT])], vector([6._FLOAT, 3._FLOAT]) )
    call pg%initPolytopeGrid( gp, 2, log )
    
    call createIntAveScalarMomentGrid( iasmf, sfv1, dsd, pg, pa, log )
    if ( log%test(FATAL) ) return
    call iasmf%setField( 0.7_FLOAT )

    ! package as a MomentGroup ready for injection
    call createInteriorIntAveScalarMomentGroup( smg, isp, 1, iasmf, &
         .true., log )
    if ( log%test(FATAL) ) return
    
    ! create a complex profile
    call createScalarComplexProfile( complexProfile, &
         profileArray, [1], smg, log )

    ! factorise, solveIncomplete, finalise
    call profile%factoriseSystems( log )
    if ( log%test(FATAL) ) return
    
    call complexProfile%buildSystems( log )
    if ( log%test(FATAL) ) return
    call complexProfile%factoriseSystems( log=log )
    if ( log%test(FATAL) ) return
    
    call profile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    call complexProfile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    call complexProfile%finalise( log=log )
    if ( log%test(FATAL) ) return
    
    call profile%finalise( log=log )
    if ( log%test(FATAL) ) return

    ! calculated in ScalarTriangularProfileVerification.m
    expected = reshape([&
          -0.09999999999998_FLOAT, &
          0.64166666666664_FLOAT, &
          -0.13888888888889_FLOAT, &
          0.00833333333333_FLOAT, &
          1.43333333333330_FLOAT, &
          -0.42222222222219_FLOAT, &
          0.03333333333333_FLOAT, &
          -0.58888888888888_FLOAT, &
          0.06666666666666_FLOAT, &
          0.06666666666667_FLOAT ], [NCOEFS_ADV, 1])
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'X', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitSolutionColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'ExactlyConstr', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call profile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return

    call destroy( complexProfile, log )

    ! smg must be destroyed because it does not get embedded as a
    ! component
    call destroy( smg, log )
    call destroy( iasmf, log )
  end subroutine testSolveAsComplexProfile


end module ScalarTriangularProfileTests
