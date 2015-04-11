module ScalarPentagonalProfileTests
  ! this test will emulate numerical experiments carried out in Matlab.
  ! The following values will be stored at the various points.  The
  ! 1./.1/.2 on the Cartesian corners refers to zeroth, x- and y-
  ! gradients respectively.  The cell integrated average is 0.75.
  !     
  !             1./.1/.2          .8
  !     1.0--      +--------------+--------------+ .5
  !                |                           /- \
  !                |                         /-    \
  !                |                       /-       + .5
  !                |       1.1           /-         \
  !                |        +          /-            \
  !                |                 /-               \
  !                |               /-                  \
  !     0.5--      |   T#1        + .8      .9          \
  !                |            /-         +             + .5
  !                |          /-                         \
  !                |        /-     T#3                    \
  !    0.25--      |      /-                          /----+ .5
  !                |    /-            .8    /---------     |
  !   0.125--      |  /-          /----+----         T#2   + .8
  !                |/-  /---------           + .7          |
  ! y = 0.0--      +---------------------------------------+ 
  !             1./.1/.2                                1./.1/.2
  !     
  !                |              |    |         |         |
  !          x =  0.0          0.375  0.5       0.75       1.0
  !
  ! The normal vector of the sloping face is [3, 1]/sqrt(10).  We will go
  ! anticlockwise when ordering the various elements, except when it comes
  ! to the triangles.  The order of the triangles is given above in the
  ! labels T#1-T#3, reflecting the ordering chosen by functions in the
  ! Matlab verification code ScalarPentagonalProfileVerification.m
  !
  ! The purpose of this test module is twofold.  Firstly, although the
  ! functionality of PrimitiveProfiles has been tested to precision in
  ! ScalarTriangularProfileTests, that test case is not bulletproof.  So
  ! the current test module retests some of PrimitiveProfile
  ! functionality, albeit at a lower tolerance for brevity.  Secondly,
  ! ComplexProfile is tested.
  
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

  character(*), parameter :: MOD_NAME = 'ScalarPentagonalProfileTests'

  ! systems under test
  class(ScalarProfileInterface), allocatable :: complexProfile
  class(ScalarProfileInterface), dimension(:), allocatable :: &
       simplexProfiles
  class(ScalarFlowVariableInterface), allocatable, target :: sfv1
  class(VectorFlowVariableInterface), allocatable, target :: vfv
  class(CellArrayInterface), pointer :: cellArrayExtension
  

  ! helpers
  class(BoundaryRegionInterface), allocatable :: boundaryRegion
  class(InteriorRegionInterface), allocatable :: interiorRegion
  class(PolytopeArrayInterface), allocatable :: interiorPointArray, &
       boundaryPointArray, uncutInteriorLineArray, &
       otherInteriorLineArray, boundaryLineArray, triangleArray
  type(PolytopeGridStub) :: rectangleGrid
  type(InteriorStaticPointCollectionStub) :: rectangleGaussianPoints
  class(IntAveScalarMomentFieldInterface), allocatable, target :: &
       intAveScalarMomentGrid

  ! the following MomentGroup MUST be persistent within this module
  ! because it does not get embedded as a component.
  class(ScalarMomentGroupInterface), allocatable :: sharedIntAveMoment
  type(TestLogType) :: log
  
  class(ImpureScalarLinearOperationInterface), allocatable :: &
       linOpAssert
  class(ImpureScalarProfileOperationInterface), allocatable :: &
       profileAssert
     
  logical, parameter :: DEBUG_MODE = .false. 

contains


  subroutine setUp
    class(DirectionInterface), allocatable :: d
    type(PointScalarBoundaryConditionListType) :: spbcList
    class(PointScalarBoundaryConditionInterface), allocatable :: spbc
    class(PointVectorBoundaryConditionInterface), allocatable :: vpbc
    type(PolytopePointerSetType) :: pps
    type(CellPointerType) :: cp
    type(GridParametersType) :: gp
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    type(PointArrangementStub) :: pa
    integer :: i
    call log%init( MOD_NAME, 'setup' )

    ! create flow variables
    call createScalarFlowVariableStub( sfv1, 'var1', log )
    call createVectorFlowVariableStub( vfv, 'velocity', log )
    if ( log%test(FATAL) ) return

    ! create boundary conditions.  Make a one-node list of scalar
    ! boundary conditions.  We will choose Dirichlet BCs.  Choose a
    ! value of 0.5.  We will be choosing unique values for the various
    ! moments in order to make the system easier to debug.
    call createPointDirichletBoundaryCondition( spbc, sfv1, 0.5_FLOAT, &
         log )
    if ( log%test(FATAL) ) return
    call spbcList%append( spbc, log )

    ! do the same for the velocity.  Make a no-slip condition.
    call createPointNoSlipBoundaryCondition( vpbc, vfv, vector(), &
         log )
    if ( log%test(FATAL) ) return

    ! create regions
    call createObliqueDirection( d, unitVector([3._FLOAT, 1._FLOAT]), &
         log )
    call createFlatBoundaryRegion( boundaryRegion, 'BoundaryRegion', &
         vpbc, spbcList, d, log )
    call createInteriorRegion( interiorRegion, 'InteriorRegion', log )
    if ( log%test(FATAL) ) return

    ! create points
    call setUp_InteriorVerticies( log )
    call setUp_BoundaryVerticies( log )
    call setUp_UncutInteriorLines( log )
    call setUp_OtherInteriorLines( log )
    call setUp_BoundaryLines( log )
    call setUp_InteriorTriangles( log )
    if ( log%test(FATAL) ) return

    ! create a moment grid to represent the cell integrated average.  Most
    ! of its components can have minimal initialisations; we are only
    ! interested in storing the value of 0.75.
    call createZerothSpatialDerivative( dsd, log )
    if ( log%test(FATAL) ) return
    
    call gp%init( sizeVector([1, 1]), [vector([0._FLOAT, 0._FLOAT]), &
         vector([1._FLOAT, 1._FLOAT])], vector([1._FLOAT, 1._FLOAT]) )
    call rectangleGrid%initPolytopeGrid( gp, 2, log )
    
    call createIntAveScalarMomentGrid( intAveScalarMomentGrid, sfv1, &
         dsd, rectangleGrid, pa, log )
    if ( log%test(FATAL) ) return
    call intAveScalarMomentGrid%setField( 0.75_FLOAT )

    ! package as a MomentGroup ready for injection
    call createInteriorIntAveScalarMomentGroup( sharedIntAveMoment, &
         rectangleGaussianPoints, 1, intAveScalarMomentGrid, .true., &
         log )
    if ( log%test(FATAL) ) return
    
    ! create three primitive profiles
    call allocScalarSimplexProfileArray( simplexProfiles, 3, log )
    if ( log%test(FATAL) ) return
    do i = 1, 3
       call cp%init( cellArrayExtension, i )
       call createZerothSpatialDerivative( dsd, log )
       
       call initScalarSimplexProfileArrayElement( simplexProfiles, i, &
            sfv1, cp, dsd, NCOEFS_ADV, log )
       if ( log%test(FATAL) ) return
       
       ! get the profile to build its matricies
       call simplexProfiles(i)%buildSystems( log )
       if ( log%test(FATAL) ) return
    end do

    ! create the complex profile
    call createScalarComplexProfile( complexProfile, &
         simplexProfiles, [1:3], sharedIntAveMoment, log )

    ! need to factorise simplex matricies before building the complex
    ! systems
    do i = 1, 3
       call simplexProfiles(i)%factoriseSystems( log )
       if ( log%test(FATAL) ) return
    end do    
    call complexProfile%buildSystems( log )
    if ( log%test(FATAL) ) return
  end subroutine setUp


  subroutine tearDown
    integer :: deallocStat
    call log%reinit( MOD_NAME, 'teardown' )

    call destroy( profileAssert, log )
    call destroy( linOpAssert, log )

    call destroy( sharedIntAveMoment, log )
    call destroy( intAveScalarMomentGrid, log )
    call destroy( complexProfile, log )
    call destroy( simplexProfiles, log )

    call destroy( triangleArray, log )
    call destroy( boundaryLineArray, log )
    call destroy( uncutInteriorLineArray, log )
    call destroy( otherInteriorLineArray, log )
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
    call createPointArray( interiorPointArray, [0._FLOAT, 0._FLOAT, &
         1._FLOAT, 0._FLOAT, 0._FLOAT, 1._FLOAT], log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the point arrangement
    call createArrayPointArrangement( pa, interiorPointArray, 1, log )
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

       ! assign 1 for the 0th sp deriv and 0.1/0.2 for the 1st.  These
       ! scalars should propagate to all three points.
       select case ( i )
       case (1)
          call psmf%setField( 1._FLOAT )
       case (2)
          call psmf%setField( 0.1_FLOAT )
       case (3)
          call psmf%setField( 0.2_FLOAT )
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
    call createInteriorArrivalPointCollection( ispc, pa, &
         interiorRegion, log )
    call createInteriorDeparturePointCollection( dpc, ispc, .true., &
         pvmfList, psmfList, log )
    call interiorPointArray%injectDeparturePoints( dpc, log )

    call addEvent( DEBUG_MODE, ADVICE, 'interiorPointArray is '//trim(&
         interiorPointArray%describe()), log )
    
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
    call createPointArray( boundaryPointArray, [1._FLOAT, .25_FLOAT, &
         .75_FLOAT, 1._FLOAT], log )
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

    call addEvent( DEBUG_MODE, ADVICE, 'boundaryPointArray is '//trim(&
         boundaryPointArray%describe()), log )

    call endSub( log )
  end subroutine setUp_BoundaryVerticies


  subroutine setUp_UncutInteriorLines( log )
    class(LogType), intent(inout), optional :: log
    type(PointScalarMomentFieldListType) :: psmfList
    type(PointVectorMomentFieldListType) :: pvmfList
    class(PointScalarMomentFieldInterface), allocatable :: psmf
    class(PointVectorMomentFieldInterface), allocatable :: pvmf
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: iapc
    class(StaticPointCollectionInterface), allocatable :: apc
    integer :: i
    call beginSub( MOD_NAME, 'setUp_UncutInteriorLines', log )

    ! create the polytope array
    call createSimplexArray( uncutInteriorLineArray, 1, [&
         ptr(interiorPointArray, 1), ptr(interiorPointArray, 2), &
         ptr(interiorPointArray, 1), ptr(interiorPointArray, 3)], &
         log=log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! no points on these lines 

    call addEvent( DEBUG_MODE, ADVICE, 'uncutInteriorLineArray is '//trim(&
         uncutInteriorLineArray%describe()), log )

    call endSub( log )
  end subroutine setUp_UncutInteriorLines


  subroutine setUp_OtherInteriorLines( log )
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
    call beginSub( MOD_NAME, 'setUp_OtherInteriorLines', log )

    ! create the polytope array.  We will NOT count the uncut Cartesian
    ! lines; these do not carry arrival points.
    call createSimplexArray( otherInteriorLineArray, 1, [&
         ptr(interiorPointArray, 2), ptr(boundaryPointArray, 1), &
         ptr(interiorPointArray, 1), ptr(boundaryPointArray, 1), &
         ptr(interiorPointArray, 1), ptr(boundaryPointArray, 2), &
         ptr(boundaryPointArray, 2), ptr(interiorPointArray, 3)], &
         log=log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! create the point arrangement
    call createArrayPointArrangement( pa, otherInteriorLineArray, 1, &
         log )
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
         otherInteriorLineArray, pa, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! assign 0.8 for the 0th sp deriv (N.B. four polytopes)
    call psmf%setField( [0.8_FLOAT, 0.8_FLOAT, 0.8_FLOAT, 0.8_FLOAT] )
    call psmfList%append( psmf, log )
    call assertEqual( 1, psmfList%size(), 'psmfList should be one &
         &node long.' )

    ! make a dummy vector field to act as the velocity field
    call createZerothSpatialDerivative( dsd, log )
    call createPointVectorMomentArray( pvmf, vfv, dsd, &
         otherInteriorLineArray, pa, log )
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
    call otherInteriorLineArray%injectDeparturePoints( dpc, log )

    call addEvent( DEBUG_MODE, ADVICE, 'otherInteriorLineArray is '//trim(&
         otherInteriorLineArray%describe()), log )
    
    call endSub( log )
  end subroutine setUp_OtherInteriorLines


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

    ! create the polytope array.
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

    call addEvent( DEBUG_MODE, ADVICE, 'boundaryLineArray is '//trim(&
         boundaryLineArray%describe()), log )
    
    call endSub( log )
  end subroutine setUp_BoundaryLines


  subroutine setUp_InteriorTriangles( log )
    class(LogType), intent(inout), optional :: log
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: iapc
    class(StaticPointCollectionInterface), allocatable :: apc
    type(PointScalarMomentFieldListType) :: psmfList
    type(PointVectorMomentFieldListType) :: pvmfList
    class(PointScalarMomentFieldInterface), allocatable :: psmf
    class(PointVectorMomentFieldInterface), allocatable :: pvmf
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    type(IntAveScalarMomentFieldListType) :: iasmfList
    class(IntAveScalarMomentFieldInterface), allocatable :: iasmf
    class(IntAveVectorMomentFieldInterface), allocatable :: iavmf
    class(InteriorStaticPointCollectionInterface), allocatable :: ispc
    class(DeparturePointCollectionInterface), allocatable :: dpc
    integer :: i
    call beginSub( MOD_NAME, 'setUp_InteriorTriangles', log )

    ! create the polytope array.  Be careful to implement the same order
    ! as specified in the header which reflects the verification code.
    ! Note that the 'subPolytopeArray' argument and its implications are
    ! (probably) not important in these tests; but in any case
    ! otherInteriorLineArray is the appropriate supporting
    ! subPolytopeArray as it completely contains the two interfacial
    ! simplicies (lines) interior to the complex.
    call createSimplexArray( triangleArray, 2, [&
         
         ptr(otherInteriorLineArray, 3), &
         ptr(otherInteriorLineArray, 4), &
         ptr(uncutInteriorLineArray, 2), &

         ptr(uncutInteriorLineArray, 1), &
         ptr(otherInteriorLineArray, 1), &
         ptr(otherInteriorLineArray, 2), &

         ptr(otherInteriorLineArray, 2), &
         ptr(boundaryLineArray, 1), &
         ptr(otherInteriorLineArray, 3)], &
    
         otherInteriorLineArray, log )
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

    ! assign [1.1, 0.9, 0.7] for the 0th sp deriv
    call psmf%setField( [1.1_FLOAT, 0.9_FLOAT, 0.7_FLOAT] )
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
    call assertEqual( 1, pvmfList%size(), 'pvmfList should be one &
         &node long.' )

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
    call triangleArray%getArrayExtension( cellArrayExtension, log )
    call addEvent( DEBUG_MODE, ADVICE, 'triangleArray is '//trim(&
         triangleArray%describe()), log )
    
    call endSub( log )
  end subroutine setUp_InteriorTriangles

  
  !-------------------------------------------------------------------
  !- the tests
  !-------------------------------------------------------------------
  
  
  subroutine testNumEquations
    if ( log%test(MOD_NAME, 'testNumEquations', FATAL) ) &
         return

    call assertEqual( 3, complexProfile%getNumEquations('L'), 'Should &
         &have three equations.' )
  end subroutine testNumEquations


  subroutine testExactlyConstrainedSimplexMatricies
    real(FLOAT), dimension(10, 9, 3) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testExactlyConstrainedSimplexMatricies', &
         FATAL) ) return
    
    ! this test helps us check the order of simplicies as well as letting
    ! us know that matricies are still building ok.  The test sits
    ! awkwardly in the current module in that the last lines of the setup
    ! routine (beginning with 'call createScalarComplexProfile...')
    ! transform the as-built Vandermonde matricies being compared.  For
    ! this test only, backtrack by getting each simplexProfile to rebuild
    ! its matricies.
    do i = 1, 3
       call simplexProfiles(i)%buildSystems( log )
       if ( log%test(FATAL) ) return
    end do
    
    ! note transposition.
    expected(:, :, 1) = reshape([ 1.0000_FLOAT, 1.0000_FLOAT, &
         1.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.7500_FLOAT, 0.3750_FLOAT, 0.3750_FLOAT, &
         1.0000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.5625_FLOAT, 0.1406_FLOAT, &
         0.1406_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.4219_FLOAT, &
         0.0527_FLOAT, 0.0527_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, &
         1.0000_FLOAT, 0.5000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.7500_FLOAT, 0.1875_FLOAT, 0.3750_FLOAT, &
         1.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.5625_FLOAT, 0.0703_FLOAT, &
         0.1406_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, 1.0000_FLOAT, &
         0.2500_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         2.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.7500_FLOAT, 0.0938_FLOAT, 0.3750_FLOAT, 1.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 1.0000_FLOAT, &
         0.0000_FLOAT, 1.0000_FLOAT, 0.1250_FLOAT, 1.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 3.0000_FLOAT, 0.0000_FLOAT ], &
         shape=[10,9], order=[2,1] )
    
    expected(:, :, 2) = reshape([1.0000_FLOAT, 1.0000_FLOAT, &
         1.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         1.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, 0.5000_FLOAT, &
         1.0000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, &
         0.2500_FLOAT, 0.0000_FLOAT, 2.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, &
         1.0000_FLOAT, 0.1250_FLOAT, 0.0000_FLOAT, 3.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.2500_FLOAT, 0.1250_FLOAT, 0.1250_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.2500_FLOAT, 0.1250_FLOAT, 0.0625_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 1.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.2500_FLOAT, 0.1250_FLOAT, &
         0.0312_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         1.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0625_FLOAT, &
         0.0156_FLOAT, 0.0156_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0625_FLOAT, 0.0156_FLOAT, 0.0078_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0156_FLOAT, 0.0020_FLOAT, 0.0020_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT ], &
         shape=[10,9], order=[2,1] )
    
    expected(:, :, 3) = reshape([ 1.0000_FLOAT, 1.0000_FLOAT, &
         1.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, 1.0000_FLOAT, &
         1.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         1.0000_FLOAT, 0.7500_FLOAT, 0.5000_FLOAT, 0.9472_FLOAT, &
         0.8028_FLOAT, 0.3750_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 1.0000_FLOAT, 0.5625_FLOAT, 0.2500_FLOAT, &
         0.8971_FLOAT, 0.6445_FLOAT, 0.1406_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 1.0000_FLOAT, 0.4219_FLOAT, &
         0.1250_FLOAT, 0.8497_FLOAT, 0.5175_FLOAT, 0.0527_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.2500_FLOAT, &
         1.0000_FLOAT, 0.1250_FLOAT, 0.4085_FLOAT, 0.8415_FLOAT, &
         0.5000_FLOAT, 0.0000_FLOAT, 1.0000_FLOAT, 0.0000_FLOAT, &
         0.2500_FLOAT, 0.7500_FLOAT, 0.0625_FLOAT, 0.3869_FLOAT, &
         0.6756_FLOAT, 0.1875_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.2500_FLOAT, 0.5625_FLOAT, 0.0312_FLOAT, &
         0.3665_FLOAT, 0.5424_FLOAT, 0.0703_FLOAT, 0.0000_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0625_FLOAT, 1.0000_FLOAT, &
         0.0156_FLOAT, 0.1669_FLOAT, 0.7081_FLOAT, 0.2500_FLOAT, &
         0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0625_FLOAT, &
         0.7500_FLOAT, 0.0078_FLOAT, 0.1581_FLOAT, 0.5685_FLOAT, &
         0.0938_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT, &
         0.0156_FLOAT, 1.0000_FLOAT, 0.0020_FLOAT, 0.0682_FLOAT, &
         0.5959_FLOAT, 0.1250_FLOAT, 0.0000_FLOAT, 0.0000_FLOAT ], &
         shape=[10,9], order=[2,1] )
    
    do i = 1, 3
       ! create a Profile assertion object.  This has a nested
       ! LinearOperand assertion which needs to be (a) created with the
       ! expected values array and (b) injected into the Profile assertion
       ! object
       call createAssertEqualLinearOperand_Scalar( linOpAssert, 'A', &
            'TransposedEqualWithPermutations', tolerance=1.E-3_FLOAT, &
            log=log )
       if ( log%test(FATAL) ) return
       call linOpAssert%visitMatrixForOperand1( expected(:, :, i) )
       call createAssertEqualProfileComponent_Scalar( profileAssert, &
            'ExactlyConstr', linOpAssert, log )
       if ( log%test(FATAL) ) return

       ! link in and execute the assertion object
       call simplexProfiles(i)%acceptAsOperand2( profileAssert )
       call profileAssert%perform( log )
       if ( log%test(FATAL) ) return

       ! we have to cycle creation and destruction because linOpAssert has
       ! been injected into profileAssert and is therefore inaccessible
       call destroy( profileAssert, log )
       if ( log%test(FATAL) ) return
    end do
    
  end subroutine testExactlyConstrainedSimplexMatricies


  subroutine testLeastSquaresMatrix
    real(FLOAT), dimension(3, 3) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testLeastSquaresMatrix', FATAL) ) &
         return

    expected = reshape([&
         -0.01178511301978_FLOAT, &
         0.00000000000000_FLOAT, &
         0.00000000000000_FLOAT, &
         0.00000000000000_FLOAT, &
         0.00158794986243_FLOAT, &
         0.00000000000000_FLOAT, &
         0.00000000000000_FLOAT, &
         0.00000000000000_FLOAT, &
         0.01348522940502_FLOAT], shape=[3,3] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'A', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitMatrixForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'LeastSquares', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testLeastSquaresMatrix


  subroutine testPolytopeIntAveRow
    real(FLOAT), dimension(3, 1) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testPolytopeIntAveRow', FATAL) ) &
         return
    
    ! n.b. transposed
    expected = reshape([-0.00219446932092_FLOAT, &
         0.00009856240525_FLOAT, 0.00272029627653_FLOAT], shape=[3,1] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'A', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitMatrixForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'PolytpIntAve', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testPolytopeIntAveRow


  subroutine testFinalMatrixFactorisation
    real(FLOAT), dimension(3, 2) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testFinalMatrixFactorisation', FATAL) ) &
         return
    
    call complexProfile%factoriseSystems( log )
    if ( log%test(FATAL) ) return
    
    expected = reshape([0.01248253838295_FLOAT, 0.00098823449307_FLOAT, -&
         0.39120691959267_FLOAT, 0.00011800200974_FLOAT, -&
         0.00162744568114_FLOAT, -0.11129849506180_FLOAT], [3, 2] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'A', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitMatrixForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'Final', linOpAssert, log )
    if ( log%test(FATAL) ) return
    
    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testFinalMatrixFactorisation

  
  subroutine testLeastSquaresRHS
    real(FLOAT), dimension(3, 1) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testLeastSquaresRHS', FATAL) ) return
        
    ! update the RHS in each case by calling solve
    do i = 1, 3
       call simplexProfiles(i)%solveIncomplete( log=log )
       if ( log%test(FATAL) ) return
    end do
    call complexProfile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    expected = reshape( [0.22496296296296_FLOAT, &
         0.02549019607843_FLOAT, 0.01465069041620_FLOAT], shape=[3,1] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'B', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitRHSColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'LeastSquares', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testLeastSquaresRHS


  subroutine testPolytopeIntAveRHS
    real(FLOAT), dimension(1, 1) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testPolytopeIntAveRHS', FATAL) ) return

    ! update the RHS in each case by calling solve
    do i = 1, 3
       call simplexProfiles(i)%solveIncomplete( log=log )
       if ( log%test(FATAL) ) return
    end do
    call complexProfile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    expected = reshape( [-0.03604406119968_FLOAT], shape=[1,1] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'B', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitRHSColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'PolytpIntAve', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testPolytopeIntAveRHS


  subroutine testFinalRHS
    real(FLOAT), dimension(3, 1) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testFinalRHS', FATAL) ) return
    
    ! factorising the complex profile allows us to solve for the polytope
    ! integrated average system's particular solution, a prerequisite to
    ! computing the final system's RHS.
    call complexProfile%factoriseSystems( log=log )
    if ( log%test(FATAL) ) return

    ! update the RHS in each case by calling solve
    do i = 1, 3
       call simplexProfiles(i)%solveIncomplete( log=log )
       if ( log%test(FATAL) ) return
    end do
    call complexProfile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    expected = reshape( [0.30121178268021_FLOAT, &
          0.02595163894770_FLOAT, 0.12280514360473_FLOAT], shape=[3,1] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'B', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitRHSColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'Final', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testFinalRHS


  subroutine testPolytopeIntAveMatrixFactorisation
    real(FLOAT), dimension(3, 1) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testPolytopeIntAveMatrixFactorisation', FATAL) ) &
         return
    
    call complexProfile%factoriseSystems( log )
    if ( log%test(FATAL) ) return
    
    expected = reshape([0.00349648709140_FLOAT, &
          -0.01731912847565_FLOAT, -0.47800335821232_FLOAT], [3, 1] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'A', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitMatrixForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'PolytpIntAve', linOpAssert, log )
    if ( log%test(FATAL) ) return
    
    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testPolytopeIntAveMatrixFactorisation
  

  subroutine testPolytopeIntAveParticularSolution
    real(FLOAT), dimension(3, 1) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testPolytopeIntAveParticularSolution', FATAL) ) return

    ! factorising the complex profile allows us to solve it rather than
    ! merely updating the RHS
    call complexProfile%factoriseSystems( log=log )
    if ( log%test(FATAL) ) return
    
    do i = 1, 3
       call simplexProfiles(i)%solveIncomplete( log=log )
       if ( log%test(FATAL) ) return
    end do
    call complexProfile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    expected = reshape( [6.46992689754459_FLOAT, &
          -0.29059032667322_FLOAT, &
          -8.02021604084074_FLOAT], shape=[3, 1] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'X', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitSolutionColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'PolytpIntAve', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testPolytopeIntAveParticularSolution


  subroutine testFinalParticularSolution
    real(FLOAT), dimension(2, 1) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testFinalParticularSolution', FATAL) ) return
    
    ! factorising the complex profile allows us to solve it rather than
    ! merely updating the RHS
    call complexProfile%factoriseSystems( log=log )
    if ( log%test(FATAL) ) return
    
    do i = 1, 3
       call simplexProfiles(i)%solveIncomplete( log=log )
       if ( log%test(FATAL) ) return
    end do
    call complexProfile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    expected = reshape( [-10.82098447382507_FLOAT, &
          -24.50751390725420_FLOAT], shape=[2, 1] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'X', &
         'EqualAndAllowDiffSizes', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitSolutionColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'Final', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testFinalParticularSolution


  subroutine testSolvedFreeVariables
    real(FLOAT), dimension(3, 1) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testSolvedFreeVariables', FATAL) ) return
    
    call complexProfile%factoriseSystems( log=log )
    if ( log%test(FATAL) ) return
    
    do i = 1, 3
       call simplexProfiles(i)%solveIncomplete( log=log )
       if ( log%test(FATAL) ) return
    end do
    call complexProfile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    call complexProfile%finalise( log=log )
    if ( log%test(FATAL) ) return
    
    expected = reshape( [-2.63973158465081_FLOAT, -&
         24.64033288830496_FLOAT, -14.48675316800909_FLOAT], &
         shape=[3, 1] )
    
    call createAssertEqualLinearOperand_Scalar( linOpAssert, 'X', &
         'ExactlyEqual', log=log )
    if ( log%test(FATAL) ) return
    call linOpAssert%visitSolutionColumnForOperand1( expected )
    call createAssertEqualProfileComponent_Scalar( profileAssert, &
         'PolytpIntAve', linOpAssert, log )
    if ( log%test(FATAL) ) return

    call complexProfile%acceptAsOperand2( profileAssert )
    call profileAssert%perform( log )
    if ( log%test(FATAL) ) return
  end subroutine testSolvedFreeVariables

  
  subroutine testSimplexProfileCoefficients
    real(FLOAT), dimension(10, 1, 3) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testSimplexProfileCoefficients', FATAL) ) &
         return
    
    call complexProfile%factoriseSystems( log=log )
    if ( log%test(FATAL) ) return
    
    do i = 1, 3
       call simplexProfiles(i)%solveIncomplete( log=log )
       if ( log%test(FATAL) ) return
    end do
    call complexProfile%solveIncomplete( log=log )
    if ( log%test(FATAL) ) return
    
    call complexProfile%finalise( log=log )
    if ( log%test(FATAL) ) return
    do i = 1, 3
       call simplexProfiles(i)%finalise( log=log )
       if ( log%test(FATAL) ) return
    end do

    
    expected(:, :, 1) = reshape([1.00000000000000_FLOAT, &
         0.10000000000000_FLOAT, -3.09503546099290_FLOAT, &
         1.77777777777778_FLOAT, 0.20000000000000_FLOAT, &
         0.55460992907800_FLOAT, 0.73947990543734_FLOAT, -&
         0.60000000000000_FLOAT, -0.55460992907800_FLOAT, &
         0.40000000000000_FLOAT], shape=[10, 1] )
        
    expected(:, :, 2) = reshape([1.00000000000000_FLOAT, &
         0.10000000000000_FLOAT, -0.30000000000000_FLOAT, &
         0.20000000000000_FLOAT, 0.20000000000000_FLOAT, -&
         6.57872340425531_FLOAT, 6.57872340425531_FLOAT, &
         6.31489361702126_FLOAT, -26.31489361702128_FLOAT, &
         44.80000000000010_FLOAT], shape=[10, 1] )
    
    expected(:, :, 3) = reshape([1.00000000000000_FLOAT, &
         0.10000000000000_FLOAT, 0.65854211255196_FLOAT, -&
         1.08635566875530_FLOAT, 0.20000000000000_FLOAT, -&
         10.16653657308327_FLOAT, 8.56138447980326_FLOAT, &
         5.32947249150198_FLOAT, -2.04551273980962_FLOAT, -&
         1.67333791727598_FLOAT], shape=[10, 1] )

    do i = 1, 3
       call createAssertEqualLinearOperand_Scalar( linOpAssert, 'X', &
            'ExactlyEqual', log=log )
       if ( log%test(FATAL) ) return
       call linOpAssert%visitSolutionColumnForOperand1( expected(:, :, i) )
       call createAssertEqualProfileComponent_Scalar( profileAssert, &
            'ExactlyConstr', linOpAssert, log )
       if ( log%test(FATAL) ) return

       call simplexProfiles(i)%acceptAsOperand2( profileAssert )
       call profileAssert%perform( log )
       if ( log%test(FATAL) ) return
       
       ! we have to cycle creation and destruction because linOpAssert has
       ! been injected into profileAssert and is therefore inaccessible
       call destroy( profileAssert, log )
       if ( log%test(FATAL) ) return
    end do
  end subroutine testSimplexProfileCoefficients
  

end module ScalarPentagonalProfileTests
