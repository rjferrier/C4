module InteriorArrPtArrayAndMomentTests
  
  use TestUtilities
  use Global
  use FiniteVolumeModule
  use FlowFieldModule
  use SemiLagrangianModule
  use DomainModule
  
  ! systems under test
  use InteriorArrivalPointsModule
  use PointMomentFieldsModule

  ! helpers
  use PointArrangementsModule
  use FakePolytopeArrays_RightAngleTriangle
  use FlowField_Stubs
  use SemiLagrangian_Stubs
  use SpatialDerivativesModule
  use Domain_Stubs
  use LinearSystemsModule
  use LinearOperandAssertions
  
  implicit none
  
  character(*), parameter :: MOD_NAME = 'InteriorArrPtArrayAndMomentTests'
  
  ! systems under test
  class(DeparturePointCollectionInterface), allocatable :: &
       departurePtCollection
  class(ScalarFlowVariableInterface), allocatable, target :: sfv1, sfv2
  class(VectorFlowVariableInterface), allocatable, target :: vfv

  ! helpers
  type(InteriorRegionStub) :: interiorRegion
  class(ScalarMatrixInterface), allocatable :: expected, actual
  class(ImpureScalarLinearOperationInterface), allocatable :: assertion
  
  type(TestLogType) :: log
  
contains

  
  subroutine setUp
    integer :: allocStat
    class(ScalarFlowVariableInterface), pointer :: sfv
    type(PointScalarMomentFieldListType) :: psmfList
    type(PointVectorMomentFieldListType) :: pvmfList
    class(PointScalarMomentFieldInterface), allocatable :: psmf
    class(PointVectorMomentFieldInterface), allocatable :: pvmf
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: ispc
    integer :: i
    call log%init( MOD_NAME, 'setup' )

    call setup_FakePolytopeArrays( log )
    if ( log%test(FATAL) ) return

    ! create flow variables 
    call createScalarFlowVariableStub( sfv1, 'var1', log )
    call createScalarFlowVariableStub( sfv2, 'var2', log )
    call createVectorFlowVariableStub( vfv, 'velocity', log )
    if ( log%test(FATAL) ) return

    ! create the point arrangement
    call createArrayPointArrangement( pa, pointArray, 1, log )
    if ( log%test(FATAL) ) return
    
    ! make a list of moment fields.
    do i = 1, 4
       
       ! For both variables, we will store the point value.  For the
       ! second variable, we will further store x-gradient and
       ! y-gradient.
       select case ( i )
       case (1)
          call createZerothSpatialDerivative( dsd, log )
          sfv => sfv1
       case (2)
          call createZerothSpatialDerivative( dsd, log )
          sfv => sfv2
       case (3)
          ! here we create a directed first spatial derivative (i.e.
          ! gradient).  Compare with boundary conditions who must
          ! necessarily omit the directional argument.
          call createFirstSpatialDerivative( dsd, 1, log )
          sfv => sfv2
       case (4)
          call createFirstSpatialDerivative( dsd, 2, log )
          sfv => sfv2
       end select
       if ( log%test(FATAL) ) return

       call createPointScalarMomentArray( psmf, sfv, dsd, pointArray, pa, &
            log )
       if ( log%test(FATAL) ) return
       
       ! in order to distinguish the moment fields, we will assign
       ! odd scalar values for the first variable, evens for the second,
       ! 0.# for x-gradients, and 0.0# for y-gradients.
       select case ( i )
       case (1)
          call psmf%setField( [1._FLOAT, 3._FLOAT, 5._FLOAT] )
       case (2)
          call psmf%setField( [2._FLOAT, 4._FLOAT, 6._FLOAT] )
       case (3)
          call psmf%setField( [0.2_FLOAT, 0.4_FLOAT, 0.6_FLOAT] )
       case (4)
          call psmf%setField( [0.02_FLOAT, 0.04_FLOAT, 0.06_FLOAT] )
       end select
       
       call psmfList%append( psmf, log )
    end do
    call assertEqual( 4, psmfList%size(), 'psmfList should be four &
         &nodes long.' )
    
    ! make a dummy vector field to act as the velocity field.  It also
    ! needs to be added to a list (because the total velocity field
    ! could be further be made up by velocity gradients)
    call createZerothSpatialDerivative( dsd, log )
    call createPointVectorMomentArray( pvmf, vfv, dsd, pointArray, &
         pa, log )
    if ( log%test(FATAL) ) return
    
    call pvmfList%append( pvmf, log )
    call assertEqual( 1, pvmfList%size(), 'pvmfList should be one node &
         &long.' )
    call interiorRegion%init( 'InteriorRegion', log )
    if ( log%test(FATAL) ) return
    
    call createInteriorArrivalPointCollection( ispc, pa, interiorRegion, &
         log )
    call createInteriorDeparturePointCollection( departurePtCollection, &
         ispc, .true., pvmfList, psmfList, log )
    if ( log%test(FATAL) ) return
    
    ! create helper matricies for assertions
    call createScalarSingleRowMatrix( expected, NCOEFS_ADV, log=log )
    call createScalarSingleRowMatrix( actual, NCOEFS_ADV, log=log )
    if ( log%test(FATAL) ) return
    
    call createAssertEqualLinearOperand_Scalar( assertion, 'A', &
         'ExactlyEqual', log=log )
  end subroutine setUp
  
  
  subroutine tearDown
    integer :: deallocStat
    call log%reinit( MOD_NAME, 'teardown' )

    call destroy( assertion, log )
    
    call destroy( expected, log )
    call destroy( actual, log )

    call destroy( departurePtCollection, log )

    call destroy( vfv, log )
    call destroy( sfv1, log )
    call destroy( sfv2, log )
    
    call interiorRegion%deinit( log )

    call teardown_FakePolytopeArrays( log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown
  
  
  subroutine testNumMomentGroups_Vertex1_FlowVar1
    type(ScalarMomentGroupListType) :: smgList
    if ( log%test(MOD_NAME, 'testNumMomentGroups_Vertex1_FlowVar1', &
         FATAL) ) return

    call smgList%setFlowVariable( sfv1 )
    call departurePtCollection%appendScalarMoments( &
         smgList, 1, log )
    call assertEqual( 1, smgList%size(), 'Wrong &
         &number of moments.' )
    
    call smgList%deinit( log )
    
    if ( log%test(FATAL) ) return
  end subroutine testNumMomentGroups_Vertex1_FlowVar1
  
  
  subroutine testNumMomentGroups_Vertex2_FlowVar2
    type(ScalarMomentGroupListType) :: smgList
    if ( log%test(MOD_NAME, 'testNumMomentGroups_Vertex2_FlowVar2', &
         FATAL) ) return
    
    call smgList%setFlowVariable( sfv2 )
    call departurePtCollection%appendScalarMoments( smgList, 2, &
         log )
    call assertEqual( 3, smgList%size(), 'Wrong number of moments.' )
    
    call smgList%deinit( log )
    
    if ( log%test(FATAL) ) return
  end subroutine testNumMomentGroups_Vertex2_FlowVar2


  subroutine testMatrixRow_Vertex2_FlowVar1
    type(ScalarMomentGroupListType) :: smgList
    class(ScalarMomentGroupInterface), pointer :: smg
    integer :: i
    if ( log%test(MOD_NAME, 'testMatrixRow_Vertex2_FlowVar1', &
         FATAL) ) return

    call smgList%setFlowVariable( sfv1 )
    call departurePtCollection%appendScalarMoments( smgList, 2, log )

    ! matlab>> x = meshgrid([7 1], 1:10).'; p = [0, 1, 2, 3, 0, 1, 2,
    ! 0, 1, 0; 0, 0, 0, 0, 1, 1, 1, 2, 2, 3]; prod( x.^p );
    ! fprintf('%.0f._FLOAT, ', ans); fprintf('\n');
    call expected%setRows( reshape([1._FLOAT, 7._FLOAT, 49._FLOAT, &
         343._FLOAT, 1._FLOAT, 7._FLOAT, 49._FLOAT, 1._FLOAT, 7._FLOAT, &
         1._FLOAT], [1, 10]), log )
    
    call smgList%find( smg, 1, log )
    if ( log%test(FATAL) ) return

    call smg%appendRows( actual, log )
    if ( log%test(FATAL) ) return

    call expected%acceptAsOperand1( assertion )
    call actual%acceptAsOperand2( assertion )
    call assertion%perform()
    
    call smgList%deinit( log )
    
    if ( log%test(FATAL) ) return
  end subroutine testMatrixRow_Vertex2_FlowVar1


  subroutine testMatrixRow_Vertex2_FlowVar2_GradX
    type(ScalarMomentGroupListType) :: smgList
    class(ScalarMomentGroupInterface), pointer :: smg
    integer :: i
    if ( log%test(MOD_NAME, 'testMatrixRow_Vertex2_FlowVar2_GradX', &
         FATAL) ) return

    call smgList%setFlowVariable( sfv2 )
    call departurePtCollection%appendScalarMoments( &
         smgList, 2, log )
    
    ! matlab>> x = meshgrid([7 1], 1:10).'; p = [0, 1, 2, 3, 0, 1, 2,
    ! 0, 1, 0; 0, 0, 0, 0, 1, 1, 1, 2, 2, 3]; p(1,:).*x(1,:).^max(0,
    ! p(1,:)-1) .* x(2,:).^p(2,:); fprintf('%.f._FLOAT, ', ans);
    ! fprintf('\n')
    call expected%setRows( reshape([0._FLOAT, 1._FLOAT, 14._FLOAT, &
         147._FLOAT, 0._FLOAT, 1._FLOAT, 14._FLOAT, 0._FLOAT, 1._FLOAT, &
         0._FLOAT], [1, 10]), log )
    
    call smgList%find( smg, 2, log )
    if ( log%test(FATAL) ) return

    call smg%appendRows( actual, log )
    if ( log%test(FATAL) ) return

    call expected%acceptAsOperand1( assertion )
    call actual%acceptAsOperand2( assertion )
    call assertion%perform()

    call smgList%deinit( log )
    
    if ( log%test(FATAL) ) return
  end subroutine testMatrixRow_Vertex2_FlowVar2_GradX


  subroutine testRHSValue_Vertex3_FlowVar1
    type(ScalarMomentGroupListType) :: smgList
    class(ScalarMomentGroupInterface), pointer :: smg
    real(FLOAT) :: actual
    if ( log%test(MOD_NAME, 'testRHSValue_Vertex3_FlowVar1', &
         FATAL) ) return

    call smgList%setFlowVariable( sfv1 )
    call departurePtCollection%appendScalarMoments( &
         smgList, 3, log )
    
    call smgList%find( smg, 1, log )
    if ( log%test(FATAL) ) return

    call smg%getValue(actual, 1)
    call assertEqual( 5._FLOAT, actual, TOL, 'Wrong right hand &
         &side value recovered.' )

    call smgList%deinit( log )
    
    if ( log%test(FATAL) ) return
  end subroutine testRHSValue_Vertex3_FlowVar1


  subroutine testRHSValue_Vertex3_FlowVar2_GradY
    type(ScalarMomentGroupListType) :: smgList
    class(ScalarMomentGroupInterface), pointer :: smg
    real(FLOAT) :: actual
    if ( log%test(MOD_NAME, 'testRHSValue_Vertex3_FlowVar2_GradY', &
         FATAL) ) return

    call smgList%setFlowVariable( sfv2 )
    call departurePtCollection%appendScalarMoments( &
         smgList, 3, log )
        
    call smgList%find( smg, 3, log )
    if ( log%test(FATAL) ) return

    call smg%getValue(actual, 1)
    call assertEqual( 0.06_FLOAT, actual, TOL, 'Wrong right &
         &hand side value recovered.' )
    
    call smgList%deinit( log )
    
    if ( log%test(FATAL) ) return
  end subroutine testRHSValue_Vertex3_FlowVar2_GradY
    
  
  
end module InteriorArrPtArrayAndMomentTests

