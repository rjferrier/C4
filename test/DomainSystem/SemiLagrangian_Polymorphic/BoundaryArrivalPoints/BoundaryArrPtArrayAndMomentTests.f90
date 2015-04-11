module BoundaryArrPtArrayAndMomentTests
  
  use TestUtilities
  use Global
  use FiniteVolumeModule
  use FlowFieldModule
  use SemiLagrangianModule

  ! systems under test
  use BoundaryArrivalPointsModule
  use PointBoundaryConditionsModule

  ! helpers
  use PointArrangementsModule
  use FakePolytopeArrays_QuadWithBoundary
  use FlowField_Stubs
  use SemiLagrangian_Stubs
  use SpatialDerivativesModule
  use LinearOperandAssertions
  use LinearSystemsModule
  
  implicit none
  
  character(*), parameter :: MOD_NAME = 'BoundaryArrPtArrayAndMomentTests'

  ! systems under test
  class(DeparturePointCollectionInterface), allocatable :: &
       departurePointCollection
  class(ScalarFlowVariableInterface), allocatable, target :: sfv1, sfv2
  class(VectorFlowVariableInterface), allocatable, target :: vfv

   ! helpers
  class(ScalarMatrixInterface), allocatable :: expected, actual
  class(ImpureScalarLinearOperationInterface), allocatable :: assertion

  type(TestLogType) :: log
  
contains
  
  subroutine setUp
    integer :: allocStat
    class(ScalarFlowVariableInterface), pointer :: sfv
    type(PointScalarBoundaryConditionListType) :: spbcList
    class(PointScalarBoundaryConditionInterface), allocatable :: spbc
    class(PointVectorBoundaryConditionInterface), allocatable :: vpbc
    class(PointArrangementInterface), allocatable :: pa
    class(DirectionInterface), allocatable :: d
    class(BoundaryStaticPointCollectionInterface), allocatable :: bspc
    integer :: i
    real(FLOAT) :: b
    call log%init( MOD_NAME, 'setup' )

    call setup_FakePolytopeArrays( log )
    if ( log%test(FATAL) ) return

    ! create the point arrangement
    call createArrayPointArrangement( pa, pointArray, 1, log )
    if ( log%test(FATAL) ) return

    ! create flow variables 
    call createScalarFlowVariableStub( sfv1, 'var1', log )
    call createScalarFlowVariableStub( sfv2, 'var2', log )
    call createVectorFlowVariableStub( vfv, 'velocity', log )
    if ( log%test(FATAL) ) return
    
    ! make a short list of boundary conditions.
    do i = 1, 2
       select case ( i )
       case (1)
          sfv => sfv1
          b = 1._FLOAT
          call createPointDirichletBoundaryCondition( spbc, sfv, &
               1._FLOAT, log )
       case (2)
          sfv => sfv2
          b = 0.1_FLOAT
          call createPointNeumannBoundaryCondition( spbc, sfv, &
               0.1_FLOAT, log )
       end select
       if ( log%test(FATAL) ) return

       call spbcList%append( spbc, log )
    end do
    call assertEqual( 2, spbcList%size(), 'bcList should be two nodes &
         &long.' )
    
    ! make a dummy no slip boundary condition to act as the velocity
    ! BC.  It also needs to be added to a list.
    call createPointNoSlipBoundaryCondition( vpbc, vfv, &
         realVector(), log )
    if ( log%test(FATAL) ) return

    ! when we initialise the boundary region, we inject the BCs created
    ! above.  So we cannot refer to those BCs after this; they are
    ! empty.
    call boundaryRegion%init( 'FlatBoundaryRegion', vpbc, spbcList, &
         log )
    
    call createBoundaryArrivalPointCollection( bspc, pa, boundaryRegion, &
         log )
    call createBoundaryDeparturePointCollection( departurePointCollection, &
         bspc, .true., boundaryRegion, log )
    if ( log%test(FATAL) ) return


    ! create helper matricies for assertions
    call createScalarSingleRowMatrix( expected, NCOEFS_ADV, log=log )
    call createScalarSingleRowMatrix( actual, NCOEFS_ADV, log=log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call createAssertEqualLinearOperand_Scalar( assertion, 'A', &
         'ExactlyEqual', log=log )
  end subroutine setUp
  
  
  subroutine tearDown
    integer :: deallocStat
    call log%reinit( MOD_NAME, 'teardown' )

    call destroy( assertion, log )

    call destroy( expected, log )
    call destroy( actual, log )
    
    call destroy( departurePointCollection, log )

    call destroy( vfv, log )
    call destroy( sfv1, log )
    call destroy( sfv2, log )

    call boundaryRegion%deinit( log )
    
    call teardown_FakePolytopeArrays( log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown
  
  
  subroutine testNumMomentGroups_Vertex1_FlowVar1
    type(ScalarMomentGroupListType) :: smgList
    if ( log%test(MOD_NAME, 'testNumMomentGroups_Vertex1_FlowVar1', &
         FATAL) ) return

    call smgList%setFlowVariable( sfv1 )
    call departurePointCollection%appendScalarMoments( &
         smgList, 1, log )
    call assertEqual( 1, smgList%size(), 'Wrong &
         &number of moments.' )
    
    call smgList%deinit( log )
    if ( log%test(FATAL) ) return
  end subroutine testNumMomentGroups_Vertex1_FlowVar1


  subroutine testMatrixRow_Vertex2_FlowVar1
    type(ScalarMomentGroupListType) :: smgList
    class(ScalarMomentGroupInterface), pointer :: smg
    integer :: i
    if ( log%test(MOD_NAME, 'testMatrixRow_Vertex2_FlowVar1', &
         FATAL) ) return
    
    call smgList%setFlowVariable( sfv1 )
    call departurePointCollection%appendScalarMoments( smgList, 2, log )
    
    ! matlab>> x = meshgrid([5 1], 1:10).'; p = [0, 1, 2, 3, 0, 1, 2,
    ! 0, 1, 0; 0, 0, 0, 0, 1, 1, 1, 2, 2, 3]; prod( x.^p );
    ! fprintf('%.0f._FLOAT, ', ans); fprintf('\n');
    call expected%setRows( reshape([1._FLOAT, 5._FLOAT, 25._FLOAT, &
         125._FLOAT, 1._FLOAT, 5._FLOAT, 25._FLOAT, 1._FLOAT, 5._FLOAT, &
         1._FLOAT], [1, 10]) )
    
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

  
  subroutine testMatrixRow_Vertex3_FlowVar2_Grad
    type(ScalarMomentGroupListType) :: smgList
    class(ScalarMomentGroupInterface), pointer :: smg
    integer :: i
    if ( log%test(MOD_NAME, 'testMatrixRow_Vertex3_FlowVar2_Grad', &
         FATAL) ) return

    call smgList%setFlowVariable( sfv2 )
    call departurePointCollection%appendScalarMoments( smgList, 3, log )
    
    ! matlab>> x = meshgrid([3 5], 1:10).'; p = [0, 1, 2, 3, 0, 1, 2, 0,
    ! 1, 0; 0, 0, 0, 0, 1, 1, 1, 2, 2, 3]; fx = p(1,:).*x(1,:).^max(0,
    ! p(1,:)-1) .* x(2,:).^p(2,:); fy = x(1,:).^p(1,:) .*
    ! p(2,:).*x(2,:).^max(0, p(2,:)-1); 2/sqrt(5)*fx + 1/sqrt(5)*fy;
    ! fprintf(' %.14f_FLOAT, ', ans); fprintf('\n')
    call expected%setRows( reshape([ 0.00000000000000_FLOAT, &
         0.89442719099992_FLOAT, 5.36656314599950_FLOAT, &
         24.14953415699773_FLOAT, 0.44721359549996_FLOAT, &
         5.81377674149945_FLOAT, 30.85773808949710_FLOAT, &
         4.47213595499958_FLOAT, 35.77708763999664_FLOAT, &
         33.54101966249684_FLOAT], [1, 10]) )
    call smgList%find( smg, 1, log )
    if ( log%test(FATAL) ) return
        
    call smgList%find( smg, 1, log )
    if ( log%test(FATAL) ) return

    call smg%appendRows( actual, log )
    if ( log%test(FATAL) ) return

    call expected%acceptAsOperand1( assertion )
    call actual%acceptAsOperand2( assertion )
    call assertion%perform()
    
    call smgList%deinit( log )
    if ( log%test(FATAL) ) return
  end subroutine testMatrixRow_Vertex3_FlowVar2_Grad

  

  subroutine testRHSValue_Vertex3_FlowVar2
    type(ScalarMomentGroupListType) :: smgList
    class(ScalarMomentGroupInterface), pointer :: smg
    real(FLOAT) :: actual
    if ( log%test(MOD_NAME, 'testRHSValue_Vertex3_FlowVar2', &
         FATAL) ) return
    
    call smgList%setFlowVariable( sfv2 )
    call departurePointCollection%appendScalarMoments( smgList, 3, log )
        
    call smgList%find( smg, 1 )
    call smg%getValue( actual, 1 )
    call assertEqual( 0.1_FLOAT, actual, TOL, 'Wrong right hand &
         &side value recovered.' )

    call smgList%deinit( log )
    if ( log%test(FATAL) ) return
  end subroutine testRHSValue_Vertex3_FlowVar2


end module BoundaryArrPtArrayAndMomentTests

