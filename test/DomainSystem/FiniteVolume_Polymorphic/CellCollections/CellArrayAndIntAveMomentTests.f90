module CellArrayAndMomentTests
  
  use TestUtilities
  use Global
  use FiniteVolumeModule
  use FlowFieldModule
  use SemiLagrangianModule
  use DomainModule
  use FlowFieldModule
  
  ! systems under test
  use InteriorGaussianPointsModule
  use IntAveMomentFieldsModule
  use CellCollectionsModule

  ! helpers
  use PointArrangementsModule
  use FakePolytopeArrays_RightAngleTriangle
  use ExtendedFakePolytopes
  use FlowField_Stubs
  use SemiLagrangian_Stubs
  use SpatialDerivativesModule
  use Domain_Stubs
  use LinearSystemsModule
  use LinearOperandAssertions
  
  implicit none
  
  character(*), parameter :: MOD_NAME = 'CellArrayAndMomentTests'
  
  ! systems under test
  class(ScalarFlowVariableInterface), allocatable, target :: sfv1, sfv2
  class(VectorFlowVariableInterface), allocatable, target :: vfv
  class(CellArrayInterface), pointer :: cellExtension

  ! helpers
  type(InteriorRegionStub) :: interiorRegion
  class(ScalarMatrixInterface), allocatable :: expected, actual
  class(ImpureScalarLinearOperationInterface), allocatable :: assertion

  type(TestLogType) :: log
  
contains

  
  subroutine setUp
    integer :: allocStat
    class(ScalarFlowVariableInterface), pointer :: sfv
    type(IntAveScalarMomentFieldListType) :: siamfList
    class(IntAveScalarMomentFieldInterface), allocatable :: siamf
    class(IntAveVectorMomentFieldInterface), allocatable :: viamf
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    class(PointArrangementInterface), allocatable :: pa
    class(InteriorStaticPointCollectionInterface), allocatable :: igpc
    integer :: i
    call log%init( MOD_NAME, 'setup' )

    call setup_FakePolytopeArrays( log )

    ! create flow variables 
    call createScalarFlowVariableStub( sfv1, 'var1', log )
    call createScalarFlowVariableStub( sfv2, 'var2', log )
    call createVectorFlowVariableStub( vfv, 'velocity', log )
    if ( log%test(FATAL) ) return

    ! create the interior region ahead of the Gaussian Points (see below)
    call interiorRegion%init( 'InteriorRegion', log )

    ! create the point arrangement
    call createArrayPointArrangement( pa, extendedTriangleArray, 3, &
         log )
    if ( log%test(FATAL) ) return

    ! unlike arrival points, gaussian points do not hold moment fields.
    ! rather it is cells that hold both gaussian points and moment fields.
    ! Create the gaussian points first.  [2012-07-26: we previously
    ! provided a pointer to the not-yet created integrable element
    ! collection, but of course the object will only end up pointing to
    ! the pointer's null target.  For now, omit the argument.  If the IE
    ! collection is really needed we can provide the host
    ! PolytopeCollection and the object can query for the IE extension in
    ! due course.]
    call createInteriorGaussianPointCollection( igpc, interiorRegion, &
         pa, log )
    if ( log%test(FATAL) ) return
    
!!$    ! make a list of moment fields.
!!$    do i = 1, 4
!!$       
!!$       ! For both variables, we will store the point value.  For the
!!$       ! second variable, we will further store x-gradient and
!!$       ! y-gradient.
!!$       select case ( i )
!!$       case (1)
!!$          call createZerothSpatialDerivative( dsd, log )
!!$          sfv => sfv1
!!$       case (2)
!!$          call createZerothSpatialDerivative( dsd, log )
!!$          sfv => sfv2
!!$       case (3)
!!$          ! here we create a directed first spatial derivative (i.e.
!!$          ! gradient).  Compare with boundary conditions who must
!!$          ! necessarily omit the directional argument.
!!$          call createFirstSpatialDerivative( dsd, 1, log )
!!$          sfv => sfv2
!!$       case (4)
!!$          call createFirstSpatialDerivative( dsd, 2, log )
!!$          sfv => sfv2
!!$       end select
!!$       if ( log%test(FATAL) ) return
!!$
!!$       call createScalarIntAveMomentArray( siamf, sfv, dsd, &
!!$            pointArray, pa, log )
!!$       if ( log%test(FATAL) ) return
!!$       
!!$       ! in order to distinguish the moment fields, we will assign
!!$       ! odd scalar values for the first variable, evens for the second,
!!$       ! 0.# for x-gradients, and 0.0# for y-gradients.
!!$       select case ( i )
!!$       case (1)
!!$          call siamf%setField( [1._FLOAT, 3._FLOAT, 5._FLOAT] )
!!$       case (2)
!!$          call siamf%setField( [2._FLOAT, 4._FLOAT, 6._FLOAT] )
!!$       case (3)
!!$          call siamf%setField( [0.2_FLOAT, 0.4_FLOAT, 0.6_FLOAT] )
!!$       case (4)
!!$          call siamf%setField( [0.02_FLOAT, 0.04_FLOAT, 0.06_FLOAT] )
!!$       end select
!!$       call siamfList%append( siamf, log )
!!$    end do
!!$    call assertEqual( 4, siamfList%size(), 'siamfList should be four &
!!$         &nodes long.' )
!!$    
!!$    ! make a dummy vector field to act as the velocity field.  It also
!!$    ! needs to be added to a list (because the total velocity field
!!$    ! could be further be made up by velocity gradients)
!!$    call createZerothSpatialDerivative( dsd, log )
!!$    call createVectorIntAveMomentArray( viamf, vfv, dsd, pointArray, &
!!$         pa, log )
!!$    if ( log%test(FATAL) ) return

    ! wrap the triangleArray to be compatible with cell extension.
    ! Then attach the extension, and get access to it
    call extendedTriangleArray%init( triangleArray, log )
    call attachCellArray( extendedTriangleArray, igpc, log )
    call extendedTriangleArray%getArrayExtension( cellExtension, log )

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
    
    nullify(cellExtension)
    call extendedTriangleArray%deinit( log )
    
    call interiorRegion%deinit( log )

    call teardown_FakePolytopeArrays( log )

    call destroy( vfv, log )
    call destroy( sfv1, log )
    call destroy( sfv2, log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown


  subroutine testMatrixRow_FlowVar1
    type(ScalarMomentGroupListType) :: smgList
    class(ScalarMomentGroupInterface), pointer :: smg
    integer :: i
    if ( log%test(MOD_NAME, 'testMatrixRow_FlowVar1', FATAL) ) &
         return

    call smgList%setFlowVariable( sfv1 )
    call cellExtension%appendScalarMoments( smgList, 1, log )
    
    ! matlab>>
    ! vertexCoords = [0 0 4; 0 7 0];
    ! vertexCoords = [1 7 1; 1 1 4];
    ! gaussBaryCoords = [ 0.6*diag(ones(1, 3)) + 0.2*~diag(ones(1, 3)); 
    !     1/3*ones(1, 3)]; 
    ! gaussWeights = [25/48*ones(3, 1); -9/16];
    ! px = [0, 1, 2, 3, 0, 1, 2, 0, 1, 0];
    ! py = [0, 0, 0, 0, 1, 1, 1, 2, 2, 3]; 
    ! gaussCoords = vertexCoords*gaussBaryCoords.';
    ! [X, PX] = ndgrid( gaussCoords(1,:), px );
    ! [Y, PY] = ndgrid( gaussCoords(2,:), py );
    ! W = ndgrid( gaussWeights, px );
    ! sum( W.* X.^PX .* Y.^PY, 1 );
    ! fprintf('%.6f_FLOAT, ', ans); 
    ! fprintf('\n');
    call expected%setRows( reshape([1.000000_FLOAT, 3.000000_FLOAT, &
         11.000000_FLOAT, 46.600000_FLOAT, 2.000000_FLOAT, &
         5.500000_FLOAT, 18.600000_FLOAT, 4.500000_FLOAT, &
         11.300000_FLOAT, 11.200000_FLOAT], [1, 10]) )

    call smgList%find( smg, 1, log )
    if ( log%test(FATAL) .or. .not. associated(smg) ) return
    call smg%appendRows( actual, log )
    if ( log%test(FATAL) ) return

    call expected%acceptAsOperand1( assertion )
    call actual%acceptAsOperand2( assertion )
    call assertion%perform()
    
  end subroutine testMatrixRow_FlowVar1
  


end module CellArrayAndMomentTests

