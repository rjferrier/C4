module FlowField_Stubs
  
  use LogModule
  use Global
  use FlowFieldModule
  
  
  implicit none
  private
  public :: createScalarFlowVariableStub, createVectorFlowVariableStub, &
       createPointArrangementStub, createScalarMomentSourceStub, &
       createVectorMomentSourceStub, createStaticPointCollectionStub, &
       createInteriorStaticPointCollectionStub, &
       createBoundaryStaticPointCollectionStub
  
  character(*), parameter :: MOD_NAME = 'FlowField_Stubs'


  type, extends(DirectionInterface), public :: DirectionStub
     private
   contains
     procedure :: deinit => deinit_Direction
     procedure :: dotProduct => &
          dotProduct_Direction
     procedure :: clone_single => clone_single_Direction
     procedure :: clone_multi => clone_multi_Direction
     procedure :: convert => convert_Direction
     procedure :: sameAs => sameAs_Direction
  end type DirectionStub

  
  type, extends(ScalarFlowVariableInterface), public :: ScalarFlowVariableStub
     private
   contains
     procedure :: init => init_ScalarFlowVariable
     procedure :: deinit => deinit_ScalarFlowVariable
     procedure :: clone => clone_ScalarFlowVariable
     procedure :: memberOf => memberOf_ScalarFlowVariable
  end type ScalarFlowVariableStub

  
  type, extends(VectorFlowVariableInterface), public :: VectorFlowVariableStub
     private
   contains
     procedure :: init => init_VectorFlowVariable
     procedure :: deinit => deinit_VectorFlowVariable
     procedure :: clone => clone_VectorFlowVariable
  end type VectorFlowVariableStub

  
  ! corresponds to orthotopic polytopes.
  type, extends(PointArrangementInterface), public :: PointArrangementStub
     private
     integer :: dummy
   contains
     procedure :: init => init_PointArrangement
     procedure :: deinit => deinit_PointArrangement
     procedure :: getPositions => getPositions_PointArrangement
     procedure :: createGaussianWeights
     procedure :: getNumPointsPerPolytope => &
          getNumPointsPerPolytope_PointArrangement
     procedure :: createPositionsMatrix => &
          createPositionsMatrix_PointArrangement
     procedure :: convertToPolytopeValues => &
          convertToPolytopeValues_PointArrangement
  end type PointArrangementStub

  
  type, extends(StaticPointCollectionInterface), public :: StaticPointCollectionStub
     private
   contains
     procedure :: init => init_StaticPointCollection
     procedure :: deinit => deinit_StaticPointCollection
     procedure :: appendScalarMoments => &
          appendScalarMoments_StaticPointCollection
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_StaticPointCollection
     
  end type StaticPointCollectionStub


  
  type, extends(InteriorStaticPointCollectionInterface), public :: InteriorStaticPointCollectionStub
     private
   contains
     procedure :: init => init_InteriorStaticPointCollection
     procedure :: deinit => deinit_InteriorStaticPointCollection
     procedure :: appendScalarMoments => &
          appendScalarMoments_InteriorStaticPointCollection
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_InteriorStaticPointCollection
     procedure :: computeSubRows => &
          computeSubRows_InteriorStaticPointCollection
!!$     procedure :: initialiseScalarFields
!!$     procedure :: initialiseVectorFields
  end type InteriorStaticPointCollectionStub


  
  type, extends(BoundaryStaticPointCollectionInterface), public :: BoundaryStaticPointCollectionStub
     private
   contains
     procedure :: init => init_BoundaryStaticPointCollection
     procedure :: deinit => deinit_BoundaryStaticPointCollection
     procedure :: appendScalarMoments => &
          appendScalarMoments_BoundaryStaticPointCollection
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_BoundaryStaticPointCollection
     procedure :: computeSubRows => &
          computeSubRows_BoundaryStaticPointCollection
     procedure :: getFaceNormals => &
          getFaceNormals_BoundaryStaticPointCollection
  end type BoundaryStaticPointCollectionStub


  
  type, extends(ScalarMomentSourceInterface), public :: ScalarMomentSourceStub
     private
   contains
     procedure :: init => init_ScalarMomentSource
     procedure :: deinit => deinit_ScalarMomentSource
     procedure :: appendRows => &
          appendRows_ScalarMomentSource
     procedure :: appendElements => &
          appendElements_ScalarMomentSource
     procedure :: appendEquations => &
          appendEquations_ScalarMomentSource
  end type ScalarMomentSourceStub
  
  
  type, extends(VectorMomentSourceInterface), public :: VectorMomentSourceStub
     private
   contains
     procedure :: init => init_VectorMomentSource
     procedure :: deinit => deinit_VectorMomentSource
     procedure :: appendRows => &
          appendRows_VectorMomentSource
     procedure :: appendElements => &
          appendElements_VectorMomentSource
     procedure :: appendEquations => &
          appendEquations_VectorMomentSource
     procedure :: hasCoupledComponents => &
          hasCoupledComponents_VectorMomentSource
  end type VectorMomentSourceStub
  
  
contains

  
  !-------------------------------------------------------------------
  !- DirectionStub methods
  !-------------------------------------------------------------------
  
  pure function dotProduct_Direction( obj, &
       singlePointRowComponents )
    class(DirectionStub), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: &
         singlePointRowComponents
    real(FLOAT), dimension(size(singlePointRowComponents, 2)) :: &
         dotProduct_Direction
    
    dotProduct_Direction = 0._FLOAT
  end function dotProduct_Direction

  
  pure subroutine clone_single_Direction( obj, tgt, &
       log )
    class(DirectionStub), intent(in) :: obj
    class(DirectionInterface), allocatable, intent(inout) :: tgt
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'clone_single_Direction', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine clone_single_Direction

  
  pure subroutine clone_multi_Direction( obj, directions, &
       nElements, log )
    class(DirectionStub), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(&
         inout) :: directions
    integer, intent(in) :: nElements
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'clone_multi_Direction', log )
!!$
!!$    allocate( DirectionStub :: directions(nElements), stat=&
!!$         allocStat )
!!$    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
!!$         &DirectionStub :: directions(nElements).  STAT='//&
!!$         int2str(allocStat), log )
!!$    if ( checkSub(FATAL, log) ) then
!!$       call endSub( log )
!!$       return
!!$    end if
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine clone_multi_Direction

  
  pure subroutine deinit_Direction( obj, log )
    class(DirectionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_DirectionStub', log )

    call endSub( log )
  end subroutine deinit_Direction


  pure function convert_Direction( obj )
    class(DirectionStub), intent(in) :: obj
    type(RealVectorType) :: convert_Direction
    convert_Direction = 0._FLOAT
  end function convert_Direction  


  pure function sameAs_Direction( obj, direction )
    class(DirectionStub), intent(in) :: obj
    class(DirectionInterface), intent(in) :: direction
    logical :: sameAs_Direction
    sameAs_Direction = .false.
  end function sameAs_Direction

  
  !-------------------------------------------------------------------
  !- ScalarFlowVariableStub methods
  !-------------------------------------------------------------------

  subroutine createScalarFlowVariableStub( fv, name, log )
    class(ScalarFlowVariableInterface), allocatable, intent(out) :: fv
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createScalarFlowVariableStub', log )
    
    ! allocate
    if ( allocated(fv) ) then
       call addEvent( FATAL, 'fv already allocated.', log )
    else
       allocate( ScalarFlowVariableStub :: fv, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &ScalarFlowVariableStubType :: fv.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( fv )
    class is (ScalarFlowVariableStub)
       call fv%init( name, log )
    end select
    call endSub( log )
  end subroutine createScalarFlowVariableStub

  
  subroutine init_ScalarFlowVariable( obj, name, log )
    class(ScalarFlowVariableStub), intent(inout) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_ScalarFlowVariable', log )
    
    ! just initialise supertype
    call obj%initScalarFlowVariable( name, log )
    
    call endSub( log )
  end subroutine init_ScalarFlowVariable
  
  
  subroutine deinit_ScalarFlowVariable( obj, log )
    class(ScalarFlowVariableStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_ScalarFlowVariable', log )

    ! deinitialise supertype
    call obj%deinitScalarFlowVariable( log )
    
    call endSub( log )
  end subroutine deinit_ScalarFlowVariable
  
  
  subroutine clone_ScalarFlowVariable( obj, tgt, log )
    class(ScalarFlowVariableStub), intent(in) :: obj
    class(ScalarFlowVariableInterface), allocatable, intent(out) :: tgt
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'clone_ScalarFlowVariable', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine clone_ScalarFlowVariable

  
  pure function memberOf_ScalarFlowVariable( obj, vectorFlowVariable )
    class(ScalarFlowVariableStub), intent(in) :: obj
    class(VectorFlowVariableInterface), intent(in) :: vectorFlowVariable
    logical :: memberOf_ScalarFlowVariable
    memberOf_ScalarFlowVariable = .false.
  end function memberOf_ScalarFlowVariable
  
  

  !-------------------------------------------------------------------
  !- VectorFlowVariableStub methods
  !-------------------------------------------------------------------

  subroutine createVectorFlowVariableStub( fv, name, log )
    class(VectorFlowVariableInterface), allocatable, intent(out) :: fv
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createVectorFlowVariableStub', log )
    
    ! allocate
    if ( allocated(fv) ) then
       call addEvent( FATAL, 'fv already allocated.', log )
    else
       allocate( VectorFlowVariableStub :: fv, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &VectorFlowVariableStubType :: fv.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( fv )
    class is (VectorFlowVariableStub)
       call fv%init( name, log )
    end select
    call endSub( log )
  end subroutine createVectorFlowVariableStub

  
  subroutine init_VectorFlowVariable( obj, name, log )
    class(VectorFlowVariableStub), intent(inout) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_VectorFlowVariable', log )
    
    ! just initialise supertype
    call obj%initVectorFlowVariable( name, log )
    
    call endSub( log )
  end subroutine init_VectorFlowVariable
  
  
  subroutine deinit_VectorFlowVariable( obj, log )
    class(VectorFlowVariableStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_VectorFlowVariable', log )

    ! deinitialise supertype
    call obj%deinitVectorFlowVariable( log )
    
    call endSub( log )
  end subroutine deinit_VectorFlowVariable

  
  subroutine clone_VectorFlowVariable( obj, tgt, log )
    class(VectorFlowVariableStub), intent(in) :: obj
    class(VectorFlowVariableInterface), allocatable, intent(out) :: tgt
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'clone_VectorFlowVariable', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine clone_VectorFlowVariable

!!$  subroutine computeMatrixRows_FlowVariable( obj, pointMomentGroup, &
!!$       log )
!!$    class(FlowVariableStub), intent(in) :: obj
!!$    class(PointMomentGroupInterface), intent(inout) :: &
!!$         pointMomentGroup
!!$    class(LogType), intent(inout) :: log
!!$    call beginSub( MOD_NAME, 'computeMatrixRows_FlowVariable', log )
!!$    call addEvent( WARNING, 'This stub method should be overridden.' )
!!$    
!!$    call endSub( log )
!!$  end subroutine computeMatrixRows_FlowVariable

  
  !-------------------------------------------------------------------
  !- PointArrangementStub methods
  !-------------------------------------------------------------------

  subroutine createPointArrangementStub( pa, log )
    class(PointArrangementInterface), allocatable, intent(out) :: pa
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createPointArrangementStub', log )
    
    ! allocate
    if ( allocated(pa) ) then
       call addEvent( FATAL, 'pa already allocated.', log )
    else
       allocate( PointArrangementStub :: pa, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &PointArrangementStubType :: pa.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( pa )
    class is (PointArrangementStub)
       call pa%init( log )
    end select
    call endSub( log )
  end subroutine createPointArrangementStub

  
  subroutine init_PointArrangement( obj, log )
    class(PointArrangementStub), intent(out) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_PointArrangement', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    ! shut up the compiler
    obj%dummy = 0

    call endSub( log )    
  end subroutine init_PointArrangement
  

  pure subroutine deinit_PointArrangement( obj, log )
    class(PointArrangementStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_PointArrangement', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine deinit_PointArrangement

  
  pure subroutine getPositions_PointArrangement( obj, pointCoords, &
       polytopeIndex, pointIndex, log )
    class(PointArrangementStub), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    integer, intent(in), optional :: pointIndex
    type(RealVectorType), dimension(:), intent(out) :: pointCoords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getPositions_PointArrangement', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    ! tk
    pointCoords = 0._FLOAT

    call endSub( log )
  end subroutine getPositions_PointArrangement


  pure subroutine createGaussianWeights( obj, gaussianWeights, &
       log )
    class(PointArrangementStub), intent(in) :: obj
    real(FLOAT), dimension(:), allocatable, intent(inout) :: &
         gaussianWeights
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'createGaussianWeights_Array', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine createGaussianWeights


  pure function getNumPointsPerPolytope_PointArrangement( obj )
    class(PointArrangementStub), intent(in) :: obj
    integer :: getNumPointsPerPolytope_PointArrangement
    getNumPointsPerPolytope_PointArrangement = 0
  end function getNumPointsPerPolytope_PointArrangement

  
  subroutine createPositionsMatrix_PointArrangement( obj, &
       positions, log )
    class(PointArrangementStub), intent(in) :: obj
    type(RealVectorType), dimension(:, :), allocatable, intent(inout) :: &
         positions
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'createPositionsMatrix_PointArrangement', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine createPositionsMatrix_PointArrangement


  function convertToPolytopeValues_PointArrangement( obj, &
       valuesMatrix ) result ( valuesArray )
    class(PointArrangementStub), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: valuesMatrix
    real(FLOAT), dimension(size(valuesMatrix, 2)) :: valuesArray

    valuesArray = 0._FLOAT
  end function convertToPolytopeValues_PointArrangement


  !-------------------------------------------------------------------
  !- StaticPointCollectionStub methods
  !-------------------------------------------------------------------

  subroutine createStaticPointCollectionStub( apc, log )
    class(StaticPointCollectionInterface), allocatable, intent(out) :: apc
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createStaticPointCollectionStub', log )
    
    ! allocate
    if ( allocated(apc) ) then
       call addEvent( FATAL, 'apc already allocated.', log )
    else
       allocate( StaticPointCollectionStub :: apc, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &StaticPointCollectionStubType :: apc.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( apc )
    class is (StaticPointCollectionStub)
       call apc%init( log )
    end select
    call endSub( log )
  end subroutine createStaticPointCollectionStub

  
  subroutine init_StaticPointCollection( obj, log )
    class(StaticPointCollectionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    class(PointArrangementInterface), allocatable :: pa
    call beginSub( MOD_NAME, 'init_StaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call createPointArrangementStub( pa, log )
    call obj%initStaticPointCollection( pa, log )

    call endSub( log )
  end subroutine init_StaticPointCollection
  

  subroutine deinit_StaticPointCollection( obj, log )
    class(StaticPointCollectionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_StaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call obj%deinitStaticPointCollection( log )

    call endSub( log )
  end subroutine deinit_StaticPointCollection
  
  
  pure subroutine getPositions_StaticPointCollection( obj, pointCoords, &
       polytopeIndex, log )
    class(StaticPointCollectionStub), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    type(RealVectorType), dimension(:), intent(out) :: pointCoords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getPositions_StaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    pointCoords = 0._FLOAT

    call endSub( log )
  end subroutine getPositions_StaticPointCollection


  subroutine appendScalarMoments_StaticPointCollection( obj, &
       scalarMomentGroupList, polytopeIndex, log )
    class(StaticPointCollectionStub), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendMoments_StaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine appendScalarMoments_StaticPointCollection


  subroutine appendVelocityMoments_StaticPointCollection( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class(StaticPointCollectionStub), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendMoments_StaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine appendVelocityMoments_StaticPointCollection

  

  !-------------------------------------------------------------------
  !- InteriorStaticPointCollectionStub methods
  !-------------------------------------------------------------------

  subroutine createInteriorStaticPointCollectionStub( apc, log )
    class(InteriorStaticPointCollectionInterface), allocatable, intent(out) :: apc
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createInteriorStaticPointCollectionStub', log )
    
    ! allocate
    if ( allocated(apc) ) then
       call addEvent( FATAL, 'apc already allocated.', log )
    else
       allocate( InteriorStaticPointCollectionStub :: apc, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &InteriorStaticPointCollectionStubType :: apc.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( apc )
    class is (InteriorStaticPointCollectionStub)
       call apc%init( log )
    end select
    call endSub( log )
  end subroutine createInteriorStaticPointCollectionStub

  
  subroutine init_InteriorStaticPointCollection( obj, log )
    class(InteriorStaticPointCollectionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    class(PointArrangementInterface), allocatable :: pa
    call beginSub( MOD_NAME, 'deinit_InteriorStaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call createPointArrangementStub( pa, log )
    call obj%initInteriorStaticPointCollection( pa, log )

    call endSub( log )
  end subroutine init_InteriorStaticPointCollection
  

  subroutine deinit_InteriorStaticPointCollection( obj, log )
    class(InteriorStaticPointCollectionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_InteriorStaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call obj%deinitInteriorStaticPointCollection( log )

    call endSub( log )
  end subroutine deinit_InteriorStaticPointCollection

    
  subroutine appendScalarMoments_InteriorStaticPointCollection( obj, &
       scalarMomentGroupList, polytopeIndex, log )
    class(InteriorStaticPointCollectionStub), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendMoments_InteriorStaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine appendScalarMoments_InteriorStaticPointCollection


  subroutine appendVelocityMoments_InteriorStaticPointCollection( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class(InteriorStaticPointCollectionStub), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendMoments_InteriorStaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine appendVelocityMoments_InteriorStaticPointCollection

  
  pure function computeSubRows_InteriorStaticPointCollection( obj, &
       polytopeIndex, directedSpatialDerivative, nPoints ) result ( &
       rows )
    class(InteriorStaticPointCollectionStub), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    integer, intent(in) :: nPoints
    real(FLOAT), dimension(nPoints, NCOEFS_ADV) :: rows

    rows = 0._FLOAT
  end function computeSubRows_InteriorStaticPointCollection

  
!!$  subroutine initialiseScalarFields( obj, scalarFieldSpecifications, log )
!!$    class(InteriorStaticPointCollectionStub), intent(in) :: obj
!!$    type(ScalarFieldSpecificationListType), intent(in) :: &
!!$         scalarFieldSpecifications
!!$    class(LogType), intent(inout), optional :: log
!!$    call beginSub( MOD_NAME, 'initialiseScalarFields', log )
!!$
!!$    call addEvent( WARNING, 'This stub method should be overridden.' )
!!$
!!$    call endSub( log )
!!$  end subroutine initialiseScalarFields
!!$  
!!$  
!!$  subroutine initialiseVectorFields( obj, vectorFieldSpecifications, log )
!!$    class(InteriorStaticPointCollectionStub), intent(in) :: obj
!!$    type(VectorFieldSpecificationListType), intent(in) :: &
!!$         vectorFieldSpecifications
!!$    class(LogType), intent(inout), optional :: log
!!$    call beginSub( MOD_NAME, 'initialiseVectorFields', log )
!!$
!!$    call addEvent( WARNING, 'This stub method should be overridden.' )
!!$
!!$    call endSub( log )
!!$  end subroutine initialiseVectorFields

  
  !-------------------------------------------------------------------
  !- BoundaryStaticPointCollectionStub methods
  !-------------------------------------------------------------------

  subroutine createBoundaryStaticPointCollectionStub( apc, log )
    class(BoundaryStaticPointCollectionInterface), allocatable, intent(out) :: apc
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createBoundaryStaticPointCollectionStub', log )
    
    ! allocate
    if ( allocated(apc) ) then
       call addEvent( FATAL, 'apc already allocated.', log )
    else
       allocate( BoundaryStaticPointCollectionStub :: apc, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &BoundaryStaticPointCollectionStubType :: apc.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( apc )
    class is (BoundaryStaticPointCollectionStub)
       call apc%init( log )
    end select
    call endSub( log )
  end subroutine createBoundaryStaticPointCollectionStub

  
  subroutine init_BoundaryStaticPointCollection( obj, log )
    class(BoundaryStaticPointCollectionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    class(PointArrangementInterface), allocatable :: pa
    call beginSub( MOD_NAME, 'deinit_BoundaryStaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call createPointArrangementStub( pa, log )
    call obj%initBoundaryStaticPointCollection( pa, log )

    call endSub( log )
  end subroutine init_BoundaryStaticPointCollection
  

  subroutine deinit_BoundaryStaticPointCollection( obj, log )
    class(BoundaryStaticPointCollectionStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_BoundaryStaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call obj%deinitBoundaryStaticPointCollection( log )

    call endSub( log )
  end subroutine deinit_BoundaryStaticPointCollection

    
  subroutine appendScalarMoments_BoundaryStaticPointCollection( obj, &
       scalarMomentGroupList, polytopeIndex, log )
    class(BoundaryStaticPointCollectionStub), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendMoments_BoundaryStaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine appendScalarMoments_BoundaryStaticPointCollection


  subroutine appendVelocityMoments_BoundaryStaticPointCollection( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class(BoundaryStaticPointCollectionStub), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendMoments_BoundaryStaticPointCollection', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine appendVelocityMoments_BoundaryStaticPointCollection

  
  pure function computeSubRows_BoundaryStaticPointCollection( obj, &
       polytopeIndex, undirectedSpatialDerivative, nPoints ) result ( &
       rows )
    class(BoundaryStaticPointCollectionStub), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class(UndirectedSpatialDerivativeInterface), intent(in) :: &
         undirectedSpatialDerivative
    integer, intent(in) :: nPoints
    real(FLOAT), dimension(nPoints, NCOEFS_ADV) :: rows
    
    rows = 0._FLOAT
  end function computeSubRows_BoundaryStaticPointCollection
  
  
  pure subroutine getFaceNormals_BoundaryStaticPointCollection( obj, directions, &
       polytopeIndex, log )
    class(BoundaryStaticPointCollectionStub), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(out) :: &
         directions
    integer, intent(in) :: polytopeIndex
    type(BoundaryStaticPointGroupPointerType) :: bspgp
    class(LogType), intent(inout), optional :: log
    integer :: allocStat, n
    call beginSub( MOD_NAME, &
         'getFaceNormal_BoundaryStaticPointCollection', log )

    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine getFaceNormals_BoundaryStaticPointCollection

  !-------------------------------------------------------------------
  !- ScalarMomentSourceStub methods
  !-------------------------------------------------------------------

  subroutine createScalarMomentSourceStub( sms, scalarFlowVariable, log )
    class(ScalarMomentSourceInterface), allocatable, intent(out) :: sms
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createScalarMomentSourceStub', log )
    
    ! allocate
    if ( allocated(sms) ) then
       call addEvent( FATAL, 'sms already allocated.', log )
    else
       allocate( ScalarMomentSourceStub :: sms, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &ScalarMomentSourceStubType :: sms.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( sms )
    class is (ScalarMomentSourceStub)
       call sms%init( scalarFlowVariable, log )
    end select
    call endSub( log )
  end subroutine createScalarMomentSourceStub

  
  subroutine init_ScalarMomentSource( obj, scalarFlowVariable, log )
    class(ScalarMomentSourceStub), intent(inout) :: obj
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_ScalarMomentSource', log )

    ! initialise supertype
    call obj%initScalarMomentSource( scalarFlowVariable, log )
    
    call endSub( log )
  end subroutine init_ScalarMomentSource

  
  subroutine deinit_ScalarMomentSource( obj, log )
    class(ScalarMomentSourceStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_ScalarMomentSource', log )

    ! deinitialise supertype
    call obj%deinitScalarMomentSource( log )
    
    call endSub( log )
  end subroutine deinit_ScalarMomentSource

  
  subroutine appendRows_ScalarMomentSource( obj, &
       scalarMatrix, staticPointGroupPointer , log )
    class(ScalarMomentSourceStub), intent(in) :: obj
    class(ScalarMatrixInterface), intent(inout) :: &
         scalarMatrix
    class(StaticPointGroupPointerInterface), intent(in) :: &
         staticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_ScalarMomentSource', &
         log )
    
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine appendRows_ScalarMomentSource

  
  subroutine appendElements_ScalarMomentSource( obj, &
       scalarRHSColumn, log )
    class(ScalarMomentSourceStub), intent(in) :: obj
    class(ScalarRHSColumnInterface), intent(inout) :: &
         scalarRHSColumn
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendElements_ScalarMomentSource', &
         log )
    
    call addEvent( WARNING, 'This stub method should be overridden.' )
    
    call endSub( log )
  end subroutine appendElements_ScalarMomentSource

  
  subroutine appendEquations_ScalarMomentSource( obj, &
       scalarLinearSystem, staticPointGroupPointer, log )
    class(ScalarMomentSourceStub), intent(in) :: obj
    class(ScalarLinearSystemInterface), intent(inout) :: &
         scalarLinearSystem
    class(StaticPointGroupPointerInterface), intent(in) :: &
         staticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendEquations_ScalarMomentSource', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendEquations_ScalarMomentSource  

  
  !-------------------------------------------------------------------
  !- VectorMomentSourceStub methods
  !-------------------------------------------------------------------

  subroutine createVectorMomentSourceStub( vms, vectorFlowVariable, log )
    class(VectorMomentSourceInterface), allocatable, intent(out) :: vms
    class(VectorFlowVariableInterface), target, intent(in) :: &
         vectorFlowVariable
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createVectorMomentSourceStub', log )
    
    ! allocate
    if ( allocated(vms) ) then
       call addEvent( FATAL, 'vms already allocated.', log )
    else
       allocate( VectorMomentSourceStub :: vms, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &VectorMomentSourceStubType :: vms.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( vms )
    class is (VectorMomentSourceStub)
       call vms%init( vectorFlowVariable, log )
    end select
    call endSub( log )
  end subroutine createVectorMomentSourceStub

  
  subroutine init_VectorMomentSource( obj, vectorFlowVariable, log )
    class(VectorMomentSourceStub), intent(inout) :: obj
    class(VectorFlowVariableInterface), target, intent(in) :: &
         vectorFlowVariable
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_VectorMomentSource', log )

    ! initialise supertype
    call obj%initVectorMomentSource( vectorFlowVariable, log )
    
    call endSub( log )
  end subroutine init_VectorMomentSource

  
  subroutine deinit_VectorMomentSource( obj, log )
    class(VectorMomentSourceStub), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_VectorMomentSource', log )

    ! deinitialise supertype
    call obj%deinitVectorMomentSource( log )
    
    call endSub( log )
  end subroutine deinit_VectorMomentSource

  
  subroutine appendRows_VectorMomentSource( obj, &
       vectorMatrix, staticPointGroupPointer, log )
    class(VectorMomentSourceStub), intent(in) :: obj
    class(VectorMatrixInterface), intent(inout) :: &
         vectorMatrix
    class(StaticPointGroupPointerInterface), intent(in) :: &
         staticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_VectorMomentSource', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendRows_VectorMomentSource

  
  subroutine appendElements_VectorMomentSource( obj, &
       vectorRHSColumn, log )
    class(VectorMomentSourceStub), intent(in) :: obj
    class(VectorRHSColumnInterface), intent(inout) :: &
         vectorRHSColumn
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendElements_VectorMomentSource', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendElements_VectorMomentSource

  
  subroutine appendEquations_VectorMomentSource( obj, &
       vectorLinearSystem, staticPointGroupPointer, log )
    class(VectorMomentSourceStub), intent(in) :: obj
    class(VectorLinearSystemInterface), intent(inout) :: &
         vectorLinearSystem
    class(StaticPointGroupPointerInterface), intent(in) :: &
         staticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendEquations_VectorMomentSource', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendEquations_VectorMomentSource

  
  pure function hasCoupledComponents_VectorMomentSource( obj )
    class(VectorMomentSourceStub), intent(in) :: obj
    logical :: hasCoupledComponents_VectorMomentSource
    hasCoupledComponents_VectorMomentSource = .false.
  end function hasCoupledComponents_VectorMomentSource


end module FlowField_Stubs
