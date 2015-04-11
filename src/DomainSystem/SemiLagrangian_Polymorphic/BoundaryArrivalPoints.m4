HEADER()
module BoundaryArrivalPointsModule

  USE_MACROS({ArrivalPoints})

  use LogModule
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FlowFieldModule
  use DomainModule
  
  implicit none
  private
  public :: createBoundaryPointScalarMomentGroup, &
       createBoundaryPointVectorMomentGroup, &
       createBoundaryArrivalPointCollection, &
       createBoundaryDeparturePointCollection
  
  character(*), parameter :: MOD_NAME = 'BoundaryArrivalPointsModule'
     

  !-------------------------------------------------------------------
  !- BoundaryArrivalPointMomentGroups
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_POINTMOMENTGROUP_CONCRETE({Scalar}, {Boundary},
    {BoundaryCondition})})
    
  EXPAND({TYPEDEF_POINTMOMENTGROUP_CONCRETE({Vector}, {Boundary},
    {BoundaryCondition})})
  

  !-------------------------------------------------------------------
  !- BoundaryArrivalPointCollection, BoundaryDeparturePointCollection
  !-------------------------------------------------------------------

  ! the idea of arrival and departure points on the boundary may seem
  ! strange, since these point values are updated by boundary conditions
  ! and not a semi-Lagrangian method.  The reason for splitting boundary
  ! points into two types is to achieve type matching.  We need to be able
  ! to inject DeparturePoints into PolytopeCollections, whether the points
  ! are interior or on the boundary, but we also need the points to
  ! conform to the BoundaryStaticPointCollection interface in order to be
  ! targeted by a (Boundary)PointMomentGroup.

  EXPAND({TYPEDEF_ARRIVALPOINTCOLLECTION({Boundary},
     {}, {
     procedure :: getFaceNormals => &
          getFaceNormals_BoundaryArrival})})

  EXPAND({TYPEDEF_DEPARTUREPOINTCOLLECTION({Boundary},
     {
     ! these BCs are pointers, because they already exist under some
     ! BoundaryRegion.  Note that we have exactly one velocity BC but
     ! multiple scalar BCs
     class(PointVectorBoundaryConditionInterface), pointer :: &
          pointVelocityBoundaryCondition
     type(PointScalarBoundaryConditionListType), pointer :: &
          pointScalarBoundaryConditionList})})
  
contains
  
  !-------------------------------------------------------------------
  !- BoundaryPointScalarMomentGroup methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_POINTMOMENTGROUP_CONCRETE({Scalar}, {Boundary},
  {BoundaryCondition})})
  
  elemental subroutine getValue_BoundaryPointScalarMomentGroup( obj, &
       value, momentIndex )
    class(BoundaryPointScalarMomentGroupType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: momentIndex
    call obj%pointScalarBoundaryCondition%getValue( value )
  end subroutine getValue_BoundaryPointScalarMomentGroup
  
  pure subroutine getValues_BoundaryPointScalarMomentGroup( obj, &
       values )
    class(BoundaryPointScalarMomentGroupType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    call obj%pointScalarBoundaryCondition%getValues( values )
  end subroutine getValues_BoundaryPointScalarMomentGroup
 
  
  !-------------------------------------------------------------------
  !- BoundaryPointVectorMomentGroup methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_POINTMOMENTGROUP_CONCRETE({Vector}, {Boundary},
  {BoundaryCondition})})
  
  elemental subroutine getValue_BoundaryPointVectorMomentGroup( obj, &
       value, momentIndex, componentIndex ) 
    class(BoundaryPointVectorMomentGroupType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: momentIndex
    integer, intent(in) :: componentIndex
    ! tk
    value = 0._FLOAT
  end subroutine getValue_BoundaryPointVectorMomentGroup
  
  pure subroutine getValues_BoundaryPointVectorMomentGroup( obj, &
       values, componentIndex ) 
    class(BoundaryPointVectorMomentGroupType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: componentIndex
    ! tk
    values = 0._FLOAT
  end subroutine getValues_BoundaryPointVectorMomentGroup

  
  !-------------------------------------------------------------------
  !- BoundaryArrivalPointCollection methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_ARRIVALPOINTCOLLECTION_CREATEINIT({Boundary})})
  
  EXPAND({METHODDEF_ARRIVALPOINTCOLLECTION_DEINIT({Boundary})})

  pure function computeSubRows_BoundaryArrival( obj, &
       polytopeIndex, undirectedSpatialDerivative, nPoints ) result ( &
       rows )
    class(BoundaryArrivalPointCollectionType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class(UndirectedSpatialDerivativeInterface), intent(in) :: &
         undirectedSpatialDerivative
    integer, intent(in) :: nPoints
    real(FLOAT), dimension(nPoints, NCOEFS_ADV) :: rows
    type(RealVectorType), dimension(nPoints) :: x
    class(DirectionInterface), dimension(:), allocatable :: n
    real(FLOAT), dimension(NDIM, size(x), NCOEFS_ADV) :: c

    call obj%getPositions( x, polytopeIndex )
    c = undirectedSpatialDerivative%computeSubRowComponents( x, &
         POWERS_ADV )
    ! need to fetch spatialDerivative direction from geometry
    call obj%getFaceNormals( n, polytopeIndex )
    rows = undirectedSpatialDerivative%computeSubRows( c, n )
    call destroy( n )
    
  end function computeSubRows_BoundaryArrival
  
  
  pure subroutine getFaceNormals_BoundaryArrival( obj, &
       directions, polytopeIndex, log )
    class(BoundaryArrivalPointCollectionType), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(out) :: &
         directions
    integer, intent(in) :: polytopeIndex
    type(BoundaryStaticPointGroupPointerType) :: bspgp
    class(LogType), intent(inout), optional :: log
    integer :: allocStat, n
    call beginSub( MOD_NAME, &
         'getFaceNormal_BoundaryArrival', log )

!!$    ! make a PointGroupPointer and pass control to the BoundaryRegion
!!$    call bspgp%init( obj, polytopeIndex )
!!$    call obj%boundaryRegion%getFaceNormals( directions, bspgp, log )
    call obj%boundaryRegion%getFaceNormals( directions, obj, &
         polytopeIndex, log )
    
    call endSub( log )
  end subroutine getFaceNormals_BoundaryArrival


  !-------------------------------------------------------------------
  !- InteriorDeparturePointCollection methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_DEPARTUREPOINTCOLLECTION_CREATEINIT({Boundary}, 
    {boundaryRegion, },
    {
    class(BoundaryRegionInterface), target, intent(in) :: boundaryRegion}, 
    {
    ! boundaryRegion actually contains the PointBoundaryConditions we
    ! are about to link in.  Should we delegate the
    ! append(Scalar|Vector)PointMoments methods to this component
    ! (i.e. uphold the Law of Demeter)?  I don't think so: for one, we
    ! would be forcing BoundaryRegion to work with PointMoments when
    ! it shouldn't be concerned with them.  For another it breaks the
    ! nice symmetry between BoundaryDeparturePoints and
    ! InteriorDeparturePoints.  So use pointing accessors to retrieve
    ! the point BCs.
    call boundaryRegion%getBoundaryConditions( obj%&
         pointVelocityBoundaryCondition )
    call boundaryRegion%getBoundaryConditions( obj%&
         pointScalarBoundaryConditionList )})})
  
  EXPAND({METHODDEF_DEPARTUREPOINTCOLLECTION_DEINIT({Boundary},
   {nullify(obj%pointVelocityBoundaryCondition)
    nullify(obj%pointScalarBoundaryConditionList)})})
  
  
  EXPAND({METHODDEF_APPENDMOMENTS({BoundaryDeparture}, {PointCollection},
  {Boundary}, {BoundaryCondition}, {Point},
  {obj%boundaryArrivalPointCollection}, {Scalar})})

  EXPAND({METHODDEF_APPENDVELOCITYMOMENTS_SINGLESOURCE(
  {BoundaryDeparture}, {PointCollection}, {Boundary}, {BoundaryCondition},
  {Point}, {obj%boundaryArrivalPointCollection})})

  EXPAND({METHODDEF_DEPARTUREPOINTCOLLECTION_COMPUTESUBROWS({Boundary},
  {Undirected})})

  
  subroutine initialiseScalarFields_BoundaryDeparture( obj, &
       scalarFieldSpecifications, log )
    class(BoundaryDeparturePointCollectionType), intent(inout) :: obj
    type(ScalarFieldSpecificationListType), intent(in) :: &
         scalarFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'initialiseScalarFields_BoundaryDeparture', log )
    
    call addEvent( ADVICE, 'Boundary points are not associated with &
         &fields.  Nothing to do.', log )
    
    call endSub( log )
  end subroutine initialiseScalarFields_BoundaryDeparture


  subroutine initialiseVectorFields_BoundaryDeparture( obj, &
       vectorFieldSpecifications, log )
    class(BoundaryDeparturePointCollectionType), intent(inout) :: obj
    type(VectorFieldSpecificationListType), intent(in) :: &
         vectorFieldSpecifications
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'initialiseVectorFields_BoundaryDeparture', log )
    
    call addEvent( ADVICE, 'Boundary points are not associated with &
         &fields.  Nothing to do.', log )
    
    call endSub( log )
  end subroutine initialiseVectorFields_BoundaryDeparture

  
end module BoundaryArrivalPointsModule
