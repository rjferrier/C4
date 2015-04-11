HEADER()
module InteriorArrivalPointsModule

  USE_MACROS({ArrivalPoints})

  use LogModule
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FlowFieldModule
  use DomainModule
  
  implicit none
  private
  public :: createInteriorPointScalarMomentGroup, &
       createInteriorPointVectorMomentGroup, &
       createInteriorArrivalPointCollection, &
       createInteriorDeparturePointCollection
  
  character(*), parameter :: MOD_NAME = 'InteriorArrivalPointsModule'

  
  !-------------------------------------------------------------------
  !- InteriorPointMomentGroups
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_POINTMOMENTGROUP_CONCRETE({Scalar},
  {Interior}, {MomentField})})

  EXPAND({TYPEDEF_POINTMOMENTGROUP_CONCRETE({Vector},
  {Interior}, {MomentField})})

  
  !-------------------------------------------------------------------
  !- InteriorArrivalPointCollection, InteriorDeparturePointCollection
  !-------------------------------------------------------------------

  
  EXPAND({TYPEDEF_ARRIVALPOINTCOLLECTION({Interior})})

  EXPAND({TYPEDEF_DEPARTUREPOINTCOLLECTION({Interior},
     {
     ! The velocity moment field is a singular entity.  However, it must
     ! be added to a list, because it may be joined by velocity gradients.
     type(PointVectorMomentFieldListType) :: pointVelocityMomentFieldList
     type(PointScalarMomentFieldListType) :: pointScalarMomentFieldList})})

     
contains
  
  !-------------------------------------------------------------------
  !- InteriorPointScalarMomentGroup methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_POINTMOMENTGROUP_CONCRETE({Scalar},
  {Interior}, {MomentField})})
  
  elemental subroutine getValue_InteriorPointScalarMomentGroup( obj, &
       value, momentIndex ) 
    class(InteriorPointScalarMomentGroupType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: momentIndex
    integer :: i
    i = obj%interiorStaticPointGroupPointer%getPolytopeIndex()
    call obj%pointScalarMomentField%getValue( value, i, momentIndex )
  end subroutine getValue_InteriorPointScalarMomentGroup
  

  pure subroutine getValues_InteriorPointScalarMomentGroup( obj, &
       values ) 
    class(InteriorPointScalarMomentGroupType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer :: i
    i = obj%interiorStaticPointGroupPointer%getPolytopeIndex()
    call obj%pointScalarMomentField%getValues( values, i )
  end subroutine getValues_InteriorPointScalarMomentGroup
  

  !-------------------------------------------------------------------
  !- InteriorPointVectorMomentGroup methods
  !-------------------------------------------------------------------
  
  EXPAND({METHODDEFS_POINTMOMENTGROUP_CONCRETE({Vector},
  {Interior}, {MomentField})})
  
  elemental subroutine getValue_InteriorPointVectorMomentGroup( obj, &
       value, momentIndex, componentIndex ) 
    class(InteriorPointVectorMomentGroupType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: momentIndex
    integer, intent(in) :: componentIndex
    integer :: i
    i = obj%interiorStaticPointGroupPointer%getPolytopeIndex()
    call obj%pointVectorMomentField%getValue( value, i, momentIndex, &
         componentIndex )
  end subroutine getValue_InteriorPointVectorMomentGroup

  
  pure subroutine getValues_InteriorPointVectorMomentGroup( obj, &
       values, componentIndex ) 
    class(InteriorPointVectorMomentGroupType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: componentIndex
    integer :: i
    i = obj%interiorStaticPointGroupPointer%getPolytopeIndex()
    call obj%pointVectorMomentField%getValues( values, i, componentIndex )
  end subroutine getValues_InteriorPointVectorMomentGroup

  
  !-------------------------------------------------------------------
  !- InteriorArrivalPointCollection methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_ARRIVALPOINTCOLLECTION_CREATEINIT({Interior})})
  
  EXPAND({METHODDEF_ARRIVALPOINTCOLLECTION_DEINIT({Interior})})
  
  pure function computeSubRows_InteriorArrival( obj, polytopeIndex, &
       directedSpatialDerivative, nPoints ) result ( rows )
    class(InteriorArrivalPointCollectionType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    integer, intent(in) :: nPoints
    real(FLOAT), dimension(nPoints, NCOEFS_ADV) :: rows
    type(RealVectorType), dimension(nPoints) :: x
    real(FLOAT), dimension(NDIM, size(x), NCOEFS_ADV) :: c

    call obj%getPositions( x, polytopeIndex )
    c = directedSpatialDerivative%computeSubRowComponents( x, &
         POWERS_ADV )
    ! direction already embedded in spatialDerivative
    rows = directedSpatialDerivative%computeSubRows( c )
    
  end function computeSubRows_InteriorArrival


  
  !-------------------------------------------------------------------
  !- InteriorDeparturePointCollection methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_DEPARTUREPOINTCOLLECTION_CREATEINIT({Interior}, 
   {pointVelocityMomentFieldList, pointScalarMomentFieldList, },
   {type(PointVectorMomentFieldListType), intent(inout) :: &
         pointVelocityMomentFieldList
    type(PointScalarMomentFieldListType), intent(inout) :: &
         pointScalarMomentFieldList},
   {call obj%pointVelocityMomentFieldList%takeNodes( &
         pointVelocityMomentFieldList, log )
    call obj%pointScalarMomentFieldList%takeNodes( &
         pointScalarMomentFieldList, log )})})
  
  EXPAND({METHODDEF_DEPARTUREPOINTCOLLECTION_DEINIT({Interior},
   {call obj%pointVelocityMomentFieldList%deinit( log )
    call obj%pointScalarMomentFieldList%deinit( log )})})
  
  EXPAND({METHODDEF_APPENDMOMENTS({InteriorDeparture}, {PointCollection},
  {Interior}, {MomentField}, {Point}, {obj%interiorArrivalPointCollection},
  {Scalar})})

  EXPAND({METHODDEF_APPENDVELOCITYMOMENTS_SOURCELIST({InteriorDeparture},
  {PointCollection}, {Interior}, {MomentField}, {Point},
  {obj%interiorArrivalPointCollection})})

  EXPAND({METHODDEF_DEPARTUREPOINTCOLLECTION_COMPUTESUBROWS({Interior},
  {Directed})})

  EXPAND({METHODDEF_INTERIORDEPARTUREPOINTCOLLECTION_INITFIELDS({Scalar},
  {Scalar})})
  
  EXPAND({METHODDEF_INTERIORDEPARTUREPOINTCOLLECTION_INITFIELDS({Vector},
  {Velocity})})
  



end module InteriorArrivalPointsModule
