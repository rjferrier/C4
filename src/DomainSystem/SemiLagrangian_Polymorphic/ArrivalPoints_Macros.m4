! This module consists mainly of macros to unify methods of the
! concrete classes implementing the SemiLagrangian interfaces.
module ArrivalPoints_Macros

  USE_MACROS({FlowField})

  
  MACRO({TYPEDEF_ARRIVALPOINTCOLLECTION}, {{
  type, extends($1StaticPointCollectionInterface) :: $1ArrivalPointCollectionType
     private
     class($1RegionInterface), pointer :: LWR($1)Region => null()
     $2
   contains
     procedure :: init => init_$1Arrival
     procedure :: deinit => deinit_$1Arrival
     procedure :: computeSubRows => &
          computeSubRows_$1Arrival
     $3
  end type $1ArrivalPointCollectionType
  }})

  MACRO({TYPEDEF_DEPARTUREPOINTCOLLECTION}, {{
  type, extends(DeparturePointCollectionInterface) :: $1DeparturePointCollectionType
     private
     logical :: exactlyConstrained = .true.
     class($1StaticPointCollectionInterface), allocatable :: &
          LWR($1)ArrivalPointCollection
     
     ! Although the macro-generated methods append<...>PointMoments use the
     ! following variables, the variables themselves are not
     ! macro-generated.  This is because the ownership of the variables,
     ! and hence their allocatable/pointer statuses, may vary.
     $2
   contains
     procedure :: init => init_$1Departure
     procedure :: deinit => deinit_$1Departure
     procedure :: appendScalarMoments => &
          appendScalarMoments_$1Departure
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_$1Departure
     generic :: appendMoments => appendScalarMoments, &
          appendVelocityMoments
     procedure :: computeSubRows => &
          computeSubRows_$1Departure
     procedure :: initialiseScalarFields => &
          initialiseScalarFields_$1Departure
     procedure :: initialiseVectorFields => &
          initialiseVectorFields_$1Departure
     $3
  end type $1DeparturePointCollectionType
  }})

  
  MACRO({TYPEDEF_POINTMOMENTGROUP_CONCRETE}, {{
  type, extends($1MomentGroupInterface) :: $2Point$1MomentGroupType
     private
     type($2StaticPointGroupPointerType) :: &
          LWR($2)StaticPointGroupPointer
     class(Point$1$3Interface), pointer :: &
          LWR(Point$1$3)
     $4
   contains
     procedure :: init => &
          init_$2Point$1MomentGroup
     procedure :: deinit => &
          deinit_$2Point$1MomentGroup
     procedure :: clone => &
          clone_$2Point$1MomentGroup
     procedure :: appendRows => &
          appendRows_$2Point$1MomentGroup
     procedure :: appendElements => &
          appendElements_$2Point$1MomentGroup
     procedure :: appendEquations => &
          appendEquations_$2Point$1MomentGroup
     procedure :: getValue => &
          getValue_$2Point$1MomentGroup
     procedure :: getValues => &
          getValues_$2Point$1MomentGroup
     $5
  end type $2Point$1MomentGroupType
  }})
  
     
contains

  
  
  !-------------------------------------------------------------------
  !- ArrivalPointCollection methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_ARRIVALPOINTCOLLECTION_CREATEINIT}, {{ 
  m4_define({ARGLINE}, {&
       pointArrangement, LWR($1)Region, $2})
  m4_dnl
  m4_define({ARGLIST},
   {class(PointArrangementInterface), allocatable, intent(inout) :: &
         pointArrangement
    class($1RegionInterface), target, intent(in) :: &
         LWR($1)Region
    $3}) m4_dnl
  
  EXPAND({PROCEDURE_CREATE({$1ArrivalPointCollection},
  {$1StaticPointCollection}, {ARGLINE}, {ARGLIST})})

  subroutine init_$1Arrival( obj, ARGLINE()log )
    class($1ArrivalPointCollectionType), intent(inout) :: obj
    ARGLIST
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_$1Arrival', log )

    $4
    obj%LWR($1)Region => LWR($1)Region
    ! init superclass
    call obj%initStaticPointCollection( pointArrangement, log )
    
    call endSub( log )
  end subroutine init_$1Arrival
  }})

  
  MACRO({METHODDEF_ARRIVALPOINTCOLLECTION_DEINIT}, {{ 
  subroutine deinit_$1Arrival( obj, log )
    class($1ArrivalPointCollectionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_$1Arrival', log )

    call obj%deinitStaticPointCollection( log )
    nullify(obj%LWR($1)Region)
    $2
    
    call endSub( log )
  end subroutine deinit_$1Arrival
  }})

  
  !-------------------------------------------------------------------
  !- DeparturePointCollection methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_DEPARTUREPOINTCOLLECTION_CREATEINIT}, {{ 
  m4_define({ARGLINE}, {&
       LWR($1)ArrivalPointCollection, &
       exactlyConstrained, $2})
  m4_dnl
  m4_define({ARGLIST},
    {class($1StaticPointCollectionInterface), allocatable, intent(&
         inout) :: LWR($1)ArrivalPointCollection
    logical, intent(in) :: exactlyConstrained
    $3}) m4_dnl
  
  EXPAND({PROCEDURE_CREATE({$1DeparturePointCollection},
  {DeparturePointCollection}, {ARGLINE}, {ARGLIST})})

  subroutine init_$1Departure( obj, ARGLINE()log )
    class($1DeparturePointCollectionType), intent(inout) :: obj
    ARGLIST
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_$1Departure', log )

    EXPAND({INJECT({LWR($1)ArrivalPointCollection})})
    obj%exactlyConstrained = exactlyConstrained
    $4
    
    call endSub( log )
  end subroutine init_$1Departure
  }})

  
  MACRO({METHODDEF_DEPARTUREPOINTCOLLECTION_DEINIT}, {{ 
  subroutine deinit_$1Departure( obj, log )
    class($1DeparturePointCollectionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_$1Departure', log )

    $2
    call destroy( obj%LWR($1)ArrivalPointCollection, log )
    obj%exactlyConstrained = .true.
    
    call endSub( log )
  end subroutine deinit_$1Departure
  }})

  MACRO({METHODDEF_DEPARTUREPOINTCOLLECTION_COMPUTESUBROWS}, {{ 
  pure function computeSubRows_$1Departure( obj, &
       polytopeIndex, LWR($2)SpatialDerivative ) result ( row )
    class($1DeparturePointCollectionType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class($2SpatialDerivativeInterface), intent(in) :: &
         LWR($2)SpatialDerivative
    real(FLOAT), dimension(1, NCOEFS_ADV) :: row
    integer :: n
    
    n = obj%LWR($1)ArrivalPointCollection%getNumPointsPerPolytope()
    ! delegate
    row = obj%LWR($1)ArrivalPointCollection%computeSubRows( &
         polytopeIndex, LWR($2)SpatialDerivative, n )
    
  end function computeSubRows_$1Departure
  }})

  
  MACRO({METHODDEF_INTERIORDEPARTUREPOINTCOLLECTION_INITFIELDS}, {{
  subroutine initialise$1Fields_InteriorDeparture( obj, &
       LWR($1)FieldSpecifications, log )
    class(InteriorDeparturePointCollectionType), intent(inout) :: obj
    type($1FieldSpecificationListType), intent(in) :: &
         LWR($1)FieldSpecifications
    class(LogType), intent(inout), optional :: log
    type(RealVectorType), dimension(:, :), allocatable :: r
    TYPE_TENSOR_PRIMITIVE($1), dimension(:), allocatable :: v
    integer :: i, stat
    type($1FieldSpecificationListIteratorType) :: ABB($1)fsli
    class($1FieldSpecificationInterface), pointer :: ABB($1)fs
    type(Point$1MomentFieldListIteratorType) :: p{}ABB($1)mfli
    class(Point$1MomentFieldInterface), pointer :: p{}ABB($1)mf
    call beginSub( MOD_NAME, &
         'initialise$1Fields_InteriorDeparture', log )

    ! get all point coordinates
    call obj%interiorArrivalPointCollection%createPositionsMatrix( r, &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! from this matrix, allocate a values array
    allocate( v(size(r)), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating v(size(r, 1), &
         &size(r, 2)).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise list iterators
    call ABB($1)fsli%init( LWR($1)FieldSpecifications )
    call p{}ABB($1)mfli%init( obj%point$2MomentFieldList )
    
    ! loop over departure point fields
    call p{}ABB($1)mfli%first( p{}ABB($1)mf )
    do
       if ( p{}ABB($1)mfli%isDone() ) exit
       
       ! loop over specs
       call ABB($1)fsli%first( ABB($1)fs )
       do
          if ( ABB($1)fsli%isDone() ) exit

          ! if suitable spec, apply it
          if ( ABB($1)fs%isAssociatedWith(p{}ABB($1)mf) ) then
             call ABB($1)fs%getValues( v, pack(r, .true.) )
             call p{}ABB($1)mf%setField( v )
          end if
          
          call ABB($1)fsli%next( ABB($1)fs )
       end do
       
       call p{}ABB($1)mfli%next( p{}ABB($1)mf )
    end do

    deallocate( r, v, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating r, v.  &
         &STAT='//int2str(stat), log )
    
    call endSub( log )
  end subroutine initialise$1Fields_InteriorDeparture
  }})


  !-------------------------------------------------------------------
  !- PointMomentGroup methods
  !-------------------------------------------------------------------

  m4_define({ARGLINE}, {LWR($2ArrivalPointCollection), &
       polytopeIndex, point$1$3, exactlyConstrained, })
  
  m4_define({ARGLIST},
  {class($2StaticPointCollectionInterface), target, intent(inout) :: &
         LWR($2ArrivalPointCollection)
    integer, intent(in) :: polytopeIndex
    class(Point$1$3Interface), target, intent(inout) :: &
         point$1$3
    logical, intent(in) :: exactlyConstrained})
    
  {class($2GaussianPointCollectionInterface), target, intent(in) :: &
         LWR($2GaussianPointCollection)
    integer, intent(in) :: polytopeIndex})

    
  MACRO({METHODBODY_POINTMOMENTGROUP_APPENDROWS}, {{
    call obj%point$1$3%appendRows_$4( LWR($1$4), &
         obj%LWR({$2StaticPointGroupPointer}), log )
    }})
    
    MACRO({METHODBODY_POINTMOMENTGROUP_APPENDELEMENTS}, {{
    call rhsg%init( obj )
    call LWR($1$4)%set$5Elements( rhsg{}m4_ifelse($1, {Vector}, {, &
         obj%LWR(Point$1$3)%hasCoupledComponents()}) )
    }})
    
    
  MACRO({METHODDEFS_POINTMOMENTGROUP_CONCRETE}, {{
  EXPAND({PROCEDURE_CREATE({$2Point$1MomentGroup}, {$1MomentGroup},
  {ARGLINE($1, $2, $3)}, 
  {ARGLIST($1, $2, $3)})})
    
  pure subroutine init_$2Point$1MomentGroup( obj, ARGLINE($1, $2, $3)log )
    class($2Point$1MomentGroupType), intent(out) :: obj
    ARGLIST($1, $2, $3)
    class(LogType), intent(inout), optional :: log
    integer :: n
    $4
    call beginSub( MOD_NAME, 'init_$3$1PointMomentGroup', log )

    call obj%LWR($2StaticPointGroupPointer)%init( &
         LWR($2ArrivalPointCollection), polytopeIndex )
    
    obj%point$1$3 => point$1$3
    $5
    
    n = LWR($2ArrivalPointCollection)%getNumPointsPerPolytope()
    call obj%init$1MomentGroup( n, exactlyConstrained )
    
    call endSub( log )
  end subroutine init_$2Point$1MomentGroup

  
  pure subroutine deinit_$2Point$1MomentGroup( obj, log )
    class($2Point$1MomentGroupType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_$3$1PointMomentGroup', log )
    
    call obj%deinit$1MomentGroup( )
    $6
    call obj%LWR($2StaticPointGroupPointer)%deinit()
    nullify(obj%point$1$3)

    call endSub( log )
  end subroutine deinit_$2Point$1MomentGroup

  
  pure subroutine clone_$2Point$1MomentGroup( obj, tgt, log )
    class($2Point$1MomentGroupType), intent(inout) :: obj
    class($1MomentGroupInterface), allocatable, intent(out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, &
         'clone_$3$1PointMomentGroup', log )
    
    allocate( $2Point$1MomentGroupType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &$2Point$1MomentGroupType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    select type ( tgt )
    type is ($2Point$1MomentGroupType)
       call tgt%LWR($2)StaticPointGroupPointer%init( &
            obj%LWR($2)StaticPointGroupPointer )
       tgt%LWR(Point$1$3) => obj%LWR(Point$1$3)
    end select

    call endSub( log )
  end subroutine clone_$2Point$1MomentGroup
  
  
  subroutine appendRows_$2Point$1MomentGroup( obj, &
          LWR($1Matrix), log )
    class($2Point$1MomentGroupType), intent(in) :: obj
    class($1MatrixInterface), intent(inout) :: LWR($1Matrix)
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_$2Point$1MomentGroup', log )

    EXPAND({METHODBODY_POINTMOMENTGROUP_APPENDROWS($1, $2, $3,
    {Matrix})})

    call endSub( log )
  end subroutine appendRows_$2Point$1MomentGroup

  
  subroutine appendElements_$2Point$1MomentGroup( obj, &
          LWR($1RHSColumn), log )
    class($2Point$1MomentGroupType), intent(inout) :: obj
    class($1RHSColumnInterface), intent(inout) :: LWR($1RHSColumn)
    class(LogType), intent(inout), optional :: log
    type($1MomGpBasedRHSGeneratorType) :: rhsg
    integer :: i
    call beginSub( MOD_NAME, &
         'appendElements_$2Point$1MomentGroup', log )

    EXPAND({METHODBODY_POINTMOMENTGROUP_APPENDELEMENTS($1, $2, $3, 
    {RHSColumn}, {})})
    
    call endSub( log )
  end subroutine appendElements_$2Point$1MomentGroup
  
  
  subroutine appendEquations_$2Point$1MomentGroup( obj, &
          LWR($1LinearSystem), log )
    class($2Point$1MomentGroupType), intent(inout) :: obj
    class($1LinearSystemInterface), intent(inout) :: LWR($1LinearSystem)
    class(LogType), intent(inout), optional :: log
    type($1MomGpBasedRHSGeneratorType) :: rhsg
    integer :: i
    call beginSub( MOD_NAME, &
         'appendEquations_$2Point$1MomentGroup', log )
    
    EXPAND({METHODBODY_POINTMOMENTGROUP_APPENDROWS($1, $2, $3,
    {LinearSystem})})
    EXPAND({METHODBODY_POINTMOMENTGROUP_APPENDELEMENTS($1, $2, $3, 
    {LinearSystem}, {RHS})})
    
    call endSub( log )
  end subroutine appendEquations_$2Point$1MomentGroup
  }})

  
end module ArrivalPoints_Macros
