! This module consists mainly of macros to unify methods of the
! concrete classes implementing the SemiLagrangian interfaces.
module GaussianPoints_Macros

  USE_MACROS({FlowField})

  MACRO({ATTRIBUTELIST_GAUSSIANPOINTCOLLECTION}, {{
!!$     class(PolytopeCollectionInterface), pointer :: &
!!$          polytopeCollection
     real(FLOAT), dimension(:), allocatable :: gaussianWeights
     }})
     
  MACRO({TYPEDEF_INTAVEMOMENT}, {{
  type, extends($1IntAveMomentInterface) :: $1$2IntAveMomentType
     private
     type($2GaussianPointGroupPointerType) :: &
          LWR($2)GaussianPointGroupPointer
     class($1IntAve$3Interface), pointer :: &
          LWR($1IntAve$3)
     $4
   contains
     procedure :: init => &
          init_$1$2IntAveMoment
     procedure :: deinit => &
          deinit_$1$2IntAveMoment
     procedure :: computeMatrixRows => &
          computeMatrixRows_$1$2IntAveMoment
     procedure :: getRHSValues => &
          getRHSValues_$1$2IntAveMoment
     $5
  end type $1$2IntAveMomentType
  }})

  MACRO({TYPEDEF_SCALARINTAVEMOMENT},{{
  EXPAND({TYPEDEF_INTAVEMOMENT(Scalar, $1, $2, {
     real(FLOAT), dimension(:, :), allocatable :: rows}, {
     procedure :: getMatrixRows => &
          getMatrixRows_Scalar$1IntAveMoment})})
     }})
     
  MACRO({TYPEDEF_VECTORINTAVEMOMENT},{{
  EXPAND({TYPEDEF_INTAVEMOMENT(Vector, $1, $2, {
     integer :: activeComponent
     real(FLOAT), dimension(:, :, :), allocatable :: rows}, {
     procedure :: getMatrixRows => &
          getMatrixRows_Vector$1IntAveMoment
     procedure :: selectComponent => &
          selectComponent_Vector$1IntAveMoment})})
     }})

     
contains
  
  !-------------------------------------------------------------------
  !- GaussianPointCollection methods
  !-------------------------------------------------------------------
  
  MACRO({INIT_ARGLINE_GAUSSIANPOINTCOLLECTION}, {{
       LWR($1)Region, pointArrangement, &
!!$       polytopeCollection, &
       }})
       
  MACRO({INIT_ARGLIST_GAUSSIANPOINTCOLLECTION}, {{
    class($1RegionInterface), intent(in), target :: &
         LWR($1)Region
    class(PointArrangementInterface), allocatable, intent(inout) :: &
         pointArrangement
!!$    class(PolytopeCollectionInterface), target, &
!!$         intent(in) :: polytopeCollection
    }})
  
  MACRO({INIT_BODY_GAUSSIANPOINTCOLLECTION}, {{
!!$    ! This object will need to know about the associated integrable
!!$    ! elements (faces or cells), but there is a circular dependency in
!!$    ! that the IEs cannot be created before the points.  The best we can
!!$    ! do is pass in the associated polytopeCollection.  The object can ask
!!$    ! polytopeCollection for the IEs in due course.
!!$    obj%polytopeCollection => polytopeCollection

    ! update to the above: maybe there is no need to know about the IEs at
    ! all?
    obj%LWR($1)Region => LWR($1)Region
    call pointArrangement%createGaussianWeights( obj%gaussianWeights, &
         log )
    ! init superclass
    call obj%initStaticPointCollection( pointArrangement, log )
    }})
  
  MACRO({DEINIT_BODY_GAUSSIANPOINTCOLLECTION}, {{
    call obj%deinitStaticPointCollection( log )
    nullify(obj%LWR($1)Region)
!!$    nullify(obj%polytopeCollection)
    call destroy( obj%gaussianWeights, log )
    }})


  
  !-------------------------------------------------------------------
  !- IntAveMoment methods
  !-------------------------------------------------------------------

  m4_define({ARGLINE},
  {LWR($1GaussianPointCollection), polytopeIndex, &
       LWR($3IntAve$2), })
  
  m4_define({ARGLIST},
    {class($1GaussianPointCollectionInterface), target, intent(in) :: &
         LWR($1GaussianPointCollection)
    integer, intent(in) :: polytopeIndex
    class($3IntAve$2Interface), target, intent(in) :: &
         LWR($3IntAve$2)})
  
  MACRO({METHODDEFS_INTAVEMOMENT}, {{
  EXPAND({PROCEDURE_CREATE({$3$1IntAveMoment}, {$3IntAveMoment},
  {ARGLINE($1, $2, $3)},
  {ARGLIST($1, $2, $3)})})
      
  subroutine init_$3$1IntAveMoment( obj, ARGLINE($1, $2, $3)log )
    class($3$1IntAveMomentType), intent(out) :: obj
    ARGLIST($1, $2, $3)
    class(LogType), intent(inout), optional :: log
    integer :: n
    call beginSub( MOD_NAME, 'init_$2$3IntAveMoment', log )

    call obj%LWR($1GaussianPointGroupPointer)%init( &
         LWR($1GaussianPointCollection), polytopeIndex )
    $4
    
    call endSub( log )
  end subroutine init_$3$1IntAveMoment

  
  subroutine deinit_$3$1IntAveMoment( obj, log )
    class($3$1IntAveMomentType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_$2$3IntAveMoment', log )
    
    $5
    call obj%LWR($1GaussianPointGroupPointer)%deinit()
    if ( allocated(obj%rows) ) then
       deallocate( obj%rows, stat=deallocStat )
       call addEvent( deallocStat/=0, WARNING, 'Problem deallocating &
            &obj%rows.  STAT='//int2str(deallocStat), log )
    end if

    call endSub( log )
  end subroutine deinit_$3$1IntAveMoment

  
  subroutine computeMatrixRows_$3$1IntAveMoment( obj, log )
    class($3$1IntAveMomentType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    real(FLOAT), dimension(m4_ifelse($3, {Vector}, {:, :, :}, {:, :})), &
         allocatable :: rows
    call beginSub( MOD_NAME, &
         'computeMatrixRows_ScalarGaussianIntAveMoment', log )

    ! pass $1GaussianPointGroupPointer to $3IntAve$2
    call obj%LWR($3IntAve$2)%computeMatrixRows( rows, obj%&
         LWR({$1GaussianPointGroupPointer}), log )

    ! store the rows in the IAM.  We may bypass storage in the future.
    EXPAND({INJECT({rows})})
        
    call endSub( log )
  end subroutine computeMatrixRows_$3$1IntAveMoment
  

  pure subroutine getMatrixRows_$3$1IntAveMoment( obj, rows )
    class($3$1IntAveMomentType), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(out) :: rows
    rows = obj%rows{}m4_ifelse($3, {Vector}, {(:, :, obj%activeComponent)})
  end subroutine getMatrixRows_$3$1IntAveMoment
  }})
  
  MACRO({METHODDEFS_SCALARINTAVEMOMENT},{{
  EXPAND({METHODDEFS_INTAVEMOMENT($1, $2, {Scalar}, {})})
  }})

  MACRO({METHODDEFS_VECTORINTAVEMOMENT},{{
  EXPAND({METHODDEFS_INTAVEMOMENT($1, $2, {Vector}, {})})
    
  pure subroutine selectComponent_Vector$1IntAveMoment( obj, &
         component )
    class(Vector$1IntAveMomentType), intent(inout) :: obj
    integer, intent(in) :: component
    obj%activeComponent = component
  end subroutine selectComponent_Vector$1IntAveMoment
  }})

  
end module GaussianPoints_Macros
