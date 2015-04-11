module IntegrableElements_Macros

  USE_MACROS({FiniteVolume})
  
  MACRO({TYPEDEF_INTEGRABLEELEMENT}, {{
  type, extends($2Interface) :: $1Type
     private
     class($3StaticPointCollectionInterface), allocatable :: &
          LWR($3)GaussianPointCollection{}$4
   contains
     procedure :: init => init_$1
     procedure :: deinit => deinit_$1
     procedure :: computeSubRow => computeSubRow_$1{}$5
  end type $1Type
  }})
  
  MACRO({TYPEDEF_FACECOLLECTION_CONCRETE}, {{
  EXPAND({TYPEDEF_INTEGRABLEELEMENT({$2Face$1}, {Face$1}, {$2}, {$3}, {
     procedure :: appendVelocityMoments => appendVelocityMoments_$2Face$1
     procedure :: appendScalarMoments => appendScalarMoments_$2Face$1
     procedure :: getFaceNormal => getFaceNormal_$2Face$1
     procedure :: findSide => findSide_$2Face$1{}$4})})
  }})

  MACRO({TYPEDEF_CELLCOLLECTION_CONCRETE}, {{
  EXPAND({TYPEDEF_INTEGRABLEELEMENT({Cell$1}, {Cell$1}, {Interior}, {$2}, {
     procedure :: appendVelocityMoments => appendVelocityMoments_Cell$1
     procedure :: appendScalarMoments => appendScalarMoments_Cell$1{}$3})})
  }})
  
  
  MACRO({TYPEDEF_INTAVEMOMENT_CONCRETE}, {{
  type, extends($1MomentGroupInterface) :: $2IntAve$1MomentGroupType
     private
     type($2StaticPointGroupPointerType) :: &
          LWR($2)GaussianPointGroupPointer
     class(IntAve$1$3Interface), pointer :: &
          LWR(IntAve$1$3)
     $4
   contains
     procedure :: init => &
          init_$2IntAve$1MomentGroup
     procedure :: deinit => &
          deinit_$2IntAve$1MomentGroup
     procedure :: clone => &
          clone_$2IntAve$1MomentGroup
     procedure :: appendRows => appendRows_$2IntAve$1MomentGroup
     procedure :: appendElements => appendElements_$2IntAve$1MomentGroup
     procedure :: appendEquations => appendEquations_$2IntAve$1MomentGroup
     procedure :: getValue => &
          getValue_$2IntAve$1MomentGroup
     procedure :: getValues => &
          getValues_$2IntAve$1MomentGroup
     
     $5
  end type $2IntAve$1MomentGroupType
  }})

  MACRO({TYPEDEF_SCALARINTAVEMOMENT_CONCRETE},{{
  EXPAND({TYPEDEF_INTAVEMOMENT_CONCRETE(Scalar, $1, $2, {}, {})})
  }})
     
  MACRO({TYPEDEF_VECTORINTAVEMOMENT_CONCRETE},{{
  EXPAND({TYPEDEF_INTAVEMOMENT_CONCRETE(Vector, $1, $2, {}, {})})
  }})

     
contains

  
  !-------------------------------------------------------------------
  !- IntegrableElement methods
  !-------------------------------------------------------------------
  
  MACRO({PROCEDURE_ATTACH_INTEGRABLEELEMENTCOLLECTION}, {{
  EXPAND({PROCEDURE_ATTACH({$1$3}, {$2$3}, {Polytope$3}, {&
       LWR($4)GaussianPointCollection, $5},
  {class(InteriorStaticPointCollectionInterface), allocatable, &
        intent(inout) :: LWR($4)GaussianPointCollection $6}, {$7})})
  }})
  
  MACRO({METHODDEF_INTEGRABLEELEMENTCOLLECTION_INIT}, {{
  subroutine init_$1$2( obj, polytope$2, LWR($3)GaussianPointCollection, &
       $4log )
    class($1$2Type), intent(out) :: obj
    class(Polytope$2Interface), target, intent(in) :: polytope$2
    class($3StaticPointCollectionInterface), allocatable, &
        intent(inout) :: LWR($3)GaussianPointCollection
    class(LogType), intent(inout), optional :: log $5
    call beginSub( MOD_NAME, 'init_$1$2', log )

    ! init extension superclass
    call obj%initExtension( polytope$2, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! init components
    EXPAND({INJECT({LWR($3)GaussianPointCollection})})
    $6

    call endSub( log )
  end subroutine init_$1$2
  }})

  
  MACRO({METHODDEF_INTEGRABLEELEMENTCOLLECTION_DEINIT}, {{
  subroutine deinit_$1( obj, $3log )
    class($1Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log $4
    call beginSub( MOD_NAME, 'deinit_$1', log )

    ! deinit components
    $5
    call destroy( obj%LWR($2)GaussianPointCollection, log )

    ! deinit extension superclass
    call obj%deinitExtension( log )
    call endSub( log )
  end subroutine deinit_$1
  }})

  
  MACRO({METHODDEF_INTEGRABLEELEMENTCOLLECTION_COMPUTESUBROW}, {{
  pure function computeSubRow_$1( obj, polytopeIndex, &
       directedSpatialDerivative{}TRIMCOMMA({, $3}) ) result ( row )
    class($1Type), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    real(FLOAT), dimension(1, NCOEFS_ADV) :: row
    integer, parameter :: nPoints = 1 $4

    ! delegate
    row = obj%LWR($2)GaussianPointCollection%computeSubRows( &
         polytopeIndex, directedSpatialDerivative, nPoints )
    $5
  end function computeSubRow_$1
  }})

  
  !-------------------------------------------------------------------
  !- FaceCollection methods
  !-------------------------------------------------------------------

  MACRO({PROCEDURE_ATTACH_FACECOLLECTION}, {{
  EXPAND({PROCEDURE_ATTACH_INTEGRABLEELEMENTCOLLECTION({$2Face}, {Face}, {$1}, {$2})})
  }})
  
  MACRO({METHODDEF_FACECOLLECTION_INIT}, {{
  EXPAND({METHODDEF_INTEGRABLEELEMENTCOLLECTION_INIT({$2Face}, {$1}, {$2})})
  }})
  
  MACRO({METHODDEF_FACECOLLECTION_DEINIT}, {{
  EXPAND({METHODDEF_INTEGRABLEELEMENTCOLLECTION_DEINIT({$2Face$1}, {$2})})
  }})
  
  MACRO({METHODDEF_FACECOLLECTION_COMPUTESUBROW}, {{
  EXPAND({METHODDEF_INTEGRABLEELEMENTCOLLECTION_COMPUTESUBROW(
  {$2Face$1}, {$2})})
  }})


  ! @TODO: Combine InteriorFace and BoundaryFace common methods in this
  ! space
  

  
  !-------------------------------------------------------------------
  !- CellCollection methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_CELLCOLLECTION_APPENDMOMENTS}, {{
!!$  ! This is a generic, macro-generated method for appending intAve
!!$  ! moments to a list based on some target flowVariable.
!!$  subroutine append$3Moments_Cell$1( obj, LWR($3)MomentGroupList, &
!!$       polytopeIndex, log )
!!$    class(Cell$1Type), intent(inout) :: obj
!!$    type($3MomentGroupListType), intent(inout) :: &
!!$         LWR($3)MomentGroupList
!!$    integer, intent(in) :: polytopeIndex
!!$    class(LogType), intent(inout), optional :: log
!!$    integer :: allocStat
!!$    type(IntAve$3$2ListIteratorType) :: iterator
!!$    class(IntAve$3$2Interface), pointer :: ABB(IntAve$3$2)
!!$    class($3MomentGroupInterface), allocatable :: ABB($3)mg
!!$    class($3FlowVariableInterface), pointer :: fv, fvTgt
!!$    logical, parameter :: DEBUG_MODE = .false.
!!$    call beginSub( MOD_NAME, &
!!$         'append$3Moments_Cell$1', log )
!!$
!!$    call LWR($3)MomentGroupList%getFlowVariable( fvTgt )
!!$    call addEvent( .not. associated(fvTgt), FATAL, 'LWR($3)&
!!$         &MomentGroupList has not been set a $3FlowVariable.', log )
!!$    if ( checkSub(FATAL, log) ) then
!!$       call endSub( log )
!!$       return
!!$    end if
!!$
!!$    call addEvent( DEBUG_MODE, ADVICE, 'Target flowVariable is '//&
!!$         fvTgt%describe(), log )
!!$    call addEvent( DEBUG_MODE, ADVICE, 'obj%intAve$3$2List &
!!$         &is '//str(obj%intAve$3$2List%size())//&
!!$         ' nodes long.', log )
!!$    
!!$    ! iterate over the IntAve$2s owned by the cells.
!!$    ! For each IntAve$2, we need to create new moment(s)
!!$    ! if it is associated with the same flowVariable.
!!$    call iterator%init( obj%intAve$3$2List )
!!$    call iterator%first( ABB(IntAve$3$2) )
!!$    do
!!$       if ( iterator%isDone() ) exit
!!$       
!!$       if ( DEBUG_MODE ) then
!!$          call ABB(IntAve$3$2)%getFlowVariable( fv )
!!$          call addEvent( ADVICE, 'Current ABB(IntAve$3$2) => '//&
!!$               fv%describe(), log )
!!$       end if
!!$       
!!$       if ( ABB(IntAve$3$2)%&
!!$            isAssociatedWith(fvTgt) ) then
!!$          call addEvent( DEBUG_MODE, ADVICE, 'Adding to the list.', log )
!!$          
!!$          call createInteriorIntAve$3Moment( ABB($3)mg, obj%&
!!$               LWR($4)GaussianPointCollection, polytopeIndex, &
!!$               ABB(IntAve$3$2), $5, log )
!!$          
!!$          ! append to the list
!!$          call $3MomentGroupList%append( ABB($3)mg, log )
!!$          if ( checkSub(FATAL, log) ) then
!!$             call endSub( log )
!!$             return
!!$          end if
!!$          
!!$       end if
!!$       
!!$       call iterator%next( ABB(IntAve$3$2) )
!!$    end do
!!$    
!!$    call endSub( log )
!!$  end subroutine append$3Moments_Cell$1
  }})

  MACRO({PROCEDURE_ATTACH_CELLCOLLECTION}, {{
  EXPAND({PROCEDURE_ATTACH_INTEGRABLEELEMENTCOLLECTION({Cell}, {Cell}, {$1}, {Interior}, {$2}, {$3}, {$4})})
  }})
  
  MACRO({METHODDEF_CELLCOLLECTION_INIT}, {{
  EXPAND({METHODDEF_INTEGRABLEELEMENTCOLLECTION_INIT({Cell}, {$1}, {Interior}, {$2}, {$3}, {$4})})
  }})
  
  MACRO({METHODDEF_CELLCOLLECTION_DEINIT}, {{
  EXPAND({METHODDEF_INTEGRABLEELEMENTCOLLECTION_DEINIT({Cell$1}, {Interior})})
  }})
  
  MACRO({METHODDEF_CELLCOLLECTION_COMPUTESUBROW}, {{
  EXPAND({METHODDEF_INTEGRABLEELEMENTCOLLECTION_COMPUTESUBROW({Cell$1}, {Interior})})
  }})

  
  !-------------------------------------------------------------------
  !- IntAveMoment methods
  !-------------------------------------------------------------------
  
  
  MACRO({METHODDEF_INTEGRABLEELEMENTCOLLECTION_CONCRETE_CREATEINTAVEMOMENT}, {{
  ! This is a generic, macro-generated method for returning an
  ! integrated average moment based on some target flowVariable.
  subroutine createIntAve$3Moment_$1( obj, LWR($3)Moment, &
       polytopeIndex, LWR($3)FlowVariable, log )
    class($1Type), intent(inout) :: obj
    class($3MomentGroupInterface), allocatable, intent(inout) :: &
         LWR($3)Moment
    integer, intent(in) :: polytopeIndex
    class($3FlowVariableInterface), intent(in) :: &
         LWR($3)FlowVariable
    class(LogType), intent(inout), optional :: log
    integer :: stat
    type(IntAve$3$2ListIteratorType) :: iterator
    class(IntAve$3$2Interface), pointer :: ABB(IntAve$3$2)
    class($3FlowVariableInterface), pointer :: fv, fvTgt
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, 'createIntAve$3Moment_$1', log )

    call addEvent( DEBUG_MODE, ADVICE, 'Target flowVariable is '//&
         LWR($3)FlowVariable%describe(), log )
    call addEvent( DEBUG_MODE, ADVICE, &
         'obj%intAve$3$2List is '//&
         str(obj%intAve$3$2List%size())//&
         ' nodes long.', log )
    
    ! iterate over the IntAve$2s owned by the collection.
    ! For each IntAve$2, we need to create new moment(s) if it is
    ! associated with the same flowVariable.
    call iterator%init( obj%intAve$3$2List )
    call iterator%first( ABB(IntAve$3$2) )
    do
       if ( iterator%isDone() ) exit
       
       if ( DEBUG_MODE ) then
          call ABB(IntAve$3$2)%getFlowVariable( fv )
          call addEvent( ADVICE, 'Current ABB(IntAve$3$2) => '//&
               fv%describe(), log )
       end if
       
       if ( ABB(IntAve$3$2)%&
            isAssociatedWith(LWR($3)FlowVariable) ) then

          if ( allocated(LWR($3)Moment) ) then
             call addEvent( FATAL, 'LWR($3)Moment already &
                  &allocated.  Check the containing list.  Only one &
                  &MomentField per variable is allowed in the list.' )
          else
             call addEvent( DEBUG_MODE, ADVICE, 'Creating integrated &
                  &average moment.', log )
          end if
          
          ! delegate straightforwardly to the appropriate creation
          ! procedure residing in the same module
          call create$4IntAve$3MomentGroup( LWR($3)Moment, obj%&
               LWR($4)GaussianPointCollection, polytopeIndex, &
               ABB(IntAve$3$2), $5, log )
          
!!$          ! delegate to the object's InteriorStaticPointCollection
!!$          call obj%interiorGaussianPointCollection%&
!!$               createIntAveScalarMoment( LWR($3)Moment, &
!!$               polytopeIndex, ABB(IntAve$3$2), log )
       end if
       
       call iterator%next( ABB(IntAve$3$2) )
    end do
    
    call endSub( log )
  end subroutine createIntAve$3Moment_$1
  }})


  MACRO({METHODBODY_INTAVEMOMENTGROUP_APPENDROWS}, {{
    call obj%LWR(IntAve$3$2)%appendRows_$4( &
         LWR($3$4), &
         obj%LWR({$1GaussianPointGroupPointer}), log )
    }})
    

  MACRO({METHODBODY_INTAVEMOMENTGROUP_APPENDELEMENTS}, {{
    call rhsg%init( obj )
    m4_ifelse($3, {Vector}, {
    call LWR($3$4)%set$5Elements( rhsg, .false. )}, {
    call LWR($3$4)%set$5Elements( rhsg )})
    }})
    

  m4_define({ARGLINE}, {LWR($1GaussianPointCollection), &
       polytopeIndex, LWR(IntAve$3$2), exactlyConstrained, })
  m4_define({ARGLIST},
   {class($1StaticPointCollectionInterface), target, intent(inout) :: &
         LWR($1GaussianPointCollection)
    integer, intent(in) :: polytopeIndex
    class(IntAve$3$2Interface), target, intent(in) :: &
         LWR(IntAve$3$2)
    logical, intent(in) :: exactlyConstrained})
  
  MACRO({METHODDEFS_INTAVEMOMENT_CONCRETE}, {{
  EXPAND({PROCEDURE_CREATE({$1IntAve$3MomentGroup}, {$3MomentGroup},
  {ARGLINE($1, $2, $3)},
  {ARGLIST($1, $2, $3)})})
  
  subroutine init_$1IntAve$3MomentGroup( obj, ARGLINE($1, $2, $3) log )
    class($1IntAve$3MomentGroupType), intent(out) :: obj
    ARGLIST($1, $2, $3)
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_$1IntAve$3MomentGroup', log )

    call obj%LWR($1GaussianPointGroupPointer)%init( &
         LWR($1GaussianPointCollection), polytopeIndex )
    obj%LWR(IntAve$3$2) => LWR(IntAve$3$2)

    ! there is exactly one moment
    call obj%init$3MomentGroup( 1, exactlyConstrained )
    $4
    
    call endSub( log )
  end subroutine init_$1IntAve$3MomentGroup

  
  pure subroutine deinit_$1IntAve$3MomentGroup( obj, log )
    class($1IntAve$3MomentGroupType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinit_$1IntAve$3MomentGroup', log )
    
    $5
    call obj%deinit$3MomentGroup()
    
    call obj%LWR($1GaussianPointGroupPointer)%deinit()
    nullify(obj%LWR(IntAve$3$2))

    call endSub( log )
  end subroutine deinit_$1IntAve$3MomentGroup

  
  pure subroutine clone_$1IntAve$3MomentGroup( obj, tgt, log )
    class($1IntAve$3MomentGroupType), intent(inout) :: obj
    class($3MomentGroupInterface), allocatable, intent(out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_$1IntAve$3MomentGroup', log )

    allocate( $1IntAve$3MomentGroupType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &$1IntAve$3MomentGroupType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    select type ( tgt )
    type is ($1IntAve$3MomentGroupType)
       call tgt%LWR($1GaussianPointGroupPointer)%init( &
            obj%LWR($1GaussianPointGroupPointer) )
       tgt%LWR(IntAve$3$2) => obj%LWR(IntAve$3$2)
    end select

    call endSub( log )
  end subroutine clone_$1IntAve$3MomentGroup

  
  subroutine appendRows_$1IntAve$3MomentGroup( obj, &
          LWR($3Matrix), log )
    class($1IntAve$3MomentGroupType), intent(in) :: obj
    class($3MatrixInterface), intent(inout) :: LWR($3Matrix)
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_$1IntAve$3MomentGroupGroup', log )

    EXPAND({METHODBODY_INTAVEMOMENTGROUP_APPENDROWS($1, $2, $3,
    {Matrix})})
    
    call endSub( log )
  end subroutine appendRows_$1IntAve$3MomentGroup
  
  
  subroutine appendElements_$1IntAve$3MomentGroup( obj, &
          LWR($3RHSColumn), log )
    class($1IntAve$3MomentGroupType), intent(inout) :: obj
    class($3RHSColumnInterface), intent(inout) :: &
         LWR($3RHSColumn)
    class(LogType), intent(inout), optional :: log
    type($3MomGpBasedRHSGeneratorType) :: rhsg
    integer :: i
    call beginSub( MOD_NAME, 'appendElements_$1IntAve$3MomentGroupGroup', &
         log )

    EXPAND({METHODBODY_INTAVEMOMENTGROUP_APPENDELEMENTS($1, $2, $3, 
    {RHSColumn}, {}, $6)})
    
    call endSub( log )
  end subroutine appendElements_$1IntAve$3MomentGroup
  
  
  subroutine appendEquations_$1IntAve$3MomentGroup( obj, &
          LWR($3LinearSystem), log )
    class($1IntAve$3MomentGroupType), intent(inout) :: obj
    class($3LinearSystemInterface), intent(inout) :: &
         LWR($3LinearSystem)
    class(LogType), intent(inout), optional :: log
    type($3MomGpBasedRHSGeneratorType) :: rhsg
    integer :: i
    call beginSub( MOD_NAME, &
         'appendEquations_$1IntAve$3MomentGroupGroup', log )

    EXPAND({METHODBODY_INTAVEMOMENTGROUP_APPENDROWS($1, $2, $3,
    {LinearSystem})})
    EXPAND({METHODBODY_INTAVEMOMENTGROUP_APPENDELEMENTS($1, $2, $3,
    {LinearSystem}, {RHS})})
    
    call endSub( log )
  end subroutine appendEquations_$1IntAve$3MomentGroup
  }})
  
  MACRO({METHODDEFS_INTAVESCALARMOMENT},{{
  EXPAND({METHODDEFS_INTAVEMOMENT_CONCRETE($1, $2, {Scalar}, {}, {})})
  }})

  MACRO({METHODDEFS_INTAVEVECTORMOMENT},{{
  EXPAND({METHODDEFS_INTAVEMOMENT_CONCRETE($1, $2, {Vector}, {}, {})})
  }})

  
end module IntegrableElements_Macros
