
  
  MACRO({DATALIST_POLYTOPECOLLECTION}, {{
     class(DeparturePointCollectionInterface), allocatable :: &
          departurePoints
     type($2SuperPolytopesManagerType) :: superPolytopesManager
     }})

  MACRO({DATALIST_POLYTOPECOLLECTION_WITHEXTENSIONS}, {{
     EXPAND({DATALIST_POLYTOPECOLLECTION({$1}, {$2})})
     ! extensions
     class(Face$2Interface), allocatable :: faceExtension
     class(Cell$2Interface), allocatable :: cellExtension
     }})
     
  MACRO({METHODLIST_POLYTOPECOLLECTION_EXTENSIONS}, {{
     procedure :: setExtension_$3s => &
          setExtension_$3s_$1$2
     procedure :: getCollectionExtension_$3s => &
          getCollectionExtension_$3s_$1$2
     procedure :: get$2Extension_$3s => &
          get$2Extension_$3s_$1$2
     }})
     
  MACRO({METHODLIST_POLYTOPECOLLECTION}, {{
     procedure :: injectDeparturePoints => &
          injectDeparturePoints_$1$2
     procedure :: extractDeparturePoints => &
          extractDeparturePoints_$1$2
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_$1$2
     procedure :: appendScalarMoments => &
          appendScalarMoments_$1$2
     EXPAND({METHODLIST_POLYTOPECOLLECTION_EXTENSIONS({$1}, {$2}, {Face})})
     EXPAND({METHODLIST_POLYTOPECOLLECTION_EXTENSIONS({$1}, {$2}, {Cell})})
     }})


  MACRO({METHODDEFS_POLYTOPECOLLECTION_WRITEPOINTS}, {{
  subroutine writePointLocations_Point$1( obj, stringList, &
       nPointsRunning, precision, indent, log )
    class(Point$1Type), intent(inout) :: obj
    type(StringListType), intent(inout) :: stringList
    integer, intent(inout) :: nPointsRunning
    integer, intent(in), optional :: precision, indent
    class(LogType), intent(inout), optional :: log
    type(StringType) :: s
    type(RealVectorType) :: r
    integer :: i, iostat, p, t
    call beginSub( MOD_NAME, 'writePointLocations_Point$1', log )

    if ( present(precision) ) then
       p = max(precision, 1)
    else
       p = 16
    end if

    if ( present(indent) ) then
       t = indent
    else
       t = 0
    end if
    
    do i = 1, obj%getNumPolytopes()
       m4_ifelse($1, {Grid}, {call obj%getCentroid( i, r )
       })
       write ( s%data, '(t'//str(t)//', '//str(NDIM)//'es'//str(8+&
            p)//'.'//str(p)//'e2)', iostat=iostat ) &
            m4_ifelse($1, {Grid}, {r}, {obj%coords(i)})%getValues()
       
       call addEvent( iostat/=0, FATAL, 'Problem during write.  &
            &IOSTAT='//str(iostat), log )
       call stringList%append( s, log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
    end do
    
    ! update globalPointIndexOffset and update the running total
    obj%globalPointIndexOffset = nPointsRunning
    nPointsRunning = nPointsRunning + obj%getNumPolytopes()
    
    call endSub( log )
  end subroutine writePointLocations_Point$1
    
  
  subroutine writePointConnectivity_Point$1( obj, connectivityList, &
       offsetList, nPolytopesRunning, startFromZero, indent, log )
    class(Point$1Type), intent(inout) :: obj
    type(StringListType), intent(inout) :: connectivityList, offsetList
    integer, intent(inout) :: nPolytopesRunning
    logical, intent(in) :: startFromZero
    integer, intent(in), optional :: indent
    class(LogType), intent(inout), optional :: log
    integer :: i, iostat, t
    type(StringListIteratorType) :: sli
    type(StringType) :: s
    call beginSub( MOD_NAME, 'writeConnectivity_Point$1', log )
    
    if ( present(indent) ) then
       t = indent
    else
       t = 0
    end if

    do i = 1, obj%getNumPolytopes()
       call obj%appendConnection( s%data, i, startFromZero, indent )
       call connectivityList%append( s, log )
       
       write ( s%data, '(t'//str(t)//', i)', iostat=iostat ) i
       call addEvent( iostat/=0, FATAL, 'Problem during write.  &
            &IOSTAT='//str(iostat), log )
       call offsetList%append( s, log )
       
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
    end do
    
    call endSub( log )
  end subroutine writePointConnectivity_Point$1


  pure subroutine appendConnection_Point$1( obj, string, &
       polytopeIndex, startFromZero, indent )
    class(Point$1Type), intent(in) :: obj
    character(*), intent(inout) :: string
    integer, intent(in)  :: polytopeIndex
    logical, intent(in)  :: startFromZero
    integer, intent(in), optional :: indent
    integer :: l, t, globalIndex
    
    ! write some whitespace.  First determine indent width
    if ( present(indent) ) then
       t = indent
    else
       t = 0
    end if
    ! add a separator if necessary
    l = len_trim(string)
    if ( l > 0 .and. t == 0 ) t = t + 1
    string(l+1:t) = ' '
    
    ! now write the actual number
    globalIndex = obj%globalPointIndexOffset + polytopeIndex
    if ( startFromZero ) globalIndex = globalIndex - 1
    string(l+1:t) = str(globalIndex)
  end subroutine appendConnection_Point$1
  }})
     
     
  MACRO({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_NOTAPPLICABLE}, {{
  subroutine setExtension_$3s_$1$2( obj, LWR($3$2), log )
    class($1$2Type), intent(inout) :: obj
    class($3$2Interface), allocatable, intent(inout) :: LWR($3$2)
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'setExtension_$3s_$1$2', log )

    ! not applicable
    call addEvent( WARNING, 'Object cannot have this extension.', &
         log )

    call endSub( log )
  end subroutine setExtension_$3s_$1$2

  
  pure subroutine getCollectionExtension_$3s_$1$2( obj, &
       LWR($3)Collection, log )
    class($1$2Type), intent(inout), target :: obj
    class($3CollectionInterface), pointer, intent(inout) :: &
         LWR($3)Collection
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'getCollectionExtension_$3s_$1$2', log )
    
    ! not applicable
    call addEvent( WARNING, 'Object cannot have this extension.', &
         log )
    nullify(LWR($3)Collection)

    call endSub( log )
  end subroutine getCollectionExtension_$3s_$1$2

  
  pure subroutine get$2Extension_$3s_$1$2( obj, LWR($3$2), log )
    class($1$2Type), intent(inout), target :: obj
    class($3$2Interface), pointer, intent(inout) :: LWR($3$2)
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'get$2Extension_$3s_$1$2', log )

    ! not applicable
    call addEvent( WARNING, 'Object cannot have this extension.', &
         log )
    nullify(LWR($3$2))
    
    call endSub( log )
  end subroutine get$2Extension_$3s_$1$2
  }})


    
  MACRO({METHODDEFS_POLYTOPECOLLECTION_EXTENSIONS_POSITIVE}, {{
  subroutine setExtension_$3s_$1$2( obj, LWR($3$2), log )
    class($1$2Type), intent(inout) :: obj
    class($3$2Interface), allocatable, intent(inout) :: LWR($3$2)
    class(LogType), intent(inout), optional :: log
    integer :: stat $4
    call beginSub( MOD_NAME, 'create$3Extension_$1$2', log )

    call addEvent( allocated(obj%LWR($3)Extension), FATAL, '$3 &
         &extension already exists.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call checkInjection( allocated(LWR($3$2)), &
         allocated(obj%LWR($3)Extension), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( LWR($3$2), obj%LWR($3)Extension )
    $5

    call endSub( log )
  end subroutine setExtension_$3s_$1$2


  pure subroutine getCollectionExtension_$3s_$1$2( obj, &
       LWR($3)Collection, log )
    class($1$2Type), intent(inout), target :: obj
    class($3CollectionInterface), pointer, intent(inout) :: &
         LWR($3)Collection
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'getCollectionExtension_$3s_$1$2', log )

    if ( allocated(obj%LWR($3)Extension) ) then
       LWR($3)Collection => obj%LWR($3)Extension
    else
       nullify(LWR($3)Collection)
    end if

    call endSub( log )
  end subroutine getCollectionExtension_$3s_$1$2


  pure subroutine get$2Extension_$3s_$1$2( obj, LWR($3$2), log )
    class($1$2Type), intent(inout), target :: obj
    class($3$2Interface), pointer, intent(inout) :: LWR($3$2)
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'get$2Extension_$3s_$1$2', log )

    if ( allocated(obj%LWR($3)Extension) ) then
       LWR($3$2) => obj%LWR($3)Extension
    else
       nullify(LWR($3$2))
    end if

    call endSub( log )
  end subroutine get$2Extension_$3s_$1$2
  }})


  
  MACRO({METHODDEFS_POLYTOPECOLLECTION_DEPARTUREPOINTS}, {{
  subroutine injectDeparturePoints_$1$2( obj, departurePoints, log )
    class($1$2Type), intent(inout) :: obj
    class(DeparturePointCollectionInterface), allocatable, intent(&
         inout) :: departurePoints
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'injectDeparturePoints_$1$2', log )

    call addEvent( FATAL, 'This method is not implemented yet.', log )

    call endSub( log )
  end subroutine injectDeparturePoints_$1$2


  subroutine extractDeparturePoints_$1$2( obj, departurePoints, log )
    class($1$2Type), intent(inout) :: obj
    class(DeparturePointCollectionInterface), allocatable, intent(&
         inout) :: departurePoints
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'extractDeparturePoints_$1$2', log )

    call addEvent( FATAL, 'This method is not implemented yet.', log )

    call endSub( log )
  end subroutine extractDeparturePoints_$1$2
  }})


  
  MACRO({METHODDEFS_POLYTOPECOLLECTION_APPENDMOMENTS}, {{
  subroutine appendVelocityMoments_$1$2( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class($1$2Type), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_$1$2', log )

    if ( allocated(obj%departurePoints) ) then
       call obj%departurePoints%appendVelocityMoments( &
            velocityMomentGroupList, polytopeIndex, log ) $3
    else
       call addEvent( .not. allocated(obj%departurePoints), ADVICE, &
            'Object ('//trim(obj%describe())//') has no departure &
            &points.  Exiting this method.', log )
    end if
    
    call endSub( log )
  end subroutine appendVelocityMoments_$1$2

  
  subroutine appendScalarMoments_$1$2( obj, &
       scalarMomentGroupList, polytopeIndex, log )
    class($1$2Type), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendScalarMoments_$1$2', log )

    if ( allocated(obj%departurePoints) ) then
       call obj%departurePoints%appendScalarMoments( &
            scalarMomentGroupList, polytopeIndex, log ) $4
    else
       call addEvent( .not. allocated(obj%departurePoints), ADVICE, &
            'Object ('//trim(obj%describe())//') has no departure &
            &points.  Exiting this method.', log )
    end if
    
    call endSub( log )
  end subroutine appendScalarMoments_$1$2
  }})

  
  MACRO({METHODBODY_POLYTOPEEXTENSION_APPENDMOMENTS}, {{
    if ( allocated(obj%LWR($1)Extension) ) then
       call obj%LWR($1)Extension%append$2Moments( &
            LWR($2)MomentGroupList, polytopeIndex, log )
    end if}})
  
  
  MACRO({METHODDEFS_POLYTOPECOLLECTION_APPENDMOMENTS_WITHEXTENSIONS}, {{
  EXPAND({METHODDEFS_POLYTOPECOLLECTION_APPENDMOMENTS({$1}, {$2},
    {

    EXPAND({METHODBODY_POLYTOPEEXTENSION_APPENDMOMENTS({Face},
    {Velocity})})
    
    EXPAND({METHODBODY_POLYTOPEEXTENSION_APPENDMOMENTS({Cell},
    {Velocity})})
    },{
    
    EXPAND({METHODBODY_POLYTOPEEXTENSION_APPENDMOMENTS({Face},
    {Scalar})})
    
    EXPAND({METHODBODY_POLYTOPEEXTENSION_APPENDMOMENTS({Cell},
    {Scalar})})
    })})
  }})
