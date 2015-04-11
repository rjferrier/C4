module PointMomentFieldsModule
  
  use LogModule
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FlowFieldModule

  ! placeholder for a module that points to any one of the variants in
  ! the folder of the same name.  The module should not be created
  ! within the source tree as it may interfere with automated tests
  ! that define their own variants.
  use PointMomentFields_DomainDim
  
  implicit none
  private
  public :: createPointScalarMomentArray, &
       createPointVectorMomentArray, createPointScalarMomentGrid, &
       createPointVectorMomentGrid
  
  character(*), parameter :: MOD_NAME = 'PointMomentFieldsModule'
  
  
  !-------------------------------------------------------------------
  !- PointScalarMomentArray and PointVectorMomentArray
  !-------------------------------------------------------------------
  
  type, extends(PointScalarMomentFieldInterface) :: PointScalarMomentArrayType
     private
     ! dimensions correspond to (1) polytopes and (2) points on each
     ! polytope
     real(FLOAT), dimension(:, :), allocatable :: values
   contains
     procedure :: init => init_ScalarArray
     procedure :: deinit => deinit_ScalarArray
     procedure :: clone => clone_ScalarArray
     procedure :: setField_uniform => &
          setField_uniform_ScalarArray
     procedure :: setField_array => &
          setField_array_ScalarArray
     procedure :: appendRows_Matrix => appendRows_Matrix_ScalarArray
     procedure :: appendRows_LinearSystem => &
          appendRows_LinearSystem_ScalarArray
     procedure :: getValue => getValue_ScalarArray
     procedure :: getValues => getValues_ScalarArray
  end type PointScalarMomentArrayType

  
!!$  type, extends(RHSElementInterface) :: RHSElement_PSMAType
!!$     type(PointScalarMomentArrayType), pointer :: psma
!!$     integer :: polytopeIndex, pointIndex
!!$   contains
!!$     procedure :: init => init_RHSElement_PSMA
!!$     procedure :: deinit => deinit_RHSElement_PSMA
!!$     procedure :: getValue => getValue_RHSElement_PSMA
!!$     procedure :: clone => clone_RHSElement_PSMA
!!$  end type RHSElement_PSMAType
  
  
  type, extends(PointVectorMomentFieldInterface) :: PointVectorMomentArrayType
     private
     ! dimensions correspond to (1) polytopes and (2) points on each
     ! polytope
     real(FLOAT), dimension(:, :, :), allocatable :: values
   contains
     procedure :: init => init_VectorArray
     procedure :: deinit => deinit_VectorArray
     procedure :: clone => clone_VectorArray
     procedure :: setField_uniform => &
          setField_uniform_VectorArray
     procedure :: setField_array => &
          setField_array_VectorArray
     procedure :: appendRows_Matrix => appendRows_Matrix_VectorArray
     procedure :: appendRows_LinearSystem => &
          appendRows_LinearSystem_VectorArray
     procedure :: getValue => getValue_VectorArray
     procedure :: getValues => getValues_VectorArray
     procedure :: hasCoupledComponents => &
          hasCoupledComponents_VectorArray
  end type PointVectorMomentArrayType
  

  !-------------------------------------------------------------------
  !- PointScalarMomentGrid and PointVectorMomentGrid 
  !-------------------------------------------------------------------

  type, extends(PointScalarMomentFieldInterface) :: PointScalarMomentGridType
     private
     type(GridParametersType) :: gridParameters
     type(PointScalarMomentGrid_NDPartsType) :: nDParts
   contains
     procedure :: init => init_ScalarGrid
     procedure :: deinit => deinit_ScalarGrid
     procedure :: clone => clone_ScalarGrid
     procedure :: setField_uniform => setField_uniform_ScalarGrid
     procedure :: setField_array => setField_array_ScalarGrid
     procedure :: appendRows_Matrix => appendRows_Matrix_ScalarGrid
     procedure :: appendRows_LinearSystem => &
          appendRows_LinearSystem_ScalarGrid
     procedure :: getValue => getValue_ScalarGrid
     procedure :: getValues => getValues_ScalarGrid

  end type PointScalarMomentGridType
  

  type, extends(PointVectorMomentFieldInterface) :: PointVectorMomentGridType
     private
     type(GridParametersType) :: gridParameters
     type(PointVectorMomentGrid_NDPartsType) :: nDParts
   contains
     procedure :: init => init_VectorGrid
     procedure :: deinit => deinit_VectorGrid
     procedure :: clone => clone_VectorGrid
     procedure :: setField_uniform => setField_uniform_VectorGrid
     procedure :: setField_array => setField_array_VectorGrid
     procedure :: appendRows_Matrix => appendRows_Matrix_VectorGrid
     procedure :: appendRows_LinearSystem => &
          appendRows_LinearSystem_VectorGrid
     procedure :: getValue => getValue_VectorGrid
     procedure :: getValues => getValues_VectorGrid
     procedure :: hasCoupledComponents => &
          hasCoupledComponents_VectorGrid
     
  end type PointVectorMomentGridType

  
contains


  !-------------------------------------------------------------------
  !- PointScalarMomentArray methods
  !-------------------------------------------------------------------
  
  subroutine createPointScalarMomentArray( psva, scalarFlowVariable, &
       directedSpatialDerivative, polytopeArray, pointArrangement, log )
    class(PointScalarMomentFieldInterface), allocatable, intent(&
         inout) :: psva
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeArrayInterface), intent(in) :: polytopeArray
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createPointScalarMomentArray', log )

    ! allocate
    if ( allocated(psva) ) then
       call addEvent( FATAL, 'psva already allocated.', log )
    else
       allocate( PointScalarMomentArrayType :: psva, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &PointScalarMomentArrayType :: psva.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise
    select type ( psva )
       class is (PointScalarMomentArrayType)
       call psva%init( scalarFlowVariable, directedSpatialDerivative, &
            polytopeArray, pointArrangement, log )
    end select
    call endSub( log )
  end subroutine createPointScalarMomentArray


  subroutine init_ScalarArray( obj, scalarFlowVariable, &
       directedSpatialDerivative, polytopeArray, pointArrangement, log )
    class(PointScalarMomentArrayType), intent(inout) :: obj
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeArrayInterface), intent(in) :: polytopeArray
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: n1, n2, allocStat
    call beginSub( MOD_NAME, 'init_ScalarArray', log )
    
    n1 = polytopeArray%getNumPolytopes()
    n2 = pointArrangement%getNumPointsPerPolytope()

    ! use pointArrangement and polytopeArray to help set up the array
    ! of values
    allocate( obj%values(n1, n2), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &obj%values(n1, n2).  STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%values = 0._FLOAT
    
    ! initialise the supertype
    call obj%initPointScalarMomentField( scalarFlowVariable, &
         directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine init_ScalarArray


  subroutine deinit_ScalarArray( obj, log )
    class(PointScalarMomentArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_ScalarArray', log )

    deallocate( obj%values, stat=&
         deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating obj%&
         &values.  STAT='//int2str(&
         deallocStat), log )
    
    ! deinitialise supertype
    call obj%deinitPointScalarMomentField( log )
    
    call endSub( log )
  end subroutine deinit_ScalarArray

  
  subroutine clone_ScalarArray( obj, tgt, log )
    class(PointScalarMomentArrayType), intent(in) :: obj
    class(PointScalarMomentFieldInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_ScalarArray', log )

    allocate( PointScalarMomentArrayType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &PointScalarMomentArrayType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! copy local data
    select type ( tgt )
    type is (PointScalarMomentArrayType)

       allocate( tgt%values(size(obj%values, 1), size(obj%values, &
            2)), stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating tgt%values(&
            &size(obj%values, 1), size(obj%values, 2)).  STAT='//&
            int2str(stat), log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
       tgt%values = obj%values
    end select
    
    ! copy supertype data
    call tgt%initPointScalarMomentField( obj, log )

    call endSub( log )
  end subroutine clone_ScalarArray

  
  pure subroutine setField_uniform_ScalarArray( obj, src )
    class(PointScalarMomentArrayType), intent(out) :: obj
    real(FLOAT), intent(in) :: src
    obj%values = src
  end subroutine setField_uniform_ScalarArray
  
  
  pure subroutine setField_array_ScalarArray( obj, src )
    class(PointScalarMomentArrayType), intent(out) :: obj
    real(FLOAT), dimension(:), intent(in) :: src
    obj%values = reshape( src, [size(obj%values, 1), size(&
         obj%values, 2)] )
  end subroutine setField_array_ScalarArray

  
  subroutine appendRows_Matrix_ScalarArray( obj, scalarMatrix, &
       interiorStaticPointGroupPointer, log )
    class(PointScalarMomentArrayType), intent(in) :: obj
    class(ScalarMatrixInterface), intent(inout) :: scalarMatrix
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    class(DirectedSpatialDerivativeInterface), pointer :: dsd
    call beginSub( MOD_NAME, 'appendRows_Matrix_ScalarArray', log )

    call obj%getSpatialDerivative( dsd )
    call scalarMatrix%setRows( interiorStaticPointGroupPointer%&
         computeSubRows( dsd, &
         interiorStaticPointGroupPointer%getNumPoints() ), log ) 
    
    call endSub( log )
  end subroutine appendRows_Matrix_ScalarArray

  
!!$  subroutine appendElements_ScalarArray( obj, scalarRHSColumn, &
!!$       interiorStaticPointGroupPointer, log )
!!$    class(PointScalarMomentArrayType), intent(in) :: obj
!!$    class(ScalarRHSColumnInterface), intent(inout) :: &
!!$         scalarRHSColumn
!!$    class(InteriorStaticPointGroupPointerType), intent(in) :: &
!!$         interiorStaticPointGroupPointer
!!$    class(LogType), intent(inout), optional :: log
!!$    type(RHSElement_PSMAType), dimension(size(obj%values, 2)) :: rhse
!!$    integer :: i
!!$    call beginSub( MOD_NAME, 'appendElements_ScalarArray', log )
!!$
!!$    do i = 1, size(obj%values, 2)
!!$       call rhse(i)%init( obj, interiorStaticPointGroupPointer%&
!!$            getPolytopeIndex(), i )
!!$    end do
!!$    call scalarRHSColumn%setElements( rhse, log ) 
!!$
!!$    call endSub( log )
!!$  end subroutine appendElements_ScalarArray

  
  subroutine appendRows_LinearSystem_ScalarArray( obj, &
       scalarLinearSystem, interiorStaticPointGroupPointer, log )
    class(PointScalarMomentArrayType), intent(in) :: obj
    class(ScalarLinearSystemInterface), intent(inout) :: &
         scalarLinearSystem
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    class(DirectedSpatialDerivativeInterface), pointer :: dsd
!!$    class(RHSElementInterface), dimension(:), allocatable :: rhse
!!$    integer :: i, stat
    call beginSub( MOD_NAME, 'appendRows_LinearSystem_ScalarArray', &
         log )

    call obj%getSpatialDerivative( dsd )
    call scalarLinearSystem%setMatrixRows( &
         interiorStaticPointGroupPointer%computeSubRows( dsd, &
         interiorStaticPointGroupPointer%getNumPoints() ), log )
    
    call endSub( log )
  end subroutine appendRows_LinearSystem_ScalarArray

  
  elemental subroutine getValue_ScalarArray( obj, value, polytopeIndex, &
       pointIndex )
    class(PointScalarMomentArrayType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex, pointIndex
    real(FLOAT), intent(out) :: value
    value = obj%values( polytopeIndex, pointIndex )
  end subroutine getValue_ScalarArray

  
  pure subroutine getValues_ScalarArray( obj, values, polytopeIndex )
    class(PointScalarMomentArrayType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    real(FLOAT), dimension(:), intent(out) :: values
    values = obj%values( polytopeIndex, : )
  end subroutine getValues_ScalarArray

  
!!$  !-------------------------------------------------------------------
!!$  !- RHSElement_PSMA methods
!!$  !-------------------------------------------------------------------
!!$    
!!$  subroutine init_RHSElement_PSMA( obj, psma, polytopeIndex, &
!!$       pointIndex, log )
!!$    class(RHSElement_PSMAType), intent(inout) :: obj
!!$    class(PointScalarMomentFieldInterface), target, intent(in) :: psma
!!$    integer, intent(in) :: polytopeIndex, pointIndex
!!$    class(LogType), intent(inout), optional :: log
!!$    call beginSub( MOD_NAME, 'init_RHSElement_PSMA', log )
!!$
!!$    select type ( psma )
!!$    type is (PointScalarMomentArrayType)
!!$       obj%psma => psma
!!$    end select
!!$
!!$    obj%polytopeIndex = polytopeIndex
!!$    obj%pointIndex = pointIndex
!!$
!!$    call endSub( log )
!!$  end subroutine init_RHSElement_PSMA
!!$
!!$
!!$  subroutine deinit_RHSElement_PSMA( obj, log )
!!$    class(RHSElement_PSMAType), intent(inout) :: obj
!!$    class(LogType), intent(inout), optional :: log
!!$    call beginSub( MOD_NAME, 'deinit_RHSElement_PSMA', log )
!!$
!!$    nullify(obj%psma)
!!$    obj%polytopeIndex = 0
!!$    obj%pointIndex = 0
!!$
!!$    call endSub( log )
!!$  end subroutine deinit_RHSElement_PSMA
!!$
!!$
!!$  elemental function getValue_RHSElement_PSMA( obj ) result ( value )
!!$    class(RHSElement_PSMAType), intent(in) :: obj
!!$    real(FLOAT) :: value
!!$    
!!$    value = obj%psma%getValue( obj%polytopeIndex, obj%pointIndex )
!!$    
!!$  end function getValue_RHSElement_PSMA
!!$
!!$  
!!$  subroutine clone_RHSElement_PSMA( obj, tgt, log )
!!$    class(RHSElement_PSMAType), intent(in) :: obj
!!$    class(RHSElementInterface), allocatable, intent(out) :: tgt
!!$    class(LogType), intent(inout), optional :: log
!!$    integer :: stat
!!$    call beginSub( MOD_NAME, 'clone_MomentPointer', log )
!!$    
!!$    allocate( RHSElement_PSMAType :: tgt, stat=stat )
!!$    call addEvent( stat/=0, FATAL, 'Problem allocating &
!!$         &RHSElement_PSMAType :: tgt.  STAT='//int2str(stat), log )
!!$    if ( checkSub(FATAL, log) ) then
!!$       call endSub( log )
!!$       return
!!$    end if
!!$
!!$    select type ( tgt )
!!$    type is (RHSElement_PSMAType)
!!$       call tgt%init( obj%psma, obj%polytopeIndex, obj%pointIndex )
!!$    end select
!!$
!!$    call endSub( log )
!!$  end subroutine clone_RHSElement_PSMA

  
  !-------------------------------------------------------------------
  !- PointVectorMomentArray methods
  !-------------------------------------------------------------------
  
  subroutine createPointVectorMomentArray( pvva, vectorFlowVariable, &
       directedSpatialDerivative, polytopeArray, pointArrangement, log )
    class(PointVectorMomentFieldInterface), allocatable, intent(&
         inout) :: pvva
    class(VectorFlowVariableInterface), target, intent(in) :: &
         vectorFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeArrayInterface), intent(in) :: polytopeArray
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createPointVectorMomentArray', log )

    ! allocate
    if ( allocated(pvva) ) then
       call addEvent( FATAL, 'pvva already allocated.', log )
    else
       allocate( PointVectorMomentArrayType :: pvva, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &PointVectorMomentArrayType :: pvva.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise
    select type ( pvva )
       class is (PointVectorMomentArrayType)
       call pvva%init( vectorFlowVariable, directedSpatialDerivative, &
            polytopeArray, pointArrangement, log )
    end select
    call endSub( log )
  end subroutine createPointVectorMomentArray


  subroutine init_VectorArray( obj, vectorFlowVariable, &
       directedSpatialDerivative, polytopeArray, pointArrangement, log )
    class(PointVectorMomentArrayType), intent(inout) :: obj
    class(VectorFlowVariableInterface), target, intent(in) :: &
         vectorFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeArrayInterface), intent(in) :: polytopeArray
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: n1, n2, allocStat
    call beginSub( MOD_NAME, 'init_VectorArray', log )

    n1 = polytopeArray%getNumPolytopes()
    n2 = pointArrangement%getNumPointsPerPolytope()

    ! use pointArrangement and polytopeArray to help set up the array
    ! of values
    allocate( obj%values(n1, n2, NDIM), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &obj%values(n1, n2, NDIM).  STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%values = 0._FLOAT

    ! initialise the supertype
    call obj%initPointVectorMomentField( vectorFlowVariable, &
         directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine init_VectorArray


  subroutine deinit_VectorArray( obj, log )
    class(PointVectorMomentArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_VectorArray', log )
    
    deallocate( obj%values, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating &
         &obj%values.  STAT='//int2str(deallocStat), log )

    ! deinitialise supertype
    call obj%deinitPointVectorMomentField( log )
    
    call endSub( log )
  end subroutine deinit_VectorArray

  
  subroutine clone_VectorArray( obj, tgt, log )
    class(PointVectorMomentArrayType), intent(in) :: obj
    class(PointVectorMomentFieldInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_VectorArray', log )

    allocate( PointVectorMomentArrayType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &PointVectorMomentArrayType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! copy local data
    select type ( tgt )
    type is (PointVectorMomentArrayType)

       allocate( tgt%values(size(obj%values, 1), size(obj%values, &
            2), size(obj%values, 3)), stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating tgt%values(&
            &size(obj%values, 1), size(obj%values, 2), size(obj%&
            &values, 3)).  STAT='//int2str(stat), log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
       tgt%values = obj%values
    end select
    
    ! copy supertype data
    call tgt%initPointVectorMomentField( obj, log )

    call endSub( log )
  end subroutine clone_VectorArray

  
  pure subroutine setField_uniform_VectorArray( obj, src )
    class(PointVectorMomentArrayType), intent(out) :: obj
    type(RealVectorType), intent(in) :: src
    integer :: i, j
    forall ( i = 1:size(obj%values, 1), j = 1:size(obj%values, 2) )
       obj%values(i, j, :) = src%getValues()
    end forall
  end subroutine setField_uniform_VectorArray
  
  
  pure subroutine setField_array_VectorArray( obj, src )
    class(PointVectorMomentArrayType), intent(out) :: obj
    type(RealVectorType), dimension(:), intent(in) :: src
    integer :: i, j
    ! tk
    obj%values = 0._FLOAT
  end subroutine setField_array_VectorArray


  subroutine appendRows_Matrix_VectorArray( obj, vectorMatrix, &
       interiorStaticPointGroupPointer, log )
    class(PointVectorMomentArrayType), intent(in) :: obj
    class(VectorMatrixInterface), intent(inout) :: &
         vectorMatrix
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_Matrix_VectorArray', log )

    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine appendRows_Matrix_VectorArray
  
  
  subroutine appendRows_LinearSystem_VectorArray( obj, &
       vectorLinearSystem, interiorStaticPointGroupPointer, log )
    class(PointVectorMomentArrayType), intent(in) :: obj
    class(VectorLinearSystemInterface), intent(inout) :: &
         vectorLinearSystem
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_LinearSystem_VectorArray', &
         log )
    
    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine appendRows_LinearSystem_VectorArray

  
  elemental subroutine getValue_VectorArray( obj, value, polytopeIndex, &
       pointIndex, componentIndex )
    class(PointVectorMomentArrayType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: polytopeIndex, pointIndex
    integer, intent(in) :: componentIndex
    value = 0._FLOAT
  end subroutine getValue_VectorArray

  
  pure subroutine getValues_VectorArray( obj, values, polytopeIndex, &
       componentIndex )
    class(PointVectorMomentArrayType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: polytopeIndex
    integer, intent(in) :: componentIndex
    values = 0._FLOAT
  end subroutine getValues_VectorArray
  
  
  pure function hasCoupledComponents_VectorArray( obj )
    class(PointVectorMomentArrayType), intent(in) :: obj
    logical :: hasCoupledComponents_VectorArray
    hasCoupledComponents_VectorArray = .false.
  end function hasCoupledComponents_VectorArray
  
  
  !-------------------------------------------------------------------
  !- PointScalarMomentGrid methods
  !-------------------------------------------------------------------
  
  subroutine createPointScalarMomentGrid( spmg, scalarFlowVariable, &
       directedSpatialDerivative, polytopeGrid, pointArrangement, log )
    class(PointScalarMomentFieldInterface), allocatable, intent(&
         inout) :: spmg
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeGridInterface), intent(in) :: polytopeGrid
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createPointScalarMomentGrid', log )

    ! allocate
    if ( allocated(spmg) ) then
       call addEvent( FATAL, 'spmg already allocated.', log )
    else
       allocate( PointScalarMomentGridType :: spmg, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &PointScalarMomentGridType :: spmg.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise
    select type ( spmg )
       class is (PointScalarMomentGridType)
       call spmg%init( scalarFlowVariable, directedSpatialDerivative, &
            polytopeGrid, pointArrangement, log )
    end select
    call endSub( log )
  end subroutine createPointScalarMomentGrid

  
  subroutine init_ScalarGrid( obj, scalarFlowVariable, &
       directedSpatialDerivative, polytopeGrid, pointArrangement, log )
    class(PointScalarMomentGridType), intent(inout) :: obj
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeGridInterface), intent(in) :: polytopeGrid
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_ScalarGrid', log )

    ! record the grid parameters; they will be needed
    obj%gridParameters = polytopeGrid%getGridParameters()

    ! initialise multi-dimensional components
    call obj%nDParts%init( obj%gridParameters%size, &
         pointArrangement%getNumPointsPerPolytope(), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise the supertype
    call obj%initPointScalarMomentField( scalarFlowVariable, &
         directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine init_ScalarGrid


  subroutine deinit_ScalarGrid( obj, log )
    class(PointScalarMomentGridType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_ScalarGrid', log )
    
    call obj%nDParts%deinit( log )
    call obj%gridParameters%deinit()

    call obj%deinitPointScalarMomentField( log )
    
    call endSub( log )
  end subroutine deinit_ScalarGrid

  
  subroutine clone_ScalarGrid( obj, tgt, log )
    class(PointScalarMomentGridType), intent(in) :: obj
    class(PointScalarMomentFieldInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_ScalarGrid', log )

    allocate( PointScalarMomentGridType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &PointScalarMomentGridType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! copy local data
    select type ( tgt )
    type is (PointScalarMomentGridType)
       tgt%gridParameters = obj%gridParameters
       call tgt%nDParts%init( obj%nDParts, log )
    end select
    
    ! copy supertype data
    call tgt%initPointScalarMomentField( obj, log )

    call endSub( log )
  end subroutine clone_ScalarGrid

  
  pure subroutine setField_uniform_ScalarGrid( obj, src )
    class(PointScalarMomentGridType), intent(out) :: obj
    real(FLOAT), intent(in) :: src
    ! delegate
    call obj%nDParts%setField( src )
  end subroutine setField_uniform_ScalarGrid
  
  
  pure subroutine setField_array_ScalarGrid( obj, src )
    class(PointScalarMomentGridType), intent(out) :: obj
    real(FLOAT), dimension(:), intent(in) :: src
    ! delegate
    call obj%nDParts%setField( src )
  end subroutine setField_array_ScalarGrid


  subroutine appendRows_Matrix_ScalarGrid( obj, scalarMatrix, &
       interiorStaticPointGroupPointer, log )
    class(PointScalarMomentGridType), intent(in) :: obj
    class(ScalarMatrixInterface), intent(inout) :: &
         scalarMatrix
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_Matrix_ScalarGrid', log )

    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine appendRows_Matrix_ScalarGrid
  
  
  subroutine appendRows_LinearSystem_ScalarGrid( obj, &
       scalarLinearSystem, interiorStaticPointGroupPointer, log )
    class(PointScalarMomentGridType), intent(in) :: obj
    class(ScalarLinearSystemInterface), intent(inout) :: &
         scalarLinearSystem
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_LinearSystem_ScalarGrid', &
         log )
    
    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine appendRows_LinearSystem_ScalarGrid

  
  elemental subroutine getValue_ScalarGrid( obj, value, polytopeIndex, &
       pointIndex )
    class(PointScalarMomentGridType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: polytopeIndex, pointIndex
    
    ! delegate with gridParameters object
    call obj%nDParts%getValue( value, &
         obj%gridParameters, polytopeIndex, pointIndex )
  end subroutine getValue_ScalarGrid

  
  pure subroutine getValues_ScalarGrid( obj, values, polytopeIndex )
    class(PointScalarMomentGridType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: polytopeIndex
    
    ! delegate with gridParameters object
    call obj%nDParts%getValues( values, obj%gridParameters, polytopeIndex )
  end subroutine getValues_ScalarGrid
  
  
  !-------------------------------------------------------------------
  !- PointVectorMomentGrid methods
  !-------------------------------------------------------------------
  
  subroutine createPointVectorMomentGrid( vpmg, vectorFlowVariable, &
       directedSpatialDerivative, polytopeGrid, pointArrangement, log )
    class(PointVectorMomentFieldInterface), allocatable, intent(&
         inout) :: vpmg
    class(VectorFlowVariableInterface), target, intent(in) :: &
         vectorFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeGridInterface), intent(in) :: polytopeGrid
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createPointVectorMomentGrid', log )

    ! allocate
    if ( allocated(vpmg) ) then
       call addEvent( FATAL, 'vpmg already allocated.', log )
    else
       allocate( PointVectorMomentGridType :: vpmg, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &PointVectorMomentGridType :: vpmg.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise
    select type ( vpmg )
       class is (PointVectorMomentGridType)
       call vpmg%init( vectorFlowVariable, directedSpatialDerivative, &
            polytopeGrid, pointArrangement, log )
    end select
    call endSub( log )
  end subroutine createPointVectorMomentGrid

  
  subroutine init_VectorGrid( obj, vectorFlowVariable, &
       directedSpatialDerivative, polytopeGrid, pointArrangement, log )
    class(PointVectorMomentGridType), intent(inout) :: obj
    class(VectorFlowVariableInterface), target, intent(in) :: &
         vectorFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeGridInterface), intent(in) :: polytopeGrid
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_VectorGrid', log )

    ! record the grid parameters; they will be needed
    obj%gridParameters = polytopeGrid%getGridParameters()

    ! initialise multi-dimensional components
    call obj%nDParts%init( obj%gridParameters%size, &
         pointArrangement%getNumPointsPerPolytope(), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise the supertype
    call obj%initPointVectorMomentField( vectorFlowVariable, &
         directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine init_VectorGrid


  subroutine deinit_VectorGrid( obj, log )
    class(PointVectorMomentGridType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_VectorGrid', log )
    
    call obj%nDParts%deinit( log )
    call obj%gridParameters%deinit()

    call obj%deinitPointVectorMomentField( log )
    
    call endSub( log )
  end subroutine deinit_VectorGrid

  
  subroutine clone_VectorGrid( obj, tgt, log )
    class(PointVectorMomentGridType), intent(in) :: obj
    class(PointVectorMomentFieldInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_VectorGrid', log )

    allocate( PointVectorMomentGridType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &PointVectorMomentGridType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! copy local data
    select type ( tgt )
    type is (PointVectorMomentGridType)
       tgt%gridParameters = obj%gridParameters
       call tgt%nDParts%init( obj%nDParts, log )
    end select
    
    ! copy supertype data
    call tgt%initPointVectorMomentField( obj, log )

    call endSub( log )
  end subroutine clone_VectorGrid

  
  pure subroutine setField_uniform_VectorGrid( obj, src )
    class(PointVectorMomentGridType), intent(out) :: obj
    type(RealVectorType), intent(in) :: src
    ! delegate
    call obj%nDParts%setField( src )
  end subroutine setField_uniform_VectorGrid
  
  
  pure subroutine setField_array_VectorGrid( obj, src )
    class(PointVectorMomentGridType), intent(out) :: obj
    type(RealVectorType), dimension(:), intent(in) :: src
    ! delegate
    call obj%nDParts%setField( src )
  end subroutine setField_array_VectorGrid


  subroutine appendRows_Matrix_VectorGrid( obj, vectorMatrix, &
       interiorStaticPointGroupPointer, log )
    class(PointVectorMomentGridType), intent(in) :: obj
    class(VectorMatrixInterface), intent(inout) :: &
         vectorMatrix
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_Matrix_VectorGrid', log )

    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine appendRows_Matrix_VectorGrid
  
  
  subroutine appendRows_LinearSystem_VectorGrid( obj, &
       vectorLinearSystem, interiorStaticPointGroupPointer, log )
    class(PointVectorMomentGridType), intent(in) :: obj
    class(VectorLinearSystemInterface), intent(inout) :: &
         vectorLinearSystem
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendRows_LinearSystem_VectorGrid', &
         log )
    
    call addEvent( FATAL, 'Not implemented yet.', log )

    call endSub( log )
  end subroutine appendRows_LinearSystem_VectorGrid

  
  elemental subroutine getValue_VectorGrid( obj, value, polytopeIndex, &
       pointIndex, componentIndex )
    class(PointVectorMomentGridType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    integer, intent(in) :: polytopeIndex, pointIndex
    integer, intent(in) :: componentIndex
    type(RealVectorType) :: a
    
    ! delegate with gridParameters object
    call obj%nDParts%getValue( a, obj%gridParameters, polytopeIndex, &
         pointIndex )
    value = a%getValue(componentIndex)
  end subroutine getValue_VectorGrid

  
  pure subroutine getValues_VectorGrid( obj, values, polytopeIndex, &
       componentIndex )
    class(PointVectorMomentGridType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: polytopeIndex
    integer, intent(in) :: componentIndex
    type(RealVectorType), dimension(size(values)) :: a
    integer :: i
    
    ! delegate with gridParameters object
    call obj%nDParts%getValues( a, obj%gridParameters, polytopeIndex )
    forall ( i = 1:size(values) )
       values(i) = a(i)%getValue(componentIndex)
    end forall
  end subroutine getValues_VectorGrid
  
  
  pure function hasCoupledComponents_VectorGrid( obj )
    class(PointVectorMomentGridType), intent(in) :: obj
    logical :: hasCoupledComponents_VectorGrid
    hasCoupledComponents_VectorGrid = .false.
  end function hasCoupledComponents_VectorGrid
  
  
end module PointMomentFieldsModule
