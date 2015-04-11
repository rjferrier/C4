module IntAveMomentFieldsModule
  
  use LogModule
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FlowFieldModule

  ! placeholder for a module that points to any one of the variants in
  ! the folder of the same name.  The module should not be created
  ! within the source tree as it may interfere with automated tests
  ! that define their own variants.
  use IntAveMomentFields_DomainDim
  
  implicit none
  private
  public :: createIntAveScalarMomentArray, &
       createIntAveVectorMomentArray, createIntAveScalarMomentGrid, &
       createIntAveVectorMomentGrid
  
  character(*), parameter :: MOD_NAME = 'IntAveMomentFieldsModule'
  
  
  !-------------------------------------------------------------------
  !- IntAveScalarMomentArray and IntAveVectorMomentArray
  !-------------------------------------------------------------------
  
  type, extends(IntAveScalarMomentFieldInterface) :: IntAveScalarMomentArrayType
     private
     ! dimensions correspond to polytopes
     real(FLOAT), dimension(:), allocatable :: values
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
  end type IntAveScalarMomentArrayType

  
  type, extends(IntAveVectorMomentFieldInterface) :: IntAveVectorMomentArrayType
     private
     ! dimensions correspond to (1) polytopes and (2) vector components
     real(FLOAT), dimension(:, :), allocatable :: values
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
     procedure :: getMatrixRowWidth => getMatrixRowWidth_VectorArray
     procedure :: hasCoupledComponents => hasCoupledComponents_VectorArray
  end type IntAveVectorMomentArrayType
  

  !-------------------------------------------------------------------
  !- IntAveScalarMomentGrid and IntAveVectorMomentGrid 
  !-------------------------------------------------------------------

  type, extends(IntAveScalarMomentFieldInterface), public :: IntAveScalarMomentGridType
     private
     type(GridParametersType) :: gridParameters
     type(IntAveScalarMomentGrid_NDPartsType) :: nDParts
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

  end type IntAveScalarMomentGridType


  type, extends(IntAveVectorMomentFieldInterface), public :: IntAveVectorMomentGridType
     private
     type(GridParametersType) :: gridParameters
     type(IntAveVectorMomentGrid_NDPartsType) :: nDParts
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
     procedure :: hasCoupledComponents => hasCoupledComponents_VectorGrid
  end type IntAveVectorMomentGridType
  
  
contains
  
  !-------------------------------------------------------------------
  !- IntAveScalarMomentArray methods
  !-------------------------------------------------------------------
  
  subroutine createIntAveScalarMomentArray( iasma, scalarFlowVariable, &
       directedSpatialDerivative, polytopeArray, pointArrangement, log )
    class(IntAveScalarMomentFieldInterface), allocatable, intent(&
         inout) :: iasma
    class(ScalarFlowVariableInterface), target, intent(in) :: scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeArrayInterface), intent(in) :: polytopeArray
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createIntAveScalarMomentArray', log )

    ! allocate
    if ( allocated(iasma) ) then
       call addEvent( FATAL, 'iasma already allocated.', log )
    else
       allocate( IntAveScalarMomentArrayType :: iasma, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &IntAveScalarMomentArrayType :: iasma.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise
    select type ( iasma )
       class is (IntAveScalarMomentArrayType)
       call iasma%init( scalarFlowVariable, directedSpatialDerivative, &
            polytopeArray, pointArrangement, log )
    end select
    call endSub( log )
  end subroutine createIntAveScalarMomentArray


  subroutine init_ScalarArray( obj, scalarFlowVariable, &
       directedSpatialDerivative, polytopeArray, pointArrangement, log )
    class(IntAveScalarMomentArrayType), intent(inout) :: obj
    class(ScalarFlowVariableInterface), target, intent(in) :: scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeArrayInterface), intent(in) :: polytopeArray
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: n, allocStat
    call beginSub( MOD_NAME, 'init_ScalarArray', log )
    
    n = polytopeArray%getNumPolytopes()

    ! use pointArrangement and polytopeArray to help set up the array
    ! of values
    allocate( obj%values(n), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &obj%values(n).  STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%values = 0._FLOAT
    
    ! initialise the supertype
    call obj%initIntAveScalarMomentField( scalarFlowVariable, &
         directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine init_ScalarArray


  subroutine deinit_ScalarArray( obj, log )
    class(IntAveScalarMomentArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_ScalarArray', log )
    
    deallocate( obj%values, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating &
         &obj%values.  STAT='//int2str(deallocStat), log )

    ! deinitialise supertype
    call obj%deinitIntAveScalarMomentField( log )
    
    call endSub( log )
  end subroutine deinit_ScalarArray

  
  subroutine clone_ScalarArray( obj, tgt, log )
    class(IntAveScalarMomentArrayType), intent(in) :: obj
    class(IntAveScalarMomentFieldInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_ScalarArray', log )

    allocate( IntAveScalarMomentArrayType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &IntAveScalarMomentArrayType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! copy local data
    select type ( tgt )
    type is (IntAveScalarMomentArrayType)

       allocate( tgt%values(size(obj%values, 1)), stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating tgt%values(&
            &size(obj%values, 1)).  STAT='//int2str(stat), log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
       tgt%values = obj%values
    end select
    
    ! copy supertype data
    call tgt%initIntAveScalarMomentField( obj, log )

    call endSub( log )
  end subroutine clone_ScalarArray

  
  pure subroutine setField_uniform_ScalarArray( obj, src )
    class(IntAveScalarMomentArrayType), intent(out) :: obj
    real(FLOAT), intent(in) :: src
    obj%values = src
  end subroutine setField_uniform_ScalarArray
  
  
  pure subroutine setField_array_ScalarArray( obj, src )
    class(IntAveScalarMomentArrayType), intent(out) :: obj
    real(FLOAT), dimension(:), intent(in) :: src
    obj%values = src
  end subroutine setField_array_ScalarArray
  
  
  subroutine appendRows_Matrix_ScalarArray( obj, scalarMatrix, &
       interiorStaticPointGroupPointer, log )
    class(IntAveScalarMomentArrayType), intent(in) :: obj
    class(ScalarMatrixInterface), intent(inout) :: &
         scalarMatrix
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_Matrix_ScalarArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendRows_Matrix_ScalarArray
  
  
  subroutine appendRows_LinearSystem_ScalarArray( obj, &
       scalarLinearSystem, interiorStaticPointGroupPointer, log )
    class(IntAveScalarMomentArrayType), intent(in) :: obj
    class(ScalarLinearSystemInterface), intent(inout) :: &
         scalarLinearSystem
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_LinearSystem_ScalarArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendRows_LinearSystem_ScalarArray
  
  
  elemental function getValue_ScalarArray( obj, polytopeIndex )
    class(IntAveScalarMomentArrayType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    real(FLOAT) :: getValue_ScalarArray
    getValue_ScalarArray = obj%values( polytopeIndex )
  end function getValue_ScalarArray
  
  
  pure subroutine getValues_ScalarArray( obj, values, polytopeIndex )
    class(IntAveScalarMomentArrayType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: polytopeIndex
    values(:) = obj%values( polytopeIndex )
  end subroutine getValues_ScalarArray

  
  !-------------------------------------------------------------------
  !- IntAveVectorMomentArray methods
  !-------------------------------------------------------------------
  
  subroutine createIntAveVectorMomentArray( iavmf, vectorFlowVariable, &
       directedSpatialDerivative, polytopeArray, pointArrangement, log )
    class(IntAveVectorMomentFieldInterface), allocatable, intent(&
         inout) :: iavmf
    class(VectorFlowVariableInterface), target, intent(in) :: vectorFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(inout) :: &
         directedSpatialDerivative
    class(PolytopeArrayInterface), intent(in) :: polytopeArray
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createIntAveVectorMomentArray', log )

    ! allocate
    if ( allocated(iavmf) ) then
       call addEvent( FATAL, 'iavmf already allocated.', log )
    else
       allocate( IntAveVectorMomentArrayType :: iavmf, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &IntAveVectorMomentArrayType :: iavmf.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise
    select type ( iavmf )
       class is (IntAveVectorMomentArrayType)
       call iavmf%init( vectorFlowVariable, directedSpatialDerivative, &
            polytopeArray, pointArrangement, log )
    end select
    call endSub( log )
  end subroutine createIntAveVectorMomentArray


  subroutine init_VectorArray( obj, vectorFlowVariable, &
       directedSpatialDerivative, polytopeArray, pointArrangement, log )
    class(IntAveVectorMomentArrayType), intent(inout) :: obj
    class(VectorFlowVariableInterface), target, intent(in) :: vectorFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, &
         intent(inout) :: directedSpatialDerivative
    class(PolytopeArrayInterface), intent(in) :: polytopeArray
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: n, n2, stat
    call beginSub( MOD_NAME, 'init_VectorArray', log )

    n = polytopeArray%getNumPolytopes()

    ! use polytopeArray to help set up the array of values
    allocate( obj%values(n, NDIM), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &obj%values(n, NDIM).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%values = 0._FLOAT

    ! initialise the supertype
    call obj%initIntAveVectorMomentField( vectorFlowVariable, &
         directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine init_VectorArray


  subroutine deinit_VectorArray( obj, log )
    class(IntAveVectorMomentArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinit_VectorArray', log )
    
    deallocate( obj%values, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating &
         &obj%values.  STAT='//int2str(stat), log )

    ! deinitialise supertype
    call obj%deinitIntAveVectorMomentField( log )
    
    call endSub( log )
  end subroutine deinit_VectorArray


  subroutine clone_VectorArray( obj, tgt, log )
    class(IntAveVectorMomentArrayType), intent(in) :: obj
    class(IntAveVectorMomentFieldInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_VectorArray', log )

    allocate( IntAveVectorMomentArrayType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &IntAveVectorMomentArrayType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! copy local data
    select type ( tgt )
    type is (IntAveVectorMomentArrayType)

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
    call tgt%initIntAveVectorMomentField( obj, log )

    call endSub( log )
  end subroutine clone_VectorArray

  
  pure subroutine setField_uniform_VectorArray( obj, src )
    class(IntAveVectorMomentArrayType), intent(out) :: obj
    type(RealVectorType), intent(in) :: src
    integer :: i
    forall ( i = 1:size(obj%values, 1) )
       obj%values(i, :) = src%getValues()
    end forall
  end subroutine setField_uniform_VectorArray
  
  
  pure subroutine setField_array_VectorArray( obj, src )
    class(IntAveVectorMomentArrayType), intent(out) :: obj
    type(RealVectorType), dimension(:), intent(in) :: src
    integer :: i, j
    ! tk
    obj%values = 0._FLOAT
  end subroutine setField_array_VectorArray
  
  
  subroutine appendRows_Matrix_VectorArray( obj, vectorMatrix, &
       interiorStaticPointGroupPointer, log )
    class(IntAveVectorMomentArrayType), intent(in) :: obj
    class(VectorMatrixInterface), intent(inout) :: &
         vectorMatrix
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_Matrix_VectorArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendRows_Matrix_VectorArray
  
  
  subroutine appendRows_LinearSystem_VectorArray( obj, &
       vectorLinearSystem, interiorStaticPointGroupPointer, log )
    class(IntAveVectorMomentArrayType), intent(in) :: obj
    class(VectorLinearSystemInterface), intent(inout) :: &
         vectorLinearSystem
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_LinearSystem_VectorArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendRows_LinearSystem_VectorArray
  
  
  elemental function getValue_VectorArray( obj, polytopeIndex, &
       componentIndex )
    class(IntAveVectorMomentArrayType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    integer, intent(in) :: componentIndex
    real(FLOAT) :: getValue_VectorArray
    getValue_VectorArray = 0._FLOAT
  end function getValue_VectorArray
  
  
  pure subroutine getValues_VectorArray( obj, values, &
       polytopeIndex, componentIndex )
    class(IntAveVectorMomentArrayType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: polytopeIndex
    integer, intent(in) :: componentIndex
    values = 0._FLOAT
  end subroutine getValues_VectorArray

  
  pure function getMatrixRowWidth_VectorArray( obj )
    class(IntAveVectorMomentArrayType), intent(in) :: obj
    logical :: getMatrixRowWidth_VectorArray
    getMatrixRowWidth_VectorArray = .false. 
  end function getMatrixRowWidth_VectorArray

  
  pure function hasCoupledComponents_VectorArray( obj )
    class(IntAveVectorMomentArrayType), intent(in) :: obj
    logical :: hasCoupledComponents_VectorArray
    hasCoupledComponents_VectorArray = .false.
  end function hasCoupledComponents_VectorArray


  !-------------------------------------------------------------------
  !- IntAveScalarMomentGrid methods
  !-------------------------------------------------------------------
  
  subroutine createIntAveScalarMomentGrid( iasmf, scalarFlowVariable, &
       directedSpatialDerivative, polytopeGrid, pointArrangement, log )
    class(IntAveScalarMomentFieldInterface), allocatable, intent(&
         inout) :: iasmf
    class(ScalarFlowVariableInterface), target, intent(in) :: scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeGridInterface), intent(in) :: polytopeGrid
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createIntAveScalarMomentGrid', log )

    ! allocate
    if ( allocated(iasmf) ) then
       call addEvent( FATAL, 'iasmf already allocated.', log )
    else
       allocate( IntAveScalarMomentGridType :: iasmf, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &IntAveScalarMomentGridType :: iasmf.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise
    select type ( iasmf )
       class is (IntAveScalarMomentGridType)
       call iasmf%init( scalarFlowVariable, directedSpatialDerivative, &
            polytopeGrid, pointArrangement, log )
    end select
    call endSub( log )
  end subroutine createIntAveScalarMomentGrid

  
  subroutine init_ScalarGrid( obj, scalarFlowVariable, &
       directedSpatialDerivative, polytopeGrid, pointArrangement, log )
    class(IntAveScalarMomentGridType), intent(inout) :: obj
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
    call obj%initIntAveScalarMomentField( scalarFlowVariable, &
         directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine init_ScalarGrid


  subroutine deinit_ScalarGrid( obj, log )
    class(IntAveScalarMomentGridType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_ScalarGrid', log )

    call obj%deinitIntAveScalarMomentField( log )
    call obj%nDParts%deinit( log )
    call obj%gridParameters%deinit()

    call endSub( log )
  end subroutine deinit_ScalarGrid

  
  subroutine clone_ScalarGrid( obj, tgt, log )
    class(IntAveScalarMomentGridType), intent(in) :: obj
    class(IntAveScalarMomentFieldInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_ScalarGrid', log )

    allocate( IntAveScalarMomentGridType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &IntAveScalarMomentGridType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! copy local data
    select type ( tgt )
    type is (IntAveScalarMomentGridType)
       tgt%gridParameters = obj%gridParameters
       call tgt%nDParts%init( obj%nDParts, log )
    end select
    
    ! copy supertype data
    call tgt%initIntAveScalarMomentField( obj, log )

    call endSub( log )
  end subroutine clone_ScalarGrid

  
  pure subroutine setField_uniform_ScalarGrid( obj, src )
    class(IntAveScalarMomentGridType), intent(out) :: obj
    real(FLOAT), intent(in) :: src
    ! delegate
    call obj%nDParts%setField( src )
  end subroutine setField_uniform_ScalarGrid
  
  
  pure subroutine setField_array_ScalarGrid( obj, src )
    class(IntAveScalarMomentGridType), intent(out) :: obj
    real(FLOAT), dimension(:), intent(in) :: src
    ! delegate
    call obj%nDParts%setField( src )
  end subroutine setField_array_ScalarGrid


  subroutine appendRows_Matrix_ScalarGrid( obj, scalarMatrix, &
       interiorStaticPointGroupPointer, log )
    class(IntAveScalarMomentGridType), intent(in) :: obj
    class(ScalarMatrixInterface), intent(inout) :: &
         scalarMatrix
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_Matrix_ScalarGrid', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendRows_Matrix_ScalarGrid


  subroutine appendRows_LinearSystem_ScalarGrid( obj, &
       scalarLinearSystem, interiorStaticPointGroupPointer, log )
    class(IntAveScalarMomentGridType), intent(in) :: obj
    class(ScalarLinearSystemInterface), intent(inout) :: &
         scalarLinearSystem
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_LinearSystem_ScalarGrid', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendRows_LinearSystem_ScalarGrid
  
  
  elemental function getValue_ScalarGrid( obj, polytopeIndex )
    class(IntAveScalarMomentGridType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    real(FLOAT) :: getValue_ScalarGrid
    
    ! delegate with gridParameters object
    getValue_ScalarGrid = obj%nDParts%getValue( &
         obj%gridParameters, polytopeIndex )
  end function getValue_ScalarGrid
  
  
  pure subroutine getValues_ScalarGrid( obj, values, polytopeIndex )
    class(IntAveScalarMomentGridType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: polytopeIndex
    
    ! delegate with gridParameters object
    call obj%nDParts%getValues( values, obj%gridParameters, &
         polytopeIndex )
  end subroutine getValues_ScalarGrid


  !-------------------------------------------------------------------
  !- IntAveVectorMomentGrid methods
  !-------------------------------------------------------------------
  
  subroutine createIntAveVectorMomentGrid( iasmf, vectorFlowVariable, &
       directedSpatialDerivative, polytopeGrid, pointArrangement, log )
    class(IntAveVectorMomentFieldInterface), allocatable, intent(&
         inout) :: iasmf
    class(VectorFlowVariableInterface), target, intent(in) :: vectorFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(PolytopeGridInterface), intent(in) :: polytopeGrid
    class(PointArrangementInterface), intent(in) :: pointArrangement
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createIntAveVectorMomentGrid', log )

    ! allocate
    if ( allocated(iasmf) ) then
       call addEvent( FATAL, 'iasmf already allocated.', log )
    else
       allocate( IntAveVectorMomentGridType :: iasmf, stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &IntAveVectorMomentGridType :: iasmf.  STAT='&
	    //int2str(stat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! initialise
    select type ( iasmf )
       class is (IntAveVectorMomentGridType)
       call iasmf%init( vectorFlowVariable, directedSpatialDerivative, &
            polytopeGrid, pointArrangement, log )
    end select
    call endSub( log )
  end subroutine createIntAveVectorMomentGrid

  
  subroutine init_VectorGrid( obj, vectorFlowVariable, &
       directedSpatialDerivative, polytopeGrid, pointArrangement, log )
    class(IntAveVectorMomentGridType), intent(inout) :: obj
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
    call obj%initIntAveVectorMomentField( vectorFlowVariable, &
         directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine init_VectorGrid


  subroutine deinit_VectorGrid( obj, log )
    class(IntAveVectorMomentGridType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_VectorGrid', log )

    call obj%deinitIntAveVectorMomentField( log )
    call obj%nDParts%deinit( log )
    call obj%gridParameters%deinit()

    call endSub( log )
  end subroutine deinit_VectorGrid

  
  subroutine clone_VectorGrid( obj, tgt, log )
    class(IntAveVectorMomentGridType), intent(in) :: obj
    class(IntAveVectorMomentFieldInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_VectorGrid', log )

    allocate( IntAveVectorMomentGridType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &IntAveVectorMomentGridType :: tgt.  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! copy local data
    select type ( tgt )
    type is (IntAveVectorMomentGridType)
       tgt%gridParameters = obj%gridParameters
       call tgt%nDParts%init( obj%nDParts, log )
    end select
    
    ! copy supertype data
    call tgt%initIntAveVectorMomentField( obj, log )

    call endSub( log )
  end subroutine clone_VectorGrid

  
  pure subroutine setField_uniform_VectorGrid( obj, src )
    class(IntAveVectorMomentGridType), intent(out) :: obj
    type(RealVectorType), intent(in) :: src
    ! delegate
    call obj%nDParts%setField( src )
  end subroutine setField_uniform_VectorGrid
  
  
  pure subroutine setField_array_VectorGrid( obj, src )
    class(IntAveVectorMomentGridType), intent(out) :: obj
    type(RealVectorType), dimension(:), intent(in) :: src
    ! delegate
    call obj%nDParts%setField( src )
  end subroutine setField_array_VectorGrid


  subroutine appendRows_Matrix_VectorGrid( obj, vectorMatrix, &
       interiorStaticPointGroupPointer, log )
    class(IntAveVectorMomentGridType), intent(in) :: obj
    class(VectorMatrixInterface), intent(inout) :: &
         vectorMatrix
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_Matrix_VectorGrid', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendRows_Matrix_VectorGrid


  subroutine appendRows_LinearSystem_VectorGrid( obj, &
       vectorLinearSystem, interiorStaticPointGroupPointer, log )
    class(IntAveVectorMomentGridType), intent(in) :: obj
    class(VectorLinearSystemInterface), intent(inout) :: &
         vectorLinearSystem
    class(InteriorStaticPointGroupPointerType), intent(in) :: &
         interiorStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendRows_LinearSystem_VectorGrid', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    call endSub( log )
  end subroutine appendRows_LinearSystem_VectorGrid
  
  
  elemental function getValue_VectorGrid( obj, polytopeIndex, &
       componentIndex )
    class(IntAveVectorMomentGridType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex, componentIndex
    real(FLOAT) :: getValue_VectorGrid
    type(RealVectorType) :: a
    
    ! delegate with gridParameters object
    a = obj%nDParts%getValue( obj%gridParameters, polytopeIndex )
    getValue_VectorGrid = a%getValue( componentIndex )
  end function getValue_VectorGrid
  
  
  pure subroutine getValues_VectorGrid( obj, values, polytopeIndex, &
       componentIndex )
    class(IntAveVectorMomentGridType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    integer, intent(in) :: polytopeIndex, componentIndex
    type(RealVectorType), dimension(size(values)) :: a
    integer :: i
    
    ! delegate with gridParameters object
    call obj%nDParts%getValues( a, obj%gridParameters, polytopeIndex )
    forall ( i = 1:size(values) )
       values(i) = a(i)%getValue( componentIndex )
    end forall
  end subroutine getValues_VectorGrid

  
  pure function hasCoupledComponents_VectorGrid( obj )
    class(IntAveVectorMomentGridType), intent(in) :: obj
    logical :: hasCoupledComponents_VectorGrid
    hasCoupledComponents_VectorGrid = .false. 
  end function hasCoupledComponents_VectorGrid
  
end module IntAveMomentFieldsModule
