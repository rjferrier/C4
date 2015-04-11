HEADER()
module FaceCollectionsModule

  USE_MACROS({IntegrableElements})
  use Global
  use LogModule
  use FiniteVolumeModule
  use FlowFieldModule

  implicit none
  private 
  public :: attachInteriorFaceArray, attachInteriorFaceGrid, &
       attachInteriorFaceComplex
  
  character(*), parameter :: MOD_NAME = &
       'FacePolytopeCollectionsModule'

  
  !-------------------------------------------------------------------
  !- main types
  !-------------------------------------------------------------------

  
  EXPAND({TYPEDEF_FACECOLLECTION_CONCRETE({Array}, {Interior}, {
     type(RealVectorType), dimension(:), allocatable :: normals }, {
     procedure :: computeFaceNormal => &
          computeFaceNormal_InteriorFaceArray})})
     
  EXPAND({TYPEDEF_FACECOLLECTION_CONCRETE({Grid}, {Interior})})
    

  type, extends(FaceComplexInterface), public :: InteriorFaceComplexType
     private
     ! we define our complex as mapping to an element in some collection.
     ! This allows us to defer certain properties of the complex to a
     ! large, possibly structured collection of otherwise primitive
     ! elements.  The following pointer object represents the placeholder
     ! element.
     type(FacePointerType) :: parentFace
   contains
     procedure :: init => init_InteriorFaceComplex
     procedure :: deinit => deinit_InteriorFaceComplex
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_InteriorFaceComplex
     procedure :: appendScalarMoments => &
          appendScalarMoments_InteriorFaceComplex
  end type InteriorFaceComplexType
  
  
  
!!$  !-------------------------------------------------------------------
!!$  !- creators/destructors
!!$  !-------------------------------------------------------------------
!!$  
!!$  
!!$  type, public :: FaceArrayFactoryType
!!$   contains
!!$     procedure :: create => createFaceArray
!!$     procedure, nopass :: destroy => destroyFaceArray
!!$  end type FaceArrayFactoryType
!!$
!!$  
!!$  type, public :: FaceGridFactoryType
!!$   contains
!!$     procedure :: create => createFaceGrid
!!$     procedure, nopass :: destroy => destroyFaceGrid
!!$  end type FaceGridFactoryType
!!$
!!$  
!!$  type, public :: FaceComplexFactoryType
!!$   contains
!!$     procedure :: create => createFaceComplex
!!$     procedure, nopass :: destroy => destroyFaceComplex
!!$  end type FaceComplexFactoryType
  

  !-------------------------------------------------------------------
  !- abstract interfaces
  !-------------------------------------------------------------------
  
  
contains  
  

  !-------------------------------------------------------------------
  !- InteriorFaceArray methods
  !-------------------------------------------------------------------
  
  m4_divert(-1)
  m4_define({ARGLINE}, {&
       interiorGaussianPointCollection, })
  m4_define({ARGLIST},
   {class(InteriorStaticPointCollectionInterface), allocatable, &
        intent(inout) :: interiorGaussianPointCollection})
  m4_divert(0)

  EXPAND({PROCEDURE_ATTACH({InteriorFaceArray}, {FaceArray}, {PolytopeArray},
  {ARGLINE()}, {ARGLIST()})})
  
  subroutine init_InteriorFaceArray( obj, polytopeArray, ARGLINE()log )
    class(InteriorFaceArrayType), intent(out) :: obj
    class(PolytopeArrayInterface), intent(in) :: polytopeArray
    ARGLIST()
    class(LogType), intent(inout), optional :: log
    integer :: stat, i
    call beginSub( MOD_NAME, 'init_InteriorFaceArray', log )

    ! init extension superclass
    call obj%initExtension( polytopeArray, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! init components
    EXPAND({INJECT({interiorGaussianPointCollection})})
        
    ! create array of normals
    allocate( obj%normals(polytopeArray%getNumPolytopes()), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &elements of obj%normals.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

    ! loop over elements
    do i = 1, polytopeArray%getNumPolytopes()
       ! compute and store normal
       call obj%computeFaceNormal( i, obj%normals(i), log )
    end do

    call endSub( log )
  end subroutine init_InteriorFaceArray

  
  subroutine deinit_InteriorFaceArray( obj, log )
    class(InteriorFaceArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinit_InteriorFaceArray', log )

    ! deinit components
    call destroy( obj%interiorGaussianPointCollection, log )
    
    ! destroy array of normals
    deallocate( obj%normals, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem deallocating &
         &elements of obj%normals.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if
    
    ! deinit extension superclass
    call obj%deinitExtension( log )
    call endSub( log )
  end subroutine deinit_InteriorFaceArray

  
  subroutine appendVelocityMoments_InteriorFaceArray( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class(InteriorFaceArrayType), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_InteriorFaceArray', log )

    ! unstructured primitive faces do not hold flow information in this
    ! implementation.  Do nothing.
    
    call endSub( log )
  end subroutine appendVelocityMoments_InteriorFaceArray

  
  subroutine appendScalarMoments_InteriorFaceArray( obj, scalarMomentGroupList, &
       polytopeIndex, log )
    class(InteriorFaceArrayType), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendScalarMoments_InteriorFaceArray', log )

    ! unstructured primitive faces do not hold flow information in this
    ! implementation.  Do nothing.
    
    call endSub( log )
  end subroutine appendScalarMoments_InteriorFaceArray

  
  pure subroutine computeFaceNormal_InteriorFaceArray( obj, index, vector, &
       log )
    class(InteriorFaceArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out) :: vector
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerSetType) :: pps
    type(PolytopePointerType), pointer :: pp
    type(RealVectorType) :: coords
    type(RealTensorType) :: coordsMatrix, workMatrix
    integer :: i, n
    logical, parameter :: DEBUG_MODE = .false.
    class(PolytopeArrayInterface), pointer :: pa
    real(FLOAT) :: dummy
    call beginSub( MOD_NAME, 'computeFaceNormal_InteriorFaceArray', log )

    call obj%getArrayOwner( pa )
    
    ! gather all bounding polytopes
    call pa%collectSubPolytopes( index, pps, log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if
    
    ! filter all points in the set to give us bounding vertices
    call pps%filter( 0, log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if
    
    ! get the number of points and (optional) check
    ! that they make a square matrix
    n = pps%size()
    if ( DEBUG_MODE ) then
       ! matrix must be square
       call addEvent( n /= NDIM, FATAL, 'Number of bounding &
            &vertices ('//str(n)//') is not equal to the number of &
            &dimensions ('//str(NDIM)//').  Exiting procedure.', &
            log )
       if ( n /= NDIM ) then
          call endSub( log )
          return
       end if
    end if
    
    ! loop over the set adding point coordinates to the coordMatrix.
    ! Note that the coordinates are arranged in rows in the matrix.
    do i = 1, n
       call pps%find( pp, i )
       ! the point's coordinates are accessed through
       ! computePolytopeCentroid again.
       call pp%getCentroid( coords )
       call coordsMatrix%setRow( i, coords )
    end do

    ! let x, y, .. be the column vectors [x1; x2; ..], [y1; y2; ..],
    ! etc.  The face vector components can be computed by det(1, y,
    ! ..), det(x, 1, ..), etc., respectively.
    workMatrix = coordsMatrix
    do i = 1, n
       call workMatrix%setColumn( i, 1._FLOAT )
       call vector%setElement( i, workMatrix%det() )
       call workMatrix%setColumn( i, coordsMatrix )
    end do

    ! finally, normalise to a unit vector 
    call vector%normalise()
    
    call pps%deinit( log ) 
    call endSub( log )
  end subroutine computeFaceNormal_InteriorFaceArray
  
  
  pure subroutine getFaceNormal_InteriorFaceArray( obj, index, vector, &
       log )
    class(InteriorFaceArrayType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out) :: vector
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getInteriorFaceNormal_InteriorFaceArray', &
         log )
    
    ! get the vector components previously computed
    vector = obj%normals(index)
    
    call endSub( log )
  end subroutine getFaceNormal_InteriorFaceArray

  
  elemental subroutine findSide_InteriorFaceArray( obj, index, coords, &
       side, log )
    class(InteriorFaceArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(in) :: coords
    logical, intent(out) :: side
    class(LogType), intent(inout), optional :: log
    class(PolytopeArrayInterface), pointer :: pa
    type(RealVectorType) :: faceNormal, faceCentroid
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, 'findSide_InteriorFaceArray', log )
    
    call obj%getArrayOwner( pa )
    call pa%getCentroid( index, faceCentroid, log )
    call addEvent( DEBUG_MODE, ADVICE, &
         'testVector = '//str(coords - faceCentroid), log )
    
    call obj%getFaceNormal( index, faceNormal, log )
    call addEvent( DEBUG_MODE, ADVICE, &
         'faceNormal =   '//str(faceNormal), log )
    
    ! basically just a dot product test
    side = faceNormal%dotProduct( coords - faceCentroid ) > 0
    call addEvent( DEBUG_MODE, ADVICE, 'result = '//&
         str(side), log )

    call endSub( log )
  end subroutine findSide_InteriorFaceArray
  
!!$
!!$  elemental subroutine findSide_multi_InteriorFaceArray( obj, index, coords, side, &
!!$       log )
!!$    class(InteriorFaceArrayType), intent(inout) :: obj
!!$    integer, intent(in) :: index
!!$    type(RealVectorType), dimension(:), intent(in) :: coords
!!$    logical, intent(out) :: side
!!$    class(LogType), intent(inout), optional :: log
     
  
  EXPAND({METHODDEF_FACECOLLECTION_COMPUTESUBROW({Array}, {Interior})})

  !-------------------------------------------------------------------
  !- InteriorFaceGrid methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_ATTACH_FACECOLLECTION({Grid}, {Interior})})
  
  EXPAND({METHODDEF_FACECOLLECTION_INIT({Grid}, {Interior})})
  
  EXPAND({METHODDEF_FACECOLLECTION_DEINIT({Grid}, {Interior})})
  
  
  subroutine appendVelocityMoments_InteriorFaceGrid( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class(InteriorFaceGridType), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_InteriorFaceGrid', log )

    ! structured primitive faces do not hold flow information in this
    ! implementation.  Do nothing.
    
    call endSub( log )
  end subroutine appendVelocityMoments_InteriorFaceGrid

  
  subroutine appendScalarMoments_InteriorFaceGrid( obj, &
       scalarMomentGroupList, polytopeIndex, log )
    class(InteriorFaceGridType), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendScalarMoments_InteriorFaceGrid', log )

    ! structured primitive faces do not hold flow information in this
    ! implementation.  Do nothing.
    
    call endSub( log )
  end subroutine appendScalarMoments_InteriorFaceGrid
  

  pure subroutine getFaceNormal_InteriorFaceGrid( obj, index, vector, log )
    class(InteriorFaceGridType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out) :: vector
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getFaceNormal_InteriorFaceGrid', log )
    ! tk
    vector = 0._FLOAT
    call endSub( log )
  end subroutine getFaceNormal_InteriorFaceGrid

  
  elemental subroutine findSide_InteriorFaceGrid( obj, index, coords, &
       side, log )
    class(InteriorFaceGridType), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(in) :: coords
    logical, intent(out) :: side
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'findSide_InteriorFaceGrid', log )

    ! tk
    side = .false. 
    call endSub( log )
  end subroutine findSide_InteriorFaceGrid
     
  
  EXPAND({METHODDEF_FACECOLLECTION_COMPUTESUBROW({Grid}, {Interior})})

  !-------------------------------------------------------------------
  !- InteriorFaceComplex methods
  !-------------------------------------------------------------------

  m4_divert(-1)
  m4_define({ARGLINE}, {parentFace, })
  m4_define({ARGLIST}, {
    class(FacePointerType), intent(in) :: parentFace})
  m4_divert(0)

  EXPAND({PROCEDURE_ATTACH({InteriorFaceComplex}, {FaceComplex},
  {PolytopeComplex}, {ARGLINE()}, {ARGLIST()})})
  
  subroutine init_InteriorFaceComplex( obj, px, ARGLINE log )
    class(InteriorFaceComplexType), intent(out) :: obj
    class(PolytopeComplexInterface), intent(in), target :: px
    ARGLIST
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_InteriorFaceComplex', log )
    
    call obj%parentFace%init( parentFace )

    ! init extension superclass
    call obj%initExtension( px, log )
    call endSub( log )
  end subroutine init_InteriorFaceComplex

  
  subroutine deinit_InteriorFaceComplex( obj, log )
    class(InteriorFaceComplexType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_InteriorFaceComplex', log )

    ! deinit extension superclass 
    call obj%deinitExtension( log )

    call obj%parentFace%deinit( log )
    
    call endSub( log )
  end subroutine deinit_InteriorFaceComplex


  subroutine appendVelocityMoments_InteriorFaceComplex( obj, &
       velocityMomentGroupList, log )
    class(InteriorFaceComplexType), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_InteriorFaceComplex', log )

    ! structured primitive faces do not hold flow information in this
    ! implementation.  Do nothing.
    
    call endSub( log )
  end subroutine appendVelocityMoments_InteriorFaceComplex

  
  subroutine appendScalarMoments_InteriorFaceComplex( obj, &
       scalarMomentGroupList, log )
    class(InteriorFaceComplexType), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendScalarMoments_InteriorFaceComplex', log )
    
    ! structured primitive faces do not hold flow information in this
    ! implementation.  Do nothing.
    
    call endSub( log )
  end subroutine appendScalarMoments_InteriorFaceComplex

  
end module FaceCollectionsModule
