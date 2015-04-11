HEADER()
module InteriorGaussianPointsModule

  USE_MACROS({GaussianPoints})

  use LogModule
  use Global
  use FiniteVolumeModule
  use FlowFieldModule
  use DomainModule
  
  implicit none
  private
  public :: createInteriorGaussianPointCollection
  
  character(*), parameter :: MOD_NAME = 'InteriorGaussianPointsModule'

  
  !-------------------------------------------------------------------
  !- InteriorGaussianPointCollection
  !-------------------------------------------------------------------

  type, extends(InteriorStaticPointCollectionInterface) :: InteriorGaussianPointCollectionType
     private
     EXPAND({ATTRIBUTELIST_GAUSSIANPOINTCOLLECTION})
     class(InteriorRegionInterface), pointer :: interiorRegion
   contains
     procedure :: init => init_InteriorGaussianPointCollection
     procedure :: deinit => deinit_InteriorGaussianPointCollection
     procedure :: computeSubRows
  end type InteriorGaussianPointCollectionType
     
  
contains
  
  
  !-------------------------------------------------------------------
  !- InteriorGaussianPointCollection methods
  !-------------------------------------------------------------------

  m4_define({INIT_ARGLINE}, {&
       EXPAND({INIT_ARGLINE_GAUSSIANPOINTCOLLECTION({Interior})})
       })
  
  m4_define({INIT_ARGLIST}, {&
    EXPAND({INIT_ARGLIST_GAUSSIANPOINTCOLLECTION({Interior})})
    })
    
  EXPAND({PROCEDURE_CREATE({InteriorGaussianPointCollection},
  {InteriorStaticPointCollection}, {INIT_ARGLINE}, {INIT_ARGLIST})})
  
  subroutine init_InteriorGaussianPointCollection( obj, INIT_ARGLINE{}log )
    class(InteriorGaussianPointCollectionType), intent(inout) :: obj
    INIT_ARGLIST
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_InteriorGaussianPointCollection', log )
    
    EXPAND({INIT_BODY_GAUSSIANPOINTCOLLECTION({Interior})})
        
    call endSub( log )
  end subroutine init_InteriorGaussianPointCollection

  
  subroutine deinit_InteriorGaussianPointCollection( obj, log )
    class(InteriorGaussianPointCollectionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_InteriorGaussianPointCollection', &
         log )
    
    EXPAND({DEINIT_BODY_GAUSSIANPOINTCOLLECTION({Interior})})
    
    call endSub( log )
  end subroutine deinit_InteriorGaussianPointCollection

  
  pure function computeSubRows( obj, polytopeIndex, &
       directedSpatialDerivative, nPoints ) result ( rows )
    class(InteriorGaussianPointCollectionType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    integer, intent(in) :: nPoints
    real(FLOAT), dimension(nPoints, NCOEFS_ADV) :: rows
    type(RealVectorType), dimension(obj%getNumPointsPerPolytope()) :: x
    real(FLOAT), dimension(NDIM, obj%getNumPointsPerPolytope(), &
         NCOEFS_ADV) :: c
    real(FLOAT), dimension(obj%getNumPointsPerPolytope(), &
         NCOEFS_ADV) :: w, sr
    
    call obj%getPositions( x, polytopeIndex )
    c = directedSpatialDerivative%computeSubRowComponents( x, &
         POWERS_ADV )
    sr = directedSpatialDerivative%computeSubRows( c )
    
    ! unlike arrival points, which preserve their numbers of points,
    ! gaussian points collapse the array along the "points" dimension with
    ! sum( w * x**px * y**py )
    w = spread( obj%gaussianWeights, dim=2, ncopies=NCOEFS_ADV )
    rows(1, :) = sum( w*sr, dim=1 )
        
  end function computeSubRows

  
end module InteriorGaussianPointsModule
