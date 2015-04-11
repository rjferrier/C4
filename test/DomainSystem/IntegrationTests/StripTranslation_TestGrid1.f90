module StripTranslation_TestGrid1

  use TestGrid1_FieldSetup1, setUp_DNU => setUp, tearDown_DNU => tearDown
  
  use TestUtilities
  use Global
  use DomainModule
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FlowFieldModule

  implicit none

  character(*), parameter, private :: MOD_NAME = &
       'StripTranslation_TestGrid1'

  logical, parameter, private :: OUTPUT_RESULTS = .true.

  
contains


  subroutine setUp
    class(ScalarFieldSpecificationInterface), allocatable :: sfs
    class(DirectedSpatialDerivativeInterface), allocatable :: dsd
    type(ScalarFieldSpecificationListType) :: sfsl
    call log%init( MOD_NAME, 'setup' )

    call createScalarFlowVariableStub( scalar, 'passiveScalar', log )
    call createVectorFlowVariableStub( velocity, 'velocity', log )
    if ( log%test(FATAL) ) return

    call createZerothSpatialDerivative( dsd, log )
    if ( log%test(FATAL) ) return
    
    call createTopHatStripSpecification( sfs, scalar, dsd, vector([&
         1._FLOAT, 0._FLOAT]), vector([2._FLOAT, 0._FLOAT]), 1._FLOAT, &
         log )
    call sfsl%append( sfs, log )
    if ( log%test(FATAL) ) return

    call setUp_TestGrid1_FieldSetup1( vector([0._FLOAT, 0._FLOAT]), &
         vector([8._FLOAT, 0.5_FLOAT]), vector([16, 8]), vector([&
         1._FLOAT, 0._FLOAT]), sfsl, log )
    
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )
    
    call tearDown_TestGrid1_FieldSetup1( log )

    call destroy( scalar, log )
    call destroy( velocity, log )

    call log%report()
    call log%deinit()
  end subroutine tearDown

  
  subroutine testInitialField
    type(RealVectorType) :: r
    real(FLOAT) :: expected, actual
    type(StringListType) :: pointLocations, connectivity, offsets
    integer :: fID, nPointsRunning, nCells, iostat
    if ( log%test(MOD_NAME, 'testInitialField', FATAL) ) return

    if ( OUTPUT_RESULTS ) then

       ! --- data formatting ---

       ! write out the points.  The nPointsRunning counter helps establish
       ! a global ordering.
       nPointsRunning = 0
       call pointGrid%writePointLocations( pointLocations, &
            nPointsRunning, precision=4, indent=10, log=log )
       call xPointArray%writePointLocations( pointLocations, &
            nPointsRunning, precision=4, indent=10, log=log )
       call yPointArray%writePointLocations( pointLocations, &
            nPointsRunning, precision=4, indent=10, log=log )
       if ( log%test(FATAL) ) return

       ! can now describe the geometry of the cells.  Simplicial cells are
       ! included in the call to the grid.
       nCells = 0
       call rectangleGrid%writePointConnectivity( connectivity, &
            offsets, nCells, .true., indent=10, log=log )
       if ( log%test(FATAL) ) return

       ! --- file writing ----
       
       ! preamble
       call requestFileID( fID, log )
       call open( fID, 'StripTranslation.out', 'w', log )
       if ( log%test(FATAL) ) return
       
       write ( unit=fID, fmt='(a)', iostat=iostat ) &
            '<?xml version="1.0"?>', &
            '<VTKFile type="PolyData" version="0.1" byte_order=&
            &"LittleEndian">', &
            '  <PolyData>', &
            '    <Piece NumberOfPoints="'//str(nPointsRunning)//'" &
            &NumberOfVerts="0" NumberOfLines="0" NumberOfStrips="0" &
            &NumberOfPolys="'//str(nCells)//'">', &
            '      <Points>', &
            '        <DataArray type="Float32" NumberOfComponents="'//str(&
            NDIM)//'" format="ascii">'
       call addEvent( iostat/=0, FATAL, 'Problem during write (1).  &
            &IOSTAT='//str(iostat), log )
       
       call pointLocations%writeContents( fID, log )
       if ( log%test(FATAL) ) return
       
       write ( unit=fID, fmt='(a)', iostat=iostat ) &
            '        </DataArray>', &
            '      </Points>'
       call addEvent( iostat/=0, FATAL, 'Problem during write (2).  &
            &IOSTAT='//str(iostat), log )

       ! postamble
       call close( fID, log )
       call returnFileID( fID, log )
       if ( log%test(FATAL) ) return


       ! cleanup
       call pointLocations%deinit( log )
       call connectivity%deinit( log )
       call offsets%deinit( log )
    end if
    
!!$    r = vector([0._FLOAT, 0._FLOAT])
!!$    expected = 0._FLOAT
!!$    actual = rectangleCellGrid%interpolateAtPoint( r, scalar, log )
!!$    call assertEqual( expected, actual, 'Interpolated scalar does not &
!!$         &match expected.' )

    call endSub( log )
  end subroutine testInitialField

  
end module StripTranslation_TestGrid1
