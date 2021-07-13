module sicn_mod

    use mpas_derived_types
    use mpas_pool_routines
    use mpas_timekeeping
    use mpas_timekeeping
    use mpas_forcing
    use mpas_stream_manager
    use mpas_log, only: mpas_log_write

    implicit none 

    private
    save

    public :: sicn_init, sicn_timestep

    logical, public :: prescribed_si     = .false.     ! true if prescribed siconc
    integer         :: stream_year_first = 0           ! first year in stream to use
    integer         :: stream_year_last  = 0     ! last year in stream to use
    integer         :: model_year_align  = 0     ! align stream_year_first
        
    type (MPAS_forcing_group_type), pointer :: sicnForcingGroups
 
!=======================================================================

contains

    function sicn_init(domain, startTimeStamp) result(ierr)

        use mpas_derived_types
        use mpas_stream_manager

        implicit none

        type (domain_type), intent(inout) :: domain
        character(len=*), intent(in) :: startTimeStamp

        character(len=256) :: cwd
      
        logical, pointer :: config_sicn_active
        character(len=strKIND), pointer :: config_sicn_input_file,  &
                                           config_sicn_start_time,       &
                                           config_sicn_cycle_start,      &
                                           config_sicn_cycle_duration
        
        real (kind=RKIND), pointer :: config_sicn_sit_timescale
        logical, pointer :: config_do_restart


        integer :: ierr


        character(len=strKIND) :: forcingReferenceTimeMonthly, forcingIntervalMonthly

        call mpas_log_write('Enter sicn_init')

        call mpas_log_write('Start timestamp: ' // trim(startTimeStamp))

        call mpas_log_write('Prescribed sic: $l', logicArgs=(/ prescribed_si /))
        call mpas_log_write('stream_year_first: $i, stream_year_last: $i', intArgs=(/ stream_year_first, stream_year_last /))

        call getcwd(cwd)
        call mpas_log_write('CWD: '//trim(cwd))


        call mpas_log_write('Try reading namelist')
        
        call mpas_pool_get_config(domain % configs, 'config_sicn_active', config_sicn_active)
        call mpas_pool_get_config(domain % configs, 'config_sicn_input_file', config_sicn_input_file)
        call mpas_pool_get_config(domain % configs, 'config_sicn_sit_timescale', config_sicn_sit_timescale)


        call mpas_log_write('config_sicn_active: $l', logicArgs=(/ config_sicn_active /))
        call mpas_log_write('config_sicn_input_file: '//trim(config_sicn_input_file))
        call mpas_log_write('config_sicn_sit_timescale: $r', realArgs=(/ config_sicn_sit_timescale /) )


        if (config_sicn_active) then

            call mpas_log_write('Sea ice nudging is set active.')
            call mpas_log_write('Now initialize forcing groups.')

            call mpas_log_write('(1) Getting configs.')
           
            call mpas_pool_get_config(domain % configs, "config_do_restart", config_do_restart)
            call mpas_pool_get_config(domain % configs, 'config_sicn_input_file', config_sicn_input_file)
            call mpas_pool_get_config(domain % configs, 'config_sicn_sit_timescale', config_sicn_sit_timescale)
            call mpas_pool_get_config(domain % configs, 'config_sicn_start_time', config_sicn_start_time)
            call mpas_pool_get_config(domain % configs, 'config_sicn_cycle_start', config_sicn_cycle_start)
            call mpas_pool_get_config(domain % configs, 'config_sicn_cycle_duration', config_sicn_cycle_duration)
        
            call mpas_log_write('config_do_restart: $l', logicArgs=(/ config_do_restart /))
            
            !  My understanding of the datastream in E3SM framework
            !
            !       stream1            stream2            stream3
            !         |                   |                  |
            !         |                   +---------------\  |
            !         |                   |                \ |
            !         |                   |                 \|
            !       Group1              Group2            Group3
            !        |                    |                 |
            !        +-pool               +-pool            +-pool
            ! 
            !
            !
            ! 
            !  pool: sicn_forcing           group: sicn_forcing_monthly    stream: sicnForcing
            !  |                            |
            !  +- field: sit_target         +- focing: sit_target         
            !  
            !  This carries the inter-      This regroups differrent       This documents the actual
            !  polated data.                streams into a group.          file information.
            !                               That is, a group may contain
            !                               data from different streams.
            !                               Each group has a pool that
            !                               contains actual data arrays.
            !
            !                               Also, same group has only one
            !                               time resolution, and thus time
            !                               intervals.

            forcingIntervalMonthly = "0000-01-00_00:00:00"
            forcingReferenceTimeMonthly = "0001-01-15_00:00:00"
            
            call mpas_log_write('(2) Create forcing group.')

            call MPAS_forcing_init_group(    &
                sicnForcingGroups,           &
                "sicn_forcing_monthly",      &   ! this is simply a name to identify this subgroup of the same time dimension
                 domain,                     & 
                 startTimeStamp,             &   ! config_sicn_start_time,     &
                 config_sicn_cycle_start,    &
                 config_sicn_cycle_duration, &
                 .false.,                    &   ! there is a bug that config_do_restart would not work. We send .false. to force the clock created every time
                 .false.                     &
            )

            call mpas_log_write('(3) Binding variable to a data stream.')
            ! Each type of input data requires
            ! one mpas_forcing_init_field call.
            call MPAS_forcing_init_field(    &
                 domain % streamManager,     &
                 sicnForcingGroups,          &
                 "sicn_forcing_monthly",     &  ! group name identifier same as above
                 "sit_target",               &  ! forcingName: name of the forcing
                 "sicnForcing",              &  ! forcingStreamID: ID defined in streams.seaice
                 "sicn_pool",             &  ! poolname: The name of the pool defined in Registry.xml or in Registry_sicn_mod.xml 
                 "sit_target"    ,           &  ! fieldname: The name of the array. This array will carry the interpolated data
                 "linear",                   &
                 forcingReferenceTimeMonthly, &
                 forcingIntervalMonthly      & 
            )
           
            call mpas_log_write('Initialization done.') 
        end if
 
        call mpas_log_write('Leave sicn_init')

    end function sicn_init  

    subroutine sicn_timestep( &
       domain,                &
       clock,                 &
       itimestep,             &
       ierr                   &
    )

        use mpas_timer

        use seaice_diagnostics, only: &
             seaice_set_time_diagnostics, &
             seaice_check_state, &
             seaice_load_balance_timers

        type (block_type), pointer :: block

        type(domain_type), intent(inout) :: domain
        type (MPAS_Clock_type), intent(in) :: clock
        
        type (MPAS_Clock_type)  :: forcing_clock
        type (MPAS_Time_Type)   :: time_holder
        character(len=StrKIND)  :: clock_timestamp, &
                                   forcing_clock_timestamp_before, &
                                   forcing_clock_timestamp_after


        integer, intent(in) :: itimestep
        integer, intent(inout) :: ierr

        type (MPAS_pool_type), pointer :: configs
        real(kind=RKIND), pointer :: config_dt
        logical, pointer       :: config_sicn_active
        
        type (MPAS_streamManager_type), pointer :: streamManager
        integer :: block_cnt

        call mpas_log_write('Enter sicn_timestep')

        call mpas_timer_start("Sea ice nuding magic")
        
        call mpas_pool_get_config(domain % configs, 'config_dt', config_dt)
        call MPAS_pool_get_config(domain % configs, "config_sicn_active", config_sicn_active)
        
        call mpas_log_write('config_dt = $r', realArgs=(/ config_dt /))

        time_holder = mpas_get_clock_time(clock, MPAS_NOW, ierr)
        call mpas_get_time(time_holder, dateTimeString=clock_timestamp)
        call mpas_log_write('Current timestamp: ' // trim(clock_timestamp))
        
        if (config_sicn_active) then
            
            call mpas_log_write("sicn is active.")
 
            time_holder = mpas_get_clock_time(sicnForcingGroups % forcingClock, MPAS_NOW, ierr)
            call mpas_get_time(time_holder, dateTimeString=forcing_clock_timestamp_before)
    
            call MPAS_forcing_get_forcing( &
                sicnForcingGroups,         &
                "sicn_forcing_monthly",    &
                domain % streamManager,    &
                config_dt                  &
            )

            time_holder = mpas_get_clock_time(sicnForcingGroups % forcingClock, MPAS_NOW, ierr)
            call mpas_get_time(time_holder, dateTimeString=forcing_clock_timestamp_after)

            call mpas_log_write("sicnForcingGroups clock moves from " // trim(forcing_clock_timestamp_before) // " to " // trim(forcing_clock_timestamp_after))
 
            block => domain % blocklist
            do while (associated(block))
                block_cnt = block_cnt + 1
                call mpas_log_write('block_cnt = $i', intArgs=(/ block_cnt /))
                call sicn_impose_forcing(block)
                block => block % next
            end do

        end if

        call mpas_timer_stop("Sea ice nuding magic")
        call mpas_log_write('Leave sicn_timestep')

    end subroutine sicn_timestep

    subroutine sicn_impose_forcing(block)

        type (block_type), pointer :: block

        type (mpas_pool_type), pointer :: mesh_pool, sicn_pool, tracers_pool, atmos_coupling
        logical, pointer :: config_do_restart

        real(kind=RKIND), dimension(:), pointer :: &
            sit_target, &
            latCell, &
            lonCell, &
            areaCell

        real(kind=RKIND), dimension(:,:,:), pointer :: iceVolumeCategory
        real(kind=RKIND), dimension(:), pointer :: airTemperature


        integer, pointer :: nCells, nCellsSolve, nCategories


        integer :: iCell, iCat
        real(kind=RKIND) :: total_ice_volume, tmp, tmp2
        
        integer :: i, j, k
        
        call mpas_log_write('Enter sicn_impose_forcing')

        ! "Struct" is defined in Registry as <var_struct /> elements
        
        call mpas_log_write('Getting block structs')
        call MPAS_pool_get_subpool(block % structs, "mesh", mesh_pool)
        call MPAS_pool_get_subpool(block % structs, "tracers", tracers_pool)
        call MPAS_pool_get_subpool(block % structs, "sicn_pool", sicn_pool)
        call MPAS_pool_get_subpool(block % structs, "atmos_coupling", atmos_coupling)

        call mpas_log_write('Retrieving dimension')
        call MPAS_pool_get_dimension(mesh_pool, "nCells",      nCells)
        call MPAS_pool_get_dimension(mesh_pool, "nCellsSolve", nCellsSolve)
        call MPAS_pool_get_dimension(mesh_pool, "nCategories", nCategories)
        call MPAS_pool_get_array(atmos_coupling, "airTemperature", airTemperature)

        call mpas_log_write('Retrieving data')
        call MPAS_pool_get_array(mesh_pool, "latCell",  latCell)
        call MPAS_pool_get_array(mesh_pool, "lonCell",  lonCell)
        call MPAS_pool_get_array(mesh_pool, "areaCell", areaCell)
        !call MPAS_pool_get_array(tracers_pool, "iceVolumeCategory", iceVolumeCategory, 1) ! ONE nCategories nCells Time
        call MPAS_pool_get_array(tracers_pool, "surfaceTemperature", iceVolumeCategory, 1) ! ONE nCategories nCells Time
        call MPAS_pool_get_array(sicn_pool, "sit_target", sit_target)

        call mpas_log_write('Dimensions retrieved: nCells = $i , nCellsSolve = $i , nCategories = $i', intArgs=(/ nCells, nCellsSolve, nCategories /))


        i = size(sit_target, 1)
        call mpas_log_write('Size of sit_target: ($i,)', intArgs=(/ i /))
        
        i = size(iceVolumeCategory, 1)
        j = size(iceVolumeCategory, 2)
        k = size(iceVolumeCategory, 3)
        call mpas_log_write('Size of iceVolumeCategory: ($i, $i, $i)', intArgs=(/ i, j, k /))

        tmp = 0.0
        tmp2 = 0.0
        total_ice_volume = 0.0
        do iCell = 1, nCellsSolve

            do iCat = 1, nCategories
                total_ice_volume = total_ice_volume + iceVolumeCategory(1, iCat, iCell) 
            enddo
            tmp = tmp + sit_target(iCell)
            tmp2 = tmp2 + airTemperature(iCell)
        enddo

        call mpas_log_write('sum of sit_target = $r ; total ice volume = $r', realArgs=(/tmp, total_ice_volume/)) 
        call mpas_log_write('sum of airTemperature = $r', realArgs=(/tmp2/)) 
        
        call mpas_log_write('Leave sicn_impose_forcing')
    end subroutine sicn_impose_forcing


    

end module sicn_mod
