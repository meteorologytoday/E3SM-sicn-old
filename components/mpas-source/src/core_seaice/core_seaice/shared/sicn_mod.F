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
        character(len=*), intent(out) :: startTimeStamp

        character(len=256) :: cwd
      
        logical, pointer :: config_sicn_active
        character(len=strKIND), pointer :: config_sicn_sit_target_file,  &
                                           config_sicn_start_time,       &
                                           config_sicn_cycle_start,      &
                                           config_sicn_cycle_duration
        
        real (kind=RKIND), pointer :: config_sicn_sit_timescale
        logical, pointer :: config_do_restart


        integer :: ierr


        character(len=strKIND) :: forcingReferenceTimeMonthly, forcingIntervalMonthly

        call mpas_log_write('Inside the sicn_mod')
        call mpas_log_write('Prescribed sic: $l', logicArgs=(/ prescribed_si /))
        call mpas_log_write('stream_year_first: $i, stream_year_last: $i', intArgs=(/ stream_year_first, stream_year_last /))

        call getcwd(cwd)
        call mpas_log_write('CWD: '//trim(cwd))


        call mpas_log_write('Try reading namelist')
        
        call mpas_pool_get_config(domain % configs, 'config_sicn_active', config_sicn_active)
        call mpas_pool_get_config(domain % configs, 'config_sicn_sit_target_file', config_sicn_sit_target_file)
        call mpas_pool_get_config(domain % configs, 'config_sicn_sit_timescale', config_sicn_sit_timescale)

        call mpas_log_write('config_sicn_active: $l', logicArgs=(/ config_sicn_active /))
        call mpas_log_write('config_sicn_sit_target_file: '//trim(config_sicn_sit_target_file))
        call mpas_log_write('config_sicn_sit_timescale: $r', realArgs=(/ config_sicn_sit_timescale /) )


        if (config_sicn_active) then

            call mpas_log_write('Sea ice nudging is set active.')
            call mpas_log_write('Now initialize forcing groups.')

            call mpas_pool_get_config(domain % configs, "config_do_restart", config_do_restart)
            call mpas_pool_get_config(domain % configs, 'config_sicn_sit_target_file', config_sicn_sit_target_file)
            call mpas_pool_get_config(domain % configs, 'config_sicn_sit_timescale', config_sicn_sit_timescale)
            call mpas_pool_get_config(domain % configs, 'config_sicn_start_time', config_sicn_start_time)
            call mpas_pool_get_config(domain % configs, 'config_sicn_cycle_start', config_sicn_cycle_start)
            call mpas_pool_get_config(domain % configs, 'config_sicn_cycle_duration', config_sicn_cycle_duration)
            

            ! Same group shares the same time dimension

            forcingIntervalMonthly = "00-01-00_00:00:00"
            forcingReferenceTimeMonthly = "0001-01-15_00:00:00"

            call MPAS_forcing_init_group(    &
                sicnForcingGroups,           &
                "sicn_forcing_monthly",      &   ! this is simply a name to identify this subgroup of the same time dimension
                 domain,                     & 
                 config_sicn_start_time,     &
                 config_sicn_cycle_start,    &
                 config_sicn_cycle_duration, &
                 config_do_restart,          &
                 .false.                     &
            )

            ! 
            !  pool: sicn_forcing           group: sicn_forcing_monthly    stream: sicnForcing
            !  |                            |
            !  +- field: sit_target         +- focing: sit_target         
            !  
            !  This carries the inter-      This regroups differrent       This documents the actual
            !  polated data.                streams into a group.          file information.
            
            call MPAS_forcing_init_field(    &
                 domain % streamManager,     &
                 sicnForcingGroups,          &
                 "sicn_forcing_monthly",     &  ! group name identifier same as above
                 "sit_target",               &  ! forcingName: name of the forcing
                 "sicnForcing",              &  ! forcingStreamID: ID defined in streams.seaice
                 "sicn_forcing",             &  ! poolname: The name of the pool defined in Registry.xml or in Registry_sicn_mod.xml 
                 "sit_target"    ,           &  ! fieldname: The name of the array. This array will carry the interpolated data
                 "linear",                   &
                 forcingReferenceTimeMonthly, &
                 forcingIntervalMonthly)
            
        end if 

    end function sicn_init  

    subroutine sicn_forcing_update( &
       streamManager,               &
       domain,                      &
       simulationClock,             &
       firstTimeStep)

    type (MPAS_streamManager_type), intent(inout) :: streamManager
    type (domain_type)     :: domain
    type (MPAS_clock_type) :: simulationClock
    logical, intent(in)    :: firstTimeStep
    logical, pointer       :: config_sicn_active

    call MPAS_pool_get_config(domain % configs, "config_sicn_active", config_sicn_active)

    if (config_sicn_active) then
        call sicn_forcing_update_action(streamManager, domain, simulationClock) 
    end if

  end subroutine sicn_forcing_update

  subroutine sicn_forcing_update_action( &
       streamManager,                    &
       domain,                           &
       simulationClock)

    type (MPAS_streamManager_type), intent(inout) :: streamManager
    type (domain_type) :: domain
    type (MPAS_clock_type) :: simulationClock
    type (block_type), pointer :: block
    real(kind=RKIND), pointer  :: config_dt

    ! configurations
    call mpas_pool_get_config(domain % configs, 'config_dt', config_dt)

   call MPAS_forcing_get_forcing( &
        sicnForcingGroups,        &
        "sicn_forcing_monthly",   &
        streamManager,            &
        config_dt)

  end subroutine sicn_forcing_update_action



    subroutine sicn_timestep(&
       domain, &
       clock, &
       itimestep, &
       ierr)!{{{

    

    use mpas_timer

    use seaice_diagnostics, only: &
         seaice_set_time_diagnostics, &
         seaice_check_state, &
         seaice_load_balance_timers

    use seaice_unit_test, only: &
         seaice_perform_unit_test

    use seaice_velocity_solver, only: &
         seaice_run_velocity_solver

    use seaice_advection, only: &
         seaice_run_advection

    use seaice_column, only: &
         seaice_column_predynamics_time_integration, &
         seaice_column_dynamics_time_integration, &
         seaice_column_postdynamics_time_integration, &
         seaice_column_reinitialize_fluxes, &
         seaice_column_reinitialize_diagnostics_thermodynamics, &
         seaice_column_reinitialize_diagnostics_bgc, &
         seaice_column_reinitialize_diagnostics_dynamics




    type(domain_type), intent(inout) :: &
         domain !< Input/Output:

    type (MPAS_Clock_type), intent(in) :: &
         clock !< Input:

    integer, intent(in) :: &
         itimestep !< Input:

    integer, intent(inout) :: &
         ierr !< Input




    type(block_type), pointer :: &
         block

    type (MPAS_pool_type), pointer :: &
         configs

    logical, pointer :: &
         config_perform_unit_test, &
         config_use_advection

    integer, pointer :: &
         config_dynamics_subcycle_number

    integer :: &
         iDynamicsSubcycle

    
    call mpas_timer_start("Sea ice nuding magic")

    
    stream_year_first = stream_year_first + 1
    stream_year_last   = 2*(stream_year_last - 1)
    call mpas_log_write('stream_year_first: $i, stream_year_laast: $i', intArgs=(/ stream_year_first, stream_year_last /))




    call mpas_timer_stop("Sea ice nuding magic")


  end subroutine sicn_timestep


end module sicn_mod
