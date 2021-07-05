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
        character(len=strKIND), pointer :: config_sicn_sit_target_file
        real (kind=RKIND), pointer :: config_sicn_sit_timescale

        integer :: ierr

        type (MPAS_forcing_group_type), pointer :: seaiceForcingGroups

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

            call MPAS_forcing_init_group(&
                seaiceForcingGroups, &
                "seaice_atmospheric_forcing_sixhrly", &
                domain, &
                 config_forcing_start_time, &
                 config_forcing_cycle_start, &
                 config_forcing_cycle_duration, &
                 config_do_restart, &
                 .false. &
            )

    forcingIntervalSixHourly = "06:00:00"
    forcingReferenceTimeSixHourly = "2000-01-01_00:00:00"

    call MPAS_forcing_init_field(&
         domain % streamManager, &
         seaiceForcingGroups, &
         "seaice_atmospheric_forcing_sixhrly", &
         "airTemperature", &
         "LYqSixHourlyForcing", &
         "atmos_coupling", &
         "airTemperature", &
         "linear", &
         forcingReferenceTimeSixHourly, &
         forcingIntervalSixHourly, &
         "next")


            
        end if 

    end function sicn_init  

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

    subroutine sicn_forcing_update(&
       streamManager, &
       domain, &
       simulationClock, &
       firstTimeStep)

        type (MPAS_streamManager_type), intent(inout) :: streamManager
        type (domain_type)     :: domain
        type (MPAS_clock_type) :: simulationClock
        logical, intent(in)    :: firstTimeStep
        logical, pointer       :: config_sicn_active

        call MPAS_pool_get_config(domain % configs, "config_sicn_active", config_sicn_active)

        if (config_sicn_active) then
            call MPAS_forcing_get_forcing(            &
                seaiceForcingGroups,                  &
                "seaice_atmospheric_forcing_monthly", &
                streamManager,                        &
                config_dt)


            block => domain % blocklist
            do while (associated(block))

               ! convert the input forcing variables to the coupling variables
               select case (trim(config_atmospheric_forcing_type))
               case ("CORE")
                  call prepare_atmospheric_coupling_variables_CORE(block)
               end select

               ! perform post coupling operations
               call post_atmospheric_coupling(block)

               ! perform post forcing
               call post_atmospheric_forcing(block)

               block => block % next
            end do


        end if


    end subroutine sicn_forcing_update

end module sicn_mod
