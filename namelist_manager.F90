module namelist_manager
! purpose:
! This module is devised for managing the namelist for all the subroutines, modules and program 
!   that used in Ocean Model Equations with Grid-point Algorithm (OMEGA).
!
! record of revisions:
!    date             programmer              description of change  
!==============    ===================   =============================
! 2013/05/17         wenyu huang                 original code
! ...
!
!any comments please send to huangwenyu@mail.tsinghua.edu.cn                      
    use precision_manager, only : i4, len100
    implicit none
    private
    public read_omega_namelist
    public calendar, start_date, end_date, if_coupled_run, if_restart_run

!------------------------------------------------------------------------    
!namelist variables for namelist omega_run
    character (len = len100) :: calendar
                                             ! type of calendar: "noleap" or "leap"
    integer (i4), dimension (6) :: start_date 
                                             ! start date for model run in format: "year month day hour minute second" 
    integer (i4), dimension (6) :: end_date
                                             ! end date for model run in format: "year month day hour minute second" 
    logical :: if_coupled_run
                                             ! flags for coupled/single run 
    logical :: if_restart_run
                                             ! flags for restart/initial run 
!------------------------------------------------------------------------    


!------------------------------------------------------------------------    
    namelist /omega_run/    calendar,       &
                            start_date,     &
                            end_date,       &
                            if_coupled_run, &
                            if_restart_run 
!------------------------------------------------------------------------    
    contains
    !---------------------------------------------------------------------
    !                          subroutine read_omega_namelist
    !---------------------------------------------------------------------
    subroutine read_omega_namelist()
        character (len = len100) :: namelist_file_name

        call get_command_argument (1, namelist_file_name) 

        open (11, file = namelist_file_name) 
            calendar        = "noleap"
            start_date      = (/1,  1,  1, 0, 0, 0/) 
            end_date        = (/1, 12, 31, 0, 0, 0/) 
            if_coupled_run  = .false.
            if_restart_run  = .false.
            read (11, nml = omega_run)
        close (11)
    end subroutine read_omega_namelist
    !---------------------------------------------------------------------
    !                      end of subroutine read_omega_namelist
    !---------------------------------------------------------------------

end module namelist_manager
