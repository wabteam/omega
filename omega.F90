program omega
! purpose:
! The main program for the Ocean Model Equations with Grid-point Algorithm (omega). 
!
!
! record of revisions:
!    date             programmer             description of change  
!==============    ===================   =============================
! 2013/05/17         wenyu huang                 original code
! ...
!
!any comments please send to huangwenyu@mail.tsinghua.edu.cn                      
    use namelist_manager,   only : read_omega_namelist 
    use namelist_manager,   only : start_date, end_date, calendar 
    use time_manager,       only : time_manager_init, time_manager_advance
    use time_manager,       only : current_year, current_mon, current_day, current_hour, current_min, current_sec 
    use time_manager,       only : is_new_year, is_new_mon, is_new_day, is_new_hour, is_new_min, is_new_sec 
    use time_manager,       only : is_end_year, is_end_mon, is_end_day, is_end_hour, is_end_min, is_end_sec 
    use grid_manager,       only : grid_manager_init, grid_manager_finish
    use nc_read_write_interface

    implicit none
    integer (4) :: k 

    call read_omega_namelist

    call time_manager_init (start_date, end_date, calendar)
    
    do k = 1, 400 
        call time_manager_advance (30*600, calendar)
        if (is_new_mon) then
            write (*, *) "new_mon"
        end if
        write (*, "(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)") current_year,"-" ,current_mon, "-", current_day,"-", current_hour,"-", current_min,"-", current_sec
        if (is_end_mon) then
            write (*, *) "end_mon"
        end if
    end do

    call omega_init

    call omega_finish
contains
    !---------------------------------------------------------------------
    !                          subroutine omega_init
    !---------------------------------------------------------------------
    subroutine omega_init()
        call grid_manager_init 
    end subroutine omega_init
    !---------------------------------------------------------------------
    !                      end of subroutine omega_init
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          subroutine omega_finish
    !---------------------------------------------------------------------
    subroutine omega_finish()
        call grid_manager_finish
    end subroutine omega_finish
    !---------------------------------------------------------------------
    !                      end of subroutine omega_finish
    !---------------------------------------------------------------------
end program omega
