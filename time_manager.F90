module time_manager
! purpose:
! this module is devised for ...
!
!
! record of revisions:
!    date             programmer                description of change  
!==============    ===================   =============================
! 2011/12/22         wenyu huang                original code
! ...
!
!any comments please send to huangwenyu@mail.tsinghua.edu.cn                      
    use precision_manager, only : i4

    implicit none
    private
    public time_manager_init
    public time_manager_advance
    public current_year, current_mon, current_day, current_hour, current_min, current_sec
    public current_date
    public is_new_year, is_new_mon, is_new_day, is_new_hour, is_new_min, is_new_sec 
    public is_end_year, is_end_mon, is_end_day, is_end_hour, is_end_min, is_end_sec 

    integer (i4) :: current_year, year_of_last_advance, year_of_future_advance
    integer (i4) :: current_mon,  mon_of_last_advance,  mon_of_future_advance
    integer (i4) :: current_day,  day_of_last_advance,  day_of_future_advance
    integer (i4) :: current_hour, hour_of_last_advance, hour_of_future_advance
    integer (i4) :: current_min,  min_of_last_advance,  min_of_future_advance
    integer (i4) :: current_sec,  sec_of_last_advance,  sec_of_future_advance

    integer (i4), dimension (6) :: current_date, date_of_last_advance, date_of_future_advance

    logical :: is_new_year, is_end_year
    logical :: is_new_mon,  is_end_mon
    logical :: is_new_day,  is_end_day
    logical :: is_new_hour, is_end_hour
    logical :: is_new_min,  is_end_min
    logical :: is_new_sec,  is_end_sec

    integer (i4), parameter :: min_year     =   1
    integer (i4), parameter :: min_mon      =   1
    integer (i4), parameter :: max_mon      =   12
    integer (i4), parameter :: min_day      =   1
    integer (i4), parameter :: min_hour     =   0
    integer (i4), parameter :: max_hour     =   23
    integer (i4), parameter :: min_min      =   0
    integer (i4), parameter :: max_min      =   59 
    integer (i4), parameter :: min_sec      =   0
    integer (i4), parameter :: max_sec      =   59 


    
    contains
    !---------------------------------------------------------------------
    !                          subroutine time_manager_init
    !---------------------------------------------------------------------
    subroutine time_manager_init (start_date_in, end_date_in, calendar_in)
        integer, intent (in), dimension (6) :: start_date_in
        integer, intent (in), dimension (6) :: end_date_in
        character (len = *),    intent (in) :: calendar_in
    
        call check_if_right_date ("start_date", start_date_in, calendar_in)
        call check_if_right_date ("end_date",   end_date_in,   calendar_in)

        current_year = start_date_in (1)
        current_mon  = start_date_in (2)
        current_day  = start_date_in (3)
        current_hour = start_date_in (4)
        current_min  = start_date_in (5)
        current_sec  = start_date_in (6)

        current_date = start_date_in

        is_new_year = .true.
        is_new_mon  = .true.
        is_new_day  = .true.
        is_new_hour = .true.
        is_new_min  = .true.
        is_new_sec  = .true.

        return
    end subroutine time_manager_init
    !---------------------------------------------------------------------
    !                      end of subroutine time_manager_init
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          subroutine time_manager_advance
    !---------------------------------------------------------------------
    subroutine time_manager_advance(advance_time_in, calendar_in)
        integer, intent (in)                :: advance_time_in
        character (len = *), intent (in)    :: calendar_in

        integer :: temp, temp2
        
        year_of_last_advance = current_year
        mon_of_last_advance  = current_mon
        day_of_last_advance  = current_day
        hour_of_last_advance = current_hour
        min_of_last_advance  = current_min
        sec_of_last_advance  = current_sec

        date_of_last_advance = (/year_of_last_advance, &
                                 mon_of_last_advance,  &
                                 day_of_last_advance,  &
                                 hour_of_last_advance, &
                                 min_of_last_advance,  &
                                 sec_of_last_advance   /)

        temp2 = current_sec + advance_time_in
        current_sec = mod (temp2, max_sec + 1)
        temp = temp2/(max_sec + 1)

        temp2 = current_min + temp
        current_min = mod (current_min + temp, max_min + 1) 
        temp = temp2/(max_min + 1)
        
        temp2 = current_hour + temp
        current_hour = mod (temp2, max_hour + 1)
        temp = temp2/(max_hour + 1)

        temp2 = current_day + temp
        current_day = mod (temp2, days_of_mon (current_year, current_mon, calendar_in) + 1) 
        current_day = max (current_day, min_day)
        temp = temp2/(days_of_mon (current_year, current_mon, calendar_in) + 1)

        temp2 = current_mon + temp
        current_mon = mod (temp2, max_mon + 1)
        current_mon = max (current_mon, min_mon)
        temp = temp2/(max_mon + 1)

        current_year = current_year + temp

        current_date = (/current_year, current_mon, current_day, current_hour, current_min, current_sec/)


        is_new_year = .false.
        is_new_mon  = .false.
        is_new_day  = .false.
        is_new_hour = .false.
        is_new_min  = .false.
        is_new_sec  = .false.

        if (year_of_last_advance /= current_year) then
            is_new_year = .true.
            is_new_mon  = .true.
            is_new_day  = .true.
            is_new_hour = .true.
            is_new_min  = .true.
            is_new_sec  = .true.
        end if

        if (mon_of_last_advance  /= current_mon)  then
            is_new_mon  = .true.
            is_new_day  = .true.
            is_new_hour = .true.
            is_new_min  = .true.
            is_new_sec  = .true.
        end if

        if (day_of_last_advance  /= current_day)  then
            is_new_day  = .true.
            is_new_hour = .true.
            is_new_min  = .true.
            is_new_sec  = .true.
        end if

        if (hour_of_last_advance /= current_hour)  then
            is_new_hour = .true.
            is_new_min  = .true.
            is_new_sec  = .true.
        end if

        if (min_of_last_advance  /= current_min)  then
            is_new_min  = .true.
            is_new_sec  = .true.
        end if

        if (sec_of_last_advance  /= current_sec)  then
            is_new_sec = .true.
        end if

        year_of_future_advance  =   current_year
        mon_of_future_advance   =   current_mon
        day_of_future_advance   =   current_day
        hour_of_future_advance  =   current_hour
        min_of_future_advance   =   current_min
        sec_of_future_advance   =   current_sec

        temp2 = sec_of_future_advance + advance_time_in
        sec_of_future_advance = mod (temp2, max_sec + 1)
        temp = temp2/(max_sec + 1)

        temp2 = min_of_future_advance + temp
        min_of_future_advance = mod (min_of_future_advance + temp, max_min + 1) 
        temp = temp2/(max_min + 1)
        
        temp2 = hour_of_future_advance + temp
        hour_of_future_advance = mod (temp2, max_hour + 1)
        temp = temp2/(max_hour + 1)

        temp2 = day_of_future_advance + temp
        day_of_future_advance = mod (temp2, days_of_mon (year_of_future_advance, mon_of_future_advance, calendar_in) + 1) 
        day_of_future_advance = max (day_of_future_advance, min_day)
        temp = temp2/(days_of_mon (year_of_future_advance, mon_of_future_advance, calendar_in) + 1)

        temp2 = mon_of_future_advance + temp
        mon_of_future_advance = mod (temp2, max_mon + 1)
        mon_of_future_advance = max (mon_of_future_advance, min_mon)
        temp = temp2/(max_mon + 1)

        year_of_future_advance = year_of_future_advance + temp

        date_of_future_advance = (/year_of_future_advance,  &
                                   mon_of_future_advance,   &
                                   day_of_future_advance,   &
                                   hour_of_future_advance,  &
                                   min_of_future_advance,   &
                                   sec_of_future_advance    /)


        is_end_year = .false.
        is_end_mon  = .false.
        is_end_day  = .false.
        is_end_hour = .false.
        is_end_min  = .false.
        is_end_sec  = .false.

        if (year_of_future_advance /= current_year) then
            is_end_year = .true. 
            is_end_mon  = .true.
            is_end_day  = .true.
            is_end_hour = .true.
            is_end_min  = .true.
            is_end_sec  = .true.
        end if

        if (mon_of_future_advance  /= current_mon)  then
            is_end_mon  = .true.
            is_end_day  = .true.
            is_end_hour = .true.
            is_end_min  = .true.
            is_end_sec  = .true.
        end if

        if (day_of_future_advance  /= current_day)  then
            is_end_day  = .true.
            is_end_hour = .true.
            is_end_min  = .true.
            is_end_sec  = .true.
        end if

        if (hour_of_future_advance /= current_hour)  then
            is_end_hour = .true.
            is_end_min  = .true.
            is_end_sec  = .true.
        end if

        if (min_of_future_advance  /= current_min)  then
            is_end_min  = .true.
            is_end_sec  = .true.
        end if

        if (sec_of_future_advance  /= current_sec)  then
            is_end_sec  = .true.
        end if

        return
    end subroutine time_manager_advance
    !---------------------------------------------------------------------
    !                      end of subroutine time_manager_advance
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          function days_of_mon
    !---------------------------------------------------------------------
    integer (i4) function days_of_mon (year_in, mon_in, calendar_in) 
        integer (i4), intent (in)           :: year_in, mon_in
        character (len = *), intent (in)    :: calendar_in

        days_of_mon = 31

        if (mon_in == 4 .or. &
            mon_in == 6 .or. &
            mon_in == 9 .or. &
            mon_in == 11 ) then
            days_of_mon = 30
            return
        end if

        if (mon_in == 2) then 
            if (is_leap_year (year_in, calendar_in)) then
                days_of_mon = 29
                return
            else
                days_of_mon = 28
                return
            end if
        end if

        return
    end function days_of_mon
    !---------------------------------------------------------------------
    !                      end of function days_of_mon
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          function is_leap_year
    !---------------------------------------------------------------------
    logical function is_leap_year (year_in, calendar_in)
        integer (i4), intent (in)           :: year_in 
        character (len = *), intent (in)    :: calendar_in
        
        is_leap_year = .false.

        if (calendar_in == "noleap") return 
        
        if (mod (year_in, 100) == 0) then
            if (mod (year_in, 400) == 0) then
                is_leap_year = .true.
                return
            end if
        else if (mod (year_in, 4) == 0) then
            is_leap_year = .true.
            return
        end if

        return 
    end function is_leap_year
    !---------------------------------------------------------------------
    !                      end of function is_leap_year
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                   subroutine check_if_right_date
    !---------------------------------------------------------------------
    subroutine check_if_right_date (date_name_in, date_in, calendar_in)

        character (len = *), intent (in)            :: date_name_in 
        integer (i4), intent (in), dimension (6)    :: date_in 
        character (len = *), intent (in)            :: calendar_in

        if (date_in (1) < min_year) then
            write (*, *) "Please check the input year of "//trim(date_name_in)//": ", date_in (1)
            stop
        end if

        if (date_in (2) < min_mon .or. date_in (2) >  max_mon) then
            write (*, *) "Please check the input month of "//trim(date_name_in)//": ", date_in (2)
            stop
        end if
    
        if (date_in (3) < min_day .or. date_in (3) > days_of_mon (date_in (1), date_in (2), calendar_in)) then
            write (*, *) "Please check the input day of "//trim(date_name_in)//": ", date_in (3)
            stop
        end if

        if (date_in (4) < min_hour .or. date_in (4) >  max_hour) then
            write (*, *) "Please check the input hour of "//trim(date_name_in)//": ", date_in (4)
            stop
        end if

        if (date_in (5) < min_min .or. date_in (5) >  max_min) then
            write (*, *) "Please check the input minute of "//trim(date_name_in)//": ", date_in (5)
            stop
        end if

        if (date_in (6) < min_sec .or. date_in (6) >  max_sec) then
            write (*, *) "Please check the input second of "//trim(date_name_in)//": ", date_in (6)
            stop
        end if
    end subroutine check_if_right_date
    !---------------------------------------------------------------------
    !                      end of subroutine check_if_right_date
    !---------------------------------------------------------------------
end module time_manager
