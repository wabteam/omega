module math_interface
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
    use precision_manager, only : i4, r8, len100
    implicit none
    private
    public check_1d_array_order

    interface check_1d_array_order
        module procedure check_1d_array_order_int
        module procedure check_1d_array_order_r8
    end interface check_1d_array_order

    contains
    !---------------------------------------------------------------------
    !                     subroutine check_1d_array_order_int
    !---------------------------------------------------------------------
    subroutine check_1d_array_order_int (array_name, array_in, array_len, order)
        character (len = *), intent (in) :: order
        integer (i4), intent (in) :: array_len
        integer (i4), intent (in) :: array_in (array_len)
        character (len = *), intent (in) :: array_name 

        integer (i4) :: k 

        if (order /= "ascending" .and. order /= "descending") then
            write (*, *) "The order you set for check_1d_array_order of "//trim(array_name)//" is not ascending or descending." 
            write (*, *) "Program omega will stop due to check_1d_array_order."
            stop
        end if

        do k = 1, array_len - 1 
            if (array_in (k) > array_in (k+1) .and. order == "ascending") then
                write (*, *) "The array "//trim(array_name)//" is not in the ascending order as wished!"
                write (*, *) "Program omega will stop due to check_1d_array_order."
                stop
            end if
            if (array_in (k) < array_in (k+1) .and. order == "descending") then
                write (*, *) "The array "//trim(array_name)//" is not in the descending order as wished!"
                write (*, *) "Program omega will stop due to check_1d_array_order."
                stop
            end if
        end do
        return 
    end subroutine check_1d_array_order_int
    !---------------------------------------------------------------------
    !                 end of subroutine check_1d_array_order_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                subroutine check_1d_array_order_r8
    !---------------------------------------------------------------------
    subroutine check_1d_array_order_r8 (array_name, array_in, array_len, order)
        character (len = *), intent (in) :: order
        integer (i4), intent (in) :: array_len
        real (r8), intent (in) :: array_in (array_len)
        character (len = *), intent (in) :: array_name 

        integer (i4) :: k 

        if (order /= "ascending" .and. order /= "descending") then
            write (*, *) "The order you set for check_1d_array_order of "//trim(array_name)//" is not ascending or descending." 
            write (*, *) "Program omega will stop due to check_1d_array_order."
            stop
        end if

        do k = 1, array_len - 1 
            if (array_in (k) > array_in (k+1) .and. order == "ascending") then
                write (*, *) "The array "//trim(array_name)//" is not in the ascending order as wished!"
                write (*, *) "Program omega will stop due to check_1d_array_order."
                stop
            end if
            if (array_in (k) < array_in (k+1) .and. order == "descending") then
                write (*, *) "The array "//trim(array_name)//" is not in the descending order as wished!"
                write (*, *) "Program omega will stop due to check_1d_array_order."
                stop
            end if
        end do
        return 
    end subroutine check_1d_array_order_r8
    !---------------------------------------------------------------------
    !                end of subroutine check_1d_array_order_r8
    !---------------------------------------------------------------------

end module math_interface
