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
    public check_array_region

    interface check_1d_array_order
        module procedure check_1d_array_order_int
        module procedure check_1d_array_order_r8
    end interface check_1d_array_order
    
    interface check_array_region
        module procedure check_1d_array_region_int
        module procedure check_1d_array_region_r8
        module procedure check_2d_array_region_int
        module procedure check_2d_array_region_r8
        module procedure check_3d_array_region_int
        module procedure check_3d_array_region_r8
        module procedure check_4d_array_region_int
        module procedure check_4d_array_region_r8
        module procedure check_5d_array_region_int
        module procedure check_5d_array_region_r8
    end interface check_array_region

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
    !---------------------------------------------------------------------
    !                 subroutine check_1d_array_region_int
    !---------------------------------------------------------------------
    subroutine check_1d_array_region_int(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        integer (i4), intent (in), dimension (dim_count (1)) :: var 
        integer (i4), intent (in) :: region_min
        integer (i4), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
            if (var (i) < region_min .or. var (i) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        
    
    end subroutine check_1d_array_region_int
    !---------------------------------------------------------------------
    !              end of subroutine check_1d_array_region_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine check_1d_array_region_r8
    !---------------------------------------------------------------------
    subroutine check_1d_array_region_r8(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        real (r8), intent (in), dimension (dim_count (1)) :: var 
        real (r8), intent (in) :: region_min
        real (r8), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
            if (var (i) < region_min .or. var (i) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        
    
    end subroutine check_1d_array_region_r8
    !---------------------------------------------------------------------
    !              end of subroutine check_1d_array_region_r8
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine check_2d_array_region_int
    !---------------------------------------------------------------------
    subroutine check_2d_array_region_int(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        integer (i4), intent (in), dimension (dim_count (1), dim_count (2)) :: var 
        integer (i4), intent (in) :: region_min
        integer (i4), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
        do j = 1, dim_count (2)
            if (var (i, j) < region_min .or. var (i, j) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        end do
        
    
    end subroutine check_2d_array_region_int
    !---------------------------------------------------------------------
    !              end of subroutine check_2d_array_region_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine check_2d_array_region_r8
    !---------------------------------------------------------------------
    subroutine check_2d_array_region_r8(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        real (r8), intent (in), dimension (dim_count (1), dim_count (2)) :: var 
        real (r8), intent (in) :: region_min
        real (r8), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
        do j = 1, dim_count (2)
            if (var (i, j) < region_min .or. var (i, j) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        end do
        
    
    end subroutine check_2d_array_region_r8
    !---------------------------------------------------------------------
    !              end of subroutine check_2d_array_region_r8
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine check_3d_array_region_int
    !---------------------------------------------------------------------
    subroutine check_3d_array_region_int(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        integer (i4), intent (in), dimension (dim_count (1), dim_count (2), dim_count (3)) :: var 
        integer (i4), intent (in) :: region_min
        integer (i4), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
        do j = 1, dim_count (2)
        do k = 1, dim_count (3)
            if (var (i, j, k) < region_min .or. var (i, j, k) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        end do
        end do
        
    
    end subroutine check_3d_array_region_int
    !---------------------------------------------------------------------
    !              end of subroutine check_3d_array_region_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine check_3d_array_region_r8
    !---------------------------------------------------------------------
    subroutine check_3d_array_region_r8(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        real (r8), intent (in), dimension (dim_count (1), dim_count (2), dim_count (3)) :: var 
        real (r8), intent (in) :: region_min
        real (r8), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
        do j = 1, dim_count (2)
        do k = 1, dim_count (3)
            if (var (i, j, k) < region_min .or. var (i, j, k) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        end do
        end do
        
    
    end subroutine check_3d_array_region_r8
    !---------------------------------------------------------------------
    !              end of subroutine check_3d_array_region_r8
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine check_4d_array_region_int
    !---------------------------------------------------------------------
    subroutine check_4d_array_region_int(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        integer (i4), intent (in), dimension (dim_count (1), dim_count (2), dim_count (3), dim_count (4)) :: var 
        integer (i4), intent (in) :: region_min
        integer (i4), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
        do j = 1, dim_count (2)
        do k = 1, dim_count (3)
        do m = 1, dim_count (4)
            if (var (i, j, k, m) < region_min .or. var (i, j, k, m) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        end do
        end do
        end do
        
    
    end subroutine check_4d_array_region_int
    !---------------------------------------------------------------------
    !              end of subroutine check_4d_array_region_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine check_4d_array_region_r8
    !---------------------------------------------------------------------
    subroutine check_4d_array_region_r8(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        real (r8), intent (in), dimension (dim_count (1), dim_count (2), dim_count (3), dim_count (4)) :: var 
        real (r8), intent (in) :: region_min
        real (r8), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
        do j = 1, dim_count (2)
        do k = 1, dim_count (3)
        do m = 1, dim_count (4)
            if (var (i, j, k, m) < region_min .or. var (i, j, k, m) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        end do
        end do
        end do
        
    
    end subroutine check_4d_array_region_r8
    !---------------------------------------------------------------------
    !              end of subroutine check_4d_array_region_r8
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine check_5d_array_region_int
    !---------------------------------------------------------------------
    subroutine check_5d_array_region_int(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        integer (i4), intent (in), dimension (dim_count (1), dim_count (2), dim_count (3), dim_count (4), dim_count (5)) :: var 
        integer (i4), intent (in) :: region_min
        integer (i4), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
        do j = 1, dim_count (2)
        do k = 1, dim_count (3)
        do m = 1, dim_count (4)
        do n = 1, dim_count (5)
            if (var (i, j, k, m, n) < region_min .or. var (i, j, k, m, n) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        end do
        end do
        end do
        end do
        
    
    end subroutine check_5d_array_region_int
    !---------------------------------------------------------------------
    !              end of subroutine check_5d_array_region_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine check_5d_array_region_r8
    !---------------------------------------------------------------------
    subroutine check_5d_array_region_r8(var_name, var, dim_count, n_d, region_min, region_max)
        integer (i4), intent (in)                  :: n_d
        integer (i4), intent (in), dimension (n_d) :: dim_count
        character (len = *), intent (in)      :: var_name
        real (r8), intent (in), dimension (dim_count (1), dim_count (2), dim_count (3), dim_count (4), dim_count (5)) :: var 
        real (r8), intent (in) :: region_min
        real (r8), intent (in) :: region_max
        
        integer :: i, j, k, m, n

        if (region_min >= region_max) then
            write (*, *) "The 'region_min >= region_max' when checking the "// trim(var_name)//" by check_array_region!" 
            write (*, *) "Program omega will stop due to check_array_region."
            stop
        end if

        do i = 1, dim_count (1) 
        do j = 1, dim_count (2)
        do k = 1, dim_count (3)
        do m = 1, dim_count (4)
        do n = 1, dim_count (5)
            if (var (i, j, k, m, n) < region_min .or. var (i, j, k, m, n) > region_max) then
                write (*, *) "There is at least one point of "//trim (var_name)// &
                             " 'outof' the region [",region_min, region_max, "] checked by check_array_region"
                write (*, *) "Program omega will stop due to check_array_region."
                stop
            end if
        end do
        end do
        end do
        end do
        end do
        
    
    end subroutine check_5d_array_region_r8
    !---------------------------------------------------------------------
    !              end of subroutine check_5d_array_region_r8
    !---------------------------------------------------------------------

end module math_interface
