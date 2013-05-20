module grid_manager
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
    use namelist_manager,  only : num_lon, num_lat, num_lev 
    use namelist_manager,  only : grid_file, lon_name_in_grid_file, lat_name_in_grid_file, thickness_name_in_grid_file
    use nc_read_write_interface, only : nc_read_write_interface_read_var
    use math_interface, only : check_1d_array_order, check_array_region
    

    implicit none
    private
    public grid_manager_init
    public grid_manager_finish
    public lon, lat, lev_t, lev_w, dzt

    real (r8), allocatable, dimension (:) :: lon
                                                    ! longitude used in omega (0 <= lon <= 360, degree measure)
                                                    ! longitude must be in ascending order
    real (r8), allocatable, dimension (:) :: lat
                                                    ! latitude used in omega (-90 <= lat <= 90, degree measure)
                                                    ! latitude must be in ascending order
    real (r8), allocatable, dimension (:) :: lev_t
                                                    ! level for t grid in the vertical direction (negative, m)
                                                    ! lev_t must be in descending order
    real (r8), allocatable, dimension (:) :: lev_w
                                                    ! level for w grid in the vertical direction (negative, m)
                                                    ! lev_w must be in descending order
    real (r8), allocatable, dimension (:) :: dzt 
                                                    ! thickness for t cell in the vertical direction (positive, m)


    contains
    !---------------------------------------------------------------------
    !                          subroutine grid_manager_init
    !---------------------------------------------------------------------
    subroutine grid_manager_init()
        integer (i4) :: i, j, k
        real (r8), parameter :: eps_check     = 1.0d-4
        real (r8), parameter :: lon_min_check = -eps_check 
        real (r8), parameter :: lon_max_check = 360.0d0 + eps_check 
        real (r8), parameter :: lat_min_check = -90.0d0 - eps_check
        real (r8), parameter :: lat_max_check =  90.0d0 + eps_check
        real (r8), parameter :: lev_min_check = -6000.0d0 
        real (r8), parameter :: lev_max_check = eps_check
        real (r8), parameter :: thick_min_check = eps_check 
        real (r8), parameter :: thick_max_check = 1000.0d0
        

        allocate (lon   (num_lon))
        allocate (lat   (num_lat))
        allocate (lev_t (num_lev))
        allocate (lev_w (num_lev+1))
        allocate (dzt   (num_lev))

        call nc_read_write_interface_read_var (lon, grid_file, lon_name_in_grid_file, [1], [num_lon], [num_lon], 1)
        call check_1d_array_order ("lon", lon, num_lon, "ascending")
        call check_array_region   ("lon", lon, [num_lon], 1, lon_min_check, lon_max_check)

        call nc_read_write_interface_read_var (lat, grid_file, lat_name_in_grid_file, [1], [num_lat], [num_lat], 1)
        call check_1d_array_order ("lat", lat, num_lat, "ascending")
        call check_array_region   ("lat", lat, [num_lat], 1, lat_min_check, lat_max_check)

        call nc_read_write_interface_read_var (dzt, grid_file, thickness_name_in_grid_file, [1], [num_lev], [num_lev], 1)
        call check_1d_array_order ("dzt", dzt, num_lev, "ascending")
        call check_array_region   ("dzt", dzt, [num_lev], 1, thick_min_check, thick_max_check)

        do k = 1, num_lev + 1
            if (k == 1) then
                lev_w (k) = 0.0d0
            else
                lev_w (k) = lev_w (k - 1) - dzt (k - 1) 
            end if
        end do
        call check_1d_array_order ("lev_w", lev_w, num_lev + 1, "descending")
        call check_array_region   ("lev_w", lev_w, [num_lev + 1], 1, lev_min_check, lev_max_check)

        do k = 1, num_lev
            lev_t (k) = 0.5d0*(lev_w (k) + lev_w (k+1)) 
        end do
        call check_1d_array_order ("lev_t", lev_t, num_lev, "descending")
        call check_array_region   ("lev_t", lev_t, [num_lev], 1, lev_min_check, lev_max_check)

    
    end subroutine grid_manager_init
    !---------------------------------------------------------------------
    !                      end of subroutine grid_manager_init
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          subroutine grid_manager_finish
    !---------------------------------------------------------------------
    subroutine grid_manager_finish()
        deallocate (lon)
        deallocate (lat)
        deallocate (lev_t)
        deallocate (lev_w)
        deallocate (dzt)
    
    end subroutine grid_manager_finish
    !---------------------------------------------------------------------
    !                      end of subroutine grid_manager_finish
    !---------------------------------------------------------------------

end module grid_manager
