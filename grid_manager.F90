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
        integer :: i, j, k

        allocate (lon   (num_lon))
        allocate (lat   (num_lat))
        allocate (lev_t (num_lev))
        allocate (lev_w (num_lev+1))
        allocate (dzt   (num_lev))

        call nc_read_write_interface_read_var (lon, grid_file, lon_name_in_grid_file, [1], [num_lon], [num_lon], 1)
        call check_1d_array_order ("lon", lon, num_lon, "ascending")
        call check_array_region ("lon", lon, [num_lon], 1, -1.d-4, 310.0d0)

        call nc_read_write_interface_read_var (lat, grid_file, lat_name_in_grid_file, [1], [num_lat], [num_lat], 1)
        call check_1d_array_order ("lat", lat, num_lat, "ascending")

        call nc_read_write_interface_read_var (dzt, grid_file, thickness_name_in_grid_file, [1], [num_lev], [num_lev], 1)
        call check_1d_array_order ("dzt", dzt, num_lev, "ascending")

        do k = 1, num_lev + 1
            if (k == 1) then
                lev_w (k) = 0.0d0
            else
                lev_w (k) = lev_w (k - 1) - dzt (k - 1) 
            end if
        end do
        call check_1d_array_order ("lev_w", lev_w, num_lev + 1, "descending")

        do k = 1, num_lev
            lev_t (k) = 0.5d0*(lev_w (k) + lev_w (k+1)) 
        end do
        call check_1d_array_order ("lev_t", lev_t, num_lev, "descending")

        print *, lev_w 
        print *, lev_t
    
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
