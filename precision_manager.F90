module precision_manager
! purpose:
! This module is devised for managing the precision of the data used in Ocean Model Equations with Grid-point Algorithm (OMEGA).
!
!
! record of revisions:
!    date             programmer                description of change  
!==============    ===================   =============================
! 2011/12/22         wenyu huang                original code
! ...
!
!any comments please send to huangwenyu@mail.tsinghua.edu.cn                      
    implicit none
    private
    public i1, i2, i4 
    public r4, r8 
    public len10, len20, len30, len100, len200, len300

    
    integer, parameter :: i1    =   1 
                                    ! integer (i1) represents the integer number in [-128,+127]
    integer, parameter :: i2    =   2 
                                    ! integer (i2) represents the integer number in [-32768,+32767]
    integer, parameter :: i4    =   4 
                                    ! integer (i4) represents the integer number in [-2147483648,+2147483647]
    integer, parameter :: r4    =   4 
                                    ! real (r4) represents the float number in [(+/-)1.18*10^(-38),(+/-)3.4*10^(+38)] with 6~7 significant digits 
    integer, parameter :: r8    =   8 
                                    ! real (r8) represents the float number in [(+/-)2.23*10^(-308),(+/-)1.79*10^(+308)] with 15 significant digits 
    integer, parameter :: len10 =   10
                                    ! the length of strings is set to be 10
    integer, parameter :: len20 =   20
                                    ! the length of strings is set to be 20
    integer, parameter :: len30 =   30
                                    ! the length of strings is set to be 30
    integer, parameter :: len100 =   100
                                    ! the length of strings is set to be 100
    integer, parameter :: len200 =   200
                                    ! the length of strings is set to be 200
    integer, parameter :: len300 =   300
                                    ! the length of strings is set to be 300

end module precision_manager
