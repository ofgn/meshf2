! -------------------------------------------------------------------------------------------------------
! @file unstructured_grid.f90
! @brief Module for handling unstructured grids.
! @author ofgn
! @date 2024-07-01
! -------------------------------------------------------------------------------------------------------
module global
    use iso_fortran_env, only: int16, int32, int64, real32, real64, real128
    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan

    implicit none

    integer, parameter :: custom_int = int32
    integer, parameter :: custom_real = real64
    character, parameter :: lf = new_line('a')
    character(len=81) :: divider = repeat('-', 80) // lf

end module global