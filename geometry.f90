! -------------------------------------------------------------------------------------------------------
! @file geometry.f90
! @brief
! @details
! @author ofgn
! @date 2024-10-03
! -------------------------------------------------------------------------------------------------------
module geometry
   use global

   implicit none

contains

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Calculates the orientation of a tetrahedron.
    ! @param[in] a The first vertex of the tetrahedron.
    ! @param[in] b The second vertex of the tetrahedron.
    ! @param[in] c The third vertex of the tetrahedron.
    ! @param[in] d The fourth vertex of the tetrahedron.
    ! @return The orientation of the tetrahedron.
    ! ---------------------------------------------------------------------------------------------------
    function orientation3d(a, b, c, d) result(orientation)
        implicit none

        real(custom_real), intent(in) :: a(3), b(3), c(3), d(3)
        real(real128) :: orientation
        real(real128) :: ax, ay, az, bx, by, bz, cx, cy, cz

        ax = real(a(1), kind=real128) - real(d(1), kind=real128)
        ay = real(a(2), kind=real128) - real(d(2), kind=real128)
        az = real(a(3), kind=real128) - real(d(3), kind=real128)
        bx = real(b(1), kind=real128) - real(d(1), kind=real128)
        by = real(b(2), kind=real128) - real(d(2), kind=real128)
        bz = real(b(3), kind=real128) - real(d(3), kind=real128)
        cx = real(c(1), kind=real128) - real(d(1), kind=real128)
        cy = real(c(2), kind=real128) - real(d(2), kind=real128)
        cz = real(c(3), kind=real128) - real(d(3), kind=real128)

        orientation = ax * (by * cz - bz * cy) &
            - ay * (bx * cz - bz * cx) &
            + az * (bx * cy - by * cx)
    end function orientation3d

end module geometry
