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

        ax = a(1) - d(1)
        ay = a(2) - d(2)
        az = a(3) - d(3)
        bx = b(1) - d(1)
        by = b(2) - d(2)
        bz = b(3) - d(3)
        cx = c(1) - d(1)
        cy = c(2) - d(2)
        cz = c(3) - d(3)

        orientation = ax * (by * cz - bz * cy) &
            - ay * (bx * cz - bz * cx) &
            + az * (bx * cy - by * cx)
    end function orientation3d

end module geometry
