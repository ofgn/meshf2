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
        ! @brief Calculates the dihedral angle between two faces of a tetrahedron.
        ! @param[in] a, b Two shared vertices of the edge between the faces.
        ! @param[in] c Vertex defining the first face.
        ! @param[in] d Vertex defining the second face.
        ! @return The dihedral angle in radians.
        ! ---------------------------------------------------------------------------------------------------
    function dihedral_angle(a, b, c, d) result(angle)
        implicit none

        real(custom_real), intent(in) :: a(3), b(3), c(3), d(3)
        real(custom_real) :: angle
        real(custom_real) :: normal1(3), normal2(3)
        real(custom_real) :: dot_product, magnitude1, magnitude2

        ! Calculate face normals for (a, b, c) and (a, b, d)
        normal1 = cross_product(a - b, c - b)
        normal2 = cross_product(a - b, d - b)

        ! Calculate magnitudes of the normals
        magnitude1 = norm(normal1)
        magnitude2 = norm(normal2)

        ! Avoid division by zero for degenerate faces
        if (magnitude1 == 0.0_custom_real .or. magnitude2 == 0.0_custom_real) then
            angle = 0.0_custom_real
            return
        end if

        ! Normalise the normals
        normal1 = normal1 / magnitude1
        normal2 = normal2 / magnitude2

        ! Calculate the dot product of the normals
        dot_product = dot_product_fn(normal1, normal2)

        ! Clamp the dot product to the range [-1, 1] to avoid numerical errors
        dot_product = max(min(dot_product, 1.0_custom_real), -1.0_custom_real)

        ! Calculate the dihedral angle
        angle = acos(dot_product)
    end function dihedral_angle

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Calculates the cross product of two 3D vectors.
    ! @param[in] u First vector.
    ! @param[in] v Second vector.
    ! @return The cross product of u and v.
    ! ---------------------------------------------------------------------------------------------------
    function cross_product(u, v) result(cross)
        implicit none

        real(custom_real), intent(in) :: u(3), v(3)
        real(custom_real) :: cross(3)

        cross(1) = u(2) * v(3) - u(3) * v(2)
        cross(2) = u(3) * v(1) - u(1) * v(3)
        cross(3) = u(1) * v(2) - u(2) * v(1)
    end function cross_product

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Calculates the dot product of two 3D vectors.
    ! @param[in] u First vector.
    ! @param[in] v Second vector.
    ! @return The dot product of u and v.
    ! ---------------------------------------------------------------------------------------------------
    function dot_product_fn(u, v) result(dot)
        implicit none

        real(custom_real), intent(in) :: u(3), v(3)
        real(custom_real) :: dot

        dot = u(1) * v(1) + u(2) * v(2) + u(3) * v(3)
    end function dot_product_fn

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Calculates the norm (magnitude) of a 3D vector.
    ! @param[in] v The vector.
    ! @return The magnitude of v.
    ! ---------------------------------------------------------------------------------------------------
    function norm(v) result(magnitude)
        implicit none

        real(custom_real), intent(in) :: v(3)
        real(custom_real) :: magnitude

        magnitude = sqrt(v(1) * v(1) + v(2) * v(2) + v(3) * v(3))
    end function norm

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Determines the orientation of a triangle in 3D space relative to the origin.
    ! @param[in] a The first vertex of the triangle.
    ! @param[in] b The second vertex of the triangle.
    ! @param[in] c The third vertex of the triangle.
    ! @return The orientation of the triangle. Positive if oriented counter-clockwise.
    ! ---------------------------------------------------------------------------------------------------
    function orientation2d(a, b, c) result(orientation)
        implicit none

        real(custom_real), intent(in) :: a(3), b(3), c(3)
        real(real128) :: orientation
        real(real128) :: ax, ay, az, bx, by, bz, cx, cy, cz

        ax = real(a(1), kind=real128)
        ay = real(a(2), kind=real128)
        az = real(a(3), kind=real128)
        bx = real(b(1), kind=real128) - ax
        by = real(b(2), kind=real128) - ay
        bz = real(b(3), kind=real128) - az
        cx = real(c(1), kind=real128) - ax
        cy = real(c(2), kind=real128) - ay
        cz = real(c(3), kind=real128) - az

        orientation = bx * (cy * az - cz * ay) &
                    - by * (cx * az - cz * ax) &
                    + bz * (cx * ay - cy * ax)
    end function orientation2d

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
