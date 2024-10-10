! -------------------------------------------------------------------------------------------------------
! @file tetrahedral.f90
! @brief A module specifically for tetrahedral meshes
! @details
! @author ofgn
! @date 2024-10-03
! -------------------------------------------------------------------------------------------------------
module tetrahedral_mesh
    use global
    use utility
    use unstructured_grid
    use data_structures
    use geometry

    implicit none

    type :: TetrahedralMesh
        integer(kind=custom_int) :: n_vertices                          ! Number of vertices in the grid
        integer(kind=custom_int) :: n_tetra                             ! Number of tetrahedra in the grid
        real(kind=custom_real), allocatable :: vertices(:, :)           ! Coordinates of each point
        integer(kind=custom_int), allocatable :: tetra(:, :)            ! Cell connectivity
        type(HashMap) :: adjacent_map                                   !
        type(LinkedList), allocatable :: adjacent_list(:)               !
        type(HashMap) :: edge_map                                       !
        logical(2) :: valid = .false.                                   !
    contains
        procedure :: initialise => initialise_tetrahedral_mesh          ! Initialise the tetrahedral mesh
        procedure :: half_edge_collapse
        procedure :: adjacent
        procedure :: delete_tetra
        ! procedure :: adjacent_vertex
        procedure :: to_unstructured_grid
    end type TetrahedralMesh

contains

    subroutine initialise_tetrahedral_mesh(self, u_grid)
        implicit none

        class(TetrahedralMesh), intent(inout) :: self
        type(UnstructuredGrid), intent(in) :: u_grid
        integer(kind=custom_int) :: i
        integer(kind=custom_int) :: u, v, w, x
        real(real128) :: orientation
        real(real64) :: start_time, end_time                    ! Timing variables

        call cpu_time(start_time)
        call report(banner("CALCULATING TOPOLOGY"))

        self%n_vertices = u_grid%n_points
        self%n_tetra = u_grid%n_cells
        allocate (self%vertices(3, self%n_vertices))
        allocate (self%tetra(4, self%n_tetra))

        self%vertices = u_grid%points
        self%tetra = reshape(u_grid%cell_connectivity, [4, self%n_tetra]) + 1

        call self%adjacent_map%initialise(4 * self%n_tetra)
        call self%edge_map%initialise(3 * self%n_vertices)
        allocate (self%adjacent_list(self%n_vertices))

        do i = 1, self%n_tetra
            u = self%tetra(1, i)
            v = self%tetra(2, i)
            w = self%tetra(3, i)
            x = self%tetra(4, i)

            orientation = orientation3d( &
                          self%vertices(:, u), &
                          self%vertices(:, v), &
                          self%vertices(:, w), &
                          self%vertices(:, x))

            if (orientation > 0.0_real128) then
                self%tetra(:, i) = [u, v, w, x]
                call self%adjacent_map%insert([u, v, w], [x])
                call self%adjacent_map%insert([v, w, x], [u])
                call self%adjacent_map%insert([w, x, u], [v])
                call self%adjacent_map%insert([x, u, v], [w])

                call self%edge_map%insert([min(u,v), max(u,v)], [0])
                call self%edge_map%insert([min(v,w), max(v,w)], [0])
                call self%edge_map%insert([min(w,u), max(w,u)], [0])
                call self%edge_map%insert([min(u,x), max(u,x)], [0])
                call self%edge_map%insert([min(v,x), max(v,x)], [0])
                call self%edge_map%insert([min(w,x), max(w,x)], [0])
            else
                self%tetra(:, i) = [u, w, v, x]
                call self%adjacent_map%insert([u, w, v], [x])
                call self%adjacent_map%insert([w, v, x], [u])
                call self%adjacent_map%insert([v, x, u], [w])
                call self%adjacent_map%insert([x, u, w], [v])

                call self%edge_map%insert([min(u,w), max(u,w)], [0])
                call self%edge_map%insert([min(w,v), max(w,v)], [0])
                call self%edge_map%insert([min(v,u), max(v,u)], [0])
                call self%edge_map%insert([min(u,x), max(u,x)], [0])
                call self%edge_map%insert([min(w,x), max(w,x)], [0])
                call self%edge_map%insert([min(v,x), max(v,x)], [0])
            end if

            call self%adjacent_list(u)%prepend(i)
            call self%adjacent_list(v)%prepend(i)
            call self%adjacent_list(w)%prepend(i)
            call self%adjacent_list(x)%prepend(i)
        end do

        self%valid = .true.

        call cpu_time(end_time)
        call report("- Status:                       Completed in " &
                    //trim(rtoa(end_time - start_time, decimal_places=4))//" seconds")
        call report(divider)
    end subroutine

    function adjacent(self, u, v, x) result(w)
        implicit none

        class(TetrahedralMesh), intent(in) :: self
        integer(kind=custom_int), intent(in) :: u, v, x
        integer(kind=custom_int) :: w
        integer(kind=custom_int), allocatable :: value(:)

        if (self%adjacent_map%get([u, v, x], value)) then
            w = value(1)
        else
            w = 0
        end if
    end function adjacent

    subroutine delete_tetra(self, i)
        implicit none

        class(TetrahedralMesh), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: i
        integer(kind=custom_int) :: u, v, w, x

        self%tetra(:, i) = 0

        self%n_tetra = self%n_tetra - 1

        u = self%tetra(1, i)
        v = self%tetra(2, i)
        w = self%tetra(3, i)
        x = self%tetra(4, i)

        call self%adjacent_map%delete([u, v, w])
        call self%adjacent_map%delete([v, w, x])
        call self%adjacent_map%delete([w, x, u])
        call self%adjacent_map%delete([x, u, v])
    end subroutine

    subroutine half_edge_collapse(self, u, v)
        implicit none

        class(TetrahedralMesh), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: u, v
        integer (kind=custom_int) :: w, x
        integer(kind=int16) :: i, j, k
        type(ListNode) :: current_node
        integer(kind=custom_int) :: current_cell

        if (.not. self%valid) then
            call report("Error: The mesh is not valid.", is_error=.true.)
            return
        end if

        current_node = self%adjacent_list(u)%head
        

        i = 1
        do while (i .le. self%adjacent_list(u)%size)
            current_cell = current_node%value
            if (any(self%tetra(:, current_cell) == v)) then
                call self%delete_tetra(current_cell)
            elseif (any(self%tetra(:, current_cell) == u)) then
                where(self%tetra(:, current_cell) == u)
                    self%tetra(:, current_cell) = v
                end where
            end if

            current_node = current_node%next
            i = i + 1
        end do

    end subroutine half_edge_collapse

    subroutine to_unstructured_grid(self, u_grid)
        implicit none

        class(TetrahedralMesh), intent(in) :: self
        type(UnstructuredGrid), intent(out) :: u_grid
        integer(kind=custom_int) :: i
        logical, allocatable :: mask(:), mask2(:, :)

        u_grid%n_points = self%n_vertices
        mask = .not. all(self%tetra == 0, dim=1)
        mask2 = spread(mask, dim=1, ncopies=4)
        u_grid%n_cells = self%n_tetra
        
        allocate (u_grid%points(3, u_grid%n_points))
        allocate (u_grid%cell_connectivity(4 * u_grid%n_cells))
        allocate (u_grid%cell_types(u_grid%n_cells))
        allocate (u_grid%points_per_cell(u_grid%n_cells))

        u_grid%points = self%vertices
        u_grid%cell_connectivity = pack(self%tetra, mask2) - 1
        u_grid%cell_types = 10
        u_grid%points_per_cell = 4
        
    end subroutine

    ! subroutine to_unstructured_grid(self, u_grid)
    !     implicit none

    !     class(TetrahedralMesh), intent(in) :: self
    !     type(UnstructuredGrid), intent(out) :: u_grid
    !     integer(kind=custom_int) :: i
    !     logical, allocatable :: mask(:), mask2(:, :)

    !     u_grid%n_points = self%n_vertices
    !     mask = .not. all(self%tetra == 0, dim=1)
    !     mask2 = spread(mask, dim=1, ncopies=4)
    !     u_grid%n_cells = count(mask)
        
    !     allocate (u_grid%points(3, u_grid%n_points))
    !     allocate (u_grid%cell_connectivity(4 * u_grid%n_cells))
    !     allocate (u_grid%cell_types(u_grid%n_cells))
    !     allocate (u_grid%points_per_cell(u_grid%n_cells))

    !     u_grid%points = self%vertices
    !     u_grid%cell_connectivity = pack(self%tetra, mask2) - 1
    !     u_grid%cell_types = 10
    !     u_grid%points_per_cell = 4
        
    ! end subroutine

end module tetrahedral_mesh
