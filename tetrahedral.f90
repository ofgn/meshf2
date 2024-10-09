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

    type :: UpdateNode
        integer(kind=custom_int) :: u                                  ! Vertex u                      
        integer(kind=custom_int) :: v                                  ! Vertex v             
        integer(kind=custom_int), allocatable :: inserted_vertices(:)  ! New vertices created in the refinement
        integer(kind=custom_int), allocatable :: removed_tetra(:)      ! Tetrahedra removed by this update
        integer(kind=custom_int), allocatable :: inserted_tetra(:)     ! Tetrahedra inserted by this update
        real(kind=custom_real) :: error                                ! Approximation error for this update
    end type UpdateNode

    type :: UpdateGraph
        integer(kind=custom_int) :: n_updates                          
    end type UpdateGraph


    type :: TetrahedralMesh
        integer(kind=custom_int) :: n_vertices                          ! Number of vertices in the grid
        integer(kind=custom_int) :: n_tetra                             ! Number of tetrahedra in the grid
        real(kind=custom_real), allocatable :: vertices(:, :)           ! Coordinates of each point
        integer(kind=custom_int), allocatable :: tetra(:, :)            ! Cell connectivity
        type(HashMap) :: adjacent_map                                   !
        type(LinkedList), allocatable :: adjacent_list(:)               !
        logical(2) :: valid = .false.                                   !
    contains
        procedure :: initialise                                         ! Initialise the tetrahedral mesh
        procedure :: collapse_edge
        ! procedure :: adjacent_face
        ! procedure :: adjacent_vertex
    end type TetrahedralMesh

contains

    ! subroutine reset(self)
    !     implicit none

    !     class(TetrahedralMesh), intent(inout) :: self

    !     deallocate (self%vertices)
    !     deallocate (self%tetra)
    !     call self%adjacent_map%reset()
    !     deallocate (self%adjacent_list)
    ! end subroutine

    subroutine initialise(self, u_grid)
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
        self%tetra = reshape(u_grid%cell_connectivity, [4, self%n_tetra])
        self%tetra = self%tetra + 1

        call self%adjacent_map%initialise(4*self%n_tetra)
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
            else
                self%tetra(:, i) = [u, w, v, x]
                call self%adjacent_map%insert([u, w, v], [x])
                call self%adjacent_map%insert([w, v, x], [u])
                call self%adjacent_map%insert([v, x, u], [w])
                call self%adjacent_map%insert([x, u, w], [v])
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

    subroutine collapse_edge(self, u, v)
        implicit none

        class(TetrahedralMesh), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: u, v
        integer(kind=int16) :: i
        type(ListNode) :: current_node
        integer(kind=custom_int) :: current_cell

        ! if (.not. self%valid) then
        !     call report("Error: The mesh is not valid.", is_error=.true.)
        !     return
        ! end if

        ! current_node = self%adjacent_list(u)%head

        ! i = 1
        ! do while (i .le. self%adjacent_list(u)%size)
        !     current_cell = current_node%value

        !     current_node = current_node%next
        !     i = i + 1
        ! end do

    end subroutine collapse_edge

end module tetrahedral_mesh
