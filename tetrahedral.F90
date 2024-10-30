! -------------------------------------------------------------------------------------------------------
! @file tetrahedral.f90
! @brief Module for handling tetrahedral mesh operations.
! @author ofgn
! @date 2024-10-03
! -------------------------------------------------------------------------------------------------------
module tetrahedral_mesh
    use global
    use utility
    use unstructured_grid
    use data_structures

    implicit none

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines a tetrahedral mesh.
    ! @details Stores information on vertices, tetrahedral connectivity, edge queue, and adjacency map
    !          to perform mesh manipulations and optimisations.
    ! ---------------------------------------------------------------------------------------------------
    type :: TetrahedralMesh
        integer(kind=custom_int) :: n_vertices                                  ! Number of vertices in the grid
        integer(kind=custom_int) :: n_tetra                                     ! Number of tetrahedra in the grid
        integer(kind=custom_int) :: n_edges                                     ! Number of edges in the grid
        real(kind=custom_real), allocatable :: vertices(:, :)                   ! Coordinates of each vertex (3 x n_vertices)
        integer(kind=custom_int), allocatable :: tetrahedra(:, :)               ! Cell connectivity (4 x n_tetra)
        logical, allocatable :: boundary(:)                                     ! Boundary vertices
        type(HashMap) :: adjacency_map                                          ! Hash map for O(1) adjacency lookups
        type(MinHeap) :: edge_queue                                             ! Min-heap for edge collapses
    contains
        procedure :: initialise => initialise_tetrahedral_mesh                  ! Initialise the tetrahedral mesh
        procedure :: build_adjacency_map                                        ! Build the adjacency map
        procedure :: calculate_boundary                                         ! Calculate the boundary of the mesh
        procedure :: calculate_domain_error                                     ! Calculate the domain error of the mesh
        procedure :: build_edge_queue                                           ! Build the edge queue
        procedure :: queue_edge                                                 ! Queue an edge for collapse
        procedure :: distance                                                   ! Calculate the length of an edge
        procedure :: face_lookup                                                ! Look up a face in the adjacency map
        procedure :: adjacent                                                   ! Find adjacent tetrahedron
        procedure :: export_u_grid => export_u_grid                             ! Export to unstructured grid
    end type TetrahedralMesh

    private :: adjacent

contains

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Initialise the tetrahedral mesh using an unstructured grid.
    ! @param[inout] self The tetrahedral mesh to initialise.
    ! @param[in] u_grid The unstructured grid containing vertices and connectivity.
    ! @details Copies vertices and cell connectivity from the unstructured grid to set up the mesh.
    ! ---------------------------------------------------------------------------------------------------
    subroutine initialise_tetrahedral_mesh(self, u_grid)
        implicit none

        class(TetrahedralMesh), intent(inout) :: self                           ! Tetrahedral mesh structure
        type(UnstructuredGrid), intent(in) :: u_grid                            ! Input unstructured grid
        integer(kind=custom_int) :: i                                           ! Loop variable
        integer(kind=custom_int) :: u, v, w, x                                  ! Vertex indices for a tetrahedron
        real(real128) :: orientation                                            ! Orientation of tetrahedron
        real(real64) :: start_time, end_time                                    ! Timing variables

        call cpu_time(start_time)
        call report(banner("OPERATION"))
        call report("- Description:      "//"Initialising mesh data structure")

        self%n_vertices = u_grid%n_points
        self%n_tetra = u_grid%n_cells
        allocate (self%vertices(3, self%n_vertices))
        allocate (self%tetrahedra(4, self%n_tetra))

        self%vertices = u_grid%points
        self%tetrahedra = reshape(u_grid%cell_connectivity, &
            [4, self%n_tetra]) + 1

        call cpu_time(end_time)
        call report("- Completed:        "// &
                    trim(rtoa(end_time - start_time, decimal_places=4))// &
                    "s" // lf)
    end subroutine initialise_tetrahedral_mesh

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Build an adjacency map for the tetrahedral mesh.
    ! @param[inout] self The tetrahedral mesh.
    ! @details Creates relationships between tetrahedra based on shared faces for fast access to neighbours.
    ! ---------------------------------------------------------------------------------------------------
    subroutine build_adjacency_map(self)
        use geometry

        implicit none

        class(TetrahedralMesh), intent(inout) :: self                           ! Tetrahedral mesh being processed
        integer(kind=custom_int) :: i                                           ! Loop variable
        integer(kind=custom_int) :: u, v, w, x                                  ! Vertex indices for a tetrahedron
        real(real128) :: orientation                                            ! Orientation of tetrahedron
        real(real64) :: start_time, end_time                                    ! Timing variables

        call cpu_time(start_time)
        call report(banner("OPERATION"))
        call report("- Description:      "//"Building adjacency map")

        call self%adjacency_map%initialise(4*self%n_tetra)

        ! Populate adjacency map with each face of tetrahedra
        do i = 1, self%n_tetra
            u = self%tetrahedra(1, i)
            v = self%tetrahedra(2, i)
            w = self%tetrahedra(3, i)
            x = self%tetrahedra(4, i)

            ! Calculate orientation and adjust vertices if necessary
            orientation = orientation3d(self%vertices(:, u), self%vertices(:, v), &
                self%vertices(:, w), self%vertices(:, x))

            if (orientation .lt. 0.0_real128) then
                v = self%tetrahedra(3, i)
                w = self%tetrahedra(2, i)
            end if
            
            self%tetrahedra(:, i) = [u, v, w, x]
            call self%adjacency_map%insert([u, v, w], [i])
            call self%adjacency_map%insert([u, x, v], [i])
            call self%adjacency_map%insert([u, w, x], [i])
            call self%adjacency_map%insert([v, x, w], [i])
        end do

        call cpu_time(end_time)
        call report("- Completed:        "// &
                    trim(rtoa(end_time - start_time, decimal_places=4))// &
                    "s" // lf)
        
    end subroutine build_adjacency_map

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Build a queue of unique edges for the tetrahedral mesh.
    ! @param[inout] self The tetrahedral mesh.
    ! @details Generates a queue of edges for future processing, with unique entries only.
    ! ---------------------------------------------------------------------------------------------------
    subroutine build_edge_queue(self)
        implicit none
    
        class(TetrahedralMesh), intent(inout) :: self                           ! The tetrahedral mesh
        integer(kind=custom_int) :: i                                           ! Loop variable
        integer(kind=custom_int) :: u, v, w, x                                  ! Vertex indices for a tetrahedron
        integer(kind=custom_int) :: temp                                        ! Temporary variable
        real(custom_real) :: uv, uw, ux, vw, vx, wx                             ! Lengths of edges
        type(HashMap) :: unique_edges                                           ! Hash map for unique edges
        integer(kind=custom_int), allocatable :: value(:)                       ! Stores values for map operations
        real(real64) :: start_time, end_time                                    ! Timing variables
    
        call cpu_time(start_time)
        call report(banner("OPERATION"))
        call report("- Description:      "//"Building edge queue")

        call self%edge_queue%initialise(6*self%n_tetra)
        call unique_edges%initialise(6*self%n_tetra)

        ! Populate edge queue with unique edges from each tetrahedron
        do i = 1, self%n_tetra
            u = self%tetrahedra(1, i)
            v = self%tetrahedra(2, i)
            w = self%tetrahedra(3, i)
            x = self%tetrahedra(4, i)

            
        end do

        call unique_edges%clear()

        self%n_edges = self%edge_queue%count

        call cpu_time(end_time)
        call report("- Completed:        "// &
                    trim(rtoa(end_time - start_time, decimal_places=4))// &
                    "s" // lf)
        
    end subroutine build_edge_queue

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Queue an edge for collapse if unique.
    ! @param[inout] self The tetrahedral mesh.
    ! @param[in] u The first vertex of the edge.
    ! @param[in] v The second vertex of the edge.
    ! @details Checks if the edge (u, v) or (v, u) is unique and queues it if so.
    ! ---------------------------------------------------------------------------------------------------
    subroutine queue_edge(self, u, v)
        implicit none

        class(TetrahedralMesh), intent(inout) :: self                           ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: u, v                            ! Edge vertices
        type(HashMap) :: unique_edges                                           ! Hash map for unique edges
        integer(kind=custom_int), allocatable :: value(:)                       ! Map values array
        real(custom_real) :: length                                             ! Length of edge
        logical :: found                                                        ! Flag for edge uniqueness

        if (u == v) then
            return
        end if

        print *, u, v
        found = unique_edges%get([u, v], value)


        ! ! Check uniqueness of edge (u, v)
        ! if (.not. ) then
        !     ! if (.not. unique_edges%get([v, u], value)) then
        !     !     length = self%distance(u, v)
        !     !     if (u < v) then
        !     !         ! call unique_edges%insert([u, v], [1])
        !     !         ! call self%edge_queue%insert([u, v], length)
        !     !     else
        !     !         call unique_edges%insert([v, u], [1])
        !     !         call self%edge_queue%insert([v, u], length)
        !     !     end if
        !     ! end if
        ! end if
    end subroutine queue_edge

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Identify and mark the boundary vertices of the tetrahedral mesh using OpenMP 
    !        for parallelisation.
    ! @param[inout] self The tetrahedral mesh.
    ! @details Iterates over each tetrahedron and uses the adjacency map to identify boundary faces. 
    !          If a face is found on the boundary, its vertices are marked as boundary vertices.
    !          Verifies that the adjacency map has been initialised before proceeding.
    ! ---------------------------------------------------------------------------------------------------
    subroutine calculate_boundary(self)
        use omp_lib
        implicit none
        class(TetrahedralMesh), intent(inout) :: self                           ! The tetrahedral mesh
        integer(kind=custom_int) :: i                                           ! Loop variable
        integer(kind=custom_int) :: u, v, w, x                                  ! Vertex indices for a tetrahedron
        integer(kind=custom_int), allocatable :: value(:)                       ! Array for map retrieval
        real(real64) :: start_time, end_time                                    ! Timing variables
        integer :: num_threads                                                  ! Number of threads for parallel run

        call cpu_time(start_time)
        call report(banner("OPERATION"))
        call report("- Description:      "//"Calculating boundary vertices")

        if (self%adjacency_map%count == 0) then
            call report ("- Error:            " &
            // "Adjacency map not initialised")
            return
        end if

        allocate (self%boundary(self%n_vertices))
        self%boundary = .false.

        num_threads = omp_get_max_threads()
        call report("- Run Mode:         " // "Parallel (OpenMP)")
        call report("- Threads:          " // trim(itoa(num_threads)))

        !$omp parallel do private(i, u, v, w, x, value) shared(self)
        do i = 1, self%n_tetra
            u = self%tetrahedra(1, i)
            v = self%tetrahedra(2, i)
            w = self%tetrahedra(3, i)
            x = self%tetrahedra(4, i)

            if (self%adjacency_map%get([u, w, v], value)) then
                !$omp atomic write
                self%boundary(u) = .true.
                !$omp atomic write
                self%boundary(v) = .true.
                !$omp atomic write
                self%boundary(w) = .true.
            end if
        end do
        !$omp end parallel do

        call cpu_time(end_time)
        call report("- Completed:        "// &
                    trim(rtoa(end_time - start_time, decimal_places=4))// &
                    "s" // lf)
        
    end subroutine calculate_boundary

        ! ---------------------------------------------------------------------------------------------------
    ! @brief Calculate the domain error of the tetrahedral mesh.
    ! @param[in] self The tetrahedral mesh to evaluate.
    ! @param[in] u The first vertex to check.
    ! @param[in] v The second vertex to check.
    ! @return error The calculated domain error.
    ! ---------------------------------------------------------------------------------------------------
    function calculate_domain_error(self, u, v) result(error)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        real(real64) :: error                                                   ! Calculated domain error
        integer(kind=custom_int) :: i                                           ! Loop variable
        integer(kind=custom_int) :: u, v, w, x                                  ! Vertex indices for a tetrahedron
        integer(kind=custom_int), allocatable :: value(:)                       ! Array for map retrieval
        real(real64) :: start_time, end_time                                    ! Timing variables

        call cpu_time(start_time)
        call report(banner("OPERATION"))
        call report("- Description:      "//"Calculating domain error")

        error = 0.0

        ! Calculate domain error based on shared faces
        do i = 1, self%n_tetra
            u = self%tetrahedra(1, i)
            v = self%tetrahedra(2, i)
            w = self%tetrahedra(3, i)
            x = self%tetrahedra(4, i)

            if (self%adjacency_map%get([u, w, v], value)) then
                error = error + 1.0
            end if
        end do

        error = error / real(self%n_tetra, kind=real64)

        call cpu_time(end_time)
        call report("- Status:                       Completed in " &
                    //trim(rtoa(end_time - start_time, decimal_places=4))//" seconds")
        
    end function calculate_domain_error

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Calculate the distance between two vertices.
    ! @param[in] self The tetrahedral mesh.
    ! @param[in] u The first vertex.
    ! @param[in] v The second vertex.
    ! @return length The distance between the two vertices.
    ! ---------------------------------------------------------------------------------------------------
    function distance(self, u, v) result(length)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: u, v                            ! Vertex indices
        real(real64) :: length                                                  ! Calculated distance between vertices

        length = sqrt(sum((self%vertices(:, u) - self%vertices(:, v))**2))
    end function distance

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Look up a face in the adjacency map.
    ! @param[in] self The tetrahedral mesh.
    ! @param[in] u The first vertex of the face.
    ! @param[in] v The second vertex of the face.
    ! @param[in] x The third vertex of the face.
    ! @return tetrahedra The index of the tetrahedron sharing the face.
    ! ---------------------------------------------------------------------------------------------------
    function face_lookup(self, u, v, x) result(tetrahedra)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: u, v, x                         ! Vertices forming the face
        integer(kind=custom_int) :: tetrahedra                                  ! Index of the tetrahedron sharing the face
        integer(kind=custom_int), allocatable :: value(:)                       ! Array for map retrieval

        if (self%adjacency_map%get([u, v, x], value)) then
            tetrahedra = value(1)
        else if (self%adjacency_map%get([v, x, u], value)) then
            tetrahedra = value(1)
        else if (self%adjacency_map%get([x, u, v], value)) then
            tetrahedra = value(1)
        else
            tetrahedra = 0
        end if
    end function face_lookup

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Find the adjacent tetrahedra to a given tetrahedron.
    ! @param[in] self The tetrahedral mesh.
    ! @param[in] index The index of the tetrahedron.
    ! @return adjacent The indices of the adjacent tetrahedra.
    ! ---------------------------------------------------------------------------------------------------
    function adjacent(self, index)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: index                           ! Index of the tetrahedron
        integer(kind=custom_int) :: u, v, w, x                                  ! Vertex indices for tetrahedron
        integer(kind=custom_int) :: adjacent(4)                                 ! Array for adjacent tetrahedra indices

        u = self%tetrahedra(1, index)
        v = self%tetrahedra(2, index)
        w = self%tetrahedra(3, index)
        x = self%tetrahedra(4, index)

        adjacent(1) = self%face_lookup(u, w, v)
        adjacent(2) = self%face_lookup(u, v, x)
        adjacent(3) = self%face_lookup(u, x, w)
        adjacent(4) = self%face_lookup(v, w, x)
    end function adjacent

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Export the tetrahedral mesh to an unstructured grid.
    ! @param[in] self The tetrahedral mesh to export.
    ! @param[out] u_grid The resulting unstructured grid.
    ! @details Converts the internal representation of vertices and tetrahedra to the unstructured grid format.
    ! ---------------------------------------------------------------------------------------------------
    subroutine export_u_grid(self, u_grid)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        type(UnstructuredGrid), intent(out) :: u_grid                           ! Output unstructured grid
        integer(kind=custom_int) :: i                                           ! Loop variable
        logical, allocatable :: mask(:), mask2(:, :)                            ! Masks for packing connectivity

        u_grid%n_points = self%n_vertices
        mask = .not. all(self%tetrahedra == 0, dim=1)
        mask2 = spread(mask, dim=1, ncopies=4)

        u_grid%n_cells = self%n_tetra

        allocate (u_grid%points(3, u_grid%n_points))
        allocate (u_grid%cell_connectivity(4 * u_grid%n_cells))
        allocate (u_grid%cell_types(u_grid%n_cells))
        allocate (u_grid%points_per_cell(u_grid%n_cells))

        u_grid%points = self%vertices
        u_grid%cell_connectivity = pack(self%tetrahedra, mask2) - 1
        u_grid%cell_types = 10
        u_grid%points_per_cell = 4
    end subroutine export_u_grid

end module tetrahedral_mesh
