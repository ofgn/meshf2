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
        type(HashMap) :: adjacency_map                                          ! Hash map for O(1) cell adjacency lookups
        type(LinkedList), allocatable :: adjacency_list(:)                      ! Array of linked lists for O(1) vertex adjacency lookups
        type(HashMap) :: edge_map                                               ! Hash map for unique edges
        type(MinHeap) :: edge_queue                                             ! Min-heap for edge collapses
    contains
        procedure :: initialise => initialise_tetrahedral_mesh                  ! Initialise the tetrahedral mesh
        procedure :: calculate_adjacency                                        ! Build the adjacency map
        procedure :: calculate_boundary                                         ! Calculate the boundary of the mesh
        procedure :: calculate_domain_error                                     ! Calculate the domain error of the mesh
        procedure :: build_edge_queue                                           ! Build the edge queue
        procedure :: queue_edge                                                 ! Queue an edge for collapse
        procedure :: process_tetra                                              ! Process a tetrahedron for the edge queue
        procedure :: exterior_edges                                             ! Check for exterior edges
        procedure :: pseudo_exterior_edges                                      ! Check for pseudo-boundary edges
        procedure :: distance                                                   ! Calculate the length of an edge
        procedure :: face_lookup                                                ! Look up a face in the adjacency map
        procedure :: uvw                                                        ! Get the vertices of a face uvw
        procedure :: uxv                                                        ! Get the vertices of a face uwv
        procedure :: uwx                                                        ! Get the vertices of a face uwx
        procedure :: vwx                                                        ! Get the vertices of a face vwx
        procedure :: boundary_faces                                             ! Check for boundary faces
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
        call meshf_report(banner("OPERATION"))
        call meshf_report("- Description:      "//"Initialising mesh data structure")

        self%n_vertices = u_grid%n_points
        self%n_tetra = u_grid%n_cells
        allocate (self%vertices(3, self%n_vertices))
        allocate (self%tetrahedra(4, self%n_tetra))

        self%vertices = u_grid%points
        self%tetrahedra = reshape(u_grid%cell_connectivity, &
            [4, self%n_tetra]) + 1

        call cpu_time(end_time)
        call meshf_report("- Completed:        "// &
                    trim(rtoa(end_time - start_time, decimal_places=4))// &
                    "s" // lf)
    end subroutine initialise_tetrahedral_mesh

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Build an adjacency map for the tetrahedral mesh.
    ! @param[inout] self The tetrahedral mesh.
    ! @details Creates relationships between tetrahedra based on shared faces for fast access to neighbours.
    ! ---------------------------------------------------------------------------------------------------
    subroutine calculate_adjacency(self)
        use geometry

        implicit none

        class(TetrahedralMesh), intent(inout) :: self                           ! Tetrahedral mesh being processed
        integer(kind=custom_int) :: i                                           ! Loop variable
        integer(kind=custom_int) :: u, v, w, x                                  ! Vertex indices for a tetrahedron
        real(real128) :: orientation                                            ! Orientation of tetrahedron
        integer(kind=custom_int), allocatable :: value(:)                       ! Temporary variable
        real(real64) :: start_time, end_time                                    ! Timing variables

        call cpu_time(start_time)
        call meshf_report(banner("OPERATION"))
        call meshf_report("- Description:      "//"Building adjacency map")

        call self%adjacency_map%initialise(4*self%n_tetra)
        allocate(self%adjacency_list(self%n_vertices))
        call self%edge_map%initialise(6*self%n_tetra)

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


            call self%adjacency_list(u)%append(i)
            call self%adjacency_list(v)%append(i)
            call self%adjacency_list(w)%append(i)
            call self%adjacency_list(x)%append(i)
        end do

        call cpu_time(end_time)
        call meshf_report("- Completed:        "// &
                    trim(rtoa(end_time - start_time, decimal_places=4))// &
                    "s" // lf)
        
    end subroutine calculate_adjacency

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
        integer(kind=custom_int), allocatable :: value(:)                       ! Stores values for map operations
        real(real64) :: start_time, end_time                                    ! Timing variables
    
        call cpu_time(start_time)
        call meshf_report(banner("OPERATION"))
        call meshf_report("- Description:      "//"Building edge queue")

        call self%edge_queue%initialise(6*self%n_tetra)
        call self%edge_map%initialise(6*self%n_tetra)

        ! Populate edge queue with unique edges from each tetrahedron
        do i = 1, self%n_tetra
            u = self%tetrahedra(1, i)
            v = self%tetrahedra(2, i)
            w = self%tetrahedra(3, i)
            x = self%tetrahedra(4, i)

            call self%process_tetra(u, v, w, x)
            
        end do

        call self%edge_map%clear()

        self%n_edges = self%edge_queue%count

        call cpu_time(end_time)
        call meshf_report("- Completed:        "// &
                    trim(rtoa(end_time - start_time, decimal_places=4))// &
                    "s" // lf)
        
    end subroutine build_edge_queue

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Process a tetrahedron to identify and queue unique edges.
    ! @param[inout] self The tetrahedral mesh.
    ! @param[in] u, v, w, x The vertex indices for a tetrahedron.
    ! @param[in] unique_edges The hash map for unique edges.
    ! @details Checks each edge of a tetrahedron for uniqueness and queues it if so.
    ! @reference Platis, N., & Theoharis, T. "Simplification of Vector Fields over Tetrahedral Meshes."
    !            Department of Informatics & Telecommunications, University of Athens.
    !            Utilises half-edge collapses for mesh simplification with domain error thresholds to
    !            preserve boundary integrity and critical points in vector fields.
    ! ---------------------------------------------------------------------------------------------------
    subroutine process_tetra(self, u, v, w, x)
        implicit none

        class(TetrahedralMesh), intent(inout) :: self                           ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: u, v, w, x                      ! Vertex indices for a face
        integer(kind=custom_int) :: tetrahedra                                  ! Index of the tetrahedron sharing the face
        integer :: adjacent(4)                                                  ! Array for adjacent tetrahedra indices
        integer(kind=custom_int), allocatable :: flag(:)                        ! Array for map retrieval
        integer(kind=int16) :: n_boundary_vertices                              ! Number of boundary vertices

        n_boundary_vertices = 0
        if (self%boundary(u)) n_boundary_vertices = n_boundary_vertices + 1
        if (self%boundary(v)) n_boundary_vertices = n_boundary_vertices + 1
        if (self%boundary(w)) n_boundary_vertices = n_boundary_vertices + 1
        if (self%boundary(x)) n_boundary_vertices = n_boundary_vertices + 1

        if (n_boundary_vertices .ne. 0) then
            adjacent = self%adjacent(self%face_lookup(u, v, w))

            
            call self%exterior_edges(u, v, w, x, adjacent)
            call self%pseudo_exterior_edges(u, v, w, x, adjacent)
        end if

    end subroutine process_tetra

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Checks for exterior edges in a tetrahedron.
    ! @param[inout] self     The tetrahedral mesh.
    ! @param[in]    u, v, w, x  Vertex indices for the tetrahedron.
    ! @param[in]    adjacent Indices of adjacent tetrahedra.
    ! @details      Boundary edges are collapsible only if the domain error is 
    !               below a user-defined threshold; this threshold is 0 for planar
    !               boundary fields. Domain error estimation, based on the deviation 
    !               angle of boundary faces, provides a measure of acceptable 
    !               boundary deterioration.
    ! ---------------------------------------------------------------------------------------------------
    subroutine exterior_edges(self, u, v, w, x, adjacent)
        use geometry

        implicit none

        class(TetrahedralMesh), intent(inout) :: self                           ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: u, v, w, x                      ! Vertex indices for a face
        integer(kind=custom_int) :: adjacent(4)                                 ! Array for adjacent tetrahedra indices
        integer(kind=custom_int), allocatable :: affected(:)                    ! Array for affected tetrahedra
        integer(kind=custom_int), allocatable :: flag(:)                        ! Array for map retrieval
        integer(kind=custom_int) :: boundary_faces(3, 4)                        ! Array for boundary faces
        real(real128) :: theta                                                  ! Dihedral angle between faces

        ! If the tetrahedra has more than one boundary face, assign large error
        if (count(adjacent .eq. 0) .gt. 1) then
            call queue_edge(self, u, v, huge(0.0_real64))
            call queue_edge(self, u, w, huge(0.0_real64))
            call queue_edge(self, u, x, huge(0.0_real64))
            call queue_edge(self, v, w, huge(0.0_real64))
            call queue_edge(self, v, x, huge(0.0_real64))
            call queue_edge(self, w, x, huge(0.0_real64))
            return
        end if

        ! Check if uwv is a boundary face
        if (adjacent(1) == 0) then
            affected = self%adjacency_list(u)%get()
            ! print *, u, affected
            ! theta = dihedral_angle(self%vertices(:, u), self%vertices(:, w), &
            !     self%vertices(:, v), self%vertices(:, x))
            ! print *, self%face_lookup(u,v,w), theta
        end if

        ! Check if uvx is a boundary face
        if (adjacent(2) == 0) then
            continue
        end if

        ! Check if uxw is a boundary face
        if (adjacent(3) == 0) then
            continue
        end if

        ! Check if vwx is a boundary face
        if (adjacent(4) == 0) then
            continue
        end if

    end subroutine exterior_edges    

    subroutine pseudo_exterior_edges(self, u, v, w, x, adjacent)
        implicit none

        class(TetrahedralMesh), intent(inout) :: self                           ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: u, v, w, x                      ! Vertex indices for a face
        integer(kind=custom_int) :: adjacent(4)                                 ! Array for adjacent tetrahedra indices
        integer(kind=custom_int), allocatable :: flag(:)                        ! Array for map retrieval

        ! Check if uv is an internal edge with boundary vertices
        if (self%boundary(u) .and. self%boundary(v)) then
            if ((adjacent(1) .ne. 0) .and. (adjacent(2) .ne. 0)) then
                call queue_edge(self, u, v, huge(0.0_real64))
            end if
        end if

        ! Check if uw is an internal edge with boundary vertices
        if (self%boundary(u) .and. self%boundary(w)) then
            if ((adjacent(1) .ne. 0) .and. (adjacent(3) .ne. 0)) then
                call queue_edge(self, u, w, huge(0.0_real64))
            end if
        end if

        ! Check if ux is an internal edge with boundary vertices
        if (self%boundary(u) .and. self%boundary(x)) then
            if ((adjacent(2) .ne. 0) .and. (adjacent(3) .ne. 0)) then
                call queue_edge(self, u, x, huge(0.0_real64))
            end if
        end if

        ! Check if vw is an internal edge with boundary vertices
        if (self%boundary(v) .and. self%boundary(w)) then
            if ((adjacent(1) .ne. 0) .and. (adjacent(4) .ne. 0)) then
                call queue_edge(self, v, w, huge(0.0_real64))
            end if
        end if

        ! Check if vx is an internal edge with boundary vertices
        if (self%boundary(v) .and. self%boundary(x)) then
            if ((adjacent(2) .ne. 0) .and. (adjacent(4) .ne. 0)) then
                call queue_edge(self, v, x, huge(0.0_real64))
            end if
        end if

        ! Check if wx is an internal edge with boundary vertices
        if (self%boundary(w) .and. self%boundary(x)) then
            if ((adjacent(3) .ne. 0) .and. (adjacent(4) .ne. 0)) then
                call queue_edge(self, w, x, huge(0.0_real64))
            end if
        end if

    end subroutine pseudo_exterior_edges

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Queue an edge for collapse if unique.
    ! @param[inout] self The tetrahedral mesh.
    ! @param[in] a The first vertex of the edge.
    ! @param[in] b The second vertex of the edge.
    ! @details Checks if the edge (a, b) or (b, a) is unique and queues it if so.
    ! ---------------------------------------------------------------------------------------------------
    subroutine queue_edge(self, a, b, error)
        implicit none

        class(TetrahedralMesh), intent(inout) :: self                           ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: a, b                            ! Edge vertices
        real(custom_real), intent(in) :: error                                  ! Error for edge collapse
        integer(kind=custom_int), allocatable :: value(:)                       ! Map values array

        logical :: found                                                        ! Flag for edge uniqueness

        if (a == b) then
            call meshf_report("- Warning:          " // "Edge " &
            // trim(itoa(a)) // " - " // trim(itoa(b)) // " is invalid")
            return
        end if

        found = self%edge_map%get([a, b], value)

        if (.not. found) then
            found = self%edge_map%get([b, a], value)
        end if

        ! if (.not. found) then
        !     ! An edge with one boundary vertex is only allowed to collapse toward its boundary vertex.
        !     if (self%boundary(a) .and. (.not. self%boundary(b))) then
        !         error = self%distance(a, b)
        !         call self%edge_map%insert([b, a], [1])
        !         call self%edge_queue%insert([b, a], error)
        !     else
        !         error = self%distance(a, b)
        !         call self%edge_map%insert([a, b], [1])
        !         call self%edge_queue%insert([a, b], error)
        !     end if
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
        integer(kind=custom_int), allocatable :: tetrahedra                     ! Array for map retrieval
        real(real64) :: start_time, end_time                                    ! Timing variables
        integer :: num_threads                                                  ! Number of threads for parallel run

        call cpu_time(start_time)
        call meshf_report(banner("OPERATION"))
        call meshf_report("- Description:      "//"Calculating boundary vertices")

        if (self%adjacency_map%count == 0) then
            call meshf_report ("- Error:            " &
            // "Adjacency map not initialised")
            return
        end if

        allocate (self%boundary(self%n_vertices))
        self%boundary = .false.

        num_threads = omp_get_max_threads()
        call meshf_report("- Run Mode:         " // "Parallel (OpenMP)")
        call meshf_report("- Threads:          " // trim(itoa(num_threads)))

        !$omp parallel do private(i, u, v, w, x, tetrahedra) shared(self)
        do i = 1, self%n_tetra
            u = self%tetrahedra(1, i)
            v = self%tetrahedra(2, i)
            w = self%tetrahedra(3, i)
            x = self%tetrahedra(4, i)

            if (self%face_lookup(u, w, v) == 0) then
                !$omp atomic write
                self%boundary(u) = .true.
                !$omp atomic write
                self%boundary(w) = .true.
                !$omp atomic write
                self%boundary(v) = .true.
            end if

            if (self%face_lookup(u, v, x) == 0) then
                !$omp atomic write
                self%boundary(u) = .true.
                !$omp atomic write
                self%boundary(v) = .true.
                !$omp atomic write
                self%boundary(x) = .true.
            end if

            if (self%face_lookup(u, x, w) == 0) then
                !$omp atomic write
                self%boundary(u) = .true.
                !$omp atomic write
                self%boundary(x) = .true.
                !$omp atomic write
                self%boundary(w) = .true.
            end if

            if (self%face_lookup(v, w, x) == 0) then
                !$omp atomic write
                self%boundary(v) = .true.
                !$omp atomic write
                self%boundary(w) = .true.
                !$omp atomic write
                self%boundary(x) = .true.
            end if
        end do
        !$omp end parallel do

        call cpu_time(end_time)
        call meshf_report("- Completed:        "// &
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
        call meshf_report(banner("OPERATION"))
        call meshf_report("- Description:      "//"Calculating domain error")

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
        call meshf_report("- Status:                       Completed in " &
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
    ! @brief Get the vertices of a face uvw.
    ! @param[in] self The tetrahedral mesh.
    ! @param[in] tetrahedra The index of the tetrahedron.
    ! @return uvw The vertex indices for face uvw.
    ! ---------------------------------------------------------------------------------------------------
    function uvw(self, tetrahedra)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: tetrahedra                      ! Index of the tetrahedron
        integer(kind=custom_int) :: uvw(3)                                      ! Vertex indices for face uvx

        uvw(1) = self%tetrahedra(1, tetrahedra)
        uvw(2) = self%tetrahedra(2, tetrahedra)
        uvw(3) = self%tetrahedra(4, tetrahedra)
    end function uvw

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Get the vertices of a face uxv.
    ! @param[in] self The tetrahedral mesh.
    ! @param[in] tetrahedra The index of the tetrahedron.
    ! @return uxv The vertex indices for face uxv.
    ! ---------------------------------------------------------------------------------------------------
    function uxv(self, tetrahedra)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: tetrahedra                      ! Index of the tetrahedron
        integer(kind=custom_int) :: uxv(3)                                      ! Vertex indices for face uxv

        uxv(1) = self%tetrahedra(1, tetrahedra)
        uxv(2) = self%tetrahedra(3, tetrahedra)
        uxv(3) = self%tetrahedra(4, tetrahedra)
    end function uxv

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Get the vertices of a face uwx.
    ! @param[in] self The tetrahedral mesh.
    ! @param[in] tetrahedra The index of the tetrahedron.
    ! @return uwx The vertex indices for face uwx.
    ! ---------------------------------------------------------------------------------------------------
    function uwx(self, tetrahedra)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: tetrahedra                      ! Index of the tetrahedron
        integer(kind=custom_int) :: uwx(3)                                      ! Vertex indices for face uwx

        uwx(1) = self%tetrahedra(1, tetrahedra)
        uwx(2) = self%tetrahedra(2, tetrahedra)
        uwx(3) = self%tetrahedra(4, tetrahedra)
    end function uwx

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Get the vertices of a face vwx.
    ! @param[in] self The tetrahedral mesh.
    ! @param[in] tetrahedra The index of the tetrahedron.
    ! @return vwx The vertex indices for face vwx.
    ! ---------------------------------------------------------------------------------------------------
    function vwx(self, tetrahedra)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: tetrahedra                      ! Index of the tetrahedron
        integer(kind=custom_int) :: vwx(3)                                      ! Vertex indices for face vwx

        vwx(1) = self%tetrahedra(2, tetrahedra)
        vwx(2) = self%tetrahedra(3, tetrahedra)
        vwx(3) = self%tetrahedra(4, tetrahedra)
    end function vwx

    function boundary_faces(self, tetrahedra)
        implicit none

        class(TetrahedralMesh), intent(in) :: self                              ! The tetrahedral mesh
        integer(kind=custom_int), intent(in) :: tetrahedra                      ! Index of the tetrahedron
        integer(kind=custom_int) :: boundary_faces(3, 4)                        ! Array for boundary faces
        integer(kind=custom_int) :: u, v, w, x                                  ! Vertex indices for tetrahedron

        boundary_faces = 0

        u = self%tetrahedra(1, tetrahedra)
        v = self%tetrahedra(2, tetrahedra)
        w = self%tetrahedra(3, tetrahedra)
        x = self%tetrahedra(4, tetrahedra)

        if (self%face_lookup(u, w, v) == 0) then
            boundary_faces(1, 1) = u
            boundary_faces(2, 1) = w
            boundary_faces(3, 1) = v
        end if

        if (self%face_lookup(u, v, x) == 0) then
            boundary_faces(1, 2) = u
            boundary_faces(2, 2) = v
            boundary_faces(3, 2) = x
        end if

        if (self%face_lookup(u, x, w) == 0) then
            boundary_faces(1, 3) = u
            boundary_faces(2, 3) = x
            boundary_faces(3, 3) = w
        end if

        if (self%face_lookup(v, w, x) == 0) then
            boundary_faces(1, 4) = v
            boundary_faces(2, 4) = w
            boundary_faces(3, 4) = x
        end if
    end function boundary_faces

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
