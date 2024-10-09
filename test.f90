program test
    use unstructured_grid
    use data_structures
    use tetrahedral_mesh
    implicit none

    type(UnstructuredGrid) :: mesh

    ! Variables for file paths
    character(len=256) :: node_file, ele_file, vtk_file, path
    type(HashMap) :: map
    integer(kind=custom_int), allocatable :: y(:)
    logical :: found
    type(TetrahedralMesh) :: tet_mesh

    path = '/srv/wrk/temp/example_mesh/'

    ! Set the file paths
    node_file = trim(path) // 'inversion_mesh.1.node'
    ele_file = trim(path) // 'inversion_mesh.1.ele'
    vtk_file = trim(path) // 'inversion_mesh.1.vtk'


    call mesh%read_tetgen_node(node_file)
    call mesh%read_tetgen_ele(ele_file)

    ! ! Initialise the tetrahedral mesh
    call tet_mesh%initialise(mesh)
    call tet_mesh%collapse_edge(1, 3)


    ! call tet_mesh%adjacent_list(1)%print()
   
    ! call mesh%write_vtk_legacy(vtk_file)

    ! ! Perform half-edge collapse
    ! call mesh%build_edges_tetrahedral()

    ! ! Write the simplified mesh to a VTK file
    ! call mesh%write_vtk_legacy("ex.vtk")

    ! print *, 'Simplified mesh written to ', trim(vtk_file)

end program test
