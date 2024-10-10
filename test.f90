program test
    use unstructured_grid
    use data_structures
    use tetrahedral_mesh
    implicit none

    ! Variables for file paths
    character(len=256) :: node_file, ele_file, vtk_file, vtk_file2, path
    type(UnstructuredGrid) :: m
    type(TetrahedralMesh) :: t
    type(PriorityQueue) :: pq

    path = '/srv/wrk/temp/meshf/example_mesh/'

    node_file = trim(path) // 'inversion_mesh.1.node'
    ele_file = trim(path) // 'inversion_mesh.1.ele'
    vtk_file = trim(path) // 'inversion_mesh.1.vtk'
    vtk_file2 = trim(path) // 'inversion_mesh.2.vtk'

    call m%read_tetgen_node(node_file)
    call m%read_tetgen_ele(ele_file)
    call m%write_vtk_legacy(trim(vtk_file))
    ! call m%write_vtk_legacy(trim(trim(path) // 'bin.vtk'))

    call t%initialise(m)
    call m%reset()
    ! call t%adjacent_list(32010)%print()
    call t%half_edge_collapse(32010, 2614)
    call t%to_unstructured_grid(m)
    

    call m%write_vtk_legacy(trim(vtk_file2))

end program test
