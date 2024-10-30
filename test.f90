program test
    use unstructured_grid
    use data_structures
    use tetrahedral_mesh
    implicit none

    ! Variables for file paths
    character(len=256) :: node_file, ele_file, vtk_file, vtk_file2, path
    type(UnstructuredGrid) :: m
    type(TetrahedralMesh) :: t
    integer, allocatable :: edge(:)
    real(kind=custom_real) :: priority
    integer, allocatable :: extracted_value(:)
    integer, allocatable :: array(:)
    integer :: i
    

    path = '/srv/wrk/temp/ex/'

    node_file = trim(path) // 'inversion_mesh.1.node'
    ele_file = trim(path) // 'inversion_mesh.1.ele'
    vtk_file = trim(path) // 'inversion_mesh.1.vtk'
    vtk_file2 = trim(path) // 'inversion_mesh.2.vtk'

    call m%read_tetgen_node(node_file)
    call m%read_tetgen_ele(ele_file)
    call m%write_vtk_legacy(trim(vtk_file))

    call t%initialise(m)
    call t%calculate_adjacency()
    call t%calculate_boundary()
    call t%build_edge_queue()

    print*, t%uvw(1)

    ! do i = 1, 2000
    !     call t%edge_queue%pop(edge, priority)
    !     print *, edge, priority
    ! end do

    ! print *, t%boundary(1)


    call m%reset()

    call t%export_u_grid(m)
    call m%write_vtk_legacy(trim(vtk_file2))

end program test
