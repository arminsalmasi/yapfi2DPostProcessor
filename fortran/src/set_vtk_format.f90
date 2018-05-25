  module set_vtk
  use postprocessor_datastructure
  contains
  !-------------------------------------------------------------------------------------------------------------------------------------
  function set_AllInOne_vtk(root_tmp)
  implicit none
  integer :: set_AllInOne_vtk
  type(POSTPROCESSORROOT), pointer :: root_tmp
  integer :: e , p ,t , nx , ny , nz , number_of_points , number_of_cells, cell_list_size, CNTX,CNTY,CNTZ
  character(len=LONGWORD) :: f_name , f_extension, f_nameheader , element_name, timestep_number
  character(len=LONGWORD) :: element_number, f_path, tempst,phase_number,phase_name

  do t = 1 , root_tmp%processed%number_of_timesteps
    !filename
    f_name = ''
    f_nameheader = 'AllInOne_tstp_'
    f_extension='.vtk'
    write (timestep_number, *) t
    f_path = root_tmp%output_path
    f_name = trim(adjustl(f_path))//trim(adjustl(f_nameheader))//trim(adjustl(timestep_number))//trim(adjustl(f_extension))
    OPEN(UNIT=1,file = f_name, FORM="FORMATTED",  ACTION="WRITE")
    !fileheader
    write(1,'(a)')'# vtk DataFile Version 2.0'
    write(1,'(a)') 'All information at time step: '//trim(adjustl(timestep_number))//' \n'
    write(1,'(a)')'ASCII'
    write(1,'(a)')'DATASET UNSTRUCTURED_GRID'
    !coordintes
    tempst=''
    number_of_points = root_tmp%processed%number_of_gridpoints(1)*root_tmp%processed%number_of_gridpoints(2) * root_tmp%processed%number_of_gridpoints(3)
    write(tempst,*) number_of_points
    tempst = 'POINTS'//' '//(trim(adjustl(tempst)))//' '//'double'
    write(1,'(a)') tempst
    do nx = 1 , root_tmp%processed%number_of_gridpoints(1)
      do ny = 1 , root_tmp%processed%number_of_gridpoints(2)
        do nz = 1 , root_tmp%processed%number_of_gridpoints(3)
          write(1,*) root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:)
        enddo
      enddo
    enddo
    !number of cells
    CNTX = root_tmp%processed%number_of_gridpoints(1)-1
    CNTY = root_tmp%processed%number_of_gridpoints(2)-1
    CNTZ = root_tmp%processed%number_of_gridpoints(3)-1
    SELECT CASE (root_tmp%processed%number_of_dimensions)
    CASE (1)
      number_of_cells = (root_tmp%processed%number_of_gridpoints(1)-1)
      CNTY = 1
      CNTZ = 1
    CASE (2)
      number_of_cells = (root_tmp%processed%number_of_gridpoints(1)-1)*(root_tmp%processed%number_of_gridpoints(2)-1)
      CNTZ = 1
    CASE (3)
      number_of_cells = (root_tmp%processed%number_of_gridpoints(1)-1)*(root_tmp%processed%number_of_gridpoints(2)-1)*(root_tmp%processed%number_of_gridpoints(3)-1) !*#
      !PROCEED
    END SELECT


    cell_list_size = number_of_cells * ((2**root_tmp%processed%number_of_dimensions)+1)
    tempst=''
    write(tempst,*) number_of_cells
    write(1,'(a)',advance="no") 'CELLS '// trim(adjustl(tempst))//' '
    tempst=''
    write(tempst,*) cell_list_size
    write(1,'(a)') trim(adjustl(tempst))
    !vertex index
    do nx = 1 , CNTX
      do ny = 1 ,CNTY
        do nZ = 1 ,CNTZ
          SELECT CASE (root_tmp%processed%number_of_dimensions)

          CASE(1)
            tempst =''
            write(tempst,*) 2**root_tmp%processed%number_of_dimensions
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx+1,ny,nz)%vertex_index
            write(1,'(a)') trim(adjustl(tempst))//' '

          CASE(2)
            tempst =''
            write(tempst,*) 2**root_tmp%processed%number_of_dimensions
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx,ny+1,nz)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx+1,ny,nz)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx+1,ny+1,nz)%vertex_index
            write(1,'(a)') trim(adjustl(tempst))

          CASE(3)
            tempst =''
            write(tempst,*) 2**root_tmp%processed%number_of_dimensions
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''

            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx,ny,nz+1)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx,ny+1,nz)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx,ny+1,nz+1)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '

            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx+1,ny,nz)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx+1,ny,nz+1)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx+1,ny+1,nz)%vertex_index
            write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
            tempst =''
            write(tempst,*) root_tmp%processed%timesteps(t)%nodes(nx+1,ny+1,nz+1)%vertex_index
            write(1,'(a)') trim(adjustl(tempst))



          END SELECT
        enddo
      END DO
    enddo
    !celltype
    tempst =''
    write(tempst,*) number_of_cells
    write(1,'(a)') 'CELL_TYPES'//' '//trim(adjustl(tempst))
    SELECT CASE (root_tmp%processed%number_of_dimensions)
    CASE (1)
      do nx= 1 , number_of_cells
        write(1,'(a)',advance="no") '4 '
      ENDDO
    CASE(2)
      DO nx= 1 , number_of_cells
        WRITE(1,'(a)',advance="no") '8 '
      ENDDO
    CASE(3)
      DO nx= 1 , number_of_cells
        WRITE(1,'(a)',advance="no") '11 '
      ENDDO
    END SELECT
    !number of points
    write(1,*)
    tempst =''
    write(tempst,*) number_of_points
    write(1,'(a)') 'POINT_DATA'//' '//trim(adjustl(tempst))
    !write scalers of chemical potential and mole fractions
    do e = 1  ,  root_tmp%processed%number_of_elements
      !header line mole fractions
      write (element_number, *) e
      element_name = root_tmp%processed%names_of_elements(e)
      write(1,'(a)') 'SCALARS X('//trim(adjustl(element_name))//')'//' '//'double 1'
      write(1,'(a)') 'LOOKUP_TABLE default'

      CNTX = root_tmp%processed%number_of_gridpoints(1)
      CNTY = root_tmp%processed%number_of_gridpoints(2)
      CNTZ = root_tmp%processed%number_of_gridpoints(3)
      SELECT CASE (root_tmp%processed%number_of_dimensions)
      CASE (1)
        CNTY = 1
        CNTZ = 1
      CASE (2)
        CNTZ = 1
      CASE (3)
        ! PROCEED
      END SELECT

      !sclers mole fractions
      do nx = 1 , root_tmp%processed%number_of_gridpoints(1)
        do ny = 1 , root_tmp%processed%number_of_gridpoints(2)
          do nZ = 1 , root_tmp%processed%number_of_gridpoints(3)
            write(1,'(ES28.16)',advance="no") root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(e)
          ENDDO
        enddo
      enddo
      write(1,'(a)')
      !header line chemical potentials
      write(1,'(a)')'SCALARS CHP('//trim(adjustl(element_name))//')'//' '//'double 1'
      write(1,'(a)')'LOOKUP_TABLE default'
      ! scalers chemical potentials
      do nx = 1 , root_tmp%processed%number_of_gridpoints(1)
        do ny = 1 , root_tmp%processed%number_of_gridpoints(2)
          do nZ = 1 , root_tmp%processed%number_of_gridpoints(3)
            write(1,'(ES28.16)',advance="no") root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%chemical_potentials(e)
          enddo
        ENDDO
      enddo
      write(1,'(a)')
    enddo
    !write scalers of phase fractions
    do p = 1  ,  root_tmp%processed%number_of_phase
      !header line pahse fraction
      write (phase_number, *) p
      phase_name = root_tmp%processed%names_of_phases(p)
      write(1,'(a)')'SCALARS PHF('//trim(adjustl(phase_name))//')'//' '//'double 1'
      write(1,'(a)')'LOOKUP_TABLE default'
      !scalers phase fractions
      do nx = 1 , root_tmp%processed%number_of_gridpoints(1)
        do ny = 1 , root_tmp%processed%number_of_gridpoints(2)
          do nZ = 1 , root_tmp%processed%number_of_gridpoints(3)
            write(1,'(ES28.16)',advance="no") root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%phase_fractions(p)
          enddo
        ENDDO
      enddo
      write(1,'(a)')
    enddo
    ! close file
    close(1)
  enddo ! next timestep
  set_AllInOne_vtk = 0
  endfunction set_AllInOne_vtk

  !-------------------------------------------------------------------------------------------------------------------------------------

  end module set_vtk