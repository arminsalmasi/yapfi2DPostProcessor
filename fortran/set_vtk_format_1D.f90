module set_vtk_1D
  use PpDatastruct
  contains
!-------------------------------------------------------------------------------------------------------------------------------------
  function set_vtk_format_1D(PrcTmp,output_path) 
    implicit none
    integer :: set_vtk_format_1D,filetype
    type(PrcDataType) :: PrcTmp 
    character(len=LONGWORD) ::  output_path
  
    !write(*,*) 'write all information in 1 vtk file?\1=Yes\2=Seperatefiles\:'
    !read(*,*) filetype
    !if (filetype==1) then 
      function_value_int = set_AllInOne_vtk_1D(PrcTmp,output_path)   
    !elseif  (filetype==2) then 
    !  function_value_int = set_x_vtk(PrcTmp,output_path)
    !  function_value_int = set_chp_vtk(PrcTmp,output_path)
    !  function_value_int = set_phf_vtk(PrcTmp,output_path)
    !endif  
    set_vtk_format_1D = 0 
  endfunction set_vtk_format_1D
    !-------------------------------------------------------------------------------------------------------------------------------------
  function set_AllInOne_vtk_1D(PrcTmp,output_path)
  implicit none
  integer :: set_AllInOne_vtk_1D
  type(PrcDataType) :: PrcTmp 
  integer :: e , p ,t , nx , ny , nz , number_of_points , number_of_cells, cell_list_size 
  character(len=LONGWORD) :: f_name , f_extension, f_nameheader , element_name, timestep_number , element_number, f_path, tempst,phase_number,phase_name , output_path
  
  do t = 1 , PrcTmp%number_of_timesteps
    !filename
    f_name = ''
    f_nameheader = 'AllInOne_tstp_'
    f_extension='.vtk'
    write (timestep_number, *) t
    f_path = output_path
    f_name = trim(adjustl(f_path))//trim(adjustl(f_nameheader))//trim(adjustl(timestep_number))//trim(adjustl(f_extension))
    OPEN(UNIT=1,file = f_name, FORM="FORMATTED",  ACTION="WRITE")
    !fileheader
    write(1,'(a)')'# vtk DataFile Version 2.0'
    write(1,'(a)') 'All information at time step: '//trim(adjustl(timestep_number))//' \n'
    write(1,'(a)')'ASCII'
    write(1,'(a)')'DATASET STRUCTURED_POINTS'
    !coordintes
    tempst=''
    number_of_points = PrcTmp%number_of_gridpoints(1)*PrcTmp%number_of_gridpoints(2) * PrcTmp%number_of_gridpoints(3)
    write(tempst,*) number_of_points
    tempst = 'POINTS'//' '//(trim(adjustl(tempst)))//' '//'double'
    write(1,'(a)') tempst
    do nx = 1 , PrcTmp%number_of_gridpoints(1)
      do ny = 1 , PrcTmp%number_of_gridpoints(2)
        do nz = 1 , PrcTmp%number_of_gridpoints(3)
          write(1,*) PrcTmp%timestep(t)%node(nx,ny,nz)%xyz_coordinates(:)
        enddo
      enddo
    enddo
    !number of cells
    number_of_cells = (PrcTmp%number_of_gridpoints(1)-1)!*(PrcTmp%number_of_gridpoints(2)-1)
    cell_list_size = number_of_cells * ((2**PrcTmp%number_of_dimensions)+1)
    tempst=''
    write(tempst,*) number_of_cells
    write(1,'(a)',advance="no") 'CELLS '// trim(adjustl(tempst))//' '
    tempst=''
    write(tempst,*) cell_list_size
    write(1,'(a)') trim(adjustl(tempst))
    !vertex index
    do nx = 1 , PrcTmp%number_of_gridpoints(1)-1
        ny = 1 
        nz = 1
        tempst =''
        write(tempst,*) 2**PrcTmp%number_of_dimensions
        write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
        tempst =''
        write(tempst,*) PrcTmp%timestep(t)%node(nx,ny,nz)%vertex_index
        write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
        tempst =''
        write(tempst,*) PrcTmp%timestep(t)%node(nx+1,ny,nz)%vertex_index
        write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
        tempst =''
        !write(tempst,*) PrcTmp%timestep(t)%node(nx+1,ny,nz)%vertex_index
        !write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
        !tempst =''
        !write(tempst,*) PrcTmp%timestep(t)%node(nx+1,ny+1,nz)%vertex_index
        !write(1,'(a)') trim(adjustl(tempst))
    enddo
    !celltype
    tempst =''
    write(tempst,*) number_of_cells
    write(1,'(a)') 'CELL_TYPES'//' '//trim(adjustl(tempst))
    do nx= 1 , number_of_cells
      write(1,'(a)',advance="no") '4 '
    enddo
    !number of points
    write(1,*)
    tempst =''
    write(tempst,*) number_of_points
    write(1,'(a)') 'POINT_DATA'//' '//trim(adjustl(tempst))
    !write scalers of chemical potential and mole fractions
    do e = 1  ,  PrcTmp%number_of_elements
      !header line mole fractions
      write (element_number, *) e
      element_name = PrcTmp%names_of_elements(e)
      write(1,'(a)') 'SCALARS X('//trim(adjustl(element_name))//')'//' '//'double 1'
      write(1,'(a)') 'LOOKUP_TABLE default'
      !sclers mole fractions
      do nx = 1 , PrcTmp%number_of_gridpoints(1)
        do ny = 1 , PrcTmp%number_of_gridpoints(2)
          nz = 1
          write(1,'(ES28.16)',advance="no") PrcTmp%timestep(t)%node(nx,ny,nz)%mole_fractions(e)
        enddo
      enddo
      write(1,'(a)')
      !header line chemical potentials
      write(1,'(a)')'SCALARS CHP('//trim(adjustl(element_name))//')'//' '//'double 1'
      write(1,'(a)')'LOOKUP_TABLE default'
      ! scalers chemical potentials
      do nx = 1 , PrcTmp%number_of_gridpoints(1)
        do ny = 1 , PrcTmp%number_of_gridpoints(2)
          nz = 1
          write(1,'(ES28.16)',advance="no") PrcTmp%timestep(t)%node(nx,ny,nz)%chemical_potentials(e)
        enddo
      enddo
      write(1,'(a)')
    enddo
    !write scalers of phase fractions
    do p = 1  ,  PrcTmp%number_of_phase
      !header line pahse fraction
      write (phase_number, *) p
      phase_name = PrcTmp%names_of_phases(p)
      write(1,'(a)')'SCALARS PHF('//trim(adjustl(phase_name))//')'//' '//'double 1'
      write(1,'(a)')'LOOKUP_TABLE default'
      !scalers phase fractions
      do nx = 1 , PrcTmp%number_of_gridpoints(1)
        do ny = 1 , PrcTmp%number_of_gridpoints(2)
          nz = 1
          write(1,'(ES28.16)',advance="no") PrcTmp%timestep(t)%node(nx,ny,nz)%phase_fractions(p)
        enddo
      enddo
      write(1,'(a)')
    enddo
    ! close file
    close(1)
  enddo ! next timestep
  set_AllInOne_vtk_1D = 0
  endfunction set_AllInOne_vtk_1D
  
  end module set_vtk_1D