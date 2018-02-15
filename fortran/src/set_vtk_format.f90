module set_vtk
  use postprocessor_datastructure
  contains
!-------------------------------------------------------------------------------------------------------------------------------------
  function set_vtk_format(root_tmp) 
    implicit none
    integer :: set_vtk_format           
    type(POSTPROCESSORROOT), pointer :: root_tmp  
    
    function_value_int = set_x_vtk(root_tmp)
    function_value_int = set_chp_vtk(root_tmp)
    function_value_int = set_phf_vtk(root_tmp)
    set_vtk_format = 0 
  endfunction set_vtk_format
!-------------------------------------------------------------------------------------------------------------------------------------
  function set_x_vtk(root_tmp) 
    implicit none
    integer :: set_x_vtk           
    type(POSTPROCESSORROOT), pointer :: root_tmp
    integer :: e , t , nx , ny , nz , number_of_points , number_of_cells, cell_list_size
    character(len=LONGWORD) :: f_name , f_extension, f_nameheader , element_name, timestep_number , element_number, f_path, tempst
    
    do e = 1  ,  root_tmp%processed%current_region%number_of_elements
      do t = 1 , root_tmp%processed%current_region%number_of_timesteps
        !filename  
        f_name = ''
        f_nameheader = 'X'
        f_extension='.vtk'
        write (timestep_number, *) t
        write (element_number, *) e
        element_name = root_tmp%processed%current_region%names_of_elements(e)
        f_path = root_tmp%output_path
        f_name = trim(adjustl(f_path))//trim(adjustl(f_nameheader))//'_'//trim(adjustl(element_number))//'_'//trim(adjustl(element_name))//'_'//'timestep'//'_'//trim(adjustl(timestep_number))//trim(adjustl(f_extension))
        OPEN(UNIT=1,file = f_name, FORM="FORMATTED",  ACTION="WRITE")                         
        !fileheader
        write(1,'(a)')'# vtk DataFile Version 2.0'                                      
        write(1,'(a)') 'X('//trim(adjustl(element_name))//') at time step number: '//trim(adjustl(timestep_number))//' \n'
        write(1,'(a)')'ASCII'                                                                           
        write(1,'(a)')'DATASET UNSTRUCTURED_GRID'                                       
        !write_coordintes
        tempst=''
        number_of_points = root_tmp%processed%current_region%number_of_gridpoints(1)*root_tmp%processed%current_region%number_of_gridpoints(2) * root_tmp%processed%current_region%number_of_gridpoints(3)
        write(tempst,*) number_of_points
        tempst = 'POINTS'//' '//(trim(adjustl(tempst)))//' '//'double'                                     
        write(1,'(a)') tempst              
        do nx = 1 , root_tmp%processed%current_region%number_of_gridpoints(1)                                                                 
          do ny = 1 , root_tmp%processed%current_region%number_of_gridpoints(2)
            do nz = 1 , root_tmp%processed%current_region%number_of_gridpoints(3)                            
              write(1,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:)
            enddo                                                                                       
          enddo                                                                                           
        enddo
!        
        SELECT CASE (root_tmp%processed%current_region%number_of_dimensions)
!          CASE (1)
          CASE (2)
            number_of_cells = (root_tmp%processed%current_region%number_of_gridpoints(1)-1)*(root_tmp%processed%current_region%number_of_gridpoints(2)-1)
            cell_list_size = number_of_cells * ((2**root_tmp%processed%current_region%number_of_dimensions)+1)
!          CASE (3)
!          CASE DEFAULT
!            errorflag = .true.
!            goto 10
        END SELECT                   
        tempst=''                                                                                       
        write(tempst,*) number_of_cells           
        write(1,'(a)',advance="no") 'CELLS '// trim(adjustl(tempst))//' '
        tempst=''
        write(tempst,*) cell_list_size
        write(1,'(a)') trim(adjustl(tempst))
        !write_the_rest
        
        select case (root_tmp%processed%current_region%number_of_dimensions)        
!          case (1)                                                    
          case (2)                  
            do nx = 1 , root_tmp%processed%current_region%number_of_gridpoints(1)-1                                                                 
              do ny = 1 , root_tmp%processed%current_region%number_of_gridpoints(2)-1
                nz = 1                      
                tempst =''
                write(tempst,*) 2**root_tmp%processed%current_region%number_of_dimensions
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '                 
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%vertex_index
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '                                      
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny+1,nz)%vertex_index
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '                                      
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx+1,ny,nz)%vertex_index
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '                                      
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx+1,ny+1,nz)%vertex_index 
                write(1,'(a)') trim(adjustl(tempst))                                                                                                                        
              enddo                                                                                           
            enddo                                 
            tempst =''
            write(tempst,*) number_of_cells
            write(1,'(a)') 'CELL_TYPES'//' '//trim(adjustl(tempst))                                                            
            do nx= 1 , number_of_cells
                write(1,'(a)',advance="no") '8 '
            enddo
            write(1,*)                              
            tempst =''
            write(tempst,*) number_of_points
            write(1,'(a)') 'POINT_DATA'//' '//trim(adjustl(tempst))                              
            write(1,'(a)')'SCALARS X('//trim(adjustl(element_name))//')'//' '//'double 1'                              
            write(1,'(a)')'LOOKUP_TABLE default'
            do nx = 1 , root_tmp%processed%current_region%number_of_gridpoints(1)     
              do ny = 1 , root_tmp%processed%current_region%number_of_gridpoints(2)
                nz = 1
                write(1,'(ES28.16)',advance="no") root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(e)
              enddo
            enddo                                        
        
!          case (3)                                                           
!          case default 
!           errorflag = .TRUE.
!           goto 10
       end select
        close(1)
      enddo
    enddo
        
10  if (errorflag) then 
      set_x_vtk = 1
    else
      set_x_vtk = 0
    endif   
  
  endfunction set_x_vtk
!!-------------------------------------------------------------------------------------------------------------------------------------
  function set_chp_vtk(root_tmp)             
    implicit none
    integer :: set_chp_vtk           
    type(POSTPROCESSORROOT), pointer :: root_tmp
    integer :: e,t,nx,ny,nz,number_of_points,number_of_cells, cell_list_size
    character(len=LONGWORD) :: f_name , f_extension, f_nameheader , element_name, timestep_number , element_number, f_path, tempst
            
    do e = 1  ,  root_tmp%processed%current_region%number_of_elements
      do t = 1 , root_tmp%processed%current_region%number_of_timesteps                  
        f_name = ''
        f_nameheader = 'chemp'
        f_extension='.vtk'
        write (timestep_number, *) t
        write (element_number, *) e
        element_name = root_tmp%processed%current_region%names_of_elements(e)
        f_path = root_tmp%output_path
        f_name = trim(adjustl(f_path))//trim(adjustl(f_nameheader))//'_'//trim(adjustl(element_number))//'_'//trim(adjustl(element_name))//'_'//'timestep'//'_'//trim(adjustl(timestep_number))//trim(adjustl(f_extension))
        OPEN(UNIT=1,file = f_name, FORM="FORMATTED",  ACTION="WRITE")                         
        write(1,'(a)')'# vtk DataFile Version 2.0'                                      
        write(1,'(a)') 'CHP('//trim(adjustl(element_name))//') at time step number: '//trim(adjustl(timestep_number))//' \n'
        write(1,'(a)')'ASCII'                                                           
        write(1,'(a)')'DATASET UNSTRUCTURED_GRID'                                       
        tempst=''
        number_of_points = root_tmp%processed%current_region%number_of_gridpoints(1)*root_tmp%processed%current_region%number_of_gridpoints(2) * root_tmp%processed%current_region%number_of_gridpoints(3)
        write(tempst,*) number_of_points
        tempst = 'POINTS'//' '//(trim(adjustl(tempst)))//' '//'double'                                     
        write(1,'(a)') tempst                    
        do nx = 1 , root_tmp%processed%current_region%number_of_gridpoints(1)                                                                 
          do ny = 1 , root_tmp%processed%current_region%number_of_gridpoints(2)
             do nz = 1 , root_tmp%processed%current_region%number_of_gridpoints(3)                            
               write(1,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:)
             enddo                                                                                       
          enddo                                                                                           
        enddo    
        select case (root_tmp%processed%current_region%number_of_dimensions)
!          case (1)
          case (2)
            number_of_cells = (root_tmp%processed%current_region%number_of_gridpoints(1)-1)*(root_tmp%processed%current_region%number_of_gridpoints(2)-1)
            cell_list_size = number_of_cells * ((2**root_tmp%processed%current_region%number_of_dimensions)+1)
!          case (3)
!         case default
!            errorflag = .TRUE.
!            goto 10
        end select
        tempst=''                                                                                       
        write(tempst,*) number_of_cells           
        write(1,'(a)',advance="no") 'CELLS '// trim(adjustl(tempst))//' '
        tempst=''
        write(tempst,*) cell_list_size
        write(1,'(a)') trim(adjustl(tempst))
        select case (root_tmp%processed%current_region%number_of_dimensions)
!          case (1)         
          case (2)
            do nx = 1 , root_tmp%processed%current_region%number_of_gridpoints(1)-1                                                                 
              do ny = 1 , root_tmp%processed%current_region%number_of_gridpoints(2)-1
                nz = 1
                tempst =''
                write(tempst,*) 2**root_tmp%processed%current_region%number_of_dimensions
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' ' 
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%vertex_index
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny+1,nz)%vertex_index
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx+1,ny,nz)%vertex_index
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx+1,ny+1,nz)%vertex_index 
                write(1,'(a)') trim(adjustl(tempst))
              enddo                                                                                           
            enddo   
            tempst =''
            write(tempst,*) number_of_cells
            write(1,'(a)') 'CELL_TYPES'//' '//trim(adjustl(tempst))
            do nx= 1 , number_of_cells
                write(1,'(a)',advance="no") '8 '
            enddo
            write(1,*)
            tempst =''
            write(tempst,*) number_of_points
            write(1,'(a)') 'POINT_DATA'//' '//trim(adjustl(tempst))
            write(1,'(a)')'SCALARS CHP('//trim(adjustl(element_name))//')'//' '//'double 1'
            write(1,'(a)')'LOOKUP_TABLE default'
            do nx = 1 , root_tmp%processed%current_region%number_of_gridpoints(1)     
                do ny = 1 , root_tmp%processed%current_region%number_of_gridpoints(2)
                    nz = 1
                    write(1,'(ES28.16)',advance="no") root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%chemical_potentials(e)
                enddo
            enddo
            
!          case (3)
!          case default
!            errorflag = .true.
!            goto 10
       end select
        close(1)
      enddo
    enddo
10  if (errorflag) then 
      set_chp_vtk = 1
    else
      set_chp_vtk = 0    
    endif   
  endfunction set_chp_vtk
!!-------------------------------------------------------------------------------------------------------------------------------------
  function set_phf_vtk(root_tmp) 
    implicit none
    integer :: set_phf_vtk           
    type(POSTPROCESSORROOT), pointer :: root_tmp
    integer :: p , t , nx , ny , nz , number_of_points , number_of_cells, cell_list_size
    character(len=LONGWORD) :: f_name , f_extension, f_nameheader , phase_name, timestep_number , phase_number, f_path, tempst
      
    do p = 1  ,  root_tmp%processed%current_region%number_of_phase
      do t = 1 , root_tmp%processed%current_region%number_of_timesteps
        f_name = ''
        f_nameheader = 'npm'
        f_extension='.vtk'
        write (timestep_number, *) t
        write (phase_number, *) p
        phase_name = root_tmp%processed%current_region%names_of_phases(p)
        f_path = root_tmp%output_path
        f_name = trim(adjustl(f_path))//trim(adjustl(f_nameheader))//'_'//trim(adjustl(phase_number))//'_'//trim(adjustl(phase_name))//'_'//'timestep'//'_'//trim(adjustl(timestep_number))//trim(adjustl(f_extension))
        OPEN(UNIT=1,file = f_name, FORM="FORMATTED",  ACTION="WRITE")                         
        write(1,'(a)')'# vtk DataFile Version 2.0'                                      
        write(1,'(a)') 'PHF('//trim(adjustl(phase_name))//') at time step number: '//trim(adjustl(timestep_number))//' \n'
        write(1,'(a)')'ASCII'                                                           
        write(1,'(a)')'DATASET UNSTRUCTURED_GRID'                                       
        tempst=''
        number_of_points = root_tmp%processed%current_region%number_of_gridpoints(1)*root_tmp%processed%current_region%number_of_gridpoints(2) * root_tmp%processed%current_region%number_of_gridpoints(3)
        write(tempst,*) number_of_points
        tempst = 'POINTS'//' '//(trim(adjustl(tempst)))//' '//'double'                                     
        write(1,'(a)') tempst
        do nx = 1 , root_tmp%processed%current_region%number_of_gridpoints(1)                                                                 
            do ny = 1 , root_tmp%processed%current_region%number_of_gridpoints(2)
                do nz = 1 , root_tmp%processed%current_region%number_of_gridpoints(3)                            
                    write(1,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:)
                enddo                                                                                       
            enddo                                                                                           
        enddo    
        select case (root_tmp%processed%current_region%number_of_dimensions)
!          case (1) 
          case (2)
            number_of_cells = (root_tmp%processed%current_region%number_of_gridpoints(1)-1)*(root_tmp%processed%current_region%number_of_gridpoints(2)-1)
            cell_list_size = number_of_cells * ((2**root_tmp%processed%current_region%number_of_dimensions)+1)
!          case (3)
!          case default
!              errorflag =.TRUE.
!              goto 10
       end select
        tempst=''                                                                                       
        write(tempst,*) number_of_cells           
        write(1,'(a)',advance="no") 'CELLS '// trim(adjustl(tempst))//' '
        tempst=''
        write(tempst,*) cell_list_size
        write(1,'(a)') trim(adjustl(tempst)) 
        select case (root_tmp%processed%current_region%number_of_dimensions)
!          case (1)
            
          case (2)
            do nx = 1 , root_tmp%processed%current_region%number_of_gridpoints(1)-1                                                                 
              do ny = 1 , root_tmp%processed%current_region%number_of_gridpoints(2)-1
                nz = 1
                tempst =''
                write(tempst,*) 2**root_tmp%processed%current_region%number_of_dimensions
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' ' 
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%vertex_index
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny+1,nz)%vertex_index
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx+1,ny,nz)%vertex_index
                write(1,'(a)',advance="no") trim(adjustl(tempst))//' '
                tempst =''
                write(tempst,*) root_tmp%processed%current_region%timesteps(t)%nodes(nx+1,ny+1,nz)%vertex_index 
                write(1,'(a)') trim(adjustl(tempst))
              enddo                                                                                           
            enddo   
            tempst =''
            write(tempst,*) number_of_cells
            write(1,'(a)') 'CELL_TYPES'//' '//trim(adjustl(tempst))
            do nx= 1 , number_of_cells
              write(1,'(a)',advance="no") '8 '
            enddo
            write(1,*)
            tempst =''
            write(tempst,*) number_of_points
            write(1,'(a)') 'POINT_DATA'//' '//trim(adjustl(tempst))
            write(1,'(a)')'SCALARS PHF('//trim(adjustl(phase_name))//')'//' '//'double 1'
            write(1,'(a)')'LOOKUP_TABLE default'
            do nx = 1 , root_tmp%processed%current_region%number_of_gridpoints(1)     
              do ny = 1 , root_tmp%processed%current_region%number_of_gridpoints(2)
                nz = 1
                write(1,'(ES28.16)',advance="no") root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%phase_fractions(p)
              enddo
            enddo
              
!          case(3)
!          case default
!            errorflag = .true.
!            goto 10
       end select
        close(1)
      enddo
    enddo
10  if (errorflag) then 
      write(*,*) 'dimentions atribute is good for science fiction movies!'                
      set_phf_vtk = 1
    else
        set_phf_vtk = 0    
    endif   
  endfunction set_phf_vtk
!------------------------------------------------------------------------------------------------------------------------------------------------------
end module set_vtk