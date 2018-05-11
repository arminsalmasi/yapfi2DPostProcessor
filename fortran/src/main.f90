Program refactored_postprocessor         
  
  use postprocessor_datastructure
  use get_input
  use do_process
  use set_vtk
        
  implicit none
  
  allocate(root)
  allocate(root%unprocessed)
  allocate(root%processed)
  allocate(root%processed%current_region)
  !allocate(root%processed%cells)
  !allocate(root%processed%cells%regions)
  !allocate(root%processed%cells%regions%timesteps)
  
  write(*,*) 'inpout directory path:'
  read(*,*) root%input_path
  write(*,*) 'outout directory path:'
  read(*,*) root%output_path
  root%input_path ='C:\Users\salmasi\Documents\Mycodes\yapfipostprocessor\testdata_working\'         !temporary
  root%output_path ='C:\Users\salmasi\Documents\Mycodes\yapfipostprocessor\testdata_working\vtk\'    !temporary
  write(*,*) '** Reading data from files'
  function_value_int = get_unprocessed_data(root)
  if (root%unprocessed%dim(1) == 2) then 
    write(*,*) '** Processing data'
    function_value_int = do_process_data(root)
    write(*,*) '** writing to file'
    function_value_int = set_vtk_format(root)
  else
    write(*,*) '** Error: this code only process 2D YAPFI simulations'
  end if
  write(*,*) 'end process, press return!'
  read(*,*)
  deallocate(root)

endprogram refactored_postprocessor