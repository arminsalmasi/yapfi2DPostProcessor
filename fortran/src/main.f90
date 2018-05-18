Program refactored_postprocessor         
  
  use postprocessor_datastructure
  use get_input
  use do_process
  use set_vtk
        
  implicit none
  

  allocate(unprocessed)
  allocate(processed)


  
  !write(*,*) 'inpout directory path:'
  !read(*,*) root%input_path
  !write(*,*) 'outout directory path:'
  !read(*,*) root%output_path
  input_path ='C:\Users\salmasi\Documents\Mycodes\yapfiPP\test2D\'         !temporary
  output_path ='C:\Users\salmasi\Documents\Mycodes\yapfiPP\test2D\vtk\'    !temporary
  write(*,*) '** Reading data from files'
  function_value_int = get_unprocessed_data(unprocessed,input_path)
  if (unprocessed%dim(1) == 2) then 
    write(*,*) '** Processing data'
    function_value_int = do_process_data(unprocessed,processed)
    write(*,*) '** writing to file'
    function_value_int = set_vtk_format(processed,output_path)
  else
    write(*,*) '** Error: this code only process 2D YAPFI simulations'
  end if
  write(*,*) 'end process, press return!'
  read(*,*)
  deallocate(unprocessed)
  deallocate(processed)

endprogram refactored_postprocessor