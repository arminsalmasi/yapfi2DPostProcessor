  Program refactored_postprocessor

  use postprocessor_datastructure
  use get_input
  use do_process
  use set_vtk

  implicit none

  allocate(root)
  allocate(root%unprocessed)
  allocate(root%processed)

  !write(*,*) 'inpout directory path:'
  !read(*,*) root%input_path
  !write(*,*) 'outout directory path:'
  !read(*,*) root%output_path

  root%input_path ='C:\Users\salmasi\Documents\Mycodes\yapfiPP\test3D\'         !temporary
  root%output_path ='C:\Users\salmasi\Documents\Mycodes\yapfiPP\test3D\vtk\'    !temporary
  write(*,*) '** Reading data from files'

  function_value_int = get_unprocessed_data(root)

  write(*,*) '** Processing data'
  function_value_int = do_process_data(root)

  
  write(*,*) '** writing to file'
  function_value_int = set_AllInOne_vtk(root)
  
  write(*,*) 'end process, press return!'
  read(*,*)
  deallocate(root)

  endprogram refactored_postprocessor