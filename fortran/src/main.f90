  Program refactored_postprocessor

  use postprocessor_datastructure
  use get_input
  use do_process
  use set_vtk

  implicit none

  allocate(root)
  allocate(root%unprocessed)
  allocate(root%processed)
  !allocate(root%processed%current_region)
  !allocate(root%processed%cells)
  !allocate(root%processed%cells%regions)
  !allocate(root%processed%cells%regions%timesteps)

  !write(*,*) 'inpout directory path:'
  !read(*,*) root%input_path
  !write(*,*) 'outout directory path:'
  !read(*,*) root%output_path

  root%input_path ='C:\Users\salmasi\Documents\Mycodes\yapfiPP\test2D\'         !temporary
  root%output_path ='C:\Users\salmasi\Documents\Mycodes\yapfiPP\test2D\vtk\'    !temporary
  write(*,*) '** Reading data from files'

  function_value_int = get_unprocessed_data(root)

  write(*,*) '** Processing data'
  
  SELECT CASE (root%unprocessed%dim(1))
  CASE (1)
    function_value_int = do_process_data_1D(root)
    write(*,*) '** writing to file'
    !function_value_int = set_vtk_format(root)
  CASE (2)
    function_value_int = do_process_data_2D(root)
    write(*,*) '** writing to file'
    function_value_int = set_vtk_format(root)
  CASE (3)
    !function_value_int = do_process_data_3D(root)
    !write(*,*) '** writing to file'
    !function_value_int = set_vtk_format(root)
  END SELECT
  write(*,*) 'end process, press return!'
  read(*,*)
  deallocate(root)

  endprogram refactored_postprocessor