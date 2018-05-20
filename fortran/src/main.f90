  Program refactored_postprocessor

  use PpDatastruct
  use get_input
  use do_process
  use set_vtk
  use set_vtk_1D

  implicit none


  integer :: t, nx,ny,nz




  !write(*,*) 'inpout directory path:'
  !read(*,*) root%input_path
  !write(*,*) 'outout directory path:'
  !read(*,*) root%output_path
  input_path ='C:\Users\salmasi\Documents\Mycodes\yapfiPP\test2D\'         !temporary
  output_path ='C:\Users\salmasi\Documents\Mycodes\yapfiPP\test2D\vtk\'    !temporary
  write(*,*) '** Reading data from files'
  function_value_int = get_unprocessed_data(UNPRC,input_path)


  !allocation
  write(*,*) '** Allocation!'
  allocate(PRC%timestep(size(UNPRC%tm)))
  select case (UNPRC%dim(1))
  case(1)
    UNPRC%ngrd(2:3) =1
  case(2)
    UNPRC%ngrd(3) =1
  case(3)
  end select
  do t = 1 , size(UNPRC%tm)
    allocate(PRC%timestep(t)%node(UnPrc%ngrd(1),UnPrc%ngrd(2),UnPrc%ngrd(3)))
    do nx=1, UNPRC%ngrd(1)
      do ny=1, UNPRC%ngrd(2)
        do nz=1, UNPRC%ngrd(3)
          allocate(PRC%timestep(t)%node(nx,ny,nz)%mole_fractions(UnPrc%nel(1)))
          allocate(PRC%timestep(t)%node(nx,ny,nz)%chemical_potentials(UnPrc%nel(1)))
          allocate(PRC%timestep(t)%node(nx,ny,nz)%phase_fractions(UnPrc%nphs(1)))
        end do
      end do
    end do
  end do
  ! end allocation






  write(*,*) '** Processing data'
  SELECT CASE (UNPRC%dim(1) )
  CASE (1)
    function_value_int = do_process_data_1D(UNPRC,PRC)
    function_value_int = set_vtk_format_1D(PRC,output_path)
    WRITE(*,*) '** 1D Process finished'

  CASE (2)
    function_value_int = do_process_data(UNPRC,PRC)
    function_value_int = set_vtk_format(PRC,output_path)
    WRITE(*,*) '** 3D Process finished'

  CASE (3 )
    function_value_int = do_process_data_1D(UNPRC,PRC)
    WRITE(*,*) '** 3D Process finished'
  END SELECT

  write(*,*) 'end process, press return!'

  read(*,*)


  endprogram refactored_postprocessor