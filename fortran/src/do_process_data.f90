  module do_process
  use postprocessor_datastructure
  contains

  !-------------------------------------------------------------------------------------------------------------------------------------
  function do_process_data(root_tmp)

  implicit none
  integer :: do_process_data
  integer :: c,e,p,t,cc,cc_interval,vc,nx,ny,nz,i
  type(POSTPROCESSORROOT), pointer :: root_tmp

  root_tmp%processed%number_of_dimensions = root_tmp%unprocessed%dim(1)
  root_tmp%processed%domain_size = root_tmp%unprocessed%dsz
  root_tmp%processed%number_of_elements = root_tmp%unprocessed%nel(1)
  root_tmp%processed%number_of_phase = root_tmp%unprocessed%nphs(1)
  root_tmp%processed%number_of_gridpoints = root_tmp%unprocessed%ngrd
  root_tmp%processed%number_of_timesteps = size(root_tmp%unprocessed%tm)
  cc_interval = root_tmp%processed%number_of_dimensions

  SELECT CASE (root_tmp%processed%number_of_dimensions)
  CASE (1)
    root_tmp%processed%number_of_gridpoints(2:3) = 1 !DIMENSIONALITY_ERROR_HANDLER
  CASE (2)
    root_tmp%processed%number_of_gridpoints(3) = 1 !DIMENSIONALITY_ERROR_HANDLER
    ! CASE (3) PROCEED
  END SELECT

  function_value_int = allocate_processed_data(root_tmp)

  root_tmp%processed%names_of_phases = root_tmp%unprocessed%phsnames
  root_tmp%processed%names_of_elements = root_tmp%unprocessed%elnames
  e = 1 !counter-all-data-elementwise
  p = 1 !counter_all_data_pahseswise
  do t = 1 , root_tmp%processed%number_of_timesteps
    root_tmp%processed%timesteps(t)%timestep_index = t
    root_tmp%processed%timesteps(t)%timestep_value = root_tmp%unprocessed%tm(t)
    cc = 1 !counter for coordinates !initializing
    vc = 0 !counter on vertex indexes
    do nx=1, root_tmp%processed%number_of_gridpoints(1)
      do ny=1, root_tmp%processed%number_of_gridpoints(2)
        do nz=1, root_tmp%processed%number_of_gridpoints(3)
          root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%vertex_index = vc
          root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_indexes(:) = (/nx,ny,nz/)

          SELECT CASE (root_tmp%processed%number_of_dimensions)
          CASE (1)
            root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:) = (/root_tmp%unprocessed%fvcc(cc), 0.0D0, 0.0D0/)!adjust_number_of_coordinates_with_dimensio
          CASE (2)
            root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:) = (/root_tmp%unprocessed%fvcc(cc), root_tmp%unprocessed%fvcc(cc+1), 0.0D0/)!adjust_number_of_coordinates_with_dimensio
          CASE (3)
            root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:) = (/root_tmp%unprocessed%fvcc(cc), root_tmp%unprocessed%fvcc(cc+1), root_tmp%unprocessed%fvcc(cc+2)/)!adjust_number_of_coordinates_with_dimensio
          END SELECT

          cc= cc+cc_interval !adjust_number_of_coordinates_with_dimensionality
          !prob_elements
          do i = 1, root_tmp%processed%number_of_elements
            root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(i) = root_tmp%unprocessed%molfrc(e)
            root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%chemical_potentials(i) = root_tmp%unprocessed%chpot(e)
            e=e+1
          enddo
          !prob_phases
          do i = 1, root_tmp%processed%number_of_phase
            root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%phase_fractions(i) = root_tmp%unprocessed%phsfrc(p)
            p=p+1
          enddo
          vc= vc+1
        enddo
      enddo
    enddo
  enddo
  do_process_data = 0
  endfunction do_process_data
  !-------------------------------------------------------------------------------------------------------------------------------------
  function allocate_processed_data(tmp)

  implicit none
  integer :: allocate_processed_data, t,nx,ny,nz
  type(POSTPROCESSORROOT), pointer ::tmp

  allocate(tmp%processed%timesteps(tmp%processed%number_of_timesteps))
  do t = 1 , tmp%processed%number_of_timesteps
    allocate(tmp%processed%timesteps(t)%nodes(tmp%processed%number_of_gridpoints(1),tmp%processed%number_of_gridpoints(2),tmp%processed%number_of_gridpoints(3)))
    do nx=1, tmp%processed%number_of_gridpoints(1)
      do ny=1, tmp%processed%number_of_gridpoints(2)
        do nz=1, tmp%processed%number_of_gridpoints(3)
          allocate(tmp%processed%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(tmp%processed%number_of_elements))
          allocate(tmp%processed%timesteps(t)%nodes(nx,ny,nz)%chemical_potentials(tmp%processed%number_of_elements))
          allocate(tmp%processed%timesteps(t)%nodes(nx,ny,nz)%phase_fractions(tmp%processed%number_of_phase))
        END DO
      END DO
    END DO
  END DO
  allocate_processed_data = 0
  endfunction allocate_processed_data
  !-------------------------------------------------------------------------------------------------------------------------------------
  end module do_process