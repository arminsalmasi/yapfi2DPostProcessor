  module do_process
  use postprocessor_datastructure
  contains
  !-------------------------------------------------------------------------------------------------------------------------------------
  function do_process_data(unprocessed_tmp,processed_tmp)

  implicit none
  integer :: do_process_data
  integer :: c,e,p,t,cc,cc_interval,vc,nx,ny,nz,i
  double precision :: temp_cx, temp_cy, xcc, ycc
  type(PROCESSEDDATA), pointer :: processed_tmp
  type(UNPROCESSEDDATA), pointer :: unprocessed_tmp

  processed_tmp%number_of_dimensions = unprocessed_tmp%dim(1)
  processed_tmp%domain_size = unprocessed_tmp%dsz
  processed_tmp%number_of_elements = unprocessed_tmp%nel(1)
  processed_tmp%number_of_phase = unprocessed_tmp%nphs(1)
  processed_tmp%number_of_gridpoints = unprocessed_tmp%ngrd



  processed_tmp%number_of_gridpoints(3) = 1   !DIMENSIONALITY_ERROR_HANDLER
  cc_interval = 2                             ! interval coordinate for 2D

  processed_tmp%number_of_timesteps = size(unprocessed_tmp%tm) !int
  processed_tmp%names_of_phases = unprocessed_tmp%phsnames !array_str
  processed_tmp%names_of_elements = unprocessed_tmp%elnames !array_str
  allocate(processed_tmp%timesteps(processed_tmp%number_of_timesteps))
  e = 1           !counter-all-data-elementwise
  p = 1           !counter_all_data_pahseswise
  do t = 1 , processed_tmp%number_of_timesteps
    processed_tmp%timesteps(t)%timestep_index = t
    processed_tmp%timesteps(t)%timestep_value = unprocessed_tmp%tm(t)
    cc = 1          !counter for coordinates !initializing
    vc = 0          !counter on vertex indexes

    allocate(processed_tmp%timesteps(t)%node(processed_tmp%number_of_gridpoints(1),processed_tmp%number_of_gridpoints(2),processed_tmp%number_of_gridpoints(3)))

    temp_cx=0
    do nx=1, processed_tmp%number_of_gridpoints(1)
      temp_cy=0
      do ny=1, processed_tmp%number_of_gridpoints(2)
        do nz=1, processed_tmp%number_of_gridpoints(3)
          processed_tmp%timesteps(t)%node(nx,ny,nz)%vertex_index = vc
          processed_tmp%timesteps(t)%node(nx,ny,nz)%xyz_indexes(:) = (/nx,ny,nz/)

          processed_tmp%timesteps(t)%node(nx,ny,nz)%xyz_coordinates(:) = (/unprocessed_tmp%fvcc(cc), unprocessed_tmp%fvcc(cc+1), 0.0D0/)    !adjust_number_of_coordinates_with_dimensio CC is for 2D
          xcc =  ((processed_tmp%timesteps(t)%node(nx,ny,nz)%xyz_coordinates(1)-temp_cx)*2)
          ycc =  ((processed_tmp%timesteps(t)%node(nx,ny,nz)%xyz_coordinates(2)-temp_cy)*2)
          temp_cy= temp_cy+ (ycc)
          cc= cc+cc_interval                                                                                                                                                                                        !adjust_number_of_coordinates_with_dimensionality


          allocate(processed_tmp%timesteps(t)%node(nx,ny,nz)%mole_fractions(processed_tmp%number_of_elements))
          allocate(processed_tmp%timesteps(t)%node(nx,ny,nz)%chemical_potentials(processed_tmp%number_of_elements))
          allocate(processed_tmp%timesteps(t)%node(nx,ny,nz)%phase_fractions(processed_tmp%number_of_phase))

          !prob_elements
          do i = 1, processed_tmp%number_of_elements
            processed_tmp%timesteps(t)%node(nx,ny,nz)%mole_fractions(i) = unprocessed_tmp%molfrc(e)
            processed_tmp%timesteps(t)%node(nx,ny,nz)%chemical_potentials(i) = unprocessed_tmp%chpot(e)
            e=e+1
          enddo

          !prob_phases
          do i = 1, processed_tmp%number_of_phase
            processed_tmp%timesteps(t)%node(nx,ny,nz)%phase_fractions(i) = unprocessed_tmp%phsfrc(p)
            p=p+1
          enddo
          vc= vc+1
        enddo
      enddo
      temp_cx= temp_cx+ (xcc)
    enddo
  enddo
  do_process_data = 0
  endfunction do_process_data
  !-------------------------------------------------------------------------------------------------------------------------------------
  end module do_process