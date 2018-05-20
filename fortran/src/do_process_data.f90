  module do_process
  use PpDatastruct
  contains
  !-------------------------------------------------------------------------------------------------------------------------------------
  function do_process_data(UnPrcTmp,PrcTmp)

  implicit none
  integer :: do_process_data
  integer :: c,e,p,t,cc,cc_interval,vc,nx,ny,nz,i
  double precision :: temp_cx, temp_cy, xcc, ycc
  type(PrcDataType) :: PrcTmp
  type(UnPrcDataType) :: UnPrcTmp

  PrcTmp%number_of_dimensions = UnPrcTmp%dim(1)
  PrcTmp%domain_size = UnPrcTmp%dsz
  PrcTmp%number_of_elements =   UnPrcTmp%nel(1)
  PrcTmp%number_of_phase =      UnPrcTmp%nphs(1)
  PrcTmp%number_of_gridpoints = UnPrcTmp%ngrd


  
  cc_interval = PrcTmp%number_of_dimensions     ! interval coordinate for 1D ==2
  PrcTmp%number_of_timesteps = size(UnPrcTmp%tm) !int
  PrcTmp%names_of_phases = UnPrcTmp%phsnames !array_str
  PrcTmp%names_of_elements = UnPrcTmp%elnames !array_str
  !allocate(PrcTmp%timestep(PrcTmp%number_of_timesteps))
  e = 1           !counter-all-data-elementwise
  p = 1           !counter_all_data_pahseswise
  do t = 1 , PrcTmp%number_of_timesteps
    PrcTmp%timestep(t)%timestep_index = t
    PrcTmp%timestep(t)%timestep_value = UnPrcTmp%tm(t)
    cc = 1          !counter for coordinates !initializing
    vc = 0          !counter on vertex indexes
    temp_cx=0
    do nx=1, PrcTmp%number_of_gridpoints(1)
      temp_cy=0
      do ny=1, PrcTmp%number_of_gridpoints(2)
        do nz=1, PrcTmp%number_of_gridpoints(3)
          PrcTmp%timestep(t)%node(nx,ny,nz)%vertex_index = vc
          PrcTmp%timestep(t)%node(nx,ny,nz)%xyz_indexes(:) = (/nx,ny,nz/)
          PrcTmp%timestep(t)%node(nx,ny,nz)%xyz_coordinates(:) = (/UnPrcTmp%fvcc(cc), UnPrcTmp%fvcc(cc+1), 0.0D0/)    !adjust_number_of_coordinates_with_dimensio CC is for 2D
          xcc =  ((PrcTmp%timestep(t)%node(nx,ny,nz)%xyz_coordinates(1)-temp_cx)*2)
          ycc =  ((PrcTmp%timestep(t)%node(nx,ny,nz)%xyz_coordinates(2)-temp_cy)*2)
          temp_cy= temp_cy+ (ycc)
          cc= cc+cc_interval                                                                                                                                                                                        !adjust_number_of_coordinates_with_dimensionality
          !prob_elements
          do i = 1, PrcTmp%number_of_elements
            PrcTmp%timestep(t)%node(nx,ny,nz)%mole_fractions(i) = UnPrcTmp%molfrc(e)
            PrcTmp%timestep(t)%node(nx,ny,nz)%chemical_potentials(i) = UnPrcTmp%chpot(e)
            e=e+1
          enddo
          !prob_phases
          do i = 1, PrcTmp%number_of_phase
            PrcTmp%timestep(t)%node(nx,ny,nz)%phase_fractions(i) = UnPrcTmp%phsfrc(p)
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
  function do_process_data_1D(UnPrcTmp,PrcTmp)

  implicit none
  integer :: do_process_data_1D
  integer :: c,e,p,t,cc,cc_interval,vc,nx,ny,nz,i
  double precision :: temp_cx, temp_cy, xcc, ycc
  type(PrcDataType) :: PrcTmp
  type(UnPrcDataType) :: UnPrcTmp

  PrcTmp%number_of_dimensions = UnPrcTmp%dim(1)
  PrcTmp%domain_size = UnPrcTmp%dsz
  PrcTmp%number_of_elements = UnPrcTmp%nel(1)
  PrcTmp%number_of_phase = UnPrcTmp%nphs(1)
  PrcTmp%number_of_gridpoints = UnPrcTmp%ngrd

  cc_interval = PrcTmp%number_of_dimensions     ! interval coordinate for 1D ==1
  PrcTmp%number_of_timesteps = size(UnPrcTmp%tm) !int
  PrcTmp%names_of_phases = UnPrcTmp%phsnames !array_str
  PrcTmp%names_of_elements = UnPrcTmp%elnames !array_str
  e = 1           !counter-all-data-elementwise
  p = 1           !counter_all_data_pahseswise
  do t = 1 , PrcTmp%number_of_timesteps
    PrcTmp%timestep(t)%timestep_index = t
    PrcTmp%timestep(t)%timestep_value = UnPrcTmp%tm(t)
    cc = 1          !counter for coordinates !initializing
    vc = 0          !counter on vertex indexes
    temp_cx=0
    do nx=1, PrcTmp%number_of_gridpoints(1)
      temp_cy=0
      do ny=1, PrcTmp%number_of_gridpoints(2)
        do nz=1, PrcTmp%number_of_gridpoints(3)
          PrcTmp%timestep(t)%node(nx,ny,nz)%vertex_index = vc
          PrcTmp%timestep(t)%node(nx,ny,nz)%xyz_indexes(:) = (/nx,ny,nz/)
          PrcTmp%timestep(t)%node(nx,ny,nz)%xyz_coordinates(:) = (/UnPrcTmp%fvcc(cc), 0.0D0, 0.0D0/)    !adjust_number_of_coordinates_with_dimensio CC is for 2D
          xcc =  ((PrcTmp%timestep(t)%node(nx,ny,nz)%xyz_coordinates(1)-temp_cx)*2)
          ycc =  ((PrcTmp%timestep(t)%node(nx,ny,nz)%xyz_coordinates(2)-temp_cy)*2)
          temp_cy= temp_cy+ (ycc)
          cc= cc+cc_interval                                                                                                                                                                                        !adjust_number_of_coordinates_with_dimensionality
          !prob_elements
          do i = 1, PrcTmp%number_of_elements
            PrcTmp%timestep(t)%node(nx,ny,nz)%mole_fractions(i) = UnPrcTmp%molfrc(e)
            PrcTmp%timestep(t)%node(nx,ny,nz)%chemical_potentials(i) = UnPrcTmp%chpot(e)
            e=e+1
          enddo
          !prob_phases
          do i = 1, PrcTmp%number_of_phase
            PrcTmp%timestep(t)%node(nx,ny,nz)%phase_fractions(i) = UnPrcTmp%phsfrc(p)
            p=p+1
          enddo
          vc= vc+1
        enddo
      enddo
      temp_cx= temp_cx+ (xcc)
    enddo
  enddo
  do_process_data_1D = 0
  endfunction do_process_data_1D
  !-------------------------------------------------------------------------------------------------------------------------------------

  
  
  
  
  end module do_process