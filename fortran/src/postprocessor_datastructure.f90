  module postprocessor_datastructure
  public

  !-------------------------------------------------------------------------------------------------------------------------------------
  !-------------------------------------------------------------------------------------------------------------------------------------
  integer :: function_value_int
  integer,parameter::SHORTWORD=40
  integer,parameter::LONGWORD=200
  integer,parameter::HUGEWORD=1000
  logical :: errorflag
  character(len=LONGWORD) :: input_path
  character(len=LONGWORD) :: output_path
  !-------------------------------------------------------------------------------------------------------------------------------------
  !-------------------------------------------------------------------------------------------------------------------------------------
  !3D
  !-------------------------------------------------------------------------------------------------------------------------------------
  type UNPROCESSEDDATA
    doubleprecision, dimension(:), allocatable :: fvcc, molfrc, phsfrc, chpot, tm
    doubleprecision, dimension(3) :: dsz
    integer, dimension(1) :: dim
    integer, dimension(:), allocatable :: nel, nphs
    integer, dimension(3) :: ngrd
    character(len=SHORTWORD), dimension(:), allocatable :: elnames, phsnames
  endtype UNPROCESSEDDATA
  !-------------------------------------------------------------------------------------------------------------------------------------
  type nodes
    integer :: vertex_index
    integer, dimension(3) :: xyz_indexes !ngpx_ngpy_ngpz
    doubleprecision, dimension(3) :: xyz_coordinates !FINITE_VOLUME_CENTROID_COORDINATES
    doubleprecision, dimension(:), allocatable :: phase_fractions(:), mole_fractions(:), chemical_potentials(:)
  endtype nodes
  !-------------------------------------------------------------------------------------------------------------------------------------
  type TIMESTEP   !
    doubleprecision :: timestep_value
    integer :: timestep_index
    type(nodes), pointer, dimension(:,:,:) :: node
  endtype TIMESTEP
  !-------------------------------------------------------------------------------------------------------------------------------------
  type PROCESSEDDATA !
    integer :: number_of_dimensions
    doubleprecision, dimension(3) :: domain_size
    integer, dimension(3) :: number_of_gridpoints
    integer :: number_of_elements
    integer :: number_of_phase
    integer :: number_of_timesteps
    character(len=SHORTWORD), dimension(:), allocatable :: names_of_phases
    character(len=SHORTWORD), dimension(:), allocatable :: names_of_elements
    type(TIMESTEP), pointer, dimension(:) :: timesteps
  endtype PROCESSEDDATA
  !-------------------------------------------------------------------------------------------------------------------------------------
  !main_variables_declaration
  !-------------------------------------------------------------------------------------------------------------------------------------
  type(PROCESSEDDATA), pointer :: processed
  type(UNPROCESSEDDATA), pointer :: unprocessed



  !-------------------------------------------------------------------------------------------------------------------------------------
  !-------------------------------------------------------------------------------------------------------------------------------------
  !1D
  !-------------------------------------------------------------------------------------------------------------------------------------
  type UNPROCESSEDDATA_1D
    doubleprecision, dimension(:), allocatable :: fvcc,  molfrc,  phsfrc, chpot, tm, dsz
    integer, dimension(1) :: dim, ngrd
    integer, dimension(:), allocatable :: nel, nphs
    character(len=SHORTWORD), dimension(:), allocatable :: elnames, phsnames
  endtype UNPROCESSEDDATA_1D
  !-------------------------------------------------------------------------------------------------------------------------------------
  type nodes_1D
    integer :: vertex_index
    integer, dimension(1) :: x_indexes !ngpx
    doubleprecision, dimension(1) :: x_coordinates !FINITE_VOLUME_CENTROID_COORDINATES
    doubleprecision, dimension(:), allocatable :: phase_fractions(:), mole_fractions(:), chemical_potentials(:)
  endtype nodes_1D
  !-------------------------------------------------------------------------------------------------------------------------------------
  type TIMESTEP_1D   !
    doubleprecision :: timestep_value
    integer :: timestep_index
    type(nodes_1D), pointer, dimension(:) :: node
  endtype TIMESTEP_1D
  !-------------------------------------------------------------------------------------------------------------------------------------
  type PROCESSEDDATA_1D !
    doubleprecision, dimension(1) :: domain_size
    integer, dimension(1) :: number_of_gridpoints
    integer ::  number_of_dimensions, number_of_elements,  number_of_phase,  number_of_timesteps
    character(len=SHORTWORD), dimension(:), allocatable :: names_of_phases, names_of_elements
    type(TIMESTEP_1D), pointer, dimension(:) :: timesteps_1D
  endtype PROCESSEDDATA_1D
  !-------------------------------------------------------------------------------------------------------------------------------------
  !main_variables_declaration
  !-------------------------------------------------------------------------------------------------------------------------------------
  type(PROCESSEDDATA_1D), pointer :: processed_1D
  type(UNPROCESSEDDATA_1D), pointer :: unprocessed_1D
  !-------------------------------------------------------------------------------------------------------------------------------------
  !-------------------------------------------------------------------------------------------------------------------------------------
  !2D
  !-------------------------------------------------------------------------------------------------------------------------------------




  !-------------------------------------------------------------------------------------------------------------------------------------

  endmodule postprocessor_datastructure



