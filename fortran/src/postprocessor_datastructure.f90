  module postprocessor_datastructure
  !-------------------------------------------------------------------------------------------------------------------------------------
  public
  !-------------------------------------------------------------------------------------------------------------------------------------
  !global_parameters
  integer :: function_value_int
  integer,parameter::SHORTWORD=40
  integer,parameter::LONGWORD=200
  integer,parameter::HUGEWORD=1000
  logical :: errorflag
  !-------------------------------------------------------------------------------------------------------------------------------------
  !global_types
  !-------------------------------------------------------------------------------------------------------------------------------------
  type UNPROCESSEDDATA
    doubleprecision, dimension(:), allocatable :: fvcc
    doubleprecision, dimension(:), allocatable :: molfrc
    doubleprecision, dimension(:), allocatable :: phsfrc
    doubleprecision, dimension(:), allocatable :: chpot
    doubleprecision, dimension(:), allocatable :: tm
    doubleprecision, dimension(3) :: dsz
    integer, dimension(1) :: dim
    integer, dimension(:), allocatable :: nel
    integer, dimension(:), allocatable :: nphs
    integer, dimension(3) :: ngrd
    character(len=SHORTWORD), dimension(:), allocatable :: elnames
    character(len=SHORTWORD), dimension(:), allocatable :: phsnames
  endtype UNPROCESSEDDATA
  !-------------------------------------------------------------------------------------------------------------------------------------
  type NODE
    integer :: vertex_index
    integer, dimension(3) :: xyz_indexes
    doubleprecision, dimension(3) :: xyz_coordinates !FINITE_VOLUME_CENTROID_COORDINATES
    doubleprecision, dimension(:), allocatable :: phase_fractions(:)
    doubleprecision, dimension(:), allocatable :: mole_fractions(:)
    doubleprecision, dimension(:), allocatable :: chemical_potentials(:)
  endtype NODE
  !-------------------------------------------------------------------------------------------------------------------------------------
  type TIMESTEP   !
    doubleprecision :: Gmtot
    doubleprecision :: Gmtot2
    doubleprecision :: Gmtot3
    doubleprecision :: timestep_value
    integer :: timestep_index
    type(NODE), pointer, dimension(:,:,:) :: nodes
  endtype TIMESTEP
  !-------------------------------------------------------------------------------------------------------------------------------------
  type PROCESSEDDATA
    !type REGION !
    !integer :: region_index
    integer :: number_of_dimensions
    doubleprecision, dimension(3) :: domain_size
    integer, dimension(3) :: number_of_gridpoints
    integer :: number_of_elements
    integer :: number_of_phase
    integer :: number_of_timesteps
    character(len=SHORTWORD), dimension(:), allocatable :: names_of_phases
    character(len=SHORTWORD), dimension(:), allocatable :: names_of_elements

    type(TIMESTEP), pointer, dimension(:) :: timesteps
    !endtype REGION
  end type PROCESSEDDATA
  !-------------------------------------------------------------------------------------------------------------------------------------
  type POSTPROCESSORROOT
    type(PROCESSEDDATA), pointer :: processed
    type(UNPROCESSEDDATA), pointer :: unprocessed
    character(len=LONGWORD) :: input_path
    character(len=LONGWORD) :: output_path
  endtype POSTPROCESSORROOT
  !-------------------------------------------------------------------------------------------------------------------------------------
  !main_variables_declaration
  !-------------------------------------------------------------------------------------------------------------------------------------
  type(POSTPROCESSORROOT), pointer :: root
  !-------------------------------------------------------------------------------------------------------------------------------------
  endmodule postprocessor_datastructure



