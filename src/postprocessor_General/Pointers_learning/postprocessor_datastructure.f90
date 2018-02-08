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
        !character(len=SHORTWORD), dimension(4), parameter :: intiger_inputfiles_name_holder = (/'DIMENSIONALITY.TXT', 'NUMBER_OF_ELEMENTS.TXT','NUMBER_OF_GRID_POINTS.TXT','NUMBER_OF_PHASES.TXT'/) !open_ended
        !character(len=SHORTWORD), dimension(5), parameter :: double_inputfiles_name_holder =  (/'CHEMICAL_POTENTIALS.TXT','DOMAIN_SIZE.TXT','FINITE_VOLUME_CENTROID_COORDINATES.TXT','MOLE_FRACTIONS.TXT','PHASE_FRACTIONS.TXT'/) !open_ended
        !character(len=SHORTWORD), dimension(2), parameter :: string_inputfiles_name_holder =  (/'ELEMENT_NAMES.TXT','PHASE_NAMES.TXT'/) !open_ended
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
            !doubleprecision, dimension(:), allocatable :: volfrc
            integer, dimension(1) :: dim
            integer, dimension(:), allocatable :: nel
            integer, dimension(:), allocatable :: nphs
            integer, dimension(3) :: ngrd
            !integer, dimension(:), allocatable :: ncl
            !integer, dimension(:), allocatable :: nreg
            character(len=SHORTWORD), dimension(:), allocatable :: elnames  
            character(len=SHORTWORD), dimension(:), allocatable :: phsnames 
            !character(len=SHORTWORD), dimension(:), allocatable :: clnames
            !character(len=SHORTWORD), dimension(:), allocatable :: regnames
        endtype UNPROCESSEDDATA   
    !-------------------------------------------------------------------------------------------------------------------------------------       
        type NODE   !avariableoftypeNODE:allofthepossibleresultsataspecificnodeataspecifictimesteparesaved
            integer :: vertex_index
            doubleprecision :: vm
            integer, dimension(3) :: xyz_indexes !ngpx_ngpy_ngpz
            doubleprecision, dimension(3) :: xyz_coordinates !FINITE_VOLUME_CENTROID_COORDINATES
            doubleprecision, dimension(:), allocatable :: phase_fractions(:)
            doubleprecision, dimension(:), allocatable :: mole_fractions(:)
            doubleprecision, dimension(:), allocatable :: chemical_potentials(:)
            doubleprecision, dimension(5) :: u_fractions !must be allocatable like others will get back to it later!
                doubleprecision :: Gm
                doubleprecision :: Gm2
                doubleprecision :: Gm3 ! U-fractions!
            !doubleprecision,dimension(:),allocatable::volume_fraction(:)!if_any
            !doubleprecision,dimension(:),allocatable::u_fraction(:)!if_any
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
        type REGION !
                !integer :: region_index
                integer :: number_of_dimensions
                doubleprecision, dimension(3) :: domain_size
                integer, dimension(3) :: number_of_gridpoints
                integer :: number_of_elements
                integer :: number_of_phase
                integer :: number_of_timesteps
                character(len=SHORTWORD), dimension(:), allocatable :: names_of_phases
                character(len=SHORTWORD), dimension(:), allocatable :: names_of_elements
                
                !Boundryconditions>whatsoeverboundryconditionsappplytothisspecificregion
                !Meshinfooranyotherinformationregardstogridandcellsandsoon>Type,size,shape...
                type(TIMESTEP), pointer, dimension(:) :: timesteps
        endtype REGION
    !-------------------------------------------------------------------------------------------------------------------------------------    
        !type CELL   !
        !    integer :: cell_index
        !    integer :: number_of_regions
        !    character(len=SHORTWORD), dimension(:), allocatable :: names_of_regions(:)
        !    type(REGION), pointer, dimension(:) :: regions
        !endtype CELL
    !-------------------------------------------------------------------------------------------------------------------------------------    
        type PROCESSEDDATA  !a variable of type PROCESSEDDATA:Fieldrootcontainsthewholesystemofresultsconsistingofroot>cells>regions>timesteps>nodes    !Field PROCESSEDDATA_infooftypeROOTINFO
        !        integer :: number_of_cells
        !        character(len=SHORTWORD), dimension(:), allocatable :: names_of_cells(:)
        !        type(CELL), pointer, dimension(:) :: cells
            type(REGION), pointer :: current_region
        endtype PROCESSEDDATA
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
    
    
    
        