module datatype
    public
            !/global types
    !integer        
    
    type node
                !integer,dimension(2)                       ::  nxny
                double precision, dimension(2)              ::  xcyc
                double precision, dimension(:), allocatable ::  phasefractions(:)
                double precision, dimension(:), allocatable ::  molefractions(:)
                double precision, dimension(:), allocatable ::  chemicalpotentials(:)
                integer                                     ::  vtxidxs
            end type node
            
            type timestep
                !integer                                :: timestep_number
                double precision                        :: timestep_value
                type(node),dimension(:,:), allocatable  :: nodes(:,:)
                !integer,dimension(:,:), allocatable     ::  vtxidxs(:,:)
            end type timestep
        
            type datainput
                double precision, dimension(:), allocatable ::  chempot
                double precision, dimension(:), allocatable ::  domsz
                double precision, dimension(:), allocatable ::  fvcc
                double precision, dimension(:), allocatable ::  molfrac
                double precision, dimension(:), allocatable ::  phsfrac
                double precision, dimension(:), allocatable ::  time
                integer, dimension(:), allocatable          ::  dimention
                integer, dimension(:), allocatable          ::  nel
                integer, dimension(:), allocatable          ::  ngrd
                integer, dimension(:), allocatable          ::  nphs
            endtype datainput
            
            !/global variable definition
            character(len=500)                          :: filename, fileplace
            !type(datainput)                             :: indata
            !type(timestep), dimension(:), allocatable   :: system(:)
    end module datatype
    
    
    




!FINITE_VOLUME_CENTROID_COORDINATES.TXT
!PHASE_FRACTIONS.TXT
!PHASE_NAMES.TXT
!CHEMICAL_POTENTIALS.TXT
!NUMBER_OF_ELEMENTS.TXT
!ELEMENT_NAMES.TXT
!MOLE_FRACTIONS.TXT
!NUMBER_OF_GRID_POINTS.TXT
!GRADIENT_ENERGY_CONTRIBUTION.TXT
!DOMAIN_SIZE.TXT
!PERMEABILITIES.TXT
!TIME.TXT
!DIMENSIONALITY.TXT
!NUMBER_OF_PHASES.TXT

 
    
    !type SHORTNAME
        !character*50 :: shortname
    !end type SHORTNMAE

    
    
    !type NODE
    !!a variable of type NODE: all of the possible results at a specific node at a specific timestep are saved
    !   integer                                                 ::  vertex_index
    !   integer            ,dimension(3)                        ::  xyz_indexes                 !ngpx_ngpy_ngpz
    !   double precision   ,dimension(3)                        ::  xyz_coordinates             !FINITE_VOLUME_CENTROID_COORDINATES
    !   double precision   ,dimension(:)        ,allocatable    ::  phase_fraction(:)
    !   double precision   ,dimension(:)        ,allocatable    ::  mole_fraction(:)
    !   double precision   ,dimension(:)        ,allocatable    ::  chemical_potential(:)
    !!   double precision   ,dimension(:)        ,allocatable    ::  volume_fraction(:)          ! if any
    !!   double precision   ,dimension(:)        ,allocatable    ::  u_fraction(:)               ! if any
    !end type NODE
    
    !type TIMESTEPINFO
        !double precission                                      :: timestep_value
        !integer                                                :: timestep_index
    !end type TIMESTEPINFO
    
    !type TIMESTEP
    !!a variable of type TIMESTEP:  Field nodes is an spatial array of NODEs (where each NODE keeps all the possible results at that specific timestep)
    !!                              Field timestepinfo keeps timestep number and timestep value
    !    type(TIMESTEPINFO)                                     ::  timestep_info
    !    type(NODE)         ,dimension(:,:,:)   ,allocatable    ::  nodes    
    !end type TIMESTEP
    
    !type REGIONINFO
    !    integer                                                ::  region_index
    !    integer                                                ::  number_of_dimensins
    !    double precision   ,dimension(:,:,:)   ,allocatable    ::  domain_size
    !    integer                                                ::  number_of_elements
    !    integer                                                ::  number_of_phase
    !    type(shortname)    ,dimension(:)       ,allocatable    ::  phases_names
    !    type(shortname)    ,dimension(:)       ,allocatable    ::  elements_names    
    !    !Boundry conditions > what so ever boundry conditions appply to this specific region
    !    !Mesh info or any other information regards to grid and cells and soon > Type, size, shape...
    !end type REGIONINFO
    
    !type REGION
    !!a variable of type REGION:  Field tstps is an array of all TIMESTEPs (where each TIMESTEP keeps all the possible results at that specific timestep in any given part of the geometry)
    !!                            Field reginfo keeps all information on the geometery and boundryconditions
    !    type(REGIONINFO)                                       ::  regin_info
    !    type(TIMESTEP)     ,dimension(:)       ,allocatable    ::  time_steps    
    !end type TIMESTEP
        
    
    !type CELLINFO
    !!keeps number and name of the regions in the cell as well as other possible informations
    !    integer                                                ::  cell_index
    !    type(shortname)    ,dimension(:)       ,allocatbale    ::  regions_names(:)
    !endtype CELLINFO
    
    !type CELL
    !!a variable of type CELL: Field regions contains results of all of regions in the cell regions>timesteps>nodes
    !!                         Field cell_info of type CELLINFO
    !    type(CELLINFO)                                         ::  cell_info
    !    type(REGION)       ,dimension(:)       ,allocatable    ::  regions    
    !end type CELL
    
    !type ROOTINFO
    !!keeps number and name of the cells as well as other possible informations
    !   integer                                                 ::  number_of_cells
    !   type(shortname)     ,dimension(:)       ,allocatbale    ::  cells_names(:)
    !end type ROOTINFO
    
    !type ROOT
    !!a variable of type ROOT: Field root contains the whole system of results consisting of root>cells>regions>timesteps>nodes
    !!                         Field roo_info of type ROOTINFO
        !type(ROOTINFO)                                         ::  root_info
        !type(CELL)         ,dimension(:)       ,allocatable    ::  cells
    !end type(ROOT)
