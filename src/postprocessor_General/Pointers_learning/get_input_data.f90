module get_input
    !include
        use postprocessor_datastructure
!       use ifport

    !functions
    contains
    !-------------------------------------------------------------------------------------------------------------------------------------    
        function get_input_data(root_tmp)
            !declarations
                implicit none
                integer :: get_input_data
                type(POSTPROCESSORROOT), pointer :: root_tmp           
                character*80 cwd
            !routine
 
!                function_value_int = getcwd(cwd)
                
                function_value_int = get_data_path(root_tmp)
                function_value_int = set_unprocessed_data(root_tmp)
            !report
                get_input_data = 0
        endfunction get_input_data
    !-------------------------------------------------------------------------------------------------------------------------------------    
        function get_data_path(root_tmp)
            !declarations
                implicit none
                integer :: get_data_path
                type(POSTPROCESSORROOT), pointer :: root_tmp  
            !routine
                write(*,*) 'specify inpout data path please:'
                read(*,*) root_tmp%input_path
                write(*,*) 'specify outout data path please:'
                read(*,*) root_tmp%output_path
                root_tmp%input_path ='C:\Users\salmasi\Documents\Mycodes\yapfipostprocessor\testdata_working\'             !temporary
                root_tmp%output_path ='C:\Users\salmasi\Documents\Mycodes\yapfipostprocessor\testdata_working\vtk\'    !temporary
            !report
                get_data_path = 0
        endfunction get_data_path
    !!-------------------------------------------------------------------------------------------------------------------------------------    
        function set_unprocessed_data(root_tmp)
            !declarations
                implicit none
                integer :: set_unprocessed_data
                !integer :: f_size
                type(POSTPROCESSORROOT), pointer :: root_tmp
                character(len=LONGWORD) :: f_name
            !routine
                !reading_integer_type_files_alphabetical_order
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"DIMENSIONALITY.TXT"
                    !allocate(root_tmp%unprocessed%dim(file_size(f_name)))
                    root_tmp%unprocessed%dim = get_file_int(f_name,file_size(f_name))
                    
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"NUMBER_OF_ELEMENTS.TXT"
                    allocate(root_tmp%unprocessed%nel(file_size(f_name)))
                
                    root_tmp%unprocessed%nel = get_file_int(f_name,file_size(f_name))
                                        
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"NUMBER_OF_GRID_POINTS.TXT"
                    !allocate(root_tmp%unprocessed%ngrd(file_size(f_name)))
                    root_tmp%unprocessed%ngrd = get_file_int(f_name,file_size(f_name))
                    
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"NUMBER_OF_PHASES.TXT"
                    allocate(root_tmp%unprocessed%nphs(file_size(f_name)))
                    root_tmp%unprocessed%nphs = get_file_int(f_name,file_size(f_name))
                    
                    !/number_of_regions/number_of_cells
                !reading_double_type_files_alphabetical_order
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"CHEMICAL_POTENTIALS.TXT"
                    allocate(root_tmp%unprocessed%chpot(file_size(f_name)))
                    root_tmp%unprocessed%chpot = get_file_dp(f_name,file_size(f_name))
    
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"DOMAIN_SIZE.TXT"
                    !allocate(root_tmp%unprocessed%dsz(file_size(f_name)))
                    root_tmp%unprocessed%dsz = get_file_dp(f_name,file_size(f_name))
                    
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"FINITE_VOLUME_CENTROID_COORDINATES.TXT"
                    allocate(root_tmp%unprocessed%fvcc(file_size(f_name)))
                    root_tmp%unprocessed%fvcc = get_file_dp(f_name,file_size(f_name))
                    
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"MOLE_FRACTIONS.TXT" 
                    allocate(root_tmp%unprocessed%molfrc(file_size(f_name)))
                    root_tmp%unprocessed%molfrc = get_file_dp(f_name,file_size(f_name))
    
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"PHASE_FRACTIONS.TXT"
                    allocate(root_tmp%unprocessed%phsfrc(file_size(f_name)))
                    root_tmp%unprocessed%phsfrc = get_file_dp(f_name,file_size(f_name))
                    
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"TIME.TXT"
                    allocate(root_tmp%unprocessed%tm(file_size(f_name)))
                    root_tmp%unprocessed%tm = get_file_dp(f_name,file_size(f_name))
                    !volume_fraction/.....
                
                !reading_string_type_files_alphabetical_order
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"ELEMENT_NAMES.TXT"
                    allocate(root_tmp%unprocessed%elnames(file_size(f_name)))
                    root_tmp%unprocessed%elnames = get_file_str(f_name,file_size(f_name)) 
                    
                    f_name =''
                    f_name = trim(root_tmp%input_path)//"PHASE_NAMES.TXT"
                    allocate(root_tmp%unprocessed%phsnames(file_size(f_name)))
                    root_tmp%unprocessed%phsnames = get_file_str(f_name,file_size(f_name)) 
                    !regions_names/cells_names/....
                    
            !report
                set_unprocessed_data = 0
        endfunction set_unprocessed_data
    !------------------------------------------------------------------------------------------------------------------------------------- 
        function get_file_dp(f_name,f_size)
            !declarations    
                implicit none
                Integer :: f_size
                Double Precision, dimension(:), allocatable :: array, get_file_dp
                character(len=LONGWORD) :: f_name
            !routine   
                
                OPEN(UNIT=1,file = f_name, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
                    allocate(array(f_size))
                    read(1,*) array
                close(1)
                allocate(get_file_dp(f_size))
            !report
                get_file_dp = array
        end function get_file_dp
    !------------------------------------------------------------------------------------------------------------------------------------- 
        function get_file_int(f_name,f_size)
            !declarations 
                implicit none
                Integer :: f_size
                integer, dimension(:), allocatable :: array, get_file_int
                character(len=LONGWORD) :: f_name
            !routine
                OPEN(UNIT=1,file = f_name, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
                    allocate(array(f_size))
                    read(1,*) array
                close(1)
                allocate(get_file_int(f_size))
            !report
                get_file_int = array
        end function get_file_int
    !------------------------------------------------------------------------------------------------------------------------------------- 
        function get_file_str(f_name,f_size)
            !declarations 
                implicit none
                Integer :: f_size
                character(len=SHORTWORD), dimension(:), allocatable :: array, get_file_str
                character(len=LONGWORD) :: f_name
            !routine       
                OPEN(UNIT=1,file = f_name, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
                    allocate(array(f_size))
                    read(1,*) array
                close(1)
                allocate(get_file_str(f_size))
            !report
                get_file_str = array
        endfunction get_file_str
    !------------------------------------------------------------------------------------------------------------------------------------- 
        function file_size(f_name) 
            !declerations
                implicit none
                character(len=LONGWORD) :: f_name
                integer :: file_size , size, reason              
            !routine    
                OPEN(UNIT=1,file = f_name, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
                    size = 0 
                    reason = 0
                    do while (reason == 0)
                        read(1,*,IOSTAT=reason) 
                        if (reason > 0) then 
                            print*, 'ERROR READING FILE'
                            EXIT
                        else                    
                            size=size+1
                        end if    
                    end do       
                    if (size == 1) then 
                        print*, ' FILE IS EMPTY'
                    endif    
                close(1) 
            !report
                file_size= size-1
        endfunction file_size
    !------------------------------------------------------------------------------------------------------------------------------------- 

    !------------------------------------------------------------------------------------------------------------------------------------- 
endmodule get_input
    
    
    
    

    