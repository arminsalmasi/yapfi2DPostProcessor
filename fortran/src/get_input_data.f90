module get_input
  use PpDatastruct
  contains
!----------------------------------------------------------------------------------------------------------------------------    
  function get_unprocessed_data(UnPrcTmp,input_path)
          
    implicit none
    integer :: get_unprocessed_data
    type(UnPrcDataType) :: UnPrcTmp
    character(len=LONGWORD) :: f_name, input_path
      
    !reading_integer_type_files_alphabetical_order
    f_name =''
    f_name = trim(input_path)//"DIMENSIONALITY.TXT"
    UnPrcTmp%dim = get_file_int(f_name,file_size(f_name))
              
    f_name =''
    f_name = trim(input_path)//"NUMBER_OF_ELEMENTS.TXT"
    allocate(UnPrcTmp%nel(file_size(f_name)))
          
    UnPrcTmp%nel = get_file_int(f_name,file_size(f_name))
                                  
    f_name =''
    f_name = trim(input_path)//"NUMBER_OF_GRID_POINTS.TXT"
    UnPrcTmp%ngrd = get_file_int(f_name,file_size(f_name))
              
    f_name =''
    f_name = trim(input_path)//"NUMBER_OF_PHASES.TXT"
    allocate(UnPrcTmp%nphs(file_size(f_name)))
    UnPrcTmp%nphs = get_file_int(f_name,file_size(f_name))
              
    !/number_of_regions/number_of_cells
    !reading_double_type_files_alphabetical_order
    f_name =''
    f_name = trim(input_path)//"CHEMICAL_POTENTIALS.TXT"
    allocate(UnPrcTmp%chpot(file_size(f_name)))
    UnPrcTmp%chpot = get_file_dp(f_name,file_size(f_name))
  
    f_name =''
    f_name = trim(input_path)//"DOMAIN_SIZE.TXT"
    UnPrcTmp%dsz = get_file_dp(f_name,file_size(f_name))
              
    f_name =''
    f_name = trim(input_path)//"FINITE_VOLUME_CENTROID_COORDINATES.TXT"
    allocate(UnPrcTmp%fvcc(file_size(f_name)))
    UnPrcTmp%fvcc = get_file_dp(f_name,file_size(f_name))
              
    f_name =''
    f_name = trim(input_path)//"MOLE_FRACTIONS.TXT" 
    allocate(UnPrcTmp%molfrc(file_size(f_name)))
    UnPrcTmp%molfrc = get_file_dp(f_name,file_size(f_name))
  
    f_name =''
    f_name = trim(input_path)//"PHASE_FRACTIONS.TXT"
    allocate(UnPrcTmp%phsfrc(file_size(f_name)))
    UnPrcTmp%phsfrc = get_file_dp(f_name,file_size(f_name))
              
    f_name =''
    f_name = trim(input_path)//"TIME.TXT"
    allocate(UnPrcTmp%tm(file_size(f_name)))
    UnPrcTmp%tm = get_file_dp(f_name,file_size(f_name))
              
    !reading_string_type_files_alphabetical_order
    f_name =''
    f_name = trim(input_path)//"ELEMENT_NAMES.TXT"
    allocate(UnPrcTmp%elnames(file_size(f_name)))
    UnPrcTmp%elnames = get_file_str(f_name,file_size(f_name)) 
              
    f_name =''
    f_name = trim(input_path)//"PHASE_NAMES.TXT"
    allocate(UnPrcTmp%phsnames(file_size(f_name)))
    UnPrcTmp%phsnames = get_file_str(f_name,file_size(f_name)) 
       
    get_unprocessed_data = 0
  endfunction get_unprocessed_data
!----------------------------------------------------------------------------------------------------------------------------
  function get_file_dp(f_name,f_size)
         
    implicit none
    Integer :: f_size
    Double Precision, dimension(:), allocatable :: array, get_file_dp
    character(len=LONGWORD) :: f_name
         
    OPEN(UNIT=1,file = f_name, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
    allocate(array(f_size))
    read(1,*) array
    close(1)
    allocate(get_file_dp(f_size))
      
    get_file_dp = array
  end function get_file_dp
!----------------------------------------------------------------------------------------------------------------------------
  function get_file_int(f_name,f_size)
      
    implicit none
    Integer :: f_size
    integer, dimension(:), allocatable :: array, get_file_int
    character(len=LONGWORD) :: f_name
      
    OPEN(UNIT=1,file = f_name, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
    allocate(array(f_size))
    read(1,*) array
    close(1)
    allocate(get_file_int(f_size))
      
    get_file_int = array
  end function get_file_int
!----------------------------------------------------------------------------------------------------------------------------
  function get_file_str(f_name,f_size)

    implicit none
    Integer :: f_size
    character(len=SHORTWORD), dimension(:), allocatable :: array, get_file_str
    character(len=LONGWORD) :: f_name
       
    OPEN(UNIT=1,file = f_name, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
    allocate(array(f_size))
    read(1,*) array
    close(1)
    allocate(get_file_str(f_size))

    get_file_str = array
  endfunction get_file_str
!---------------------------------------------------------------------------------------------------------------------------- 
  function file_size(f_name) 
    implicit none
    character(len=LONGWORD) :: f_name
    integer :: file_size , size, reason              
          
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
      file_size= size-1
  endfunction file_size
!-----------------------------------------------------------------------------------------------------------------------------   
endmodule get_input    