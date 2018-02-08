module readfromfile
    !read all txt files and put unprocessed data in one datatype 'output'
    !/***************************************************************************************
    contains   
    !/***************************************************************************************
        function main_filereader(output) 
            !types
                use datatype
            !Declare types and variables
                implicit none    
                type(datainput) :: output
                integer :: main_filereader     
            !Body
                fileplace ="C:\Users\salmasi\Documents\yapfi-postprocessor\testdata_working\"
                
                filename = trim(fileplace)//"CHEMICAL_POTENTIALS.TXT"
                allocate(output%chempot(file_size(filename)))
                output%chempot = readfile_dp(filename,file_size(filename))
                            
                filename = trim(fileplace)//"DIMENSIONALITY.TXT"
                allocate(output%dimention(file_size(filename)))
                output%dimention = readfile_int(filename,file_size(filename))
                
                filename = trim(fileplace)//"DOMAIN_SIZE.TXT"
                allocate(output%domsz(file_size(filename)))
                output%domsz = readfile_dp(filename,file_size(filename))
                
                filename = trim(fileplace)//"FINITE_VOLUME_CENTROID_COORDINATES.TXT"
                allocate(output%fvcc(file_size(filename)))
                output%fvcc = readfile_dp(filename,file_size(filename))
                                        
                filename = trim(fileplace)//"MOLE_FRACTIONS.TXT" 
                allocate(output%molfrac(file_size(filename)))
                output%molfrac = readfile_dp(filename,file_size(filename))
                
                filename = trim(fileplace)//"NUMBER_OF_ELEMENTS.TXT"
                allocate(output%nel(file_size(filename)))
                output%nel = readfile_int(filename,file_size(filename))
                
                filename = trim(fileplace)//"NUMBER_OF_GRID_POINTS.TXT"
                allocate(output%ngrd(file_size(filename)))
                output%ngrd = readfile_int(filename,file_size(filename))
                        
                filename = trim(fileplace)//"NUMBER_OF_PHASES.TXT"
                allocate(output%nphs(file_size(filename)))
                output%nphs = readfile_int(filename,file_size(filename))
                
                filename = trim(fileplace)//"PHASE_FRACTIONS.TXT"
                allocate(output%phsfrac(file_size(filename)))
                output%phsfrac = readfile_dp(filename,file_size(filename))
                
                filename = trim(fileplace)//"TIME.TXT"
                allocate(output%time(file_size(filename)))
                output%time = readfile_dp(filename,file_size(filename))              
            !return function value
                main_filereader = 0
        end function main_filereader
    !/***************************************************************************************
        function readfile_dp(path,f_size)
            implicit none
            Integer                                     :: f_size
            Double Precision, dimension(:), allocatable :: array, readfile_dp
            character(len=500)                          :: Path

                
            OPEN(UNIT=1,file = path, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
                allocate(array(f_size))
                read(1,*) array
            close(1)
            allocate(readfile_dp(f_size))
            readfile_dp = array
        end function readfile_dp
    !/***************************************************************************************
        function readfile_int(path,f_size)
            implicit none
            Integer                                     :: f_size
            integer, dimension(:), allocatable          :: array, readfile_int
            character(len=500)                          :: Path

                
            OPEN(UNIT=1,file = path, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
                allocate(array(f_size))
                read(1,*) array
            close(1)
            allocate(readfile_int(f_size))
            readfile_int = array
        end function readfile_int
    !/***************************************************************************************
        function file_size(path) 
            implicit none
            character(500)                              :: path
            integer                                     :: file_size , size, reason              
                
            OPEN(UNIT=1,file = path, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
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
        end function file_size
    !/***************************************************************************************
end module readfromfile
    