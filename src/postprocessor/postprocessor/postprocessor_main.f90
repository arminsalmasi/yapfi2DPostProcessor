    program  postprocessor
    !Modules           
        use datatype
        use readfromfile       
        use excavator
        use printtofile
    ! Variables and Types
        implicit none
        !Declare types
            type(datainput)                             ::  rawdata
            type(timestep), dimension(:), allocatable   ::  structureddata(:)
        !Declare Variables
            integer                                     ::  A , idx , i
    
    ! Main Body      
        A = main_filereader(rawdata)                                 !   read data from files and write it to datastructure rawdata) ---Read the fileplace from screen!!!
        A = excavate(rawdata, structureddata)                        !   process raw data from rawdata and put it in the structureddata 
        A = fprintvtk(structureddata)                           !   reads  : plot conditions from screen
                                                                     !   reads  : processed data structure
                                                                     !   returns: file ready to plot with paraview             
            !!idx = 3
            !!OPEN(UNIT=1,file = 'diagonalnitrogen.txt')                   
            !    
            !    do idx =1 , rawdata%nel(1)
            !        write(*,*) ' xcc' , ' ycc' , ' molefraction of element' , idx, ' timestep:', size(rawdata%time), ' timevalue:', rawdata%time(size(rawdata%time))
            !        write(*,*) 'data_s  element_index', idx
            !        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            !        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            !        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            !        do i=1 , rawdata%ngrd(1)
            !            write(*,*) ' mole fraction:', structureddata(size(rawdata%time))%nodes(i,i)%xcyc(1) ,'  ', structureddata(size(rawdata%time))%nodes(i,i)%xcyc(2) ,'  ', structureddata(size(rawdata%time))%nodes(i,i)%molefractions(idx)
            !            write(*,*) '********************************************************************'
            !            write(*,*) ' chemical potential:', structureddata(size(rawdata%time))%nodes(i,i)%xcyc(1) ,'  ', structureddata(size(rawdata%time))%nodes(i,i)%xcyc(2) ,'  ', structureddata(size(rawdata%time))%nodes(i,i)%chemicalpotentials(idx)
            !        enddo
            !        write(*,*) ' data_e element_index', idx
            !        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            !        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            !        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            !    enddo
            !    
            !    
            !     do idx =1 , rawdata%nphs(1)                   
            !        do i=1 , rawdata%ngrd(1)
            !            write(*,*) structureddata(size(rawdata%time))%nodes(i,i)%phasefractions(idx)
            !        enddo
            !        enddo
            !!close(1)       
            !
    !/*****************************************************************************************************
    end program postprocessor     
   
     