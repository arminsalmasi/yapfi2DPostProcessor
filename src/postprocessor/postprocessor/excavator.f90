  module excavator
    use datatype
    contains
        function excavate(indata,system)    
            use datatype           
            implicit none   
            type(datainput)                                     :: indata                                       !   all data from files in one structure
            type(timestep), dimension(:), allocatable           :: system(:)                                    !   all data of tghe system in one structure
            integer                                             :: excavate                                     !   function type
            integer                                             :: tn, cnt_el, cnt_ph, cnt_cc, cnt_vx, nx, ny, el , ph ,A !   counters and internal private variables
          
            allocate(system(size(indata%time)))
            cnt_el=1 !counter for number of elements initialaize
            cnt_ph=1 !counter for number of phases initialaize
            do tn = 1, size(indata%time) !probe all the timestep numbers 
                !system(tn)%timestep_value= indata%time(tn)
                allocate(system(tn)%nodes(indata%ngrd(1),indata%ngrd(2)))
                cnt_cc=1                                                            !counter for coordinates !initializing
                cnt_vx=0                                                            !counter on vertex indexes
                do nx=1, indata%ngrd(1)                   
                    do ny=1, indata%ngrd(2)    
                        A = read_coordinates(indata, nx , ny, cnt_cc, system(tn))    ! read coordinateds of node xn yn and put it in the coordinate array of the timestep system(tn)
                        A = readXreadUreadPhX(indata,nx , ny, cnt_el, cnt_ph, system(tn))                       
                            system(tn)%nodes(nx,ny)%vtxidxs = cnt_vx
                            cnt_vx= cnt_vx+1
                    enddo               
                enddo
            enddo
            !!!Begin test writer block
            !OPEN(UNIT=1,file = 'C:\Users\salmasi\Documents\yapfi-postprocessor\testdata_working\excavate_tst.txt') !test file
            !do tn = 1, size(indata%time)
            !    do nx=1, indata%ngrd(1)                   
            !        do ny=1, indata%ngrd(2)
            !            write(1,*) 'tstp num:',tn 
            !            write(1,*)
            !            write(1,*) 'timeval tstp=', system(tn)%timestep_value
            !            write(1,*)
            !            write(1,*) 'grdpnt nxny:', nx, ny 
            !            write(1,*)
            !            write(1,*) 'centcoord xy:', system(tn)%nodes(nx,ny)%xcyc(:)
            !            write(1,*)
            !            write(1,*) 'molfrac:', system(tn)%Nodes(nx,ny)%molefractions(:)
            !            write(1,*)
            !            write(1,*) 'chempot:', system(tn)%Nodes(nx,ny)%chemicalpotentials(:)
            !            write(1,*)
            !            write(1,*) 'phsfrac:',system(tn)%Nodes(nx,ny)%phasefractions(:)
            !            write(1,*) '*********************************************************'
            !        enddo               
            !        write(1,*) '*********************************************************'
            !        write(1,*) '********************end grid*****************************'
            !        write(1,*) '*********************************************************'
            !        write(1,*) '*********************************************************'
            !    enddo
            !    write(1,*) '*********************************************************'
            !    write(1,*) '********************end tstp*****************************'
            !    write(1,*) '*********************************************************'
            !    write(1,*) '*********************************************************'
            !enddo
            !close(1) 
            !!!End test writer block
          excavate = 0         
        end function excavate
        !/*************************************************************************
        function read_coordinates(input, n1, n2, idx, output)
            use datatype
            implicit none
            type(datainput)                                     :: input                                       !   all data from files in one structure
            type(timestep)                                      :: output                                    !   all data of tghe system in one structure
            integer ::idx, read_coordinates, n1 , n2
            
            output%nodes(n1,n2)%xcyc(1)                             =   input%fvcc(idx)
            output%nodes(n1,n2)%xcyc(2)                             =   input%fvcc(idx+1)
            idx=idx+2 
            read_coordinates=0
        endfunction read_coordinates
        !/*************************************************************************
        function readXreadUreadPhX(input,n1 , n2, numel,numph, output)  
            use datatype
            implicit none
            type(datainput)                                     :: input                                     !   all data from files in one structure
            type(timestep)                                      :: output                                    !   all data of tghe system in one structure
            integer                                             :: readXreadUreadPhX, n1, n2, numel, numph, el, ph                        
            
            allocate(output%nodes(n1,n2)%phasefractions(input%nphs(1)))
            allocate(output%nodes(n1,n2)%molefractions(input%nel(1)))
            allocate(output%nodes(n1,n2)%chemicalpotentials(5))!input%nel(1)))
            do el = 1, input%nel(1) !prob elements in a specific node(nx,ny) of a specific timestep (tn)
                    output%nodes(n1,n2)%molefractions(el)           =   input%molfrac(numel) 
                    output%nodes(n1,n2)%chemicalpotentials(el)      =   input%chempot(numel) 
                    numel=numel+1
            enddo
            do ph = 1, input%nphs(1) !prob phases in a specific node(nx,ny) of a specific timestep (tn)
                    output%nodes(n1,n2)%phasefractions(ph)          =   input%phsfrac(numph) 
                    numph=numph+1                                                                    
            enddo
            readXreadUreadPhX = 0
        endfunction readXreadUreadPhX
        !/*************************************************************************
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        function closesttimestep(tarr, tx)                    !find the closest timestep
            implicit none        
            integer                                             :: low, high, mid, clst
            double precision                                    :: tx , closesttimestep
            double precision, dimension(:), allocatable         :: tarr
          
            if (tx>tarr(size(tarr))) then
                clst = size(tarr)
            elseif (tx <= 0) then
                clst = 1
            else
                low  = 1
                high  = size(tarr)
                do while ((high - low > 1)) 
                    mid = ((low + high)/2)
                    if (tarr(mid) <= tx) then 
                            low = mid
                        else
                            high = mid
                        end if
                        if (tarr(low) == tx) then
                            high = low
                        end if                       
                end do        
                if (abs(tarr(low)-tx) <= abs(tarr(high)-tx)) then
                    clst = low
                else
                    clst = high                    
                endif
            end if
            closesttimestep = clst
        end function closesttimestep  
        
    end module excavator
    
    
    