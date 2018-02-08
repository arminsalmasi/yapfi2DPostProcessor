module printtofile
    !
    !/***************************************************************************************
    contains   
    !/***************************************************************************************
        function fprintvtk(input)
            use datatype           
            implicit none
            type(timestep), dimension(:), allocatable   ::  input(:)
            integer                                     ::  fprintvtk, i, j, npoint, nx , ny, ncells, cell_list_size,k, n1 , n2, nel
            character(len=1000)                          ::  fnamnnmr,fnamnhdr,elnummer,fnamn,fnamnext,fplace, tempst,tempst1
            double precision                            ::  tempz 
            
            tempz = 0
            do nel=1, 5
                do i=1 , size(input)                                                                        !loop on timesteps
                        fnamn = ''
                        fplace='C:\Users\salmasi\Documents\yapfi-postprocessor\testdata_working\vtktest\'
                        fnamnext='.vtk'                                                                     !file name string
                        fnamnhdr='foo_el_'                                                                !file name string
                        write (fnamnnmr, *) i                                                               !file name string
                        write (elnummer, *) nel
                        fnamn   = trim(fnamnhdr)//(ADJUSTL(trim(elnummer)))                                 !file name string
                        fnamn   = trim(fnamn)//'_tstp_'//(ADJUSTL(trim(fnamnnmr)))                                 !file name string
                        fnamn   = trim(fnamn)//(ADJUSTL(trim(fnamnext)))                                    !file name string
                        fnamn   = (ADJUSTL(trim(fplace)))//(ADJUSTL(trim(fnamn)))                           !file name string
                        
                       
                        OPEN(UNIT=1,file = fnamn, FORM="FORMATTED",  ACTION="WRITE")                        !STATUS="old"??  
                            
                            write(1,'(a)')'# vtk DataFile Version 2.0'                                      !write vtk header
                            tempst=''                                                                       !write vtk header
                            tempst= 'element_'//(ADJUSTL(trim(elnummer)))//'_tstp_'//(ADJUSTL(trim(fnamnnmr)))
                            write(1,'(a)')'header \n'                                                          !write vtk header
                            write(1,'(a)')'ASCII'                                                           !write vtk header
                            write(1,'(a)')'DATASET UNSTRUCTURED_GRID'                                       !write vtk header
                            npoint = size(input(i)%nodes,1)*size(input(i)%nodes,2)                          !write vtk header
                            tempst=''                                                                       !write vtk header
                            write(tempst,*) npoint                                                          !write vtk header
                            tempst='POINTS '//(ADJUSTL(trim(tempst)))                                       !write vtk header
                            tempst= trim(tempst)//' double'                                                 !write vtk header
                            write(1,'(a)') tempst                                                           !write vtk header
                            k=0
                            
                            do nx=1 , size(input(1)%nodes,1)                                                                 !write coordinates to vtk       
                                do ny=1 , size(input(1)%nodes,2)                                                             !write coordinates to vtk
                                    !write(1,'(100(ES28.16))') input(1)%nodes(nx,ny)%xcyc(1),input(1)%nodes(nx,ny)%xcyc(2)    !write coordinates to vtk
                                    write(1,*) input(1)%nodes(nx,ny)%xcyc(1),input(1)%nodes(nx,ny)%xcyc(2),tempz!, nx , ny
                                enddo                                                                                        !write coordinates to vtk
                            enddo                                                                                            !write coordinates to vtk
                            
                            ncells = (size(input(i)%nodes,1)-1)*(size(input(i)%nodes,2)-1)                                  !write header for number of cells and cell list size
                            cell_list_size= ncells * 5                                                                      !write header for number of cells and cell list size
                            tempst=''                                                                                       !write header for number of cells and cell list size
                            tempst1=''                                                                                      !write header for number of cells and cell list size
                            write(tempst,*) ncells                                                                          !write header for number of cells and cell list size
                            write(tempst1,*) cell_list_size                                                                 !write header for number of cells and cell list size
                            tempst= 'CELLS '//(ADJUSTL(trim(tempst)))                                                       !write header for number of cells and cell list size
                            tempst=(ADJUSTL(trim(tempst)))//' '//(ADJUSTL(trim(tempst1)))                                   !write header for number of cells and cell list size
                            write(1,'(100(a))') tempst                                                                      !write header for number of cells and cell list size                      
                            
                           do nx=1 , size(input(1)%nodes,1)-1                                                               !write vertex indexes of cells                                                            
                               do ny=1 , size(input(1)%nodes,2)-1                                                           !write vertex indexes of cells
                                   tempst='4'                                                                               !write vertex indexes of cells
                                   tempst1=''                                                                               !write vertex indexes of cells
                                   write(tempst1,*) input(i)%nodes(nx,ny)%vtxidxs                                           !write vertex indexes of cells
                                   tempst=(adjustl(trim(tempst)))//' '//(adjustl(trim(tempst1)))                            !write vertex indexes of cells
                                   tempst1=''                                                                               !write vertex indexes of cells
                                   write(tempst1,*) input(i)%nodes(nx,ny+1)%vtxidxs                                         !write vertex indexes of cells
                                   tempst=(adjustl(trim(tempst)))//' '//(adjustl(trim(tempst1)))                            !write vertex indexes of cells
                                   tempst1=''                                                                               !write vertex indexes of cells
                                   write(tempst1,*) input(i)%nodes(nx+1,ny)%vtxidxs                                         !write vertex indexes of cells
                                   tempst=(adjustl(trim(tempst)))//' '//(adjustl(trim(tempst1)))                            !write vertex indexes of cells
                                   tempst1=''                                                                               !write vertex indexes of cells
                                   write(tempst1,*) input(i)%nodes(nx+1,ny+1)%vtxidxs                                       !write vertex indexes of cells
                                   tempst=(adjustl(trim(tempst)))//' '//(adjustl(trim(tempst1)))                            !write vertex indexes of cells
                                   write(1,'(a)') tempst                                                                    !write vertex indexes of cells
                               enddo                                                                                        !write vertex indexes of cells
                           enddo                                                                                            !write vertex indexes of cells
                           
                           tempst=''
                           write(tempst,*) ncells
                           tempst='CELL_TYPES '//(adjustl(trim(tempst)))
                           write(1,'(a)') tempst
                           
                           tempst=''
                           do j=1 ,ncells
                                tempst='8 '//(adjustl(trim(tempst)))
                           enddo
                           write(1,'(3000(a))') tempst
                           
                           tempst=''
                           write(tempst,*) npoint
                           tempst='POINT_DATA '//(adjustl(trim(tempst)))
                           write(1,'(a)') tempst
                           
                           write(1,'(a)')'SCALARS XF double 1'
                           write(1,'(a)')'LOOKUP_TABLE default'
                           
                           
                           !!!! MOLE FRACTION FOR TEST MUSRT BE GENERALIZED WITH IF OR CASE
                           
                           do nx=1 , size(input(1)%nodes,1)                                                               
                               do ny=1 , size(input(1)%nodes,2)
                                   tempst=''
                                   !write(tempst,*) input(i)%nodes(nx,ny)%molefractions(3)
                                   !tempst=(adjustl(trim(tempst)))
                                   !write(tempst,*) (input(i)%nodes(nx,ny)%molefractions(3))
                                   write(1,'(ES28.16)',advance="no") input(i)%nodes(nx,ny)%molefractions(nel)
                               enddo
                           enddo
                           
                           write(1,*) ''                                              
                        close(1)
                    enddo
            enddo
            
            fprintvtk = 0
        end function fprintvtk
    !/***************************************************************************************
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
    !/***************************************************************************************
endmodule printtofile    
    
    