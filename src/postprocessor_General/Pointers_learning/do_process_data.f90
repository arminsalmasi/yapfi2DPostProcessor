module do_process
    !modules
        use postprocessor_datastructure
    !Functions
    contains
    !-------------------------------------------------------------------------------------------------------------------------------------
        function do_process_data(root_tmp)
            !declarations
                implicit none
                integer :: do_process_data
                integer :: c, e, p, t, cc, cc_interval , vc, nx, ny, nz, i
                double precision :: Gm_tmp , Gm_tmp2, Gm_tmp3, sum_x_substitutional, temp_cx, temp_cy, xcc, ycc
                type(POSTPROCESSORROOT), pointer :: root_tmp
                
            !routine
                root_tmp%processed%current_region%number_of_dimensions = root_tmp%unprocessed%dim(1) !int    
                root_tmp%processed%current_region%domain_size = root_tmp%unprocessed%dsz !array_dp
                root_tmp%processed%current_region%number_of_elements = root_tmp%unprocessed%nel(1) !int
                root_tmp%processed%current_region%number_of_phase = root_tmp%unprocessed%nphs(1) !int
                root_tmp%processed%current_region%number_of_gridpoints = root_tmp%unprocessed%ngrd
                    
                    if (root_tmp%processed%current_region%number_of_dimensions == 1) then                                             !DIMENSIONALITY_ERROR_HANDLER
                        root_tmp%processed%current_region%number_of_gridpoints(2) = 1                                                 !DIMENSIONALITY_ERROR_HANDLER
                        root_tmp%processed%current_region%number_of_gridpoints(3) = 1                                                 !DIMENSIONALITY_ERROR_HANDLER
                        cc_interval = 1                                                                                             !DIMENSIONALITY_ERROR_HANDLER
                    elseif (root_tmp%processed%current_region%number_of_dimensions == 2) then                                         !DIMENSIONALITY_ERROR_HANDLER
                        root_tmp%processed%current_region%number_of_gridpoints(3) = 1                                                 !DIMENSIONALITY_ERROR_HANDLER
                        cc_interval = 2                                                                                             !DIMENSIONALITY_ERROR_HANDLER
                    elseif (root_tmp%processed%current_region%number_of_dimensions == 3) then                                         !DIMENSIONALITY_ERROR_HANDLER
                        cc_interval = 3                                                                                             !DIMENSIONALITY_ERROR_HANDLER
                    else                                                                                                            !DIMENSIONALITY_ERROR_HANDLER
                        write(*,*) "what is happening in a world of zero or/and more than three dimensions is really intresting"    !DIMENSIONALITY_ERROR_HANDLER
                        errorflag = .true.                                                                                          !DIMENSIONALITY_ERROR_HANDLER
                        goto 10                                                                                                     !DIMENSIONALITY_ERROR_HANDLER
                    endif                                                                                                           !DIMENSIONALITY_ERROR_HANDLER
                    
                root_tmp%processed%current_region%number_of_timesteps = size(root_tmp%unprocessed%tm) !int
                root_tmp%processed%current_region%names_of_phases = root_tmp%unprocessed%phsnames !array_str
                root_tmp%processed%current_region%names_of_elements = root_tmp%unprocessed%elnames !array_str
                allocate(root_tmp%processed%current_region%timesteps(root_tmp%processed%current_region%number_of_timesteps))
                e = 1           !counter-all-data-elementwise
                p = 1           !counter_all_data_pahseswise
                do t = 1 , root_tmp%processed%current_region%number_of_timesteps
                    root_tmp%processed%current_region%timesteps(t)%timestep_index = t
                    root_tmp%processed%current_region%timesteps(t)%timestep_value = root_tmp%unprocessed%tm(t)
                    cc = 1          !counter for coordinates !initializing
                    vc = 0          !counter on vertex indexes
                    allocate(root_tmp%processed%current_region%timesteps(t)%nodes(root_tmp%processed%current_region%number_of_gridpoints(1),root_tmp%processed%current_region%number_of_gridpoints(2),root_tmp%processed%current_region%number_of_gridpoints(3)))
                    
                            root_tmp%processed%current_region%timesteps(t)%Gmtot = 0    !*
                            root_tmp%processed%current_region%timesteps(t)%Gmtot2 = 0   !*
                    
                   temp_cx=0
                  do nx=1, root_tmp%processed%current_region%number_of_gridpoints(1)                   
                         temp_cy=0
                        do ny=1, root_tmp%processed%current_region%number_of_gridpoints(2)
                            do nz=1, root_tmp%processed%current_region%number_of_gridpoints(3)
                                root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%vertex_index = vc
                                
                                
                                root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%xyz_indexes(:) = (/nx,ny,nz/)
                                    
                                    if (root_tmp%processed%current_region%number_of_dimensions == 1) then                                                                                                                                           !adjust_number_of_coordinates_with_dimensionality  
                                        root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:) = (/root_tmp%unprocessed%fvcc(cc), 0.0D0, 0.0D0/)                                                                           !adjust_number_of_coordinates_with_dimensionality
                                        cc= cc+cc_interval                                                                                                                                                                                        !adjust_number_of_coordinates_with_dimensionality
                                    elseif (root_tmp%processed%current_region%number_of_dimensions == 2) then                                                                                                                                       !adjust_number_of_coordinates_with_dimensionality
                                        root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:) = (/root_tmp%unprocessed%fvcc(cc), root_tmp%unprocessed%fvcc(cc+1), 0.0D0/)                                                   !adjust_number_of_coordinates_with_dimensionality
                                        xcc =  ((root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(1)-temp_cx)*2)
                                        ycc =  ((root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(2)-temp_cy)*2)
                                        root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%vm=(xcc*2)*(ycc*2)
                                        
                                        temp_cy= temp_cy+ (ycc)
                                        cc= cc+cc_interval                                                                                                                                                                                        !adjust_number_of_coordinates_with_dimensionality
                                    elseif (root_tmp%processed%current_region%number_of_dimensions == 3) then                                                                                                                                       !adjust_number_of_coordinates_with_dimensionality
                                        root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:) = (/root_tmp%unprocessed%fvcc(cc), root_tmp%unprocessed%fvcc(cc+1), root_tmp%unprocessed%fvcc(cc+2)/)                           !adjust_number_of_coordinates_with_dimensionality
                                        cc= cc+cc_interval                                                                                                                                                                                        !adjust_number_of_coordinates_with_dimensionality
                                    else                                                                                                                                                                                                          !adjust_number_of_coordinates_with_dimensionality
                                        write(*,*) "what is happening in a world of zero or/and more than three dimensions is really intresting"                                                                                                  !adjust_number_of_coordinates_with_dimensionality
                                        errorflag = .true.                                                                                                                                                                                        !adjust_number_of_coordinates_with_dimensionality
                                        goto 10                                                                                                                                                                                                   !adjust_number_of_coordinates_with_dimensionality
                                    endif                                                                                                                                                                                                         !adjust_number_of_coordinates_with_dimensionality
                                    
                                allocate(root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(root_tmp%processed%current_region%number_of_elements))
                                allocate(root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%chemical_potentials(root_tmp%processed%current_region%number_of_elements))
                                allocate(root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%phase_fractions(root_tmp%processed%current_region%number_of_phase))
                                
                                
                                    Gm_tmp = 0!*
                                    Gm_tmp2 = 0 !*
                                    
                                
                                do i = 1, root_tmp%processed%current_region%number_of_elements !prob_elements
                                        root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(i) = root_tmp%unprocessed%molfrc(e) 
                                        root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%chemical_potentials(i) = root_tmp%unprocessed%chpot(e) 
                                        
                                            Gm_tmp = Gm_tmp + (root_tmp%unprocessed%chpot(e) * root_tmp%unprocessed%molfrc(e))          !*
                                            if (i /= 3 ) then                                                                           !*
                                                Gm_tmp2 = Gm_tmp2 + (root_tmp%unprocessed%chpot(e) * root_tmp%unprocessed%molfrc(e))    !*
                                            end if                                                                                      !*
                                        
                                        e=e+1
                                enddo
                                    root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%Gm = Gm_tmp      !*  
                                    root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%Gm2 = Gm_tmp2    !*  
                                
                                
                                do i = 1, root_tmp%processed%current_region%number_of_phase !prob_phases
                                        root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%phase_fractions(i) = root_tmp%unprocessed%phsfrc(p) 
                                        p=p+1                                                                    
                                enddo
                                    root_tmp%processed%current_region%timesteps(t)%Gmtot = root_tmp%processed%current_region%timesteps(t)%Gmtot + root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%Gm!*
                                    root_tmp%processed%current_region%timesteps(t)%Gmtot2 = root_tmp%processed%current_region%timesteps(t)%Gmtot2 + root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%Gm2
                                vc= vc+1
                            enddo               
                        enddo
                        temp_cx= temp_cx+ (xcc)
                    enddo
                      
                enddO
                
                !u-fractions
                    do t = 1 , root_tmp%processed%current_region%number_of_timesteps
                        do nx=1, root_tmp%processed%current_region%number_of_gridpoints(1)                   
                            do ny=1, root_tmp%processed%current_region%number_of_gridpoints(2)
                                do nz=1, root_tmp%processed%current_region%number_of_gridpoints(3)
                                    sum_x_substitutional = root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(2)+root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(4)+root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(5)
                                    do i= 1 , root_tmp%processed%current_region%number_of_elements
                                        root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%u_fractions(i) = (root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(i))/sum_x_substitutional
                                    enddO
                                enddO
                            enddO
                        enddO
                    enddO
                    !end u fractions 
                    !GM with Ufraction
                    do t = 1 , root_tmp%processed%current_region%number_of_timesteps
                        root_tmp%processed%current_region%timesteps(t)%Gmtot3 = 0
                        do nx=1, root_tmp%processed%current_region%number_of_gridpoints(1)                   
                            do ny=1, root_tmp%processed%current_region%number_of_gridpoints(2)
                                 
                                do nz=1, root_tmp%processed%current_region%number_of_gridpoints(3)
                                    Gm_tmp3 = 0 
                                    do i= 1 , root_tmp%processed%current_region%number_of_elements
                                        
                                        Gm_tmp3 = Gm_tmp3 + (root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%chemical_potentials(i) * root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%u_fractions(i))
                                    enddo
                                    
                                    root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%Gm3 = root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%vm * Gm_tmp3
                                    root_tmp%processed%current_region%timesteps(t)%Gmtot3 = root_tmp%processed%current_region%timesteps(t)%Gmtot3 + root_tmp%processed%current_region%timesteps(t)%nodes(nx,ny,nz)%Gm3
                                enddO
                            enddO
                        enddO
                    enddO
                    !end Gm with ufraction
            !Report
10              if (errorflag) then 
                    do_process_data = 1
                else
                    do_process_data = 0    
                endif
        endfunction do_process_data
    !-------------------------------------------------------------------------------------------------------------------------------------
    end module do_process
