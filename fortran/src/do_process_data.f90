module do_process
  use postprocessor_datastructure
  contains
!-------------------------------------------------------------------------------------------------------------------------------------
  function do_process_data(root_tmp)
      
    implicit none
    integer :: do_process_data
    integer :: c,e,p,t,cc,cc_interval,vc,nx,ny,nz,i
    double precision :: Gm_tmp , Gm_tmp2, Gm_tmp3, sum_x_substitutional, temp_cx, temp_cy, xcc, ycc
    type(POSTPROCESSORROOT), pointer :: root_tmp
          
      
    root_tmp%processed%number_of_dimensions = root_tmp%unprocessed%dim(1)  !int    
    root_tmp%processed%domain_size = root_tmp%unprocessed%dsz              !array_dp
    root_tmp%processed%number_of_elements = root_tmp%unprocessed%nel(1)    !int
    root_tmp%processed%number_of_phase = root_tmp%unprocessed%nphs(1)      !int
    root_tmp%processed%number_of_gridpoints = root_tmp%unprocessed%ngrd    !int
             

    SELECT CASE (root_tmp%processed%number_of_dimensions)
!      CASE (1)
!      root_tmp%processed%number_of_gridpoints(2:3) = 1                                                 !DIMENSIONALITY_ERROR_HANDLER
!      cc_interval = 1                                                                                             !DIMENSIONALITY_ERROR_HANDLER
      CASE (2)
        root_tmp%processed%number_of_gridpoints(3) = 1                                                 !DIMENSIONALITY_ERROR_HANDLER
        cc_interval = 2
!      CASE (3)
!        cc_interval = 3 
!      CASE DEFAULT
!        errorflag = .true.
!        goto 10
    END SELECT 
                  
    root_tmp%processed%number_of_timesteps = size(root_tmp%unprocessed%tm) !int
    root_tmp%processed%names_of_phases = root_tmp%unprocessed%phsnames !array_str
    root_tmp%processed%names_of_elements = root_tmp%unprocessed%elnames !array_str
    allocate(root_tmp%processed%timesteps(root_tmp%processed%number_of_timesteps))
    e = 1           !counter-all-data-elementwise
    p = 1           !counter_all_data_pahseswise
    do t = 1 , root_tmp%processed%number_of_timesteps
      root_tmp%processed%timesteps(t)%timestep_index = t
      root_tmp%processed%timesteps(t)%timestep_value = root_tmp%unprocessed%tm(t)
      cc = 1          !counter for coordinates !initializing
      vc = 0          !counter on vertex indexes
      
      allocate(root_tmp%processed%timesteps(t)%nodes(root_tmp%processed%number_of_gridpoints(1),root_tmp%processed%number_of_gridpoints(2),root_tmp%processed%number_of_gridpoints(3)))
                       
      temp_cx=0
      do nx=1, root_tmp%processed%number_of_gridpoints(1)                   
        temp_cy=0
        do ny=1, root_tmp%processed%number_of_gridpoints(2)
          do nz=1, root_tmp%processed%number_of_gridpoints(3)
            root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%vertex_index = vc
            root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_indexes(:) = (/nx,ny,nz/)
            SELECT CASE (root_tmp%processed%number_of_dimensions)
!              CASE (1)
!                ! root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:) = (/root_tmp%unprocessed%fvcc(cc), 0.0D0, 0.0D0/)                                                                           !adjust_number_of_coordinates_with_dimensionality
!                ! cc= cc+cc_interval
              CASE (2)
                root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:) = (/root_tmp%unprocessed%fvcc(cc), root_tmp%unprocessed%fvcc(cc+1), 0.0D0/)                                                   !adjust_number_of_coordinates_with_dimensio
                xcc =  ((root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(1)-temp_cx)*2)
                ycc =  ((root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(2)-temp_cy)*2)
                root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%vm=(xcc*2)*(ycc*2)      
                temp_cy= temp_cy+ (ycc)
                cc= cc+cc_interval                                                                                                                                                                                        !adjust_number_of_coordinates_with_dimensionality
!              CASE (3)
!                !    root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%xyz_coordinates(:) = (/root_tmp%unprocessed%fvcc(cc), root_tmp%unprocessed%fvcc(cc+1), root_tmp%unprocessed%fvcc(cc+2)/)                           !adjust_number_of_coordinates_with_dimens
!                !    cc= cc+cc_interval                                                                                                                                                                                        !adjust_number_of_coordinates_with_dimensionality
!              CASE DEFAULT
!                errorflag = .true.
!                goto 10
            END SELECT 
              
            allocate(root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(root_tmp%processed%number_of_elements))
            allocate(root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%chemical_potentials(root_tmp%processed%number_of_elements))
            allocate(root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%phase_fractions(root_tmp%processed%number_of_phase))
              
            !prob_elements    
            do i = 1, root_tmp%processed%number_of_elements 
              root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%mole_fractions(i) = root_tmp%unprocessed%molfrc(e) 
              root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%chemical_potentials(i) = root_tmp%unprocessed%chpot(e) 
              e=e+1
            enddo
              
            !prob_phases      
            do i = 1, root_tmp%processed%number_of_phase 
              root_tmp%processed%timesteps(t)%nodes(nx,ny,nz)%phase_fractions(i) = root_tmp%unprocessed%phsfrc(p) 
              p=p+1                                                                    
            enddo  
            vc= vc+1
          enddo               
        enddo
        temp_cx= temp_cx+ (xcc)
      enddo                
    enddo       

10  if (errorflag) then 
      write(*,*) "world of zero or/and more than three dimensions is an intresting place"  !adjust_number_of_coordinates_with_dimensionality  
      do_process_data = 1
    else
        do_process_data = 0    
    endif
  endfunction do_process_data
!-------------------------------------------------------------------------------------------------------------------------------------
end module do_process