Program refactored_postprocessor         
    !modules
        use postprocessor_datastructure
        use get_input
        use do_process
        use set_vtk
    !declarations
        implicit none
    !-------------------------------------------------------------------------------------------------------------------------------------                 
    !Mainbody
        !initialization
            allocate(root)
            allocate(root%unprocessed)
            allocate(root%processed)
            allocate(root%processed%current_region)
            !allocate(root%processed%cells)
            !allocate(root%processed%cells%regions)
            !allocate(root%processed%cells%regions%timesteps)
        !routine
            function_value_int = get_input_data(root)
            function_value_int = do_process_data(root)
            function_value_int = set_vtk_format(root)
            !function_value_int = set_Gmtot_txt(root)
            !function_value_int = set_Gmtot2_txt(root)
            !function_value_int = set_Gmtot3_txt(root)
            write(*,*) 'end'
            read(*,*)
        !closer_statments
             deallocate(root)
    !-------------------------------------------------------------------------------------------------------------------------------------  
             
                        
                        
             
endprogram refactored_postprocessor