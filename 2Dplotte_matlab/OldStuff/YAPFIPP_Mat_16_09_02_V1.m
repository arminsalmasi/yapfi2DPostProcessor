clc
clear all
close all
%************************************************************
%load files
    load NUMBER_OF_ELEMENTS.TXT %
    load NUMBER_OF_PHASES.TXT %
    load DIMENSIONALITY.TXT %
    load NUMBER_OF_GRID_POINTS.TXT %
    load DOMAIN_SIZE.TXT %
    load FINITE_VOLUME_CENTROID_COORDINATES.TXT %
    load MOLE_FRACTIONS.TXT %
    load CHEMICAL_POTENTIALS.TXT %
    load TIME.TXT %
%************************************************************
%assign vars
    n_el   = NUMBER_OF_ELEMENTS;
    n_ph   = NUMBER_OF_PHASES;
    n_gp   = NUMBER_OF_GRID_POINTS;
    d_sz   = DOMAIN_SIZE;
    m_frc  = MOLE_FRACTIONS;
    fvcco  = FINITE_VOLUME_CENTROID_COORDINATES;
    p_el   =3; % number of element to plot alphabetical order
%************************************************************
%initialize vars
    x=zeros(1,n_gp(1)); %x axis %%%%% read lower and higher limit 
    y=zeros(1,n_gp(2)); %y axis %%%%% read lower and higher limit 
    cnt_1=1;
    cnt_2=1;
%************************************************************
%Rearrange data
    for i=1:n_gp(1)
        x(i)= fvcco(cnt_1);
        for j=1:n_gp(2)
            cnt_1=cnt_1 + 1;
            if (i == 1)
                y(cnt_2) = fvcco(cnt_1);
                cnt_2 = cnt_2 + 1;
            end
            cnt_1=cnt_1+1;
        end
    end
    
    
    
    
    % %************************************************************
    %     
    %     
    %     test=size(m_frc,1)
    %     ndt=size(m_frc,1)/(n_el*n_gp(1)*n_gp(2));
    % 
    %     xf_final=zeros(n_gp(2),n_gp(1));
    % 
    % 
    % 
    % %rows are 'y' and columns are 'x'
    % for plot_timestep=1:ndt
    %     cnt_1=(plot_timestep-1)*n_el*n_gp(1)*n_gp(2)+p_el;
    %     for i=1:n_gp(1)
    %         for j=1:n_gp(2)
    %             xf_final(j,i)=MOLE_FRACTIONS(cnt_1);
    %             cnt_1=cnt_1+n_el;
    %         end
    %     end
    % end
    % 
    % 
    % 
    % 
    % 
    % 
    % 

%************************************************************
%figure
%surf(x,y,xf_final);title(strcat('t=',num2str(TIME(plot_timestep))));


%figure
%Diagonal 2D
%idx = [1:size(xf_final,1)]
%diag_coordinates = sqrt(2)*abs(x(idx)) 
%plot(diag_coordinates,xf_final(idx))



%figure
%%double cut
%idx = [20:size(xf_final,1)]
%surf(x(idx),y(idx),xf_final(idx, idx));
%title(strcat('t=',num2str(TIME(plot_timestep))));



%figure
%%axis cut
%idx = 20
%surf(x,y,xf_final);
%hold on
%xlim("manual")
%xlim([x(1,idx) max(x)])
%title(strcat('t=',num2str(TIME(plot_timestep))));



%figure
%%X cut
%plot(y,xf_final(2,:))

%************************************************************
%     figure
%     %y cut
%     plot(x,xf_final(:,2))