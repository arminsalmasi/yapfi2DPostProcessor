close all;
clear all;
clc;

timeStepList = [301, 902, 3604, 4598, 7200];
elementList = [1:5];

% 
% for timeStep = timeStepList
%     for element = elementList
%         clear data h1 h2
%         close all
%         fileName = ['data_result_ndt-' int2str(timeStep) '_plot-element_' int2str(element) '.mat'];
%         data = load(fileName);
%         
%         name =[num2str(timeStep) '-' data.elementName '-surface'];
%         h1 = figure('Name',name);
%         surf(data.x,data.y,data.xf_final); ...
%             title(strcat('Time=',num2str(data.ndtValue), ', Element=', data.elementName), ...
%             'FontWeight','bold','FontSize',16);
%         % Create ylabel
%         xlabel('Distance (m)','FontWeight','bold','FontSize',16);
%         ylabel('Distance (m)','FontWeight','bold','FontSize',16);
%         zlabel('Mole Fraction','FontWeight','bold','FontSize',16);
% 
%         n = h1.Name;
%         saveas(h1, sprintf('%s.fig', n));
%         
%         %Diagonal 2D
%         name =[num2str(timeStep) '-' data.elementName '-diagonal'];
%         h2 = figure('Name',name);
%         title(strcat('Time=',num2str(data.ndtValue), ', Element=', data.elementName), ...
%             'FontWeight','bold','FontSize',16);
%         
%         idx = [1:size(data.xf_final,1)];
%         diag_coordinates = sqrt(2)*abs(data.x(idx)); 
%         hold on
%         plot(diag_coordinates,diag(data.xf_final), 'LineWidth', 1);
%         % Create ylabel
%         xlabel('Diagonal Distance (m)','FontWeight','bold','FontSize',16);
%         ylabel('Mole Fraction','FontWeight','bold','FontSize',16);
%         n = h2.Name;
%         saveas(h2, sprintf('%s.fig', n));
%         hold off
%         
%     end
% end

for element = elementList
    %Diagonal 2D Overlay
    name =['element-' num2str(element) '-diagonal-overlay'];
    h3 = figure('Name',name);  
    
    legendmatrix=cell(size(timeStepList,2),1);
    k = 0;
    markerType = '';
    for timeStep = timeStepList
       if timeStep == timeStepList(1)
           markerType = 'diamond';
       elseif timeStep == timeStepList(2)
            markerType = 'square';
       elseif timeStep == timeStepList(3)
            markerType = 'o';
       elseif timeStep == timeStepList(4)
            markerType = 'x';
       else
            markerType = '*';
       end
        
        clear data
        fileName = ['data_result_ndt-' int2str(timeStep) '_plot-element_' int2str(element) '.mat'];
        data = load(fileName);
        idx = 1:size(data.xf_final,1);
        diag_coordinates = sqrt(2)*abs(data.x(idx)); 
        
        plot(diag_coordinates,diag(data.xf_final), 'LineWidth',1,'Color',[0 0 0], 'Marker', markerType);
        
        k = k + 1;
        legendmatrix{k}=strcat(num2str(data.ndtValue));
        hold on
    end
        
    % Create axis label
    xlabel('Diagonal Distance (m)','FontWeight','bold','FontSize',16,...
        'FontName','Calibri');
    ylabel('Mole Fraction','FontWeight','bold','FontSize',16,...
        'FontName','Calibri');
    title(strcat('Element=', data.elementName),'FontWeight','bold','FontSize',18,'FontName','Calibri');
    hold on;
    
    box on;
    % Create legend
    legend3 = legend(legendmatrix,'show');

    set(legend3,...
        'Position',[0.730077120822622 0.679802881909239 0.150064253534199 0.215356122879749],...
        'FontWeight','bold',...
        'FontSize',14);
    
    n = h3.Name;
    saveas(h3, sprintf('%s.fig', n));
    hold off;
end

markerType = '';
for timeStep = timeStepList
    %Diagonal 2D Overlay
    name =['timestep-' num2str(timeStep) '-diagonal-overlay'];
    h4 = figure('Name',name);        
    legendmatrix=cell(size(elementList,2),1);
    k = 0;
    for element = elementList
       if element == 1 
           markerType = 'diamond';
       elseif element == 2
            markerType = 'square';
       elseif element == 3
            markerType = 'o';
       elseif element == 4
            markerType = 'x';
       else
            markerType = '*';
       end
        
        clear data
        fileName = ['data_result_ndt-' int2str(timeStep) '_plot-element_' int2str(element) '.mat'];
        data = load(fileName);
        hold on;
        idx = 1:size(data.xf_final,1);
        diag_coordinates = sqrt(2)*abs(data.x); 
        k = k + 1;
        plot(diag_coordinates,diag(data.xf_final), 'LineWidth',1,'Color',[0 0 0], 'Marker', markerType);
        legendmatrix{k}=strcat(num2str(data.elementName));
        hold on;
    end
    hold on;
    % Create axis label
    xlabel('Diagonal Distance (m)','FontWeight','bold','FontSize',16,...
        'FontName','Calibri');
    ylabel('Mole Fraction','FontWeight','bold','FontSize',16,...
        'FontName','Calibri');
    title(strcat('Timestep=', num2str(data.ndtValue)),'FontWeight','bold','FontSize',18,'FontName','Calibri');
    hold on;
    
    box on;
    % Create legend
    legend4 = legend(legendmatrix,'show');
    set(legend4,...
        'Position',[0.730077120822622 0.679802881909239 0.150064253534199 0.215356122879749],...
        'FontWeight','bold',...
        'FontSize',14);

    n = h4.Name;
    saveas(h4, sprintf('%s.fig', n));
    hold off;
end