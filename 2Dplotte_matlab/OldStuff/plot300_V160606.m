%NOTE: First run plot_mf_2D_Armin_V160410.m

timeStepList = [301, 902, 3604, 4598, 7200];
elementList = [1:5];


for timeStep = timeStepList
    
    for element = elementList
        clear data h1 h2
        close all
        fileName = ['data_result_ndt-' int2str(timeStep) '_plot-element_' int2str(element) '.mat'];
        data = load(fileName);
        
        name =[num2str(timeStep) '_' data.elementName '_surface'];
        h1 = figure('Name',name);
        surf(data.x,data.y,data.xf_final); ...
            title(strcat('Time=',num2str(data.ndtValue), ', Element=', data.elementName), ...
            'FontWeight','bold','FontSize',16);
        % Create ylabel
        xlabel('Distance (m)','FontWeight','bold','FontSize',16);
        ylabel('Distance (m)','FontWeight','bold','FontSize',16);
        zlabel('Mole Fraction','FontWeight','bold','FontSize',16);

        n = h1.Name;
        saveas(h1, sprintf('%s.fig', n));
        
        %Diagonal 2D
        name =[num2str(timeStep) '_' data.elementName '_diagonal'];
        h2 = figure('Name',name);
        title(strcat('Time=',num2str(data.ndtValue), ', Element=', data.elementName), ...
            'FontWeight','bold','FontSize',16);
        
        idx = [1:size(data.xf_final,1)];
        diag_coordinates = sqrt(2)*abs(data.x(idx)); 
        hold on
        plot(diag_coordinates,data.xf_final(idx), 'LineWidth', 2);
        % Create ylabel
        xlabel('Diagonal Distance (m)','FontWeight','bold','FontSize',16);
        ylabel('Mole Fraction','FontWeight','bold','FontSize',16);
        n = h2.Name;
        saveas(h2, sprintf('%s.fig', n));
        hold off
        
    end
end

for element = elementList
    %Diagonal 2D Overlay
    name =['element_' num2str(element) '_diagonal-overlay'];
    h3 = figure('Name',name);  
    legendmatrix=cell(size(timeStepList,2),1);
    k = 0;
    for timeStep = timeStepList
        clear data
        fileName = ['data_result_ndt-' int2str(timeStep) '_plot-element_' int2str(element) '.mat'];
        data = load(fileName);
        title(strcat('Element=', data.elementName), 'FontWeight','bold','FontSize',16);
        hold on;
        idx = 1:size(data.xf_final,1);
        diag_coordinates = sqrt(2)*abs(data.x(idx)); 
        plot(diag_coordinates,data.xf_final(idx), 'LineWidth', 2);
        k = k + 1;
        legendmatrix{k}=strcat(num2str(data.ndtValue));
        hold on
    end
    hold on
    xlabel('Diagonal Distance (m)','FontWeight','bold','FontSize',16);
    ylabel('Mole Fraction','FontWeight','bold','FontSize',16);
    n = h3.Name;
    legend(legendmatrix, 'FontSize', 10, 'FontWeight', 'bold');
    saveas(h3, sprintf('%s.fig', n));
    hold off;
end


for timeStep = timeStepList
    %Diagonal 2D Overlay
    name =['timestep_' num2str(timeStep) '_diagonal-overlay'];
    h4 = figure('Name',name);        
    legendmatrix=cell(size(elementList,2),1);
    k = 0;
    for element = elementList
        clear data
        fileName = ['data_result_ndt-' int2str(timeStep) '_plot-element_' int2str(element) '.mat'];
        data = load(fileName);
        title(strcat('Timestep=', num2str(data.ndtValue)), 'FontWeight','bold','FontSize',16);
        hold on;
        idx = 1:size(data.xf_final,1);
        diag_coordinates = sqrt(2)*abs(data.x(idx)); 
        plot(diag_coordinates,data.xf_final(idx), 'LineWidth', 2);
        k = k + 1;
        legendmatrix{k}=strcat(num2str(data.elementName));
        hold on
    end
    hold on
    xlabel('Diagonal Distance (m)','FontWeight','bold','FontSize',16);
    ylabel('Mole Fraction','FontWeight','bold','FontSize',16);
    legend(legendmatrix, 'FontSize', 10, 'FontWeight', 'bold');
    n = h4.Name;
    saveas(h4, sprintf('%s.fig', n));
    hold off;
end