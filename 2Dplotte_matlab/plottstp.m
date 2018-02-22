close all
for i = 1 : size(tstps,2)
  fname= ['fs_t_' num2str(time(tstps(i)),'%10.0f')];
  
  load([folderName '\' fname],...
    'elTsMolMat','elTsChpMat','elTsPhMat',...
    'elNames','phNames','phNamesPlot','phNamesTc',...
    'nel','nph','tstp','ndm','domSize','ngp','x','y','fvcc',...
    'timeToPlot');

  if i==1
    xf = zeros(size(tstps,2), size(elTsMolMat,1),size(elTsMolMat,2),size(elTsMolMat,3));
    chp = zeros(size(tstps,2), size(elTsChpMat,1),size(elTsChpMat,2),size(elTsChpMat,3));
    npm = zeros(size(tstps,2), size(elTsPhMat,1),size(elTsPhMat,2),size(elTsPhMat,3));
  end
  tmToPlotf(i)= timeToPlot;
  xf(i,:,:,:)= elTsMolMat;
  chp(i,:,:,:) = elTsChpMat;
  npm(i,:,:,:) = elTsPhMat;
  clear elTsMolMat elTsChpMat elTsPhMat
  
end  

%% Plot individual mole fractions along diagonal
  choice = questdlg('Plot mole-fractions?','plotting','YES', 'NO', 'YES');
  switch choice
    case 'YES'
      choice = questdlg('Save to file?','save to file','.fig', '.png', 'NO', 'No');
      for i = 1 :nel
        figure
        hold on
        box on
        for j = 1 : size(tstps,2)    
          xftemp(:,:) = xf(j,i,:,:);  
          idx = [1:size(x,2)];
          diagCC = sqrt(x.^2+y.^2);
          plot(diagCC,diag(xftemp)')
          %surf(x,y, xftemp(:,:));
          legendcell(j)=cellstr(['t= ' num2str(tmToPlotf(j)','%5.0f') ' sec']);
        end
        xlabel('Distance [m]','FontSize',15); %'Interpreter','latex'
        ylabel(['Mole-Fraction ' elNames{i}],'FontSize',15); %'Interpreter','latex',
        leg=legend(legendcell,'FontSize',15);
%          choice = questdlg('Save to file?','save to file','.fig', '.png', 'NO', 'No');
          switch choice
            case '.fig'
              saveas(gcf,[folder_name '\mfclose alls' elNames{i} '.fig'])
            case '.png'
              saveas(gcf,[folder_name '\mfs' elNames{i} '.png'])
          end 
      end
  end

%% Plot individual phase fractions along diagonal
  choice = questdlg('Plot phase-fractions?','plotting','YES', 'NO', 'YES');
  switch choice
    case 'YES'
      choice = questdlg('Save to file?','save to file','.fig', '.png', 'NO', 'No');
      for i = 1 :nph
        TF = contains(phNamesPlot(i),'ZZDICTRA-GHOST','IgnoreCase',true);
        if ~TF 
          figure
          hold on
          box on
          
          for j = 1 : size(tstps,2)    
            pftemp(:,:) = npm(j,i,:,:);  
            idx = [1:size(x,2)];
            diagCC = sqrt(x.^2+y.^2);
            
            plot(diagCC, diag(pftemp)');
            legendcell(j)=cellstr(['t= ' num2str(tmToPlotf(j)','%5.0f') ' sec ']);
          end
          xlabel('Distance [m]','FontSize',15);
          ylabel(['Phase-Fraction ' ,phNamesPlot{i}],'FontSize',15);
          leg=legend(legendcell,'FontSize',15);
%           choice = questdlg('Save to file?','save to file','.fig', '.png', 'NO', 'No');
            switch choice
              case '.fig'
                  saveas(gcf,[folder_name '\pfs' num2str(i) '.fig'])
              case '.png'
                  saveas(gcf,[folder_name '\pfs' num2str(i) '.png'])
            end
        end    
      end
  end

  
%% Plot chemical potentials along diagonal
choice = questdlg('Plot chemical potentials?','plotting','YES', 'NO', 'YES');
switch choice
  
    case 'YES'
       choice = questdlg('Save to file?','save to file','.fig', '.png', 'NO', 'No');
        for i = 1 :nel
            figure
            hold on
            box on
            for j = 1 : size(tstps,2)    
                chemptemp(:,:) = chp(j,i,:,:);  
                idx = [1:size(x,2)];
                diagCC = sqrt(x.^2+y.^2);
                plot(diagCC, diag(chemptemp));
                legendcell(j)=cellstr(['t= ' num2str(tmToPlotf(j)','%5.0f') ' sec ']);
            end
            xlabel('Distance [m]','FontSize',15);
            ylabel(['Chemical potential ' elNames{i}],'FontSize',15);      
            leg=legend(legendcell,'FontSize',15);
            leg.Location='northwest';
% %             choice = questdlg('Save to file?','save to file','.fig', '.png', 'NO', 'No');
                switch choice
                    case '.fig'
                        saveas(gcf,[folder_name '\chemps' elnames{i} '.fig'])
                    case '.png'
                        saveas(gcf,[folder_name '\chemps' elnames{i} '.png'])
                end
        end  
end




%% Surfe individual mole fractions 
for j = 1 : size(tstps,2)      
  choice = questdlg(['Surfe mole-fractions at time = '  num2str(tmToPlotf(j)) '?'],'plotting','YES', 'NO', 'YES');
  switch choice
    case 'YES'
      choice = questdlg('Save to file?','save to file','.fig', '.png', 'NO', 'No');
      for i = 1 :nel
        figure
        hold on
        box on
        xftemp(:,:) = xf(j,i,:,:);  
        surf(x,y,xftemp)
        %az = 90;  %el = 45;   %view(az, el);
        view([-1,-1,1])
        legendcell=cellstr(['t= ' num2str(tmToPlotf(j)','%5.0f') ' sec']);
        xlabel('X [m]','FontSize',15); %'Interpreter','latex'
        ylabel('Y [m]','FontSize',15); %'Interpreter','latex'
        zlabel(['Mole-Fraction ' elNames{i}],'FontSize',15); %'Interpreter','latex',
        leg=legend(legendcell,'FontSize',15);
        leg.Location='north';
      end

      switch choice
        case '.fig'
          saveas(gcf,[folder_name '\mfclose alls' elNames{i} '.fig'])
        case '.png'
          saveas(gcf,[folder_name '\mfs' elNames{i} '.png'])
      end 
    end
  end
  
%% Surfe individual chemical potentials
for j = 1 : size(tstps,2)      
  choice = questdlg(['Surfe chemical-potenitals at time = '  num2str(tmToPlotf(j)) '?'],'plotting','YES', 'NO', 'YES');
  switch choice
    case 'YES'
      choice = questdlg('Save to file?','save to file','.fig', '.png', 'NO', 'No');
      for i = 1 :nel
        figure
        hold on
        box on
        chptemp(:,:) = chp(j,i,:,:);  
        surf(x,y,chptemp)
        %az = 90;  %el = 45;   %view(az, el);
        view([-1,-1,1])
        legendcell=cellstr(['t= ' num2str(tmToPlotf(j)','%5.0f') ' sec']);
        xlabel('X [m]','FontSize',15); %'Interpreter','latex'
        ylabel('Y [m]','FontSize',15); %'Interpreter','latex'
        zlabel(['Chemical-potential ' elNames{i}],'FontSize',15); %'Interpreter','latex',
        leg=legend(legendcell,'FontSize',15);
        leg.Location='north';
      end

      switch choice
        case '.fig'
          saveas(gcf,[folder_name '\mfclose alls' elNames{i} '.fig'])
        case '.png'
          saveas(gcf,[folder_name '\mfs' elNames{i} '.png'])
      end 
    end
end

  
%% Surfe individual phae fractions
for j = 1 : size(tstps,2)      
  choice = questdlg(['Surfe Phase-fractions at time = '  num2str(tmToPlotf(j)) '?'],'plotting','YES', 'NO', 'YES');
  switch choice
    case 'YES'
      choice = questdlg('Save to file?','save to file','.fig', '.png', 'NO', 'No');
      for i = 1 :nph
        figure
        hold on
        box on
        npmtemp(:,:) = npm(j,i,:,:);  
        surf(x,y,npmtemp)
        %az = 90;  %el = 45;   %view(az, el);
        view([-1,-1,1])
        legendcell=cellstr(['t= ' num2str(tmToPlotf(j)','%5.0f') ' sec']);
        xlabel('X [m]','FontSize',15); %'Interpreter','latex'
        ylabel('Y [m]','FontSize',15); %'Interpreter','latex'
        zlabel(['Phase-fraction ' phNamesPlot{i}],'FontSize',15); %'Interpreter','latex',
        leg=legend(legendcell,'FontSize',15);
        leg.Location='north';
      end

      switch choice
        case '.fig'
          saveas(gcf,[folder_name '\mfclose alls' elNames{i} '.fig'])
        case '.png'
          saveas(gcf,[folder_name '\mfs' elNames{i} '.png'])
      end 
    end
  end


  
     