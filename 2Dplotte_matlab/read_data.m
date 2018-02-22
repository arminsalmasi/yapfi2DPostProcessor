clear variables
close all
clc
currentPath = pwd ;
folderName = uigetdir(currentPath);
%% read profiles from file/ save in a .mat file
if exist([folderName '\postDataTmp.mat'], 'file')
  choice = questdlg('Warning: file postDataTmp exist','warning','Overwrite', 'Use the old file', 'Overwrite');
  switch choice
    case 'Overwrite'
      readProfiles(folderName,currentPath);
  end
else
  readProfiles(folderName,currentPath);
end
clear choice;

  function []=readProfiles(dirName,curPath)
  load ([dirName '\CHEMICAL_POTENTIALS.TXT']);
  chemPot = CHEMICAL_POTENTIALS;
  load ([dirName '\DIMENSIONALITY.TXT' ],'-ascii');
  ndm = DIMENSIONALITY;
  load ([dirName '\DOMAIN_SIZE.TXT']);
  domSize = DOMAIN_SIZE;
  load ([dirName '\TIME.TXT']);
  time = TIME;
  ndt=size(time,1);
  load ([dirName '\NUMBER_OF_ELEMENTS.TXT' ],'-ascii');
  nel = NUMBER_OF_ELEMENTS;
  load ([dirName '\FINITE_VOLUME_CENTROID_COORDINATES.TXT']);
  fvcc = FINITE_VOLUME_CENTROID_COORDINATES;
  load ([dirName '\GRADIENT_ENERGY_CONTRIBUTION.TXT']);
  load ([dirName '\NUMBER_OF_PHASES.TXT' ],'-ascii');
  nph = NUMBER_OF_PHASES;
  load ([dirName '\MOLE_FRACTIONS.TXT']);
  molFrac = MOLE_FRACTIONS;
  load ([dirName '\PHASE_FRACTIONS.TXT']);
  phFrac = PHASE_FRACTIONS;
  load ([dirName '\NUMBER_OF_GRID_POINTS.TXT']);
  ngp =NUMBER_OF_GRID_POINTS;
  load ([dirName '\PERMEABILITIES.TXT']);
  [fid,msg]=fopen([dirName '\ELEMENT_NAMES.TXT' ],'r');
  [val,count]=fread(fid);
  res=fclose(fid);
  elNames=cell(0);
  j=1;
  for i=1:nel
    k=j;
    ok=1;
    while ok==1
      k=k+1;
      if val(k)==13
        %ok=0;
        val(k)=[];
      end
      if val(k)==10
        ok=0;
      end
    end
    elNames{i}=char(val(j:k-1)');
    j=k+1;
  end

  [fid,msg]=fopen([dirName '\PHASE_NAMES.TXT' ],'r');
  [val,count]=fread(fid);
  res=fclose(fid);
  phNames=cell(0);
  j=1;
  for i=1:nph
    k=j;
    ok=1;
    while ok==1
      k=k+1;
      if val(k)==13
        %ok=0;
        val(k)=[];
      end
      if val(k)==10
        ok=0;
      end
    end
    phNames{i}=char(val(j:k-1)');
    j=k+1;
  end
  phsStr = phNames';
  for i = 1: size(phsStr,1)
    temp1 = phsStr{i};
    temp2 = temp1;
    k1 = strfind(temp1,'#');
    k2 = strfind(temp2,'_');
    if ~isempty(k1)
      temp1= temp1(1:k1-1);
    end
    if ~isempty(k2)
      temp2(k2)='-';
    end       
    phsStr1{i} = temp1;
    phsStr2{i} = temp2;
  end
  phNamesTc = phsStr1;
  phNamesPlot = phsStr2;



  plotVolFlg=0;
  if exist([dirName '\MOLAR_VOLUME.TXT'], 'file')
    %[fid,msg]=fopen([folder_name '\MOLAR_VOLUME.TXT'],'r');
    %if (fid>0)
    %res=fclose(fid);
    load ([dirName '\MOLAR_VOLUME.TXT']);
    plotVolFlg=1;
  end

  % Read Atmoic masses from text file
  atmDataTable=readtable([curPath '\atomicmass.txt']);
  atmNamesTable=atmDataTable(:,2);
  atmMassTable=atmDataTable(:,3);
  atmNames = table2array(atmNamesTable);
  atmMass = table2array(atmMassTable);
  atmNames=upper(atmNames);
  for i = 1 : nel
    a = find(contains(atmNames, char(elNames{i})));
    for j=1:size(a,1)
      if char(atmNames(a(j)))== char(elNames{i})
        M(i)=atmMass(a(j));
      end
    end
    clear a
  end
  Er1 = 'Error: Check end of file character in ELEMENT_NAMES.TXT and rerun.';
  if size(M,2)~=nel
    sprintf('%s', Er1)
  end
  save([dirName '\postDataTmp.mat'],...
       'ndm','domSize','ngp','fvcc','time','ndt', ...
       'nel','nph', ...
       'elNames','phNames','phNamesPlot','phNamesTc',...
       'plotVolFlg',...
       'molFrac', 'phFrac', 'chemPot');
  clear variables
end