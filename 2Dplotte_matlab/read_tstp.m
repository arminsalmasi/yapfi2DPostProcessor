load([folderName '\postDataTmp.mat'], 'time', 'ndt');
S1 = 'Simulated time is: ';
S2 = 'Number of times steps is: ';
S3 = 'Please select timesteps to plot (none zero posetive integers smaller than number of times steps, space seperated:)';
x = inputdlg([S1 num2str(time(end)) ', ' S2 num2str(ndt) ', ' S3],...
             'Sample', [1 200]);        
tstps = unique(sort( floor(str2num(x{:}))));
tstps = tstps(tstps>0);
tstps = tstps(tstps<=ndt);
if isempty(tstps)
    tstps=[1 ndt]
end
clear S1 S2 S3 x ;
%% read time steps from file and save to .mat file
for i = 1: size(tstps,2)
    tstpReader(tstps(i),folderName);
end
clear i;

function []=tstpReader(tstp,dirName)
load([dirName '\postDataTmp.mat'],...
  'ndm','domSize','ngp','fvcc','time','ndt', ...
  'nel','nph', ...
  'elNames','phNames','phNamesPlot','phNamesTc',...
  'plotVolFlg',...
  'molFrac', 'phFrac', 'chemPot');
%% read grid point x
x=zeros(1,ngp(1)); y=zeros(1,ngp(2)); idx=1; j=1;
for el=1:ngp(1)
  x(el)=fvcc(idx);
  for i=1:ngp(2)
    idx=idx+1;
    if (el == 1)
      y(j) = fvcc(idx);
      j=j+1;
    end
    idx=idx+1;
  end
end
%%
tsMolFrac = molFrac(nel*ngp(1)*ngp(2)*(tstp-1)+1 : nel*ngp(1)*ngp(2)*tstp);
tsPhFrac = phFrac(nph*ngp(1)*ngp(2)*(tstp-1)+1 : nph*ngp(1)*ngp(2)*tstp);
tsChemPot = chemPot(nel*ngp(1)*ngp(2)*(tstp-1)+1 : nel*ngp(1)*ngp(2)*tstp);
for el= 1 : nel
  elTsMolFracLin(el,:)=tsMolFrac(el:nel:end);
  elTsChemPotLin(el,:)=tsChemPot(el:nel:end);
  idx=1;
  for i=1:ngp(1)
    for j=1:ngp(2)
      elTsMolMat(el,j,i)=elTsMolFracLin(el,idx);
      elTsChpMat(el,j,i)=elTsChemPotLin(el,idx);
      idx=idx+1;
    end
  end
end
for ph= 1 : nph
  elTsPhFracLin(ph,:)=tsPhFrac(ph:nph:end);
  idx=1;
  for i=1:ngp(1)
    for j=1:ngp(2)
      elTsPhMat(ph,j,i)=elTsPhFracLin(ph,idx);
      idx=idx+1;
    end
  end    
end

fname= ['fs_t_' num2str(time(tstp),'%10.0f')];
msg5 = ['Variables are saved to ' fname '.mat'];
sprintf('%s',msg5)
timeToPlot=time(tstp);
save([dirName '\' fname],...
  'elTsMolMat','elTsChpMat','elTsPhMat',...
  'elNames','phNames','phNamesPlot','phNamesTc',...
  'nel','nph','tstp','ndm','domSize','ngp','x','y','fvcc',...
  'timeToPlot');

clear elTsMolMat elTsChpMat elTsPhMat
clear elNames phNames phNamesPlot phNamesTc
clear  nel nph tstp ndm domSize ngp x y fvcc
clear i j k m
clear   ndm domSize ndt elNames plotVolFlg molFrac phFrac chemPot
%   %% Calculate volume fractions
%           %for i = 1 : size(mfs,2)
%             %[v(i,:),vpv(i,:),vm(i,:),vmtot(i)]=...
%             %singlepoint(elnames,phnamesTC,mfs(:,i),1670,101325,1,'tcfe9');%(elnames,phnamesTC,mfs,T,P,N,dtbs)
%             %sprintf('%s' , [num2str(i) ' point out of ' num2str(size(mfs,2)) ' points'])         
%           %end
%         %//////////////////////////////////////////////////////////////////////////   
%           sel =[];    
%           for i = 1 : size(elnames,2)
%             Xi_names(i) = cellstr([ 'X(' elnames{i} ')']);
%             if i==1
%               sel = strcat(sel,elnames{i});
%             else
%               sel = strcat(sel, {' '}, elnames{i});
%             end
%           end
%           phs =[];
%           for i = 1 : size(phnamesTC,2)
%             if i==1
%               phs = strcat(phs,phnamesTC{i});
%             else
%               phs = strcat(phs, {' '}, phnamesTC{i});
%             end
%           end   
%         %% Initiate the TC system
%           tc_init_root;
%         %% Choose database %dtbs='TCFE9'; %tc_open_database('TCFE9');
%           dtbs='TCFE9';N=1;P=101325;T=1673;
%         %% Select elements and phases and retrieve data
%           tc_define_system(dtbs,sel,{'*'},phs);
%           tc_get_data;
%         %% set conditions 
%           tctoolbox('tc_set_condition', 'n', N);
%           tctoolbox('tc_set_condition', 'p', P);%101325
%           tctoolbox('tc_set_condition', 't', T);
%           for j= 1: size(mfs,2)    
%             %tic
%             for i = 1: (size(Xi_names,2)-1)
%               tctoolbox('tc_set_condition',char(Xi_names(i)), mfs(i,j));
%             end
%         %   [aa] = tc_degrees_of_freedom;
%         %     sprintf('%s', ['DOF = ' num2str(aa)] )
%             %% compute equilibrium
%               tc_compute_equilibrium;
%               %toc
%             %% Read volume data
%               TF=[];
%               for i = 1 :  size(phnamesTC,2)
%                 TF = contains(phnamesTC(i),'ZZDICTRA_GHOST','IgnoreCase',true);
%                 if ~TF         
%                   Np(i,j)  = tc_get_value(char(['Np( ' char(phnamesTC(i)) ')']));
%                   Npm(i,j) = tc_get_value(char(['Npm(' char(phnamesTC(i)) ')']));
%                   Npv(i,j) = tc_get_value(char(['Npv(' char(phnamesTC(i)) ')']));
%                   Bp(i,j)  = tc_get_value(char(['Bp( ' char(phnamesTC(i)) ')']));
%                   Bpm(i,j) = tc_get_value(char(['Bpm(' char(phnamesTC(i)) ')']));
%                   Bpv(i,j) = tc_get_value(char(['Bpv(' char(phnamesTC(i)) ')']));
%                   Vp(i,j)  = tc_get_value(char(['Vp( ' char(phnamesTC(i)) ')']));          
%                   Vpm(i,j) = tc_get_value(char(['vpm(' char(phnamesTC(i)) ')']));
%                   Vpv(i,j) = tc_get_value(char(['vpv(' char(phnamesTC(i)) ')']));
%                   Vmp(i,j) = tc_get_value(char(['vm( ' char(phnamesTC(i)) ')']));
%                 end        
%               end
% 
%               N = tc_get_value('N');
%               v(j) = tc_get_value('v');
%               vm(j) = tc_get_value('vm');
%               B(j) = tc_get_value('B');
% 
%           end
%   %//////////////////////////////////////////////////////////////////////////    
 
end

