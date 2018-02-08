clc
clear all
close all
load NUMBER_OF_ELEMENTS.TXT
load NUMBER_OF_PHASES.TXT
load DIMENSIONALITY.TXT
load NUMBER_OF_GRID_POINTS.TXT
load DOMAIN_SIZE.TXT
load FINITE_VOLUME_CENTROID_COORDINATES.TXT
load MOLE_FRACTIONS.TXT
load CHEMICAL_POTENTIALS.TXT
load TIME.TXT

nel=NUMBER_OF_ELEMENTS;
nph=NUMBER_OF_PHASES;
ngp=NUMBER_OF_GRID_POINTS;
domain_size=DOMAIN_SIZE;

plot_element=3;

x=zeros(1,ngp(1));
y=zeros(1,ngp(2));
k=1;
m=1;
for i=1:ngp(1)
    x(i)=FINITE_VOLUME_CENTROID_COORDINATES(k);
    for j=1:ngp(2)
        k=k+1;
        if (i == 1)
            y(m) = FINITE_VOLUME_CENTROID_COORDINATES(k);
            m=m+1;
        end
        k=k+1;
    end
end
test=size(MOLE_FRACTIONS,1)
ndt=size(MOLE_FRACTIONS,1)/(nel*ngp(1)*ngp(2));

xf_final=zeros(ngp(2),ngp(1));



%rows are 'y' and columns are 'x'
for plot_timestep=1:ndt
    k=(plot_timestep-1)*nel*ngp(1)*ngp(2)+plot_element;
    for i=1:ngp(1)
        for j=1:ngp(2)
            xf_final(j,i)=MOLE_FRACTIONS(k);
            k=k+nel;
        end
    end
end



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


figure
%y cut
plot(x,xf_final(:,2))