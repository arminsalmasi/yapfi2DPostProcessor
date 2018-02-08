
plot_element=2;

x=zeros(1,ngp(1));
y=zeros(1,ngp(2));
k=1;
m=1;
for i=1:ngp(1)
    x(i)=FINITE_VOLUME_CENTROID_COORDINATES(k);
    for j=1:ngp(2)
        k=k+1;
        if (i==1)
            y(m)=FINITE_VOLUME_CENTROID_COORDINATES(k);
            m=m+1;
        end
        k=k+1;
    end
end

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
    surf(x,y,xf_final);title(strcat('t=',num2str(TIME(plot_timestep))))
    pause(1.0)
end

