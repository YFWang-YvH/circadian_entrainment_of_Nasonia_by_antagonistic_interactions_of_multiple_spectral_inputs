%% 
% Matlab script for plotting 3D GAM model and mean data

load("dmean_matlab.mat") %average data set
load("GAMm1.mat") %all data
%%
wavelength = unique(GAMm1.wavelength)';
intensity = unique(GAMm1.lightintensity)';
R = zeros(size(intensity, 2), size(wavelength, 2)); % response (preallocation)

for cwi = 1:length(wavelength)
    R(:,cwi)=GAMm1.phaseshift((cwi-1)*50+1:cwi*50)';
end

%writematrix(R,"GAM_3D_plot.csv")
%%
clf
subplot(3, 3, [1 6]);
[X, Y] = meshgrid(intensity,wavelength); %return 2-D grid
surf(X, Y, R); %R must be a matrix
alpha 0.7
hold on
stem3(log10(dmean_matlab.lightintensity),dmean_matlab.wavelength, dmean_matlab.meanphaseshift, 'ko', 'MarkerEdgeColor','k', 'MarkerFaceColor', [1.0 0.0 0.0], 'MarkerSize', 10);
axis tight