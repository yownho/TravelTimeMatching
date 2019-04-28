clear all
addpath('C:\Users\Yuanhao\Google Drive\RschGeneral\Scale_SSS_OHRO\Code\MATLAB')
% read text file
path = 'C:\Users\Yuanhao\Google Drive\Paper_own\3rd_paper\Figures';
outfig = 'coeff_plot.tiff';
csv_coef = 'C:\Users\Yuanhao\Google Drive\Paper_own\3rd_paper\Figures\TablesII.csv';

coef = readtable(csv_coef);
Us = table2array(coef(:,1));
Uh = table2array(coef(:,2));
Pr = 1- table2array(coef(:,3));
NSE = table2array(coef(:,4));
RMSE = table2array(coef(:,5));

Uh = 1./Uh;

figure(1)
%hFig = figure(1);
%set(hFig, 'Position', [0 0 850 1100])
subplot(3,1,1);
pointsize = 10;
[U_s,U_h] = meshgrid(Us,Uh);
Pr_m = meshgrid(Pr);
contourf(U_s,U_h,griddata(Us,Uh,Pr,U_s,U_h),100,'edgecolor','none')    % draw the scatter plot
% scatter(Us,Uh,pointsize,Pr,'filled')    % draw the scatter plot
cb = colorbar;
set(gca,'FontSize',18)
set(gca,'yticklabel',num2str(get(gca,'ytick')','%.1f'))
%xlim([10,100]);
xlabel('\lambda_{s}','fontsize',18);
ylabel('\lambda_{h}','fontsize',18);
title('Peak error')

t=get(cb,'Limits');
T=linspace(t(1),t(2),5);
set(cb,'Ticks',T)
TL=arrayfun(@(x) sprintf('%.3f',x),T,'un',0);
set(cb,'TickLabels',TL)

%figure(2)
subplot(3,1,2);
pointsize = 10;
%[U_s,U_h] = meshgrid(Us,Uh);
% NSE_m = meshgrid(NSE);
contourf(U_s,U_h,griddata(Us,Uh,NSE,U_s,U_h),100,'edgecolor','none')    % draw the scatter plot
%scatter(Us,Uh,pointsize,NSE,'filled')    % draw the scatter plot
cb = colorbar;
set(gca,'FontSize',18)
set(gca,'yticklabel',num2str(get(gca,'ytick')','%.1f'))
%xlim([10,100]);
xlabel('\lambda_{s}','fontsize',18);
ylabel('\lambda_{h}','fontsize',18);
title('NSE')

t=get(cb,'Limits');
T=linspace(t(1),t(2),5);
set(cb,'Ticks',T)
TL=arrayfun(@(x) sprintf('%.3f',x),T,'un',0);
set(cb,'TickLabels',TL)

% figure(3)
subplot(3,1,3);
pointsize = 10;
[U_s,U_h] = meshgrid(Us,Uh);
% RMSE_m = meshgrid(RMSE);
contourf(U_s,U_h,griddata(Us,Uh,RMSE,U_s,U_h),100,'edgecolor','none')    % draw the scatter plot
% scatter(Us,Uh,pointsize,RMSE,'filled')    % draw the scatter plot
%contourf(U_s,U_h,RMSE_m,100,'edgecolor','none')    % draw the scatter plot
%surf(U_s,U_h,RMSE)
cb = colorbar;
set(gca,'FontSize',18)
set(gca,'yticklabel',num2str(get(gca,'ytick')','%.1f'))
%xlim([10,100]);
xlabel('\lambda_{s}','fontsize',18);
ylabel('\lambda_{h}','fontsize',18);
title('RMSE')

t=get(cb,'Limits');
T=linspace(t(1),t(2),5);
set(cb,'Ticks',T)
TL=arrayfun(@(x) sprintf('%.3f',x),T,'un',0);
set(cb,'TickLabels',TL)