%read text file
txt_para_rmse = 'C:\Research\Scale_SSS_OHRO\HRRcali_USGS\320\320_iter_para\para_rmse2.txt';

%read data
para_rmse = fopen(txt_para_rmse,'r');
fmt = '%f %f %f %f';
df_para_rmse = textscan(para_rmse,fmt);
fclose(para_rmse);

%khgw_all
khgw_all = df_para_rmse{1};

%kh_all
kh_all = df_para_rmse{2};

%n_ch_all
n_ch_all = df_para_rmse{3};

%rmse
rmse = df_para_rmse{4};

figure(1)
subplot(2,2,1);
scatter3(khgw_all,kh_all,n_ch_all,60,rmse,'filled')    % draw the scatter plot
ax = gca;
ax.XDir = 'reverse';
view(-31,14)
set(gca,'FontSize',18);
set(gca,'zticklabel',num2str(get(gca,'ztick')','%.1f'))
xlabel('f_{h}','fontsize',18)
ylabel('f_{s}','fontsize',18)
zlabel('f_{n}','fontsize',18)

cb = colorbar;                                     % create and label the colorbar
%h=colorbar
t=get(cb,'Limits');
T=linspace(t(1),t(2),5);
set(cb,'Ticks',T)
TL=arrayfun(@(x) sprintf('%.3f',x),T,'un',0);
set(cb,'TickLabels',TL)
% cb.Label.String = 'RMSE';

%2d plot with rmse as color
subplot(2,2,2);
pointsize = 10;
scatter(khgw_all, kh_all, pointsize, rmse,'filled');
cb = colorbar;
set(gca,'FontSize',18)
set(gca,'yticklabel',num2str(get(gca,'ytick')','%.2f'))
xlim([10,100]);
xlabel('f_{h}','fontsize',18);
ylabel('f_{s}','fontsize',18);
t=get(cb,'Limits');

T=linspace(t(1),t(2),5);
set(cb,'Ticks',T)
TL=arrayfun(@(x) sprintf('%.3f',x),T,'un',0);
set(cb,'TickLabels',TL)
% cb.Label.String = 'RMSE';

subplot(2,2,3);
%pointsize = 10;
scatter(khgw_all, n_ch_all, pointsize, rmse,'filled');
cb = colorbar;
set(gca,'FontSize',18)
set(gca,'yticklabel',num2str(get(gca,'ytick')','%.1f'))
xlim([10,100]);
xlabel('f_{h}','fontsize',18);
ylabel('f_{n}','fontsize',18);

t=get(cb,'Limits');
T=linspace(t(1),t(2),5);
set(cb,'Ticks',T)
TL=arrayfun(@(x) sprintf('%.3f',x),T,'un',0);
set(cb,'TickLabels',TL)


subplot(2,2,4);
%pointsize = 10;
scatter(kh_all, n_ch_all, pointsize, rmse,'filled');
cb = colorbar;
set(gca,'FontSize',18)
set(gca,'xticklabel',num2str(get(gca,'xtick')','%.2f'))
set(gca,'yticklabel',num2str(get(gca,'ytick')','%.1f'))
xlabel('f_{s}','fontsize',18);
ylabel('f_{n}','fontsize',18);
t=get(cb,'Limits');
T=linspace(t(1),t(2),5);
set(cb,'Ticks',T)
TL=arrayfun(@(x) sprintf('%.3f',x),T,'un',0);
set(cb,'TickLabels',TL)

figure(2);
subplot(3,1,1);
scatter(khgw_all, rmse, pointsize,'filled');
xlabel('f_{h}','fontsize',18);
ylabel('RMSE','fontsize',18);
% hold on;
%     coef_fit = polyfit(khgw_all,rmse,1);
%     y_fit = polyval(coef_fit,xlim);
%     plot(xlim,y_fit,'r');
% hold off;

subplot(3,1,2);
scatter(kh_all, rmse, pointsize,'filled');
xlabel('f_{h}','fontsize',18);
ylabel('RMSE','fontsize',18);

subplot(3,1,3);
scatter(n_ch_all, rmse, pointsize,'filled');
xlabel('f_{n}','fontsize',18);
ylabel('RMSE','fontsize',18);
% hold on;
%     [p,S] = polyfit(n_ch_all,rmse,2);
%     y_fit = polyval(p,n_ch_all,S);
%     plot(n_ch_all,y_fit,'r');
% hold off;