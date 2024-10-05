function movie_plot_topics(y_d, y_q)
% function to plot the topics series along with quarterly GDP growth
% in a movie-like fashion, i.e. recursive plots on same figure. Next
% series is plotted when any key is pressed. 
figure(gcf)
for n = 1:size(y_d, 1)
    plot(y_d(n,:)', 'b-')
    hold on
    plot(y_q(1,:)', 'kx')
    title(['topic ' num2str(n-1)])
    pause
    clf
end
