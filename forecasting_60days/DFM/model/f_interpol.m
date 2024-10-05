function ystar = f_interpol(y)
% function to linearly interpolate missings in an NxT matrix 
ystar = y; 

% Get the size of the matrix y
[nRows, nCols] = size(y);

% Loop over each row
for row = 1:nRows
    % Find the last non-NaN value in the row
    lastNonNan = find(~isnan(y(row,:)), 1, 'last');
    
    % If there's a NaN after the last non-NaN and before the last column, fill them
    if ~isempty(lastNonNan) && lastNonNan < nCols
        ystar(row, lastNonNan+1:nCols-1) = ystar(row, lastNonNan);
    end
end

for i = 1:size(y, 1)
    t = 2;
    while t < size(y, 2)
      ind_nextobs = 1;
        if isnan(ystar(i, t))  
            t_NaN = true;
            while t_NaN
                if isnan(ystar(i, t+ind_nextobs))
                    ind_nextobs = ind_nextobs + 1;
                else
                    t_NaN = false;
                end
            end
            ystar(i, t:t+ind_nextobs - 1) = ystar(i, t-1) + (1:ind_nextobs) * ((ystar(i, t + ind_nextobs) - ystar(i, t-1))/ind_nextobs);
        end
        t = t + ind_nextobs;
    end
end

% replace missings in period 1 and T with values from 2 or T-1
if any(isnan(y(:, 1)))
    ind_nan = isnan(y(:, 1));
    ystar(ind_nan, 1) = ystar(ind_nan, 2);
end

if any(isnan(y(:, end)))
    ind_nan = isnan(y(:, end));
    ystar(ind_nan, end) = ystar(ind_nan, end-1);
end

% Loop over each row
for row = 1:nRows
    if isnan(ystar(row,1))       
        % Find the last NaN value in the row
        lastNan = find(isnan(ystar(row,:)), 1, 'last');
        % If there are NaNs at the beginning of the series, fill them in
        % with the first non-NaN observation
        ystar(row, 1:lastNan) = ystar(row, lastNan+1);
    end
end

end