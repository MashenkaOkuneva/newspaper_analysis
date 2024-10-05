function [stT,PtT,LL] = f_KS_DK_logL(data,T,Z,H,R,Q,s0,P0)
    % empty matrices to store smoothed states (as well as their respective covariance matrices
    stT = NaN(size(T,1),size(data,2));
    PtT = NaN(size(T,1),size(T,1),size(data,2));
    a = NaN(size(T,1),size(data,2)+1);
    P = NaN(size(T,1),size(T,1),size(data,2)+1);

    % -------------------------------- %
    % -- forward recursions ---------- %
    % -------------------------------- %

    % empty cells to store v, F and L
    F = cell(size(data,2),1);
    L = NaN(size(T,1),size(T,1),size(data,2));
    v = cell(size(data,2),1);

    % initialise filter
    a(:,1) = s0;
    P(:,:,1) = P0;
    LL = - size(data,2) * size(data,1) / 2 * log(2*pi);    

    for t=1:size(data,2)   
        % check for missings 
        missing = isnan(data(:,t)) ; 
        W = eye(size(data,1)) ; 
        W = W(~missing,:) ;
        WZ = W*Z ; 
        datatemp = data(~missing,t) ; 
        
        % proceed with recursions
        v{t} = datatemp - WZ*a(:,t);
        F{t} = WZ*P(:,:,t)*WZ' + W*H*W';
        F{t} = (F{t}' + F{t}) / 2; % for numerical stability
        K = T(:,:, t)*P(:,:,t)*WZ'/F{t};
        L(:,:,t) = T(:,:, t) - K*WZ;
        
        % compute log likelihood
        LL = LL - 0.5 * (log( det( F{ t } ) ) + v{ t }' / F{ t } *  v{ t } ) ; 

        % update state vector and its covariance matrix
        a(:,t+1) = T(:,:, t)*a(:,t) + K*v{ t };
        P(:,:,t+1) = T(:,:, t)*P(:,:,t)*L(:,:,t)' + R(:,:, t)*Q*R(:,:, t)';
    end

    % -------------------------------- %
    % -- backward recursions --------- %
    % -------------------------------- %
    r = zeros(size(T,1),1);
    %rr(:,end) = r;
    N = zeros(size(T,1));
    for t=size(data,2):-1:1   
        % check for missings 
        missing = isnan(data(:,t)) ; 
        W = eye(size(data,1)) ; 
        W = W(~missing,:) ;
        WZ = W*Z ; 

        % compute r
        r = WZ'/(F{t}+eye(size(F{t},1))*1e-5)*v{t} + L(:,:,t)'*r;

        % smoothed state and covariance matrix
        stT(:,t) = a(:,t) + P(:,:,t)*r;
        N = WZ'/(F{t}+eye(size(F{t},1))*1e-5)*WZ + L(:,:,t)'*N*L(:,:,t);
        PtT(:,:,t) = P(:,:,t) - P(:,:,t)*N*P(:,:,t);
    end
end