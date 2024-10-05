function [lam, sig] = f_sample_lam_sig(y, s, P, sig)

% extract dimensions
[Nr, Nt] = size(s);
Nd = size(y, 1);

% lam
denom = zeros(Nr*Nd, Nr*Nd); 
numer = zeros(Nd, Nr);
for t = 1 : Nt 
    ytemp = y(:, t); 
    Wt = diag(~isnan(ytemp));
    ytemp(isnan(ytemp)) = 0;  
    denom = denom + kron(s(:, t) * s(:, t)' + P(:, :, t), Wt); % see Watson and Engle (1982, eqn. 16) for the precise formula (sum of squared smoothed factors + smoothed covariance) 
    numer = numer + Wt*ytemp * s(:, t)';
end 
lam = reshape(denom\numer(:), Nd, Nr);

% sig
temp = zeros(Nd) ; 
for t = 1 : Nt 
        ytemp = y(:, t); 
        Wt = diag(~isnan(ytemp));
        ytemp(isnan(ytemp)) = 0 ;   
        temp = temp + Wt*(ytemp*ytemp')*Wt' - Wt*ytemp*s(:,t)'*lam'*Wt - ...
               Wt*lam*s(:,t)*ytemp'*Wt + ...
               Wt*lam * (s(:,t)*s(:,t)' + P(:,:,t)) * lam' * Wt + ...
               (eye(Nd) - Wt) * diag(sig) * (eye(Nd) - Wt) ;
end

sig = diag(1/Nt * temp); % overwrite previous estimate of sig

