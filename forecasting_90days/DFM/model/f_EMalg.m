function params = f_EMalg(y_d, y_w, y_m, y_q, aux, params, tol)
% ----------------------------------------------------------------------- %
% - EM algorithm a la Banbura and Modugno (2014) mixing daily, weekly
% - monthly and quarterly series. For the latter, aggregation schemes
% - for both stocks and flows are implemented (see Banbura et al. 2011).
% ----------------------------------------------------------------------- %
% - Inputs:
% - y_d: Nd x Nt matrix of daily series 
% - y_w: Nw x Nt matrix of weekly series (can be empty!)
% - y_m: Nm x Nt matrix of monthly series (can be empty!)
% - y_q: Nq x Nt matrix of quarterly series (can be empty!)
% - aux: structure containing the following elements
% -     - ind_sample: logical of dimension Nt + Nh, indicating where the
% -                   obs used to estimate the model ends
% -     - ind_q_flow: logical indicating which of the quarterly series are
% -                   flow variables
% -     - Xi_qd: indicator variable that equals 0 on the first day of a
% -              quarter, 1 otherwise
% -     - W_qd_c: weights of daily obs corresponding to the current quarter
% -     - W_qd_p: weights of daily obs corresponding to the previous quarter
% - params: structure containing starting values (see below for elements)
% - tol: tolerance level determining when the algorithm has converged
% ----------------------------------------------------------------------- %
% - Output
% - params: structure with the following elements
% -          Phi: Nr x (Np*Nr) factor VAR coefficient matrix
% -          Omeg: Nr X Nr covariance matrix of factor VAR residuals
% -          lam_d: Nd x Nr loadings corresponding to daily series
% -          sig2_d: Nd x 1 vector of idiosyncratic variances of daily series
% -          lam_w: Nw x Nr loadings corresponding to weekly series
% -          sig2_w: Nw x 1 vector of idiosyncratic covariances of weekly series
% -          lam_m_flow: Nm_flow x Nr loadings corresponding to monthly flow series
% -          sig2_m_flow: Nm_flow x 1 vector of idiosyncratic covariances of monthly flow series
% -          lam_m_stock: Nm_stock x Nr loadings corresponding to monthly stock series
% -          sig2_m_stock: Nm_stock x 1 vector of idiosyncratic covariances of monthly stock series
% -          lam_q_flow: Nq_flow x Nr loadings corresponding to quarterly flow series
% -          sig2_q_flow: Nq_flow x 1 vector of idiosyncratic covariances of quarterly flow series
% -          lam_q_stock: Nq_stock x Nr loadings corresponding to quarterly stock series
% -          sig2_q_stock: Nq_stock x 1 vector of idiosyncratic covariances of quarterly stock series
% ----------------------------------------------------------------------- %
% - PH 2021/02/16
% ----------------------------------------------------------------------- %

% back out dimensions of state space system
Nd = size(params.lam_d, 1);
Nw = size(params.lam_w, 1);
Nm_flow = size(params.lam_m_flow, 1);
Nm_stock = size(params.lam_m_stock, 1);
Nm = Nm_flow + Nm_stock;
Nq_flow = size(params.lam_q_flow, 1);
Nq_stock = size(params.lam_q_stock, 1);
Nq = Nq_flow + Nq_stock;
Nr = size(params.Phi, 1);
Np_eff = size(params.Phi, 2)/Nr + 1;
Nt = size(aux.Xi_qd, 1); 

% get positions of factors in state vector
[id_f, id_f_lags, id_f_d, id_f_w, id_f_m_flow, id_f_m_stock, id_f_q_flow, id_f_q_stock] = f_id_fs(Nr, Np_eff, Nd, Nw, Nm_flow, Nm_stock, Nq_flow, Nq_stock);

% iterations
maxiter = 100; 
LL_prev = -999999 ; 
%tic
for iter = 1 : maxiter 
    % ------------------------------------------------------------------- %
    % - E-Step ---------------------------------------------------------- %
    % ------------------------------------------------------------------- %

    % - Kalman Smoother
    % ---------------------------    
    dat = [y_d; y_w; y_m; y_q];
    [Z, H, T, R, Q] = f_state_space_params(params, aux, size(dat, 2));
    s0 = zeros(size(T,1),1); 
    P0 = 100 * eye(size(T,1)); 
    [stT,PtT,LL] = f_KS_DK_logL(dat,T,Z,H,R,Q,s0,P0) ;
    %figure;plot(stT(1:Nr,:)'); title(['iter ' num2str(iter)]);
    
    % - check convergence
    % ---------------------------  
    cLL = (LL - LL_prev)/(abs(LL)/2 + abs(LL_prev)/2) ; 
    if iter>1 && cLL < tol; break; end    
    LL_prev = LL;
    
    % ------------------------------------------------------------------- %
    % - M-Step ---------------------------------------------------------- %
    % ------------------------------------------------------------------- %
    
    % lam_d and sig2_d
    if Nd > 0
        [params.lam_d, params.sig2_d] = f_sample_lam_sig(y_d, stT(id_f_d, :), PtT(id_f_d, id_f_d, :), params.sig2_d);
    end

    % lam_w and sig2_w
    if Nw > 0
        [params.lam_w, params.sig2_w] = f_sample_lam_sig(y_w, stT(id_f_w, :), PtT(id_f_w, id_f_w, :), params.sig2_w);
    end

    % lam_m and sig2_m
    if Nm > 0
        if Nm_flow > 0
            [params.lam_m_flow, params.sig2_m_flow] = f_sample_lam_sig(y_m(aux.ind_m_flow,:), stT(id_f_m_flow, :), PtT(id_f_m_flow, id_f_m_flow, :), params.sig2_m(aux.ind_m_flow));
        end
        if Nm_stock > 0
            [params.lam_m_stock, params.sig2_m_stock] = f_sample_lam_sig(y_m(~aux.ind_m_flow,:), stT(id_f_m_stock, :), PtT(id_f_m_stock, id_f_m_stock, :), params.sig2_m(~aux.ind_m_flow));
        end
        params.lam_m = [params.lam_m_flow; params.lam_m_stock];
        params.sig2_m = [params.sig2_m_flow; params.sig2_m_stock];
    end

    % lam_q and sig2_q
    if Nq > 0
        if Nq_flow > 0         
            [params.lam_q_flow, params.sig2_q_flow] = f_sample_lam_sig(y_q(aux.ind_q_flow,:), stT(id_f_q_flow, :), PtT(id_f_q_flow, id_f_q_flow, :), params.sig2_q(aux.ind_q_flow));            
        end

        if Nq_stock > 0         
            [params.lam_q_stock, params.sig2_q_stock] = f_sample_lam_sig(y_q(~aux.ind_q_flow,:), stT(id_f_q_stock, :), PtT(id_f_q_stock, id_f_q_stock, :), params.sig2_q(~aux.ind_q_flow));
        end

        params.lam_q = [params.lam_q_flow; params.lam_q_stock];
        params.sig2_q = [params.sig2_q_flow; params.sig2_q_stock];
    end
    
   
    %if Nr>1
    %    % Fix the loading of the first factor for the first variable to 1
    %    params.lam_d(1, :) = params.lam_d(1, :) / params.lam_d(1, 1);
    %end

   
    % Phi and Omeg
    params.Phi = (stT(id_f, :)*stT(id_f_lags, :)' + sum(PtT(id_f,id_f_lags,:),3))/(stT(id_f_lags, :)*stT(id_f_lags, :)' + sum(PtT(id_f_lags,id_f_lags,:),3)) ; 
    params.Omeg = 1/Nt * ((stT(id_f, :)*stT(id_f, :)' + sum(PtT(id_f,id_f,:),3))  - params.Phi * (stT(id_f, :)*stT(id_f_lags, :)' + sum(PtT(id_f,id_f_lags,:),3))') ;
end

if iter == maxiter
    disp('% ------------------------------------------------ %')
    disp(['% EM algorithm failed to converge after ' num2str(iter) ' iterations'])
    disp('% ------------------------------------------------ %')
else    
    disp('% ------------------------------------------------ %')
    disp(['% EM algorithm converged after ' num2str(iter) ' iterations'])
    disp('% ------------------------------------------------ %')
end
end

