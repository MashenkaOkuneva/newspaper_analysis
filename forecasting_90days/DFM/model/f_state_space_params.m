function [Z, H, T, R, Q] = f_state_space_params(params, aux, Nt)

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

% determine size of state vector Ns
Ns = Nr * Np_eff;  
if Nw > 0 && Nd > 0; Ns = Ns+Nr;end 
if Nm_flow > 0 && (Nw > 0 || Nd > 0); Ns = Ns+2*Nr;end % two cumulators for flow  
if Nm_stock > 0 && (Nw > 0 || Nd > 0); Ns = Ns+Nr;end % 1 cumulator for stock
if Nq_flow > 0 && (Nm > 0 || Nw > 0 || Nd > 0); Ns = Ns+2*Nr;end % two cumulators for flow  
if Nq_stock > 0 && (Nm > 0 || Nw > 0 || Nd > 0); Ns = Ns+Nr;end % 1 cumulator  for stock

% T, R => both time-varying
T = NaN(Ns, Ns, Nt);
R = NaN(Ns, Ns, Nt);
for t = 1:Nt
    % T0
    T0 = eye(Ns);
    T0tmp  = []; 
    if Nw > 0 && Nd > 0 
        T0tmp = [T0tmp; -eye(Nr)];
    end
    if Nm_flow > 0 && (Nw > 0 || Nd > 0)
        T0tmp = [T0tmp; -aux.W_md_c(t) * eye(Nr); -aux.W_md_p(t) * eye(Nr)];
    end
    if Nm_stock > 0 && (Nw > 0 || Nd > 0)
        T0tmp = [T0tmp; -eye(Nr)];
    end
    if Nq_flow > 0
        T0tmp = [T0tmp; -aux.W_qd_c(t) * eye(Nr); -aux.W_qd_p(t) * eye(Nr)];
    end
    if Nq_stock > 0
        T0tmp = [T0tmp; -eye(Nr)];
    end
    
    T0(Nr*Np_eff+1:end, 1:Nr) = T0tmp; 
    iT0 = T0 \ eye(Ns);
    
    % R
    R(:, :, t) = iT0; 
    
    % T
    Ttmp = [params.Phi zeros(Nr, Ns-size(params.Phi, 2));
            eye(Nr * (Np_eff-1)) zeros(Nr * (Np_eff-1), Ns - Nr * (Np_eff-1))];
    if Nw > 0 && Nd > 0 % if there are weekly series and its not the highest frequency. Otherwise its dynamics are governed by phi_f!
        Ttmp = [Ttmp; zeros(Nr, size(Ttmp, 1)) aux.Xi_wd(t) * eye(Nr) zeros(Nr, Ns - (size(Ttmp, 1) + Nr))];
    end
    if Nm_flow > 0 && (Nw > 0 || Nd > 0) 
        if aux.Xi_md(t) == 0
            Ttmp = [Ttmp; zeros(2*Nr, size(Ttmp, 1)) zeros(2*Nr, Nr) [eye(Nr); zeros(Nr)] zeros(2*Nr, Ns - (size(Ttmp, 1) + 2*Nr))];
        else
            Ttmp = [Ttmp; zeros(2*Nr, size(Ttmp, 1)) eye(2*Nr) zeros(2*Nr, Ns - (size(Ttmp, 1) + 2*Nr))];
        end
    end
    if Nm_stock > 0 && (Nw > 0 || Nd > 0)
        Ttmp = [Ttmp; zeros(Nr, size(Ttmp, 1)) aux.Xi_md(t)*eye(Nr) zeros(Nr, Ns - (size(Ttmp, 1) + Nr))];
    end
    if Nq_flow > 0 && (Nm > 0 || Nw > 0 || Nd > 0)
        if aux.Xi_qd(t) == 0
            Ttmp = [Ttmp; zeros(2*Nr, size(Ttmp, 1)) zeros(2*Nr, Nr) [eye(Nr); zeros(Nr)] zeros(2*Nr, Ns - (size(Ttmp, 1) + 2*Nr))];
        else
            Ttmp = [Ttmp; zeros(2*Nr, size(Ttmp, 1)) eye(2*Nr) zeros(2*Nr, Ns - (size(Ttmp, 1) + 2*Nr))];
        end
        %Ttmp = [Ttmp; zeros(Nr, size(Ttmp, 1)) params.Xi_qd(t) * eye(Nr) zeros(Nr, Ns - (size(Ttmp, 1) + Nr))];
    end
    if Nq_stock > 0 && (Nm > 0 || Nw > 0 || Nd > 0)
        if aux.Xi_qd(t) == 0
            Ttmp = [Ttmp; zeros(Nr, size(Ttmp, 1)) zeros(Nr) zeros(Nr, Ns - (size(Ttmp, 1) + Nr))];
        else
            Ttmp = [Ttmp; zeros(Nr, size(Ttmp, 1)) eye(Nr) zeros(Nr, Ns - (size(Ttmp, 1) + Nr))];
        end
        %Ttmp = [Ttmp; zeros(Nr, size(Ttmp, 1)) params.Xi_qd(t) * eye(Nr) zeros(Nr, Ns - (size(Ttmp, 1) + Nr))];
    end
    T(:, :, t) = iT0 * Ttmp;
end

% Q  
Q = zeros(Ns);
Q(1:Nr, 1:Nr) = params.Omeg; 

% Z 
Z_d = []; if Nd > 0; Z_d = [params.lam_d; zeros(Nw+Nm+Nq, Nr)]; end
Z_w = []; if Nw > 0; Z_w = [zeros(Nd, Nr); params.lam_w; zeros(Nm+Nq, Nr)]; end
%Z_m = []; if Nm > 0; Z_m = [zeros(Nd+Nw, Nr); params.lam_m; zeros(Nq, Nr)]; end
Z_m_flow = []; if Nm_flow > 0; Z_m_flow = [zeros(Nd+Nw, 2 * Nr); params.lam_m_flow zeros(Nm_flow, Nr); zeros(Nm_stock+Nq, 2*Nr)]; end
Z_m_stock = []; if Nm_stock > 0; Z_m_stock = [zeros(Nd+Nw+Nm_flow, Nr); params.lam_m_stock; zeros(Nq, Nr)]; end
Z_m = [Z_m_flow, Z_m_stock]; 
%Z_q = []; if Nq > 0; Z_q = [zeros(Nd+Nw+Nm, Nr); params.lam_q]; end
Z_q_flow = []; if Nq_flow > 0; Z_q_flow = [zeros(Nd+Nw+Nm, 2 * Nr); params.lam_q_flow zeros(Nq_flow, Nr); zeros(Nq_stock, 2*Nr)]; end
Z_q_stock = []; if Nq_stock > 0; Z_q_stock = [zeros(Nd+Nw+Nm+Nq_flow, Nr); params.lam_q_stock]; end
Z_q = [Z_q_flow, Z_q_stock];
Z = [Z_d zeros(Nd+Nw+Nm+Nq, Nr * (Np_eff-1)) Z_w Z_m Z_q]; 

% H
H = diag([params.sig2_d; params.sig2_w; params.sig2_m; params.sig2_q]);

