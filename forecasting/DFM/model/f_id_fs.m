function [id_f, id_f_lags, id_f_d, id_f_w, id_f_m_flow, id_f_m_stock, id_f_q_flow, id_f_q_stock] = f_id_fs(Nr, Np_eff, Nd, Nw, Nm_flow, Nm_stock, Nq_flow, Nq_stock)

id_f = 1:Nr; % note that id_f will be identical to id_f_d, id_f_w, ... always refering to the factor of the highest frequency!
id_f_lags = Nr+1:Nr * Np_eff;

ind_s = 0;
if Nd > 0
    id_f_d = ind_s+1:ind_s+Nr;
    ind_s = Nr * Np_eff;
else
    id_f_d = [];
end

if Nw > 0 && Nd > 0
   id_f_w = ind_s + 1 : ind_s+Nr;
   ind_s = ind_s + Nr; 
elseif Nw > 0 && Nd == 0 % w is first available frequency
   id_f_w = ind_s + 1 : ind_s + Nr;
   ind_s = Nr * Np_eff + 1;
else
    id_f_w = [];
end
Nm = Nm_flow + Nm_stock; 
if Nm > 0 && (Nw > 0 || Nd > 0)
   if Nm_flow > 0
       id_f_m_flow = ind_s+1 : ind_s+Nr;
       ind_s = ind_s + 2 * Nr; % skip due to f^MP_t
   end
   if Nm_stock > 0
       id_f_m_stock = ind_s+1 : ind_s+Nr;
       ind_s = ind_s+Nr; 
   else
       id_f_m_stock = []; 
   end
elseif (Nm > 0 && Nw == 0 && Nd == 0) || Nm == 0 % m is highest frequency, no distinction required between stocks and flows
   id_f_m_flow = [];
   id_f_m_stock = [];
end

if Nq_flow > 0
    id_f_q_flow = ind_s+1 : ind_s+Nr;
    ind_s = ind_s + 2*Nr; % skip due to f^QP_t
else
    id_f_q_flow = []; 
end

if Nq_stock > 0
    id_f_q_stock = ind_s+1 : ind_s+Nr;
else
    id_f_q_stock = []; 
end
 



