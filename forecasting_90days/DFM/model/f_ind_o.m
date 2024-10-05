function ind_o = f_ind_o(Xi)
% function to extract the indices corresponding to available observations
% Xi is a (daily or weekly) indicator that equals 0 at the start of a given period 
% (e.q. month or quarter) and 0 otherwise. See Modugno (2011, ECB WP) for
% details. 
Nt = length(Xi);
tmp = 1:Nt;
ind_o = tmp(Xi==0) - 1;
ind_o = ind_o(ind_o > 0);
ind_o = [ind_o Nt]; % add last obs which is always available in the simulated data setting