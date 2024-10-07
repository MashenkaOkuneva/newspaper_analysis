function w2=neweywest(ts,q)
% This function computes Newey-West variances for a single (autocorrelated) time series ts.
% ts is Tx1
% q is bandwidth

    w2=var(ts);
    for k=1:q
            errorcov=cov([ts(1+k:end) ts(1:end-k)]);
            w2=w2+(1-k/(q+1))*2*errorcov(1,2);
    end
