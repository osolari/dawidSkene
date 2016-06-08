function b_hat = estimate_class_imbalance_tensor(Z,delta)   
    % b_hat = estimate_class_imbalance_tensor(Z,delta)
    %
    % Estimate the class imbalance using the tensor method
    %  
    % Input: 
    % Z - m x n matrix of binary data
    % delta - bounds away the class imbalance, psi and eta estimations
    %        b_hat in [-1+delta,1-delta], psi,eta in [delta,1-delta]
    %
    % Output: 
    % b_hat - estimation of the class Y imbalance Pr(Y=1)-Pr(Y=-1)
    %
    % Written by Ariel Jaffe and Boaz Nadler, 2015
        
    %get number of classifiers
    m = size(Z,1);
                
    %estimate second moment
    R = cov(Z');
    
    % estimate the diagonal values of a single rank matrix
    R = estimate_rank_1_matrix(R);
    
    %get first eigenvector
    [V, ~] = eigs(R,1);
    V = V*sign(sum(sign(V)));
    
    %get constant C for first eigenvector min(C*V*V'-R)
    R_v = V*V';
    Y = R( logical(tril(ones(m))-eye(m)) );
    X = R_v( logical(tril(ones(m))-eye(m)) );
    [~,C] = evalc('lsqr(X,Y)');
    V = V*sqrt(C);
    
    %Estimate m x m x m tensor
    T = compute_classifier_3D_tensor(Z);
          
    %Estimate alpha
    alpha = estimate_alpha(V,T);
    
    %get b from alpha
    b_hat = -alpha / sqrt(4+alpha^2);
    
    %bound b_hat in [-1+delta,1-delta]
    b_hat = min(b_hat,1-delta);
    b_hat = max(b_hat,-1+delta);
        
end