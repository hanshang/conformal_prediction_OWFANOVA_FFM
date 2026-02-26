# novel way for selecting the number of retained factor loadings in HDFTS

select_K_new <- function(eigenvalue, index, sample_size, no_pop)
{
    return((eigenvalue[index])/sample_size + index * (max(sample_size, no_pop)^(-0.5)))
}

# data: (sample size x no_pop x no_grid)

HDFTS_factor_decomp <- function(data)
{
    sample_size = dim(data)[1]
    no_pop = dim(data)[2]
    D_val = dim(data)[3]
    
    Delta = matrix(0, sample_size, sample_size)
    for(ij in 1:no_pop)
    {
        temp = matrix(NA, sample_size, sample_size)
        for(t in 1:sample_size)
        {
            for(s in 1:sample_size)
            {
                temp[t,s] = matrix(data[t,ij,], nrow = 1) %*% matrix(data[s,ij,], ncol = 1)
            }
        }
        Delta = Delta + temp/D_val
        rm(temp)
    }
    rm(t); rm(s); rm(ij)
    
    Delta_mat = Delta/no_pop
    Delta_mat_eigen = eigen(Delta_mat)
    
    # new way of selecting the number of components
    
    K_val_k_criterion = k_criterion(eigenvalue = Delta_mat_eigen$values, plot = FALSE)
    K_val_select_K = select_K(tau = 10^-3, eigenvalue = Delta_mat_eigen$values)
    
    K_val = vector("numeric", sample_size)
    for(ik in 1:sample_size)
    {
        K_val[ik] = select_K_new(eigenvalue = Delta_mat_eigen$values, index = ik, sample_size = sample_size,
                                 no_pop = no_pop)
        rm(ik)
    }
    q_val_est = max(c(which.min(K_val) - 1, K_val_k_criterion, K_val_select_K))
    rm(K_val); rm(K_val_k_criterion); rm(K_val_select_K)
    if(q_val_est == 0)
    {
        stop("The number of components is zero.")
    }
    Delta_eigen_vector = as.matrix(Delta_mat_eigen$vectors[,1:q_val_est]) * sqrt(sample_size)
    
    ###############################
    # estimate the eigen-dimension
    ###############################
    
    factor_loading = array(NA, dim = c(q_val_est, D_val, no_pop))
    for(ij in 1:no_pop)
    {
        factor_loading[,,ij] = (crossprod(Delta_eigen_vector, data[,ij,]))/sample_size
        rm(ij)
    }
    
    # variance
    
    C_X = array(NA, dim = c(no_pop, no_pop, D_val, D_val))
    for(ij in 1:no_pop)
    {
        for(iw in 1:no_pop)
        {
            C_X[ij,iw,,] = crossprod(factor_loading[,,ij], factor_loading[,,iw])
        }
    }
    rm(ij); rm(iw)
    
    error = array(NA, dim = c(sample_size, no_pop, D_val))
    for(ij in 1:no_pop)
    {
        error[,ij,] = data[,ij,] - (Delta_eigen_vector %*% factor_loading[,,ij])
        rm(ij)
    }
    return(list(raw_data = data, factor_loading = factor_loading,
                factors = Delta_eigen_vector, error = error))
}

