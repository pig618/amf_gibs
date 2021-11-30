
###########################################################################
###########################################################################
###########################################################################
# 
#  Functions to find basis asset representatives
#  based on the GIBS algorithm
# 
###########################################################################
###########################################################################
###########################################################################

cluster_within_category = function(fac, etf_info, pca_th, h_th) {
  #########################################################################
  # Find representative ETFs within each category.
  # pca_th is the threshold of deviation for calculating pca dimension.
  # h_th is the initial height (1 - |corr|) lowerbound threshold 
  #   for the prototype clustering.
  #########################################################################
  
  ff5_names = c("mkt", "smb", "hml", "rmw", "cma")
  etf = fac[setdiff(names(fac), ff5_names)]
  
  exist_per = as.integer(substring(names(etf), first=4))
  exist_class = subset(etf_info$etf_class, permno %in% exist_per)
  cate_names = as.vector(etf_info$class_table$category)
  # 73 categories in total.
  
  etf_keep = NULL
  pca_dim = NULL
  
  for (i in 1:length(cate_names)) {
    
    # Select ETFs in this category ----------------------------------------
    per = unlist(subset(exist_class, category_no == i, "permno"))
    if (length(per) == 0) {
      pca_dim = c(pca_dim, 0)
      next
    }
    
    cur_etf = subset(etf, select=paste("per", per, sep=""))
    # Current ETF in this category.
    n_etf = ncol(cur_etf)
    
    # If no more than 1 ETF in current category, add it to etf_keep 
    # and skip this iteration. 
    if (n_etf <= 1) {
      if (is.null(etf_keep)) {
        etf_keep = cur_etf
      } else {
        etf_keep = cbind(etf_keep, cur_etf)
      }
      pca_dim = c(pca_dim, n_etf)
      next
    }
    
    # Calculate PCA dimension ---------------------------------------------
    pca = prcomp(cur_etf, scale.=T) 
    # Here we standardize ETFs to make it more related to correlation
    prop = (pca$sdev) ^ 2
    prop = prop / sum(prop)    # proportion of deviation
    n_big = min(which(cumsum(prop) >= pca_th))
    
    # Protoclust on absolute correlation ----------------------------------
    
    # If There are only 2 ETFs here, can't do protoclust.
    # Select both if correlation < cut_th, otherwise select the 1st one.
    if (n_etf == 2) {
      if (abs(cor(cur_etf)[1, 2]) >= (1 - h_th)) {
        cur_etf = cur_etf[, 1, drop=F]
      }
      # Add to etf_keep and skip this iteration
      if (is.null(etf_keep)) {
        etf_keep = cur_etf
      } else {
        etf_keep = cbind(etf_keep, cur_etf)
      }
      pca_dim = c(pca_dim, n_etf)
      next
    }
    
    # Start clustering
    d = as.dist(1 - abs(cor(cur_etf)))
    hc = protoclust(d)
    
    # Use PCA dimension or h threshold, whichever is more strict. 
    h = hc$height[n_etf - n_big + 1]
    # Here h means height (1 - |corr|), to be decided in iteration.
    if (is.na(h)) {
      h = 1
    }
    h = (h + hc$height[n_etf - n_big]) / 2
    h = max(h_th, h)
    
    cut = protocut(hc, h=h)
    select = names(cur_etf)[cut$protos]
    
    cur_etf = cur_etf[select]
    
    # Add the result to etf_keep
    if (is.null(etf_keep)) {
      etf_keep = cur_etf
    } else {
      etf_keep = cbind(etf_keep, cur_etf)
    }
    pca_dim = c(pca_dim, n_etf)
  }
  
  list(etf_keep=etf_keep, pca_dim=pca_dim)
}






cluster_all = function(fac, pca_th, h_th){
  # Clustering on all ETFs selected.
  # pca_th is the threshold of deviation for calculating pca dimension.
  # h_th is the initial height (1 - |corr|) lowerbound threshold 
  #   for the prototype clustering.
  # 
  # FF5 is included in the clustering
  
  cur_etf = fac
  n_etf = ncol(cur_etf)
  
  # Calculate PCA dimension only for referrence ---------------------------
  n_big = 0
  if (dim(rm_const_col(cur_etf))[2] > 1) {
    pca = prcomp(rm_const_col(cur_etf), scale.=T) 
    # Here we standardize ETFs to make it more related to correlation
    prop = (pca$sdev) ^ 2
    prop = prop / sum(prop)    # proportion of deviation
    n_big = min(which(cumsum(prop) >= pca_th))
  }
  
  # Protoclust on absolute correlation ------------------------------------
  d = as.dist(1 - abs(cor(cur_etf)))
  hc = protoclust(d)
  
  # Use h threshold. PCA dimension is not suitable here 
  # since it is dominated by the time length
  h = h_th
  
  cut = protocut(hc, h=h)
  select = names(cur_etf)[cut$protos]
  cur_etf = cur_etf[select]
  
  list(cur_etf=cur_etf, pca_dim=n_big)
}






gibs_cluster = function(fac, etf_info, 
                        pca_th=0.9, h_th=0.3, mkt_th=0.9) {
  # Factor cluster for data fac  
  
  # # Check correlation.
  # abs_corr = abs(cor(fac[1:5], fac[6:ncol(fac)]))
  # for (i in 1:5) {
  #   hist(abs_corr[i, ], xaxt="n", xlab="correlation",
  #        main=paste("absolute correlation with", names(fac)[i]))
  #   range(abs_corr[i, ])
  #   axis(1, at=seq(0, 1, by=0.1), las=2)
  # }
  # # Most factors are highly correlated with excess market return.
  # # Other 4 Fama-French factors are not that correlated with others.
  # rm(abs_corr); gc()
  
  # Remove etfs highly correlated with the market return ------------------
  # Since we will project out the market return afterward.
  ff5_names = c("mkt", "smb", "hml", "rmw", "cma")
  etf = fac[setdiff(names(fac), ff5_names)]
  ff5 = fac[ff5_names]
  fac = cbind(ff5, rm_high_corr_col(etf, ff5["mkt"], mkt_th))
  rm(etf, ff5); gc()  
  
  # Remove constant columns.
  fac = rm_const_col(fac)
  
  # Project out the excess market return from all other columns -----------
  fac = proj_data(dat=fac, core="mkt")
  
  # Remove constant columns.
  fac = rm_const_col(fac)  
  
  # # Check correlation
  # abs_corr = abs(cor(fac[1:5], fac[6:ncol(fac)]))
  # for (i in 1:5) {
  #   hist(abs_corr[i, ], xaxt="n", xlab="correlation",
  #        main=paste("absolute correlation with", names(fac)[i]))
  #   print(range(abs_corr[i, ]))
  #   axis(1, at=seq(0, 1, by=0.1), las=2)
  # }
  # # The strong correlation with mkt is gone after projection.
  # rm(abs_corr); gc()
  
  # Prototype cluster ETFs within categories ------------------------------
  result1 = cluster_within_category(fac, etf_info, pca_th, h_th) 
  etf_keep = result1$etf_keep
  pca_dim = result1$pca_dim
  if (!is.null(etf_keep)) {
    fac = cbind(fac[, ff5_names, drop=F], etf_keep)
  }
  dim(etf_keep) 
  
  # Prototype clustering on all selected ETFs -----------------------------
  result2 = cluster_all(fac, pca_th, h_th)     
  dim(result2$cur_etf)   
 
  # Return the results
  etf_keep_info = list(
    etf_per_keep=setdiff(names(result2$cur_etf), ff5_names),
	pca_dim=result2$pca_dim, 
	etf_per_1st_step=names(etf_keep), 
	pca_dim_1st_step=pca_dim)
  
  etf_keep_info
}






###########################################################################
# The end 
###########################################################################
