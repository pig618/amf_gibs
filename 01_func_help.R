
###########################################################################
###########################################################################
###########################################################################
# 
#  Help functions generally used in this project
# 
###########################################################################
###########################################################################
###########################################################################

options(width = 75, digits = 4, useFancyQuotes = FALSE)
# if (memory.limit() < 64 * 1024) { 
#   memory.limit(size = 64 * 1024) 
# }    # Expand memory limit to be 64 GB.



check_bad_col = function(x) {
  # Check bad columns
  col_names = names(x)
  for(i in 1:ncol(x)){
    if (class(x[, i]) == "character") {
      cat(c(col_names[i]), class(x[, i]), !any(is.na(x[, i])), 
          "\n", sep = "  ")
    } else {
      cat(c(col_names[i]), class(x[, i]), all(is.finite(x[, i])), 
          "\n", sep = "  ")
    }
  }
}



assign_batch = function(var_names, values, env=parent.frame()) { 
  # Assign values to a batch of variables
  sapply(1:length(var_names), function(i) {
    assign(var_names[[i]], values[[i]], envir = env)
  })
  return(paste("Assigned the following variables:", 
               paste(var_names, collapse = ", ")))
}



date_change = function(cur_dates, all_dates, 
                       start_change=0, end_change=0) {
  # Change cur_dates with regard to all dates in all_dates
  # change cur_dates to [cur_dates[1] + start_change, 
  #                      tail(cur_dates, 1) + end_change] 
  start_id = which(all_dates == cur_dates[1]) + start_change
  end_id = which(all_dates == tail(cur_dates, 1)) + end_change
  all_dates[start_id : end_id]
}



cum_mean = function(x, na.ignore=F) {
  # Function to calculate the cumulative mean
  if (!is.vector(x)) {
    stop("Error: in cum_mean(x), x should be a vector!")
  }
  
  if (na.ignore) {
    id_finite = is.finite(x)
    x[id_finite] = cumsum(x[id_finite]) / (1:sum(id_finite))
  } else {
    x = cumsum(x) / (1:length(x))
  }
  
  x
}



cum_prod = function(x, na.ignore=F) {
  # Function to calculate the cumulative product
  
  if (!is.vector(x)) {
    stop("Error: in cum_prod(x), x should be a vector!")
  }
  
  if (na.ignore) {
    id_finite = is.finite(x)
    x[id_finite] = cumprod(x[id_finite])
  } else {
    x = cumprod(x)
  }
  
  x
}



get_formula = function(y, x, intercept, x_coef=NULL) {
  # Form formula string with factors, coefficients
  # y: name(s) of reponse
  # x: name(s) of factors
  # intercept: include intercept if TRUE.
  # x_coef: pre-set coefficients for x. 
  #   Use NA for each non-preset coef. 
  #   Let x_coef=NULL if nothing preset.
  
  if (identical(x_coef, NULL) | identical(x_coef, NA)) {
    x_coef = rep(NA, length(x))
  }
  if (length(x) != length(x_coef)) {
    stop("Error: factor and coefficient lengths do not match!")
  }
  if (length(y) == 0) {
    stop("Error: no response y!")
  }
  
  fix_id = !is.na(x_coef)
  formula_fix = paste(paste(
    x_coef[fix_id], x[fix_id], sep=" * "), collapse=" + ")
  formula_other = paste(x[!fix_id], collapse=" + ")
  
  if (length(y) == 1) {
    formula = paste(y, "~")
  } else {
    formula = paste("cbind(", paste(y, collapse=", "), ") ~", sep="")
  }
  
  if (!intercept) {
    formula = paste(formula, "- 1")
  } else {
    formula = paste(formula, "1")
  }
  if (formula_fix != "") {
    formula = paste(formula, " + offset(", formula_fix, ")", sep="")
  }
  if (formula_other != "") {
    formula = paste(formula, formula_other, sep=" + ")
  }
  
  formula
}



pred_gf = function(y, yhat, ybar_history=0) {
  # Function for prediction goodness of fit measures
  # Return the Mean Squared Error (MSE) of y and yhat
  # Also return the Out-of-Sample R Squared (OSRS) (return NA if not possible)
  # OSRS = 1 - (sum((y - yhat) ^ 2)) / (sum((y - ybar_history) ^ 2))
  # ybar_history(t) should be the historical average of y as of time t - 1.
  
  if ((!is.vector(y)) | (!is.vector(yhat)) | (!is.vector(ybar_history))) {
    stop("Error: y, yhat, ybar_history are not all vectors!")
  }    # note that is.vector(0) is TRUE.
  
  if (length(yhat) == 1) {
    yhat = rep(yhat, length(y))
  }
  
  if (length(ybar_history) == 1) {
    ybar_history = rep(ybar_history, length(y))
  }
  
  if ((length(y) != length(yhat)) | 
      (length(yhat) != length(ybar_history))) {
    stop("Error: Lengths of y, yhat, ybar_history do not match!")
  }
  
  gf = list()    # prediction goodness of fit results
  gf$mse = sum((y - yhat) ^ 2) / length(y)
  
  ss_mean = sum((y - ybar_history) ^ 2)
  if (ss_mean == 0 | !is.finite(ss_mean)) {
    gf$osrs = NA
  } else {
    gf$osrs = 1 - gf$mse * length(y) / ss_mean
  }
  
  gf
}



# expand_win_mean = function(y_train, y_pred) {
#   # Calculate the expanding window mean till (t - 1) for each y_t in y_pred
#   # Use this to calculate ybar_history in pred_gf() function
#   # Return the expanding window mean with the same length as y_pred
#   # No NA allowed in y_train or y_pred
# 
#   if ((length(dim(y_train)) > 0) & (!(1 %in% dim(y_train)))) {
#     stop("Error: y_train is not a dimension-1 vector!")
#   }
# 
#   if ((length(dim(y_pred)) > 0) & (!(1 %in% dim(y_pred)))) {
#     stop("Error: y_train is not a dimension-1 vector!")
#   }
# 
#   y_train = unlist(y_train)
#   y_pred = unlist(y_pred)
# 
#   if (length(y_pred) == 0) {
#     return(NULL)
#   }
#   if (length(y_pred) == 1) {
#     return(mean(y_train))
#   }
# 
#   sums = cumsum(c(sum(y_train), y_pred[1 : (length(y_pred) - 1)]))
#   sums / seq(length(y_train), length(y_train) + length(y_pred) - 1, 1)
# }



libs_local = function() {
  
  # For parallel computing ------------------------------------------------
  if (!require(doParallel)) {
    install.packages('doParallel')
  }
  library(doParallel)
  
  if (!require(parallel)) {
    install.packages('parallel')
  }
  library(parallel)
  
  if (!require(snowfall)) {
    install.packages('snowfall')
  }
  library(snowfall)
  
  if (!require(snow)) { 
    install.packages('snow')
  }
  library(snow)
  
  # For prototype clustering via minimax linkage --------------------------
  # protoclust()
  
  # In centos, use this command first:
  # sudo yum install libcurl-devel
  # Then install R package "curl"
  # Then install R package "devtools"
  # if (!require(devtools)) { 
  #   install.packages("devtools") 
  # }
  # library(devtools)
  # 
  # if (!require(protoclust)) {
  #   devtools::install_github("jacobbien/protoclust")  
  # }
  # library(protoclust)
  if (!require(protoclust)) {
    install.packages("protoclust")
  }
  library(protoclust)
  
  # For LASSO -------------------------------------------------------------
  
  # glmnet()
  if (!require(glmnet)) {
    install.packages("glmnet")
  }
  library(glmnet)
  
  if (!require(Matrix)) {
    install.packages("Matrix")
  }
  library(Matrix)
  
  if (!require(expm)) {
    install.packages("expm")
  }
  library(expm)
  
  # For Vuyong non-nested test---------------------------------------------
  # vuongtest(), icci()
  if (!require(nonnest2)) {
    install.packages("nonnest2")
  }
  library(nonnest2)  
  
  # For GAM model using gam() ---------------------------------------------
  if (!require(mgcv)) {
    install.packages("mgcv")
  }
  library(mgcv)
  
  # For Time-invarience test with GAM -------------------------------------
  if (!require(mgcv)) {
    install.packages("mgcv")
  }
  library(mgcv)
  
  # Others ----------------------------------------------------------------
  
  # if (!require(data.table)) {
  #   install.packages('data.table')
  # }
  # library(data.table)
  
  if (!require(lattice)) {
    install.packages("lattice")
  }
  library(lattice)
  
  if (!require(MASS)) {
    install.packages("MASS")
  }
  library(MASS)
  
  if (!require(crayon)) { 
    install.packages("crayon") 
  }
  library(crayon)
  
  # Previously used packages ----------------------------------------------
  
  # if (!require(flare)) {
  #   install.packages("flare")
  # }
  # library(flare)
  
  # if (!require(GRS.test)) {
  #   install.packages("GRS.test")
  # }
  # library(GRS.test)
  # 
  # if (!require(tseries)) {
  #   install.packages("tseries")
  # }
  # library(tseries)
  # 
  # if (!require(foreach)) {
  #   install.packages("foreach")
  # }
  # library(foreach)
}



init_parallel = function(nmach=NULL) {
  # Initilaize parallel computing with nmach machines
  
  library(snowfall)
  
  if (is.null(nmach)) {
    library(parallel)
    nmach = detectCores() - 1
    # This is the number of processes.
  }
  
  if (nmach == 1) {
    sfInit(parallel=F)
  } else{
    sfInit(parallel=T, cpus=nmach)
  }
  
  # sfLibrary packages ----------------------------------------------------
  
  # For protoclust
  if (!require(protoclust)) {
    install.packages("protoclust")
  }
  sfLibrary(protoclust)
  
  
  # For LASSO
  if (!require(MASS)) {
    install.packages("MASS")
  }
  sfLibrary(MASS)
  
  if (!require(glmnet)) {
    install.packages("glmnet")
  }
  sfLibrary(glmnet)
  
  if (!require(Matrix)) {
    install.packages("Matrix")
  }
  sfLibrary(Matrix)
  
  if (!require(expm)) {
    install.packages("expm")
  }
  sfLibrary(expm)
  
  
  
  # In Time Invariance Analysis 
  # need to use the gam() function in mgcv package
  #   mgcv needs nlme package to work together
  
  if (!require(nlme)) {
    install.packages("nlme")
  }
  sfLibrary(nlme)
  
  if (!require(mgcv)) {
    install.packages("mgcv")
  }
  sfLibrary(mgcv)
  
  
  
  # Others 
  
  if (!require(nonnest2)) { 
    install.packages("nonnest2")
  }
  sfLibrary(nonnest2)   # For Vuyong non-nested test.
  
  if (!require(lattice)) {
    install.packages("lattice")
  }
  sfLibrary(lattice)
  
  
  
  # Previously used packages 
  
  # if (!require(flare)) {
  #   install.packages("flare")
  # }
  # sfLibrary(flare)
  
  # if (!require(GRS.test)) {
  #   install.packages("GRS.test")
  # }
  # sfLibrary(GRS.test)
  # 
  # if (!require(tseries)) {
  #   install.packages("tseries")
  # }
  # sfLibrary(tseries)
  # 
  # if (!require(foreach)) {
  #   install.packages("foreach")
  # }
  # sfLibrary(foreach)
  
  nmach
}



###########################################################################
# The end 
###########################################################################
