
###########################################################################
###########################################################################
###########################################################################
# 
#  Functions for Adaptive Multi-Factor (AMF) Model fit and prediction    
#  with Groupwise Interpretable Basis Selection (GIBS) algorithm        
# 
###########################################################################
###########################################################################
###########################################################################

pre_select = function(x, y, p_th=0.05, intercept=F, verbose=F) {
  # Function to pre-select the factors
  
  if (!is.data.frame(x) | !is.data.frame(y)) {
    stop("Error: x or y is not a data frame!")
  }
  if (ncol(x) == 0 | ncol(y) != 1) {
    stop("Error: x has 0 column or y doesn't have 1 column!")
  }
  
  formula = get_formula(
    y=colnames(y), x=colnames(x), intercept=intercept, x_coef=NULL)
  model = lm(formula=formula, data=cbind(x, y))
  pvals = summary(model)$coefficients[, "Pr(>|t|)", drop=T]
  fac_sgn = names(pvals)[pvals < p_th]
  
  abs_corr = abs(cor(y, x))[1, , drop=F]
  best = colnames(abs_corr)[order(abs_corr, decreasing=T)][1]
  
  if (verbose) {
    print(summary(model)$coefficients)
    print(abs_corr[, order(abs_corr, decreasing=T), drop=F])
  }
  
  list(fac_sgn=fac_sgn, best=best)
}






gibs_select = function(x, y, method, alpha, pmax, cv_choice) {
  #########################################################################
  # Function to do GIBS variable selection step on y ~ x.
  # No intercept in select, since we fit non-intercept model.
  #
  # Arguments:
  # x: A data frame. Each column is an independent variable.
  # y: A data frame with only 1 column, which is the response.
  # method: GIBS fit if method == "gibs". 
  #   Use Elastic Net instead of GIBS if use method == "enet".
  # alpha: The alpha in Elastic Net if method == "enet".
  #   If method == "gibs", then alpha is not used.
  # pmax: Set pmax as the hard shreshold for num factors selected. 
  #   No hard shreshold if pmax == Inf.
  # cv_choice: Cross-validation lambda choice, 
  #   "cv1se" if use cv lambda.1se, "cvmin" if use cv lambda.min.
  #
  # Return:
  # A vector of column names of the variables selected.
  #########################################################################
  
  if (!(method %in% c("gibs", "enet"))) {
    stop("Error: method has to be gibs or enet!")
  }
  x = data.matrix(x)
  y = data.matrix(y)
  
  # GIBS variable selection if method is "gibs" ---------------------------
  if (method == "gibs") {
    
    # Set a hard threshold pmax if pmax is finite
    lambda = -Inf
    if (is.finite(pmax)) {
      options(warn=-1)
      fit_select = glmnet(x, y, family="gaussian", intercept=F,
                          standardize=T, pmax=pmax)
      lambda = tail(fit_select$lambda, 1)
    }
    
    # Choose lambda with cv.1se / cv.min
    options(warn=0)
    set.seed(1)
    fit_select = cv.glmnet(x, y, family="gaussian", 
                           intercept=F, standardize=T,
                           type.measure="mse", alpha=1)
    if (!(cv_choice %in% c("cv1se", "cvmin"))) {
      stop("Error: cv_choice has to be cv1se or cvmin!")
    }
    if (cv_choice == "cv1se") {
      lambda = max(lambda, fit_select$lambda.1se)
    } else {
      lambda = max(lambda, fit_select$lambda.min)
    }
    
    # Select factors using the lambda derived above
    fit_select = glmnet(x, y, family="gaussian", intercept=F, 
                        lambda=lambda, standardize=T)
  } 
  
  # Elastic net if method is "enet" ---------------------------------------
  if (method == "enet") {
    # Choose lambda by cross-validation min for elastic net with alpha.
    options(warn=0)
    set.seed(1)
    fit_select = cv.glmnet(x, y, family="gaussian", 
                           intercept=F, standardize=T,
                           type.measure="mse", alpha=alpha)
    lambda = fit_select$lambda.min
    fit_select = glmnet(x, y, family="gaussian", intercept=F, 
                        lambda=lambda, standardize=T, alpha=alpha)
  }
  
  # Get selected factors --------------------------------------------------
  fac_select = data.matrix(fit_select$beta)
  fac_select = rownames(fac_select)[as.vector(fac_select != 0)]
  
  list(fac_select=fac_select, fit=fit_select)
}






valid_select = function(dat, vld_len, ycol, select, select_prefer, 
                        overlap=T, th_prefer=0.9, yt_base=F, 
                        valid_simplify=F, try_intercept=F, 
                        verbose=F) {
  # Function to do selection on validation set
  # select and select_prefer should be lists with x, x_coef, intercept
  # 
  # Split select$x to [fac_fix, fac_flex], then subsets on fac_flex
  # Compare mse with select_prefer mse * th_prefer
  
  if (!is.data.frame(dat)) {
    stop("Error: dat is not a data frame!")
  }
  if (!(ycol %in% colnames(dat))) {
    stop("Error: dat does not contain the ycol column!")
  }
  if (nrow(dat) < vld_len) {
    stop("Error: vld_len longer than training data available!")
  }
  
  n = nrow(dat)
  n_tr = n - vld_len
  if (overlap) {
    dat_tr = dat
  } else {
    dat_tr = dat[1:n_tr, , drop=F]
  }
  dat_vld = dat[(n - vld_len + 1) : n, , drop=F]
  
  # Model preferred
  formula_prefer = get_formula(
    y=ycol, x=select_prefer$x, intercept=select_prefer$intercept, 
    x_coef=select_prefer$x_coef)
  model_prefer = lm(formula=formula_prefer, data=dat_tr)
  
  best_mse = pred_gf(y=dat_vld[, ycol, drop=T], 
                     yhat=predict(model_prefer, newdata=dat_vld), 
                     ybar_history=Inf)$mse * th_prefer
  best_select = select_prefer
  
  if (verbose) {
    cat("\nModel preferred:----------------\n")
    cat("formula: ", formula_prefer, "\n")
    print(model_prefer$coefficients)
    cat("mse =", best_mse, "/", th_prefer, "\n\n")
  }
  
  # Try y_t base model with th_prefer
  if (yt_base) {
    formula_yt = get_formula(
      y=ycol, x="y_t", intercept=F, x_coef="1")
    model_yt = lm(formula=formula_yt, data=dat_tr)
    mse_yt = pred_gf(y=dat_vld[, ycol, drop=T],
                     yhat=predict(model_yt, newdata=dat_vld),
                     ybar_history=Inf)$mse * th_prefer
    if (mse_yt < best_mse) {
      best_mse = mse_yt
      best_select = list(x="y_t", x_coef=1, intercept=F)
    }
  }
  
  # Other candidate models
  fac_fix = select$x[!is.na(select$x_coef)]
  coef_fix = select$x_coef[!is.na(select$x_coef)]
  fac_flex = select$x[is.na(select$x_coef)]
  
  selects = list()
  if (!valid_simplify & length(fac_flex) > 0) {
    base_formula = get_formula(
      y=ycol, x=fac_fix, intercept=select$intercept, x_coef=coef_fix)
    base_model = lm(formula=base_formula, data=dat_tr)
    abs_corr = abs(cor(data.frame(y=base_model$residuals), 
                       dat_tr[, fac_flex, drop=F]))[1, , drop=F]
    
    fac_flex = fac_flex[order(abs_corr, decreasing=T)]
    selects_x = Reduce(c, fac_flex, accumulate=T)
    selects = lapply(selects_x, function(z) {
      list(x=c(fac_fix, z), x_coef=c(coef_fix, rep(NA, length(z))), 
           intercept=select$intercept)
    })
  } else {
    selects = list(select)
  }
  
  ff5_names = c("mkt", "smb", "hml", "rmw", "cma")
  selects[[length(selects) + 1]] = list(x=ff5_names, x_coef=NULL, intercept=F)
  selects[[length(selects) + 1]] = list(x="y_t", x_coef=1, intercept=F)
  
  if (try_intercept) {
    selects = c(selects, list(select_prefer))
    selects = c(selects, lapply(selects, function(z) {
      z$intercept = !z$intercept
      z}))
  }
  
  for (select in selects) {
    formula = get_formula(
      y=ycol, x=select$x, intercept=select$intercept, 
      x_coef=select$x_coef)
    model = lm(formula=formula, data=dat_tr)
    mse = pred_gf(y=dat_vld[, ycol, drop=T], 
                  yhat=predict(model, newdata=dat_vld), 
                  ybar_history=Inf)$mse
    
    if (verbose) {
      cat("\nCandidate models:----------------\n")
      cat("formula: ", formula, "\n")
      print(model$coefficients)
      cat("mse =", mse, "\n\n")
    }
    
    if (mse < best_mse) {
      best_select = select
      best_mse = mse
    }
  }
  
  if (verbose) {
    cat("\nBest model:-------------------\n")
    print(best_select)
    cat("Best mse =", best_mse, "\n\n")
  }
  best_select
}

# set.seed(1)
# p = 10
# n = 200
# x = data.frame(matrix(rnorm(n * p), nrow=n))
# colnames(x) = c("X1", "X2", "X3", "X4", 
#                 "y_t", "mkt", "smb", "cma", "hml", "rmw")
# 
# y = data.frame(yyy = x$X1 + 0.5 * x$X2 + rnorm(n))
# 
# select = list(x=c("X1", "X2", "mkt", "smb"), x_coef=c(1, NA, NA, NA), 
#               intercept=F)
# 
# select_prefer = list(x=c("X1"), x_coef=c(1), intercept=F)
# 
# dat = cbind(y, x)
# vld_len = 50
# ycol= "yyy"
# th_prefer = 0.9
# valid_simplify = F
# verbose = T
# 
# valid_select(dat=dat, vld_len=vld_len, ycol=ycol, 
#              select=select, select_prefer=select_prefer, 
#              th_prefer=th_prefer, valid_simplify=valid_simplify,
#              verbose=verbose)






lm_report = function(model, data_pred=NULL, ybar_history=NULL) {
  #########################################################################
  # Function to report metrices from a linear regression
  #########################################################################
  
  int_in = "(Intercept)" %in% names(model$coefficients)
  y = model$model[, 1, drop=F]
  y_name = colnames(y)
  y = y[, 1, drop=T]
  yhat = model$fitted.values
  resid = model$residuals
  n = length(y)
  
  # Residual, R Squared, and Adjusted R Squared
  rs = 1 - sum(resid ^ 2) / sum((y - ifelse(int_in, mean(y), 0)) ^ 2)
  ars = 1 - (1 - rs) * (n - ifelse(int_in, 1, 0)) / model$df.residual 
  
  report = list(model=model, resid_all=resid, resid_last=tail(resid, 1), 
                rs=rs, ars=ars)
  
  if (length(data_pred) > 0) {
    if (nrow(data_pred) > 0) {
      y_pred = predict(model, newdata=data_pred)
      gf = pred_gf(y=data_pred[, y_name, drop=T], yhat=y_pred, 
                   ybar_history=ybar_history)
      report = c(report, list(y_pred=y_pred), gf)
    }
  }
  
  # Summary related
  s = summary(model)
  report$summary = s
  
  pvals = s$coefficients[, "Pr(>|t|)"]
  report$fac_sgn = names(pvals)[pvals < 0.05]
  
  if (!int_in) {
    e = model$residuals 
    s = summary(lm(e~1))
  }
  report$int = s$coefficients[1, ]
  report$cii = s$coef[1, 1] + c(-1, 1) * s$coef[1, 2] * qt(0.975, s$df[2])

  report
}






test_f = function(fit_small, fit_big) {
  #########################################################################
  # Function to report F test p_value comparing fit_small, fit_big
  #########################################################################
  list(f_pval=anova(fit_small, fit_big)$"Pr(>F)"[2])
}






test_lr = function(fit_small, fit_big) {
  #########################################################################
  # Function to report the likelihood ratio test 
  # comparing fit_small, fit_big
  #########################################################################
  
  tests = list()
  
  if (any(is.na(coef(fit_big)))) {
    tests$lr_pval = list(A=NA, B=NA)
    tests$lr_measures = c(AIC1=NA, AIC2=NA, BIC1=NA, BIC2=NA)
  } else {
    options(warn=-1)
    tests$lr_pval = unlist(vuongtest(fit_small, fit_big)$p_LRT)
    
    icci_fit = icci(fit_small, fit_big)
    measure_names = c("AIC", "BIC")
    measures = icci_fit[measure_names]
    names(measures) = NULL
    options(warn=0)
    
    measures = unlist(measures)
    tests$lr_measures = measures
  }
  
  tests
}





gibs = function(dat_select, dat, 
                fac_base=NULL, coef_base=NULL, 
                fac_in=NULL, fac_pri=NULL, 
                vld_len=0, overlap=F, th_prefer=0.8, yt_base=F, 
                valid_simplify=T, try_intercept=T, 
                fac_add=NULL, 
                method="gibs", alpha=NA, pmax=20, cv_choice="cv1se",  
                f_test=F, lr_test=F, verbose=F) {
  #########################################################################
  # Function for Groupwise Interpretable Basis Selection (GIBS) algorithm.
  #  
  # Arguments:
  # dat_select: An object from the gibs_pre_one() function, the 
  #   data used to select basis assets.
  # dat: An object from the gibs_pre_one() function.
  # 
  # fac_base, coef_base: 
  #   The base factors with fixed coef coef_base. 
  # fac_in: The factors that we force to keep in.
  # fac_pri: The factors that we consider as a priority.
  # 
  # vld_len: Validation period length. 
  #   Don't do validate select if vld_len == 0
  # overlap: TRUE if training period overlap with validation period.
  # th_prefer:
  #   The preferred model is c(fac_base, fac_in)
  #   Other models will be picked if their mse is less than 
  #   th_prefer * mse_prefer.
  #   If th_prefer is NA, then skip valid select step.
  # valid_simplify: Use simplified version of valid_select().
  # try_intercept: Try with/without intercept in validattion if TRUE.
  # 
  # fac_add: Add these factors in if not selected. 
  #   This is only for comparison. Not recommended. 
  #   Use fac_in or fac_pri instead. 
  #  
  # method: GIBS fit if method == "gibs". 
  #   Use Elastic Net instead of GIBS if use method == "enet".
  # alpha: The alpha in Elastic Net if method == "enet".
  #   If method == "gibs", then alpha is not used.
  # pmax: Set pmax as the hard shreshold for num factors selected. 
  #   No hard shreshold if pmax == Inf.
  # cv_choice: Cross-validation lambda choice, 
  #   "cv1se" if use cv lambda.1se, "cvmin" if use cv lambda.min.
  # 
  # f_test: f_test=TRUE if report Anova F-test comparing GIBS and FF5. 
  # lr_test: lr_test=TRUE if report Vuong's likelihood-ratio test
  #   between GIBS and FF5.
  # verbose: Report the fit results for ff5, gibs if verbose=T.
  # 
  # Returns:
  #   A list of results
  #########################################################################
  
  result = list(permno=dat$y_name)
  
  if (length(dat$data_pred) > 0) {
    if (nrow(dat$data_pred) > 0) {
      ybar_history = dat$data_pred[, "y_t", drop=T]
      result$ybar_history = ybar_history
    }
  }
  
  # In the case of Elastic net --------------------------------------------
  if (method == "enet") {
    y = as.matrix(dat$data_train[, "y", drop=F])
    x = dat$data_train
    x = as.matrix(x[, setdiff(colnames(x), "y"), drop=F])
    
    e_result = gibs_select(
      x=x, y=y, method=method, alpha=alpha, 
      pmax=pmax, cv_choice=cv_choice)
    efit = e_result$fit
    
    result$rs_gibs = efit$dev.ratio
    if (nrow(y) <= efit$df) {
      result$ars_gibs = NA
    } else {
      result$ars_gibs = 1 - (1 - result$rs_gibs) * 
        nrow(y) / (nrow(y) - efit$df)
    }
    result$select_gibs = e_result$fac_select
    
    if (length(dat$data_pred) > 0) {
      if (nrow(dat$data_pred) > 0) {
        newy = as.matrix(dat$data_pred[, "y", drop=F])
        newx = dat$data_pred
        newx = as.matrix(dat$data_pred[, setdiff(colnames(x), "y"), drop=F])
        
        y_pred = predict(efit, newx=newx, newy=newy)[, 1, drop=T]
        gf = pred_gf(y=dat$data_pred[, "y", drop=T], yhat=y_pred, 
                     ybar_history=ybar_history)
        names(gf) = paste(names(gf), "_gibs", sep="")
        result = c(result, list(y_pred=y_pred), gf)
      }
    }
    
    return(result)
  }
    
  # Factor selection on dat_select ----------------------------------------
  
  y = dat_select$data_train[, "y", drop=F]
  x = dat_select$data_train
  x = x[, setdiff(colnames(x), "y"), drop=F]
  cur_data = list(y=y, x=x)
  select = list(x=NULL, x_coef=NULL, intercept=F)
  
  # fac_base with fixed coef
  if (length(fac_base) > 0) {
    base_formula = get_formula(
      y="y", x=fac_base, intercept=select$intercept, x_coef=coef_base)
    base_model = lm(
      formula=base_formula, data=cbind(cur_data$x, cur_data$y))
    cur_data$y = data.frame(y=base_model$residuals)
    cur_data$x = x[, setdiff(colnames(x), fac_base), drop=F]
    select$x = c(select$x, fac_base)
    select$x_coef = c(select$x_coef, coef_base)
  }
  
  # Put fac_in in  
  if (length(fac_in) > 0) {
    cur_data = proj_xy(y=cur_data$y, x=cur_data$x, cols=fac_in)
    select$x = c(select$x, fac_in)
    select$x_coef = c(select$x_coef, rep(NA, length(fac_in)))
  }
  
  # fac_pri with pre_select
  if (verbose) {
    cat("\npre_select() on fac_pri: --------\n")
  }
  best = "mkt"
  if (length(fac_pri) > 0) {
    temp = pre_select(
      x=cur_data$x[, fac_pri, drop=F], 
      y=cur_data$y, p_th=0.05, intercept=F, 
      verbose=verbose)
    best = temp$best
    
    cur_data = proj_xy(y=cur_data$y, x=cur_data$x, cols=temp$fac_sgn)
    select$x = c(select$x, temp$fac_sgn)
    select$x_coef = c(select$x_coef, rep(NA, length(temp$fac_sgn)))
  }
  
  # GIBS select
  x_proj = proj_data(
    dat=cur_data$x, core="mkt", still=c("y", "y_t", "y_bar"))
  cur_select = gibs_select(
    x=x_proj, y=cur_data$y, method=method, alpha=alpha, 
    pmax=pmax, cv_choice=cv_choice)
  
  cur_data = proj_xy(
    y=cur_data$y, x=cur_data$x, cols=cur_select$fac_select)
  select$x = c(select$x, cur_select$fac_select)
  select$x_coef = c(select$x_coef, rep(NA, length(cur_select$fac_select)))
  
  if (verbose) {
    cat("\nGIBS select: -------\n")
    print(select)
  }
  
  # Factor select on dat --------------------------------------------------
  
  # Valid select
  if (verbose) {
    cat("\nValid select: --------\n")
  }
  select_prefer=list(
    x=c(fac_base, fac_in), x_coef=c(coef_base, rep(NA, length(fac_in))), 
    intercept=F)
  if (vld_len > 0) {
    select = valid_select(
      dat=dat$data_train, vld_len=vld_len, ycol="y", select=select, 
      select_prefer=select_prefer, overlap=overlap, 
      th_prefer=th_prefer, yt_base=yt_base, 
      valid_simplify=valid_simplify, try_intercept=try_intercept, 
      verbose=verbose)
  }
  
  # Add fac_add in if fac_add is not NULL.
  fac_add = setdiff(fac_add, select$x)
  if (length(fac_add) > 0) {
    select$x = c(select$x, fac_add)
    select$x_coef = c(select$x_coef, rep(NA, length(fac_add)))
  }
  
  # Use the best if nothing selected
  if (length(select$x) == 0) {
    select$x = best
    select$x_coef = NA
  }
  
  # GIBS fit (or predict) -------------------------------------------------
  formula_gibs = get_formula(
    y="y", x=select$x, intercept=select$intercept, x_coef=select$x_coef)
  model_gibs = lm(formula=formula_gibs, data=dat$data_train)
  fit_gibs = lm_report(
    model_gibs, data_pred=dat$data_pred, ybar_history=ybar_history)
  if (verbose) {
    cat("\nGIBS model:----------------\n")
    cat("formula: ", formula_gibs, "\n")
    print(model_gibs$coefficients)
  }

  
  
  # FF5 fit (or predict) --------------------------------------------------
  ff5_names = c("mkt", "smb", "hml", "rmw", "cma")
  formula_ff5 = get_formula(y="y", x=ff5_names, intercept=F, x_coef=NULL)
  model_ff5 = lm(formula_ff5, data=dat$data_train)
  fit_ff5 = lm_report(
    model_ff5, data_pred=dat$data_pred, ybar_history=ybar_history)
  if (verbose) {
    cat("\nFF5 model:----------------\n")
    cat("formula: ", formula_ff5, "\n")
    print(model_ff5$coefficients)
  }
  
  # Save fitting (or predicting) results ----------------------------------
  result$select_gibs = select
  
  names_add = c("fac_sgn", "resid_last", "rs", "ars", "int", "cii")
  if (length(dat$data_pred) > 0) {
    if (nrow(dat$data_pred) > 0) {
      names_add = c(names_add, "y_pred", "mse", "osrs")
      result$stock_to_pred = dat$data_pred$y
    }
  }
  
  result_add = fit_gibs[names_add]
  names(result_add) = paste(names_add, "_gibs", sep="")
  result = c(result, result_add)
  
  result_add = fit_ff5[names_add]
  names(result_add) = paste(names_add, "_ff5", sep="")
  result = c(result, result_add)
  
  if (verbose) {
    result$fit_gibs = fit_gibs$model
    result$fit_ff5 = fit_ff5$model
  }
  
  # Compare tests (F test and LR test) ------------------------------------
  
  # LR test
  if (lr_test) {
    result = c(result, test_lr(fit_ff5$model, fit_gibs$model))
  }
  
  # F test
  more_id = !(select$x %in% ff5_names)
  if (f_test) {
    if (any(more_id)) {
      formula_combined = get_formula(
        y="y", x=c(ff5_names, select$x[more_id]), 
        intercept=select$intercept, 
        x_coef=c(rep(NA, 5), select$x_coef[more_id]))
      fit_new = lm(formula_combined, data=dat$data_train)
      result = c(result, test_f(fit_ff5$model, fit_new))
    }
  } else {
    result = c(result, list(f_pval = NA))
  }
  
  result
}


  
###########################################################################
# The end 
###########################################################################
