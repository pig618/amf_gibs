
###########################################################################
###########################################################################
###########################################################################
# 
#  Functions for result Reports
# 
###########################################################################
###########################################################################
###########################################################################

if (!require(pheatmap)) {
  install.packages("pheatmap")
}
library(pheatmap)

if (!require(RColorBrewer)) {
  install.packages("RColorBrewer")
}
library(RColorBrewer)



result_read = function(path_name, result_name, var_names=NULL) {
  # Function to get variables with var_names from the analysis result. 
  # Return a list, indexed by permno, with each element only
  #   contains variables in var_names
  # If var_names is NULL, set it to all variable names 
  
  
  # Read parameter nloop
  load(paste(path_name, "/para.RData", sep=""))
  nloop = para$nloop
  
  # Read and extract the variables
  dat = list()
  for (i in 1:nloop) {
    load(paste(path_name, "/", result_name, "_", i, ".RData", sep=""))
    
    # If var_names is NULL, set it to all variable names
    if (length(var_names) == 0){
      var_names = names(get(result_name)[[1]])
    }
    
    temp = lapply(get(result_name), function(x) { x[var_names] })
    names(temp) = sapply(get(result_name), function(x) { x$permno })
    
    dat = c(dat, temp)
  }
  dat
}



result_reform = function(result, var_names=NULL) {
  # Change result from indexed by permno to indexed by variable
  # If var_names is NULL, set it to all variable names in result[[1]]
  
  permnos = names(result)
  if (length(var_names) == 0){
    var_names = names(result[[1]])
  }
  
  new_result = lapply(var_names, function(var_name) { 
    cur_temp = lapply(result, function(x) { x[[var_name]] })
    names(cur_temp) = permnos
    cur_temp })
  names(new_result) = var_names
  
  new_result
}



result_to_vec = function(result, var_names=NULL) {
  # Change result[var_names] to vectors
  # If var_names is NULL, set it to all variable names in result
  
  if (length(var_names) == 0){
    var_names = names(result)
  }
  
  # Set NULL to be NA
  new_result = lapply(var_names, function(var_name) {
    temp = result[[var_name]]
    temp = lapply(temp, function(x) { 
      
      if (length(x) > 1) {
        stop("Error: More than 1 element in ", var_name, "!\n")
      }
      
      if (length(x) == 0) { 
        x = NA 
      } else if (!is.finite(x)) {
        x = NA
      }
      
      x
    })
    temp = unlist(temp)
    temp
  })
  
  names(new_result) = var_names
  new_result
}



result_by_path = function(path_prefix, start_year, end_year, 
                          result_name, var_path) {
  # Function to read and clean result by a varaible name path
  # Empty entries should be replaced by "NA"
  # Return a vector "values", with names being the permno
  
  path_name = paste(path_prefix, start_year, "_", end_year, sep="")
  
  result = NULL
  cur_var_path = var_path
  while (length(cur_var_path) > 0) {
    cur_var = cur_var_path[1]
    cur_var_path = cur_var_path[-1]
    
    # Read in the result. Indexed by permno
    if (length(result) == 0) {
      result = result_read(path_name, result_name, cur_var)
    } 
    
    # Reform the result. Indexed by cur_var.
    result = result_reform(result, cur_var)
    
    # If there are sub-levels, go to sub-level
    # If not, change it to vectors
    if (length(cur_var_path) > 0) {
      result = result[[cur_var]]
    } else {
      values = result_to_vec(result)[[1]]
    }
  }
  
  values
}



tab_periods = function(first_year, last_year, min_len, 
                       path_prefix, result_name, var_path, 
                       value_min, value_max, func, ...) {
  # Function to form the tab for all periods using the function in func
  
  pt = proc.time()
  n = last_year - first_year - min_len + 2
  start_years = first_year : (last_year - min_len + 1)
  
  tab = matrix(rep(NA, n * n), ncol=n)
  rownames(tab) = as.character(start_years)
  colnames(tab) = as.character(
    (start_years[1] + min_len - 1) : last_year)
  good_perc = tab
  
  for (start_year in start_years) {
    for (end_year in (start_year + min_len - 1) : last_year) {
      values = result_by_path(
        path_prefix=path_prefix, start_year=start_year, end_year=end_year, 
        result_name=result_name, var_path=var_path)
      
      n_stocks = length(values)
      good = is.finite(values)
      values = values[good]
      good = which(values >= values_min & values <= values_max)
      values = values[good]
      good_perc[as.character(start_year), as.character(end_year)] = 
        length(good) / n_stocks
      
      tab[as.character(start_year), as.character(end_year)] = 
        func(values, ...)
    }
  }
  cat("Time used:", (proc.time() - pt)[3], "seconds.\n")
  list(tab=tab, good_perc=good_perc)
}



tab_periods_compare = function(first_year, last_year, min_len, 
                               path_prefix, result_name, 
                               var_path_gibs, var_path_ff5,
                               value_min, value_max, include_na, 
                               func, ...) {
  # Function to form the tab for all periods using the function in func
  # compare the table for GIBS and FF5
  
  pt = proc.time()
  n = last_year - first_year - min_len + 2
  start_years = first_year : (last_year - min_len + 1)
  
  tab_gibs = matrix(rep(NA, n * n), ncol=n)
  rownames(tab_gibs) = as.character(start_years)
  colnames(tab_gibs) = as.character(
    (start_years[1] + min_len - 1) : last_year)
  tab_ff5 = tab_gibs
  good_perc = tab_gibs
  
  for (start_year in start_years) {
    for (end_year in (start_year + min_len - 1) : last_year) {
      
      values_gibs = result_by_path(
        path_prefix=path_prefix, start_year=start_year, end_year=end_year, 
        result_name=result_name, var_path=var_path_gibs)
      
      values_ff5 = result_by_path(
        path_prefix=path_prefix, start_year=start_year, end_year=end_year, 
        result_name=result_name, var_path=var_path_ff5)
      
      n_stocks = length(values_gibs)
      
      good = which(is.finite(values_gibs) & is.finite(values_ff5))
      values_gibs = values_gibs[good]
      values_ff5 = values_ff5[good]
      
      good = which(values_gibs >= value_min & values_ff5 >= value_min
                   & values_gibs <= value_max & values_ff5 <= value_max)
      values_gibs = values_gibs[good]
      values_ff5 = values_ff5[good]
      
      good_perc[as.character(start_year), as.character(end_year)] = 
        length(good) / n_stocks
      
      if (!include_na) {
        n_stocks = length(good)
      }
      tab_gibs[as.character(start_year), as.character(end_year)] =
        func(values_gibs, ...) / n_stocks
      tab_ff5[as.character(start_year), as.character(end_year)] =
        func(values_ff5, ...) / n_stocks
    }
  }
  cat("Time used:", (proc.time() - pt)[3], "seconds.\n")
  
  tab_incr = (tab_gibs - tab_ff5) / abs(tab_ff5)
  list(tab_gibs=tab_gibs, tab_ff5=tab_ff5, tab_diff=tab_gibs-tab_ff5,
       tab_incr=tab_incr, good_perc=good_perc)
}



get_signif = function(pvals_orig, q_th=0.05) {
  # Get the count of stocks that are significant
  
  pvals = pvals_orig[is.finite(pvals_orig)]
  qvals = p.adjust(pvals, "BY")
  sum(qvals < q_th) 
}



get_label = function(tab, perc=T) {
  # Get labels for the heatmap in draw_heatmap()
  
  tab = tab * ifelse(perc, 100, 1)
  
  f = function(x) {
    id_na = is.na(x)
    id_good = is.finite(x)
    if (perc) {
      x[id_good] = paste(sprintf("%.1f", x[id_good]), "%", sep="")
    } else {
      x[id_good] = paste(sprintf("%.3f", x[id_good]), sep="")
    }
    x[id_na] = ""
    x
  }
  
  if (is.vector(tab)) {
    labels = f(tab)
  } else {
    labels = apply(tab, 2, f)
  }
  
  labels
}



draw_heatmap = function(mat, color, breaks, main, display_numbers,
                        legend_breaks, legend_labels) {
  # Function to draw the heatmap for the table "tab"
  pheatmap(mat=mat, color=color, breaks=breaks, 
           main=main, display_numbers=display_numbers, 
           cellwidth=60, cellheight=40, 
           show_rownames=T, show_colnames=T, angle_col="0",
           cluster_rows=F, cluster_cols=F, 
           fontsize=16, fontsize_number=18,
           fontsize_row=18, fontsize_col=18, 
           legend=T, legend_breaks=legend_breaks, 
           legend_labels=legend_labels)
}



###########################################################################
# The end
###########################################################################

# tab_periods = function(first_year, last_year, min_len, 
#                        path_prefix, result_name, var_path_prefix, 
#                        value_min, value_max, func, ...) {
#   # Function to form the tab for all periods using the function in func
#   pt = proc.time()
#   n = last_year - first_year - min_len + 2
#   start_years = first_year : (last_year - min_len + 1)
#   
#   tab = matrix(rep(NA, n * n), ncol=n)
#   rownames(tab) = as.character(start_years)
#   colnames(tab) = as.character(
#     (start_years[1] + min_len - 1) : last_year)
#   
#   good_perc = tab
#   
#   for (start_year in start_years) {
#     for (end_year in (start_year + min_len - 1) : last_year) {
#       path_name = paste(path_prefix, start_year, "_", end_year, sep="")
#       
#       result = NULL
#       cur_var_path = var_path_prefix
#       while (length(cur_var_path) > 0) {
#         cur_var = cur_var_path[1]
#         cur_var_path = cur_var_path[-1]
#         
#         # Read in the result. Indexed by permno
#         if (length(result) == 0) {
#           result = result_read(path_name, result_name, cur_var)
#         } 
#         
#         # Reform the result. Indexed by cur_var.
#         result = result_reform(result, cur_var)
#         
#         # If there are sub-levels, go to sub-level
#         # If not, change it to vectors
#         if (length(cur_var_path) > 0) {
#           result = result[[cur_var]]
#         } else {
#           value = result_to_vec(result)[[1]]
#         }
#       }
#       
#       n_stocks = length(value)
#       good = is.finite(value)
#       value = value[good]
#       good = which(value >= value_min & value <= value_max)
#       value = value[good]
#       good_perc[as.character(start_year), as.character(end_year)] = 
#         length(good) / n_stocks
#       
#       tab[as.character(start_year), as.character(end_year)] = 
#         func(value, ...)
#     }
#   }
#   cat("Time used:", (proc.time() - pt)[3], "seconds.\n")
#   list(tab=tab, good_perc=good_perc)
# }






# tab_compare_periods = function(first_year, last_year, min_len,
#                                path_prefix, result_name,
#                                var_path_prefix, value_min, value_max, 
#                                func, ...) {
#   # Function to form the tab for all periods using the function in func
#   # Compare the results with that of FF5.
#   pt = proc.time()
#   n = last_year - first_year - min_len + 2
#   start_years = first_year : (last_year - min_len + 1)
#   
#   tab_gibs = matrix(rep(NA, n * n), ncol=n)
#   rownames(tab_gibs) = as.character(start_years)
#   colnames(tab_gibs) = as.character(
#     (start_years[1] + min_len - 1) : last_year)
#   
#   tab_ff5 = tab_gibs
#   good_perc = tab_gibs
#   
#   for (start_year in start_years) {
#     for (end_year in (start_year + min_len - 1) : last_year) {
#       path_name = paste(path_prefix, start_year, "_", end_year, sep="")
#       
#       result = NULL
#       cur_var_path = var_path_prefix
#       while (length(cur_var_path) > 1) {
#         cur_var = cur_var_path[1]
#         cur_var_path = cur_var_path[-1]
#         
#         # Read in the result. Indexed by permno
#         if (length(result) == 0) {
#           result = result_read(path_name, result_name, cur_var)
#         }
#         
#         # Reform the result. Indexed by cur_var. Go to sub-level.
#         result = result_reform(result, cur_var)[[cur_var]]
#       }
#       
#       var_gibs = paste(cur_var_path[1], "_gibs", sep="")
#       value_gibs = result_to_vec(result_reform(result, var_gibs))[[1]]
#       
#       var_ff5 = paste(cur_var_path[1], "_ff5", sep="")
#       value_ff5 = result_to_vec(result_reform(result, var_ff5))[[1]]
#       
#       n_stocks = length(value_gibs)
#       
#       good = which(is.finite(value_gibs) & is.finite(value_ff5))
#       value_gibs = value_gibs[good]
#       value_ff5 = value_ff5[good]
#       
#       good = which(value_gibs >= value_min & value_ff5 >= value_min
#                    & value_gibs <= value_max & value_ff5 <= value_max)
#       value_gibs = value_gibs[good]
#       value_ff5 = value_ff5[good]
#       
#       good_perc[as.character(start_year), as.character(end_year)] = 
#         length(good) / n_stocks
#       
#       tab_gibs[as.character(start_year), as.character(end_year)] =
#         func(value_gibs, ...)
#       tab_ff5[as.character(start_year), as.character(end_year)] =
#         func(value_ff5, ...)
#     }
#   }
#   cat("Time used:", (proc.time() - pt)[3], "seconds.\n")
#   
#   tab_incr = (tab_gibs - tab_ff5) / abs(tab_ff5)
#   
#   list(tab_gibs=tab_gibs, tab_ff5=tab_ff5, tab_diff=tab_gibs-tab_ff5,
#        tab_incr=tab_incr, good_perc=good_perc)
# }



# ###########################################################################
# # Form Table for Time-Invariance Test and Residual test
# #   for different time periods
# ###########################################################################
# 
# load("./ti_results/ti_results_2011_2018/ti_results_2011_2018_1.RData")
# ls(ti_results[[1]])
# # Names in ti_results:
# # fit, fit2, ff5_resid_fit, gibs_resid_fit,
# # pred, pred2, ff5_resid_pred, gibs_resid_pred,
# # ti_ff5, ti_gibs,
# 
# ls(ti_results[[1]]$ff5_resid_pred)
# # [1] "ars_old"      "ars_union"    "cii_old"      "cii_union"
# # [5] "f_pval"       "int_old"      "int_union"    "mse_old"
# # [9] "mse_union"    "pred_old"     "pred_union"   "resid_old"
# # [13] "resid_union"  "select_new"   "select_union" "sgn_old"
# # [17] "sgn_union"    "ybar_history"
# # 
# # a1 = sapply(ti_results, function(x) { x$ff5_resid_pred$f_pval })
# # a1 = a1[is.finite(a1)]
# # a2 = sapply(ti_results, function(x) { x$ff5_resid_fit$f_pval })
# # a2 = a2[is.finite(a2)]
# # 
# # b1 = sapply(ti_results, function(x) { x$gibs_resid_pred$f_pval })
# # b1 = b1[is.finite(b1)]
# # b2 = sapply(ti_results, function(x) { x$gibs_resid_fit$f_pval })
# # b2 = b2[is.finite(b2)]
# # 
# # # debug use
# # first_year = 2007
# # last_year = 2011
# # min_len = 3
# # folder_name = "ti_results"
# # result_name = "ti_results"
# # start_year = 2007
# # end_year = 2010
# # var_name1 = "gibs_resid_fit"
# # var_name2 = "f_pval"



# ti_table = function(first_year, last_year, min_len, 
#                     folder_name, result_name, 
#                     var_name1, var_name2) {
#   # Function to form the table of Time-Invariance Test 
#   
#   n = last_year - first_year - min_len + 2
#   start_years = first_year : (last_year - min_len + 1)
#   
#   tab = matrix(rep(NA, n * n), ncol=n)
#   rownames(tab) = as.character(start_years)
#   colnames(tab) = as.character(
#     (start_years[1] + min_len - 1) : last_year)
#   
#   for (start_year in start_years) {
#     for (end_year in (start_year + min_len - 1) : last_year) {
#       
#       path_name = paste(
#         "./", folder_name, "/", result_name, "_", start_year, "_", 
#         end_year, sep="")
#       
#       result = read_results(path_name, result_name, c(var_name1), 
#                             start_year, end_year)[[var_name1]]
#       result = reform_result(result, c(var_name2))
#       
#       pvals = unlist(result[[var_name2]])
#       pvals = pvals[is.finite(pvals)]
#       qvals = p.adjust(pvals, "BY")
#       tab[as.character(start_year), as.character(end_year)] = 
#         sum(qvals < 0.05) / length(result[[var_name2]])
#     }
#   }
#   
#   tab
# }



# get_time_periods = function(first_year, last_year, min_len) {
#   # Return all time periods within [first_year, last_year]
#   # With lengths at least min_len years
#   start_years = first_year : (last_year - min_len + 1)
#   
#   time_periods = list()
#   i = 1
#   for (start_year in start_years) {
#     for (end_year in (start_year + min_len - 1) : last_year) {
#       time_periods[[i]] = c(start_year, end_year)
#       i = i + 1
#     }
#   }
#   
#   time_periods
# }



# read_results = function(path_name, result_name, var_names) {
#   # Function to get variables with var_names from the analysis results. 
#   # Return a list, with each element a variable in var_names.
#   
#   # Read parameter nloop
#   load(paste(path_name, "/para.RData", sep=""))
#   nloop = para$nloop
#   
#   # Read and extract the variables
#   dat = list()
#   for (i in 1:nloop) {
#     load(paste(path_name, "/", result_name, "_", i, ".RData", sep=""))
#     
#     temp = lapply(var_names, function(var_name) {
#       cur_temp = lapply(get(result_name), function(x) { x[[var_name]] })
#       names(cur_temp) = sapply(get(result_name), function(x) { x$permno })
#       cur_temp })
#     names(temp) = var_names
#     
#     dat = lapply(var_names, function(var_name) { 
#       c(dat[[var_name]], temp[[var_name]]) })
#     names(dat) = var_names
#   }
#   dat
# }