
###########################################################################
# Function to present counts in each big_class and category.
###########################################################################


class_presentation = function(per_exist, secu, match) {
  # "per_exist" contains all permno of securities exist here.
  # "secu" is the big_class and category information for each security
  # "match" is the match table between:
  #    big_class, big_class_no, category, category_no
  
  secu = subset(secu, permno %in% per_exist)
  
  secu_list = split(secu, secu$big_class_no)
  match_list = split(match, match$big_class_no)
  big_class_no = sort(unique(match$big_class_no))
  
  
  
  for (i in big_class_no) {
    secu_block = secu_list[[as.character(i)]]
    match_block = match_list[[as.character(i)]]
    cat(i, ".", match_block$big_class[1], 
        " (", nrow(data.frame(secu_block)), 
        "): ---------------- \n", sep="")
    
    
    
    if (!is.null(secu_block)) {
      secu_block_list = split(secu_block, secu_block$category_no)
    }
    category_no = sort(match_block$category_no)
    
    
    
    for (j in category_no) {
      if (!is.null(secu_block)) {
        cat("  ", j, ".", 
            match_block$category[which(match_block$category_no == j)], 
            " (", nrow(data.frame(secu_block_list[[as.character(j)]])), 
            ") \n", sep="")
      } else {
        cat("  ", j, ".", 
            match_block$category[which(match_block$category_no == j)], 
            " (", 0, ") \n", sep="")
      }
      
      
      
    }
    cat("\n\n")
  }
  cat("Total count: ", nrow(secu), "\n", sep="")
}



#-- The End -------------------------------------------------------------------
