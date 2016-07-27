
varview <- function(x, vju = TRUE) {
  
  #vars 
  var_lst <- lapply(x, var_label)
  var_lst[sapply(var_lst, is.null)] <- ""
  
  # var_df <- stack(var_lst)[, 2:1]
  # var_df <- rename(var_df, vars = ind, var.labs = values)
  
  var_df <- as.data.frame(do.call(rbind, var_lst),
                          stringsAsFactors = FALSE)
  
  var_df$vars <- rownames(var_df)
  var_df <- rename(var_df, var.lab = V1)
  
  #vals
  val_lst <- lapply(
    val_labels(x, prefixed = TRUE),
    names) %>%
    lapply(paste, collapse = " . ") 
  
  # val_df <- stack(val_lst.scopes)[, 2:1]
  # val_df <- rename(val_df, vars = ind, val.labs = values)
  
  val_df <- as.data.frame(do.call(rbind, val_lst),
                          stringsAsFactors = FALSE)
  
  val_df <- rename(val_df, val.labs = V1)
  
  val_df$val.labs <- strtrim(val_df$val.labs, 255)
  
  val_df$vars <- rownames(val_df)
  
  #merge
  df <- inner_join(var_df, val_df) %>% 
    select(vars, var.lab, val.labs) %>%
    tbl_df()
  
  # df <- inner_join(var_df, val_df)
  
  if (vju) {View(df)} else {
    write.csv2(df, "temp.csv")
    shell.exec("temp.csv")
    }
}


