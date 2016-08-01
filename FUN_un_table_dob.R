# jednostavni i brzi untable
# testiran na medijalnoj dobi

un_table_dob <- function(data, frek.var = "frek", dob.var = "dob") {
  data[rep(1:nrow(data), data[[frek.var]]), ][[dob.var]]
}
