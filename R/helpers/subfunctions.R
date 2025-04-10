### Helper functions #####

# check input data
check_inputs <- function(data, yname, gname,tname, idname, unitname){
  
  for (var in list(yname, gname, idname, unitname)){
    print(var)
    if (!is.character(var)){
      stop('Variable inputs need to be strings.')
    }
    if (!(var %in% colnames(data))){
      stop('Variable inputs need to columns in the data.')
    }
    
  }
}


