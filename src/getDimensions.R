# Natura 2000 R-project
# Get dimensions of all tables for all years

# Making tablenames from filenames
tablenames_from_filenames <- function(v) {
  if (is.character(v)) {
    v <- toupper(v);
    v <- lapply(v, function(x) gsub(pattern = " ",replacement = "",as.character(x)));
    v <- lapply(v, function(x) gsub(pattern = ".CSV",replacement = "",as.character(x)));
    return(v)
  }
  else stop("Error: There are no characters to transform to make table names.")
}

# Find file-name similar to table name
filename_from_tablename <- function(name,year) {
  if (is.character(name)) {
    name <- list.files(path = paste("data",year, sep = "/"), 
                       pattern = paste0("^", name), ignore.case = TRUE)[1]
    return(name)
  }
  else stop("Error: There are no characters in the table name.")
}

# Find dim() values for a table, NA if the table does no exist
dim_values <- function(t_year,t_filename) {
  out <- tryCatch(
    {
      message("Trying to read table and get dim-values")
      dim(read.csv(file = paste("data",t_year,t_filename,sep = "/"), header=TRUE, sep=",",
                   fileEncoding=toString(guess_encoding(paste("data",t_year,t_filename,sep = "/"))[1,1]),
                   dec=".")) 
      # The return value of `read.csv()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message(paste("File does not seem to exist:", paste("data",t_year,t_filename,sep = "/")))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(c(NA,NA))
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", paste("data",t_year,t_filename,sep = "/")))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(c(NA,NA))
    },
    finally={
      message(paste("Processed URL:", paste("data",t_year,t_filename,sep = "/")))
    }
  )    
  return(out)
}

# Find folders
data_folders <- list.files(path ="data", pattern = "^20*")

# Find tables (from the frst folder)
data_table_names <- tablenames_from_filenames(list.files(
  path = paste("data", data_folders[1], sep = "/"), pattern = "*.csv"))

# Set table details
data_table_dimensions = c("Nrow","Ncol")

# Create empty array
data_table_dimensions <- array(dim = c(length(data_table_names),length(data_folders),
    length(data_table_dimensions)), dimnames = list(c(data_table_names),c(data_folders),
    c(data_table_dimensions)))

# Load the datasets and get dimensions
for (year in data_folders) {
    for (tablename in data_table_names) {
      df_filename <- filename_from_tablename(tablename,year)
      print(paste(year, tablename, df_filename))
      print(dim_values(year,df_filename))
      data_table_dimensions[tablename,year,] <- dim_values(year,df_filename)
    }
}

# Write to the output file
write.table(data_table_dimensions, "summary_tables.txt", sep="\t")
