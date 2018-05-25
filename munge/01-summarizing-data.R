# Find all csv-files in the "data-original"-directory and try to read each of
# them by guessing the encoding.

# Rules for guessing:
#     1.Take the first value in "encodings_probable" and check if it is in
#       "encodings_guessed". If it is, use that encoding to try to read the
#       file. 
#     2.If the first value of "encodings_probable" is not in "encodings_guessed",
#       OR the first encoding didn't work out, move on to the next value in
#       "encodings_probable", and so forth.
#     3.If 1 or 2 does not work out, go to town on the encodings and try all
#       available encodings. Typically, a file-read that does not use the correct
#       encoding will take a long time, and timeout can be used to discard such
#       reads as not successful.

library(readr)
files <- list.files(path="data-original", pattern="*.csv", full.names=T, recursive=TRUE)
encodings_all <- iconvlist()
encodings_probable <- c("UTF-8","UTF-16LE")
lapply(files, function(x) {
  encodings_guessed <- guess_encoding(x)
  list_encodings_guessed <- encoding_guessed[["encoding"]]
  encoding_to_try = "UTF-16LE" # list_encodings_guessed[1]
  t <- read.csv(x, header=TRUE, sep=",", fileEncoding=encoding_to_try,dec=".") # load file
  # apply function
  out <- dim(t)
  # write to file
  write.table(out, "/data/test.txt", sep="\t", quote=F, row.names=F, col.names=T)
  rm(t)
})
