# Example Unit Testing Script

context("Reading original data")

test_that("read_encoded_csv returns correct messages for a UTF-8 nonempty file", {
  expect_message(read_encoded_csv("utf8_nonempty_file.csv"),"Trying encoding")
  expect_message(read_encoded_csv("utf8_nonempty_file.csv"),"Processed file: utf8_nonempty_file.csv")
  expect_message(read_encoded_csv("utf8_nonempty_file.csv"),"Processed encoding: UTF-8")
})

test_that("read_encoded_csv returns an error from reading a file with an encoding not covered by guess_encoding", {
  expect_error(read_encoded_csv("IBM-1143-nonempty-file.csv"),"Encoding doesn't seem to work: IBM-1143")
})

test_that("reading_makes_sense returns false when the file read is empty", {
  expect_false(read_encoded_csv("empty_file.csv"))
})