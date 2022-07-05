test_that("list_files lists files", {
  expect_equal(list_files(file_location = "/rfs/LRWE_Proj59/jcfs2/Test",
                          file_type = ".txt")$files,
               data.frame(files = "/rfs/LRWE_Proj59/jcfs2/Test/hes_patient_19_253R.txt"))
})
