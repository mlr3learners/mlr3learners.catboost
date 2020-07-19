library(mlr3learners.catboost)

test_that("classif.catboost_catboost.train", {
  learner = lrn("classif.catboost")
  fun = catboost::catboost.train
  exclude = NULL


  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "\nMissing parameters:\n",
    paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})

test_that("classif.catboost_catboost.predict", {
  learner = lrn("classif.catboost")
  fun = catboost::catboost.predict
  exclude = NULL


  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "\nMissing parameters:\n",
    paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})
