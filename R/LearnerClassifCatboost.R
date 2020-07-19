#' @title Gradient Boosted Decision Trees Classification Learner
#'
#' @name mlr_learners_classif.catboost
#'
#' @description
#' FIXME: Link
#' Gradient boosted decision trees classification learner.
#' Calls [catboost::catboost.train()] and [catboost::catboost.predict()] from
#' package catboost.
#'
#' @templateVar id classif.catboost
#' @template section_dictionary_learner
#'
#' @references
#' FIXME:
#' \cite{mlr3learners.mboost}{dorogush2017a}
#' \cite{mlr3learners.mboost}{dorogush2017b}
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerClassifCatboost = R6Class("LearnerClassifCatboost",
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Create a `LearnerClassifCatboost` object.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          # FIXME:
          ParamInt$new(id = "depth", lower = 1L, upper = 16L, default = 6L, tags = "train"),
          ParamDbl$new(id = "learning_rate", lower = 0.001, upper = 1, default = 0.03, tags = "train"),
          ParamInt$new(id = "iterations", lower = 1L, upper = Inf, default = 1000, tags = "train"),
          ParamDbl$new(id = "l2_leaf_reg", lower = 1, upper = Inf, default = 3, tags = "train"),
          ParamDbl$new(id = "rsm", lower = 0.001, upper = 1, default = 1, tags = "train"),
          ParamInt$new(id = "border_count", lower = 1L, upper = 255L, default = 254, tags = "train"),
          ParamInt$new(id = "thread_count", lower = 1L, upper = Inf, default = 1L, tags = c("train", "predict")),
          ParamFct$new(id = "logging_level", levels = c("Silent", "Verbose", "Info", "Debug"), default = "Silent", tags = "train"),
          ParamLgl$new(id = "verbose", default = FALSE, tags = "predict")
        )
      )

      # explicitly set the logging_level to "Silent"
      ps$values$logging_level = "Silent"

      super$initialize(
        id = "classif.catboost",
        packages = "catboost",
        feature_types = c("numeric", "factor", "ordered"),  # FIXME: integer
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass", "multiclass"),
        man = "mlr3learners.catboost::mlr_learners_classif.catboost"
      )
    }
  ),

  private = list(
    .train = function(task) {
      # target is encoded as integer values from 0
      is_binary = (length(task$class_names) == 2L)
      label = if (is_binary) {
        ifelse(task$data(cols = task$target_names)[[1L]] == task$positive, yes = 1L, no = 0L)
      } else {
        as.integer(task$data(cols = task$target_names)[[1L]]) -1L
      }

      learn_pool = mlr3misc::invoke(catboost::catboost.load_pool, data = setDF(task$data(cols = task$feature_names)), label = label)

      pars = self$param_set$get_values(tags = "train")

      # FIXME:
      # set the loss function correctly
      pars$loss_function = if (is_binary) "Logloss" else "MultiClass"

      mlr3misc::invoke(catboost::catboost.train, learn_pool = learn_pool, test_pool = NULL, params = pars)
    },

    .predict = function(task) {
      is_binary = (length(self$state$train_task$class_names) == 2L)
      pool = mlr3misc::invoke(catboost::catboost.load_pool, data = setDF(task$data(cols = task$feature_names)))
      prediction_type = if (self$predict_type == "response") "Class" else "Probability"

      preds = mlr3misc::invoke(catboost::catboost.predict, model = self$model, pool = pool, prediction_type = prediction_type, .args = self$param_set$get_values(tags = "predict"))

      if (self$predict_type == "response") {
        response = if (is_binary) {
          ifelse(preds == 1L, yes = self$state$train_task$positive, no = setdiff(self$state$train_task$class_names, self$state$train_task$positive))
        } else {
          self$state$train_task$class_names[preds + 1L]
        }
        PredictionClassif$new(task = task, response = response)
      } else {
        if (is_binary && is.null(dim(preds))) {
          preds = matrix(c(preds, 1 - preds), ncol = 2L, nrow = length(preds))
        }
        colnames(preds) = self$state$train_task$class_names
        PredictionClassif$new(task = task, prob = preds)
      }
    }
  )
)

# FIXME: importance, oob?
