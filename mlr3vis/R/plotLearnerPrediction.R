#' @export

plotLearnerPrediction=function(learner,task,features = NULL,cv=10L,err.mark = "train",
                               pointsize = 2,err.size=pointsize,err.col = "white",
                               gridsize=100L) {
  checkmate::assertChoice(err.mark, choices = c("train", "cv", "none"))

  fns = task$feature_names
  if (is.null(features)) {
    features = if (length(fns) == 1L)
      fns
    else fns[1:2]
  }
  #  print(features)
  target=task$target_names

  # subset to features
  task$select(features)
  data=task$data()[,features,with = FALSE]

  #X1 and X2, and grid
  x1n = features[1L]
  x1 = data[, get(x1n)]
  taskdim = length(features)
  if (taskdim == 2L) {
    x2n = features[2L]
    x2 = data[, get(x2n)]
  }
  if (taskdim == 1L) {
    grid = data.frame(x = seq(min(x1), max(x1), length.out = gridsize))
  } else if (taskdim == 2L) {
    # setup data frames for ggplot. grid = background, data = points
    grid = expand.grid(
      seq(min(x1), max(x1), length.out = gridsize),
      seq(min(x2), max(x2), length.out = gridsize)
    )
  }
  colnames(grid) = features

  #add predictions to data
  e = Experiment$new(task = task, learner = learner)
  e$train()
  e$predict()
  predictions=e$prediction
  dataForPlot=cbind(data,truth=predictions$truth)

  if (err.mark == "train") {
    dataForPlot$response=predictions$response
  } else if (err.mark == "cv") {
    #   #CV
    if (cv > 0L) {
      resampling = mlr_resamplings$get("cv",param_vals = list(folds = cv))
      #    print(resampling)
      cvResult = resample(task, learner, resampling)

      perf.cv = cvResult$aggregated
      responses=unlist(lapply(cvResult$data$prediction,function(x) x$response))
      rowIds=unlist(lapply(cvResult$data$prediction,function(x) x$row_ids))
      pred.cv = responses[order(rowIds)]
      dataForPlot$response=pred.cv
    } else {
      perf.cv = NA_real_
    }
  }
  dataForPlot$.err=dataForPlot$response != dataForPlot$truth

  # grid for predictions
  grid=cbind(grid,levels(task$truth())[1])
  colnames(grid)[ncol(grid)]=target
  pred.grid = e$predict(newdata=grid)
  grid[, target] = pred.grid$prediction$response

  x1n=features[1]
  x2n=features[2]
  #  p=ggplot(dataForPlot,aes_string(x = x1n, y = x2n, col = "response"))+geom_point()

  #plot grid
  p = ggplot(grid, aes_string(x = x1n, y = x2n))
  p = p + geom_raster(mapping = aes_string(fill = target))

  ## plot correct points
  p=p+geom_point(data = subset(dataForPlot, !dataForPlot$.err),
                 mapping = aes_string(x = x1n, y = x2n, shape = "truth"), size = pointsize)
  #  p = ggplot(data = subset(dataForPlot, !dataForPlot$.err),
  #             mapping = aes_string(x = x1n, y = x2n, shape = "truth"), size = pointsize)+geom_point()

  #plot error mark on error points
  if (err.mark != "none" && any(dataForPlot$.err)) {
    p = p + geom_point(data = subset(dataForPlot, dataForPlot$.err),
                       mapping = aes_string(x = x1n, y = x2n, shape = "truth"),
                       size = err.size + 1.5, show.legend = FALSE)
    p = p + geom_point(data = subset(dataForPlot, dataForPlot$.err),
                       mapping = aes_string(x = x1n, y = x2n, shape = "truth"),
                       size = err.size + 1, col = err.col, show.legend = FALSE)
  }

  # plot error points
  p = p + geom_point(data = subset(dataForPlot, dataForPlot$.err),
                     mapping = aes_string(x = x1n, y = x2n, shape = "truth"),  size=err.size, show.legend = FALSE)
  p = p + guides(alpha = FALSE)
  return(p)



  # p = ggplot(grid, aes_string(x = x1n, y = x2n))
  # if (hasLearnerProperties(learner, "prob") && prob.alpha) {
  #   # max of rows is prob for selected class
  #   prob = apply(getPredictionProbabilities(pred.grid, cl = td$class.levels), 1, max)
  #   grid$.prob.pred.class = prob
  #   p = p + geom_raster(data = grid, mapping = aes_string(fill = target, alpha = ".prob.pred.class"),
  #                       show.legend = TRUE) + scale_fill_discrete(drop = FALSE)
  #   p = p + scale_alpha(limits = range(grid$.prob.pred.class))
  # } else {
  #   p = p + geom_raster(mapping = aes_string(fill = target))
  # }
  # # print normal points
  # p = p + geom_point(data = subset(data, !data$.err),
  #                    mapping = aes_string(x = x1n, y = x2n, shape = target), size = pointsize)
  # # mark incorrect points
  # if (err.mark != "none" && any(data$.err)) {
  #   p = p + geom_point(data = subset(data, data$.err),
  #                      mapping = aes_string(x = x1n, y = x2n, shape = target),
  #                      size = err.size + 1.5, show.legend = FALSE)
  #   p = p + geom_point(data = subset(data, data$.err),
  #                      mapping = aes_string(x = x1n, y = x2n, shape = target),
  #                      size = err.size + 1, col = err.col, show.legend = FALSE)
  # }
  # # print error points
  # p = p + geom_point(data = subset(data, data$.err),
  #                    mapping = aes_string(x = x1n, y = x2n, shape = target), size = err.size, show.legend = FALSE)
  # p = p + guides(alpha = FALSE)




}

