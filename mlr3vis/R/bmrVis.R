#' @export
plotBMRBoxplots=function(bmr,measure=bmr$measures$measure_id[1],style="box") {
  assertChoice(style, c("box", "violin","dot"))

  bmrAgg=bmr$aggregated(objects = FALSE)
  setkey(bmrAgg,"hash")
  bmrAllUniqueHash=unique(bmrAgg$hash)

  dataForPlot=NULL
  for (i in 1:length(bmrAllUniqueHash)) {
    rr = bmr$resample_result(bmrAllUniqueHash[i])
    dataForPlotOne=cbind(as.data.table(rr)[,measure,with = FALSE,drop=FALSE],bmrAgg[i,c("task_id","learner_id"),with = FALSE])
#    print(bmrAgg)
#    print(bmrAllUniqueHash[i])
#    print(bmrAgg[bmrAllUniqueHash[i],])
#    print(bmrAgg[i,])
#    print(dataForPlotOne)
    dataForPlot=rbind(dataForPlot,dataForPlotOne)
  }

#  return(dataForPlot)
  p=ggplot(data=dataForPlot,aes_string(x = "learner_id", y = measure))+facet_wrap(~task_id)
  if (style=="box") {
    p=p+geom_boxplot()
  } else if (style=="violin") {
    p=p+geom_violin()
  } else {
    p=p+geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")
  }
  return(p)
}


#' @export
plotBMRRanksAsBarChart=function(bmr) {
  bmrAgg=bmr$aggregated(objects = FALSE)
  dataForPlot=bmrAgg %>% group_by(task_id) %>%  mutate(rank = min_rank(desc(classif.acc)))
  p=ggplot(dataForPlot,aes(x=rank,y=task_id,fill=learner_id))+geom_tile()
  return(p)
}
