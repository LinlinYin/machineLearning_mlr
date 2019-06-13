#' @export
plotBMRBoxplots=function(bmr,measure=bmr$measures$measure_id[1],style="box") {
  checkmate::assertChoice(style, c("box", "violin","dot"))

  bmrAgg=bmr$aggregated(objects = FALSE)
  setkey(bmrAgg,"hash")
  bmrAllUniqueHash=unique(bmrAgg$hash)

  dataForPlot=NULL
  for (i in 1:length(bmrAllUniqueHash)) {
    rr=bmrAgg[bmrAllUniqueHash[i],resample_result][[1]]
#    rr = bmr$resample_result(bmrAllUniqueHash[i])
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
plotBMRBoxplotsMore=function(bmr,style="box") {
  checkmate::assertChoice(style, c("box", "violin","dot"))

  measure=bmr$measures$measure_id
  bmrAgg=bmr$aggregated(objects = FALSE)
  setkey(bmrAgg,"hash")
  bmrAllUniqueHash=unique(bmrAgg$hash)

  dataForPlot=NULL
  for (i in 1:length(bmrAllUniqueHash)) {
    rr=bmrAgg[bmrAllUniqueHash[i],resample_result][[1]]
    #    rr = bmr$resample_result(bmrAllUniqueHash[i])
    dataForPlotOne=cbind(as.data.table(rr)[,measure,with = FALSE,drop=FALSE],bmrAgg[i,c("task_id","learner_id"),with = FALSE])
    #    print(bmrAgg)
    #    print(bmrAllUniqueHash[i])
    #    print(bmrAgg[bmrAllUniqueHash[i],])
    #    print(bmrAgg[i,])
    #    print(dataForPlotOne)
    dataForPlot=rbind(dataForPlot,dataForPlotOne)
  }

  dataForPlot=melt(dataForPlot,id.vars = setdiff(colnames(dataForPlot),measure))
  p=ggplot(data=dataForPlot,aes_string(x = "task_id", y = "value"))+facet_wrap(~variable,scale="free")+ylab("Measure")
  if (style=="box") {
    p=p+geom_boxplot(aes(fill=learner_id))
  } else if (style=="violin") {
    p=p+geom_violin(aes(fill=learner_id))
  } else {
#    p=p+geom_dotplot(aes(fill=learner_id),binaxis = "y", stackdir = "center", position = "dodge",size=1)
    p=p+geom_jitter(aes(colour=learner_id), position =position_dodge(width = 1))
  }
  return(p)
}


#' @export
plotBMRRanksAsBarChart=function(bmr,pos="tile") {
  checkmate::assertChoice(pos, c("tile", "stack", "dodge"))

  bmrAgg=bmr$aggregated(objects = FALSE)
  dataForPlot=bmrAgg %>% group_by(task_id) %>%  mutate(rank = min_rank(desc(classif.acc)))
  p=ggplot(dataForPlot,aes(x=rank,y=task_id,fill=learner_id))
  if (pos=="tile") {
    p=ggplot(dataForPlot,aes(x=rank,y=task_id,fill=learner_id))
    p=p+geom_tile()
  } else if (pos=="dodge") {
    p = ggplot(dataForPlot, aes_string("rank", fill = "learner_id"))
    p=p+geom_bar(position = pos)
  }
  return(p)
}
