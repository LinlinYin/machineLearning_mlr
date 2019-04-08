# TODO: Add comment
# 
###############################################################################


require(mlr)
require(RColorBrewer)
require(ggplot2)
require(FSelector)



#bmrClassif<-performBenchmark(dataForModelTemp,target=outVar,featureMethod="selection")
#bmrSurvival<-performBenchmark(dataForModelTemp,benchmarkTaskFunction=makeSurvTask,target=c(timeVar,statusVar),featureMethod="selection")
performBenchmark<-function(dataForModel,benchmarkTaskFunction=makeClassifTask,target="HistologyGroup01",iters=10,learners=c("classif.lda","classif.rpart","classif.logreg","classif.randomForest","classif.svm"),
		featureMethod=c("none","filter","selection"),fw.abs=10,fw.method = "information.gain",
		fsMeasureFunction=auc,fsControlFunction=makeFeatSelControlSequential(maxit = 10,method="sfs")) {
	featureMethod<-match.arg(featureMethod)
	
	lrns = lapply(learners,function(x) makeLearner(x,predict.type = "prob"))
	rdesc = makeResampleDesc("Bootstrap",stratify = TRUE,iters=iters)
	
	if (featureMethod=="filter") {
		print("Feature filter performed")
		lrns = lapply(lrns,function(x) makeFilterWrapper(x,fw.method = fw.method, fw.abs = fw.abs, show.info = FALSE))
	} else if (featureMethod=="selection") {
		print("Feature selection performed")
		rdescFeatureSelection = makeResampleDesc("Bootstrap",stratify = TRUE,iters=iters)
		lrns = lapply(lrns,function(x) makeFeatSelWrapper(x,resampling = rdescFeatureSelection,measure=fsMeasureFunction,control = fsControlFunction, show.info = FALSE))
	} else {
		print("No feature filter nor selection performed")
	}
	
	benchmarkTask = benchmarkTaskFunction(data = dataForModel, target = target)
	
	## Conduct the benchmark experiment
	bmr = benchmark(lrns, tasks =benchmarkTask, resampling =rdesc,measure=list(auc,mmce))
	print(bmr)
	return(bmr)
}

plotBenchmarkRoc<-function(bmr) {
	allLearners = listLearners(warn.missing.packages=FALSE)
	row.names(allLearners)<-allLearners$class
	
	temp<-names(bmr$learners)
	temp<-gsub("\\.filtered","",temp)
	temp<-gsub("\\.featsel","",temp)
	learnersNames<-allLearners[temp,"name"]
	names(learnersNames)<-names(bmr$learners)
	learnersColors<-brewer.pal(length(bmr$learners),"Set1")
	names(learnersColors)<-names(bmr$learners)
	
	df = generateThreshVsPerfData(bmr, measures = list(fpr, tpr, mmce))
	if ("learner" %in% colnames(df$data)) {
		p = ggplot(df$data, aes(x=fpr, y=tpr,colour=learner))
	} else {
		p = ggplot(df$data, aes(x=fpr, y=tpr,colour=names(learnersNames)))
	}
	p<-p + geom_path(size=1)+ geom_abline(aes(intercept = 0, slope = 1), linetype = "dashed", alpha = .5)+xlab("False Positive Rate")+ylab("True Positive Rate")
	p<-p+scale_colour_manual(values=learnersColors,labels=learnersNames,name="Method")
	return(p)
}

plotBenchmarkSurvival<-function(bmr) {
	allLearners = listLearners(warn.missing.packages=FALSE)
	row.names(allLearners)<-allLearners$class
	
	temp<-names(bmr$learners)
	temp<-gsub("\\.filtered","",temp)
	temp<-gsub("\\.featsel","",temp)
	learnersNames<-allLearners[temp,"name"]
	names(learnersNames)<-names(bmr$learners)
	learnersColors<-brewer.pal(max(length(temp),3),"Set1")[1:length(temp)]
	names(learnersColors)<-names(bmr$learners)
	
	df = generateThreshVsPerfData(bmr, measures = list(fpr, tpr, mmce))
	if ("learner" %in% colnames(df$data)) {
		p = ggplot(df$data, aes(x=fpr, y=tpr,colour=learner))
	} else {
		p = ggplot(df$data, aes(x=fpr, y=tpr,colour=names(learnersNames)))
	}
	p<-p + geom_path(size=1)+ geom_abline(aes(intercept = 0, slope = 1), linetype = "dashed", alpha = .5)+xlab("False Positive Rate")+ylab("True Positive Rate")
	p<-p+scale_colour_manual(values=learnersColors,labels=learnersNames,name="Method")
	return(p)
}

extractSelectedFeatureInBenchmark<-function(bmr,getFunction=getFeatSelResult) {
	bmrModels<-getBMRModels(bmr,drop=FALSE)[[1]]
	allSelectedFeatures<-NULL
	for (learnerType in names(bmrModels)) {
		featureSelResult<-lapply(bmrModels[[learnerType]],getFunction)
		selectedFeatures<-lapply(featureSelResult,function(y) y$x)
		for (i in 1:length(selectedFeatures)) {
			if (length(selectedFeatures[[i]])>0) {
				temp<-data.frame(Feature=selectedFeatures[[i]],Learner=learnerType,iter=i,stringsAsFactors=FALSE)
				allSelectedFeatures<-rbind(allSelectedFeatures,temp)
			}
		}
	}
	return(allSelectedFeatures)
}

plotFeatureSelectionResult<-function(allSelectedFeatures,sort=TRUE) {
	if (sort) {
		temp<-table(allSelectedFeatures$Feature)
		levelOrder<-names(temp)[rev(order(temp))]
		allSelectedFeatures$Feature<-factor(as.character(allSelectedFeatures$Feature),levels=levelOrder)
	}
	allLearners = listLearners(warn.missing.packages=FALSE)
	row.names(allLearners)<-allLearners$class
	allSelectedFeatures[,"Learner"]<-gsub(".featsel$","",allSelectedFeatures[,"Learner"])
	allSelectedFeatures[,"Learner"]<-allLearners[as.character(allSelectedFeatures[,"Learner"]),"name"]
	
	p<-ggplot(allSelectedFeatures,aes(x=Feature))+geom_bar()+ylab("Count")+facet_wrap(~Learner)
	return(p)
}


extractFeatureImportanceInBenchmark<-function(bmr,learnerWithImportance=c("classif.boosting","classif.cforest","classif.gbm","classif.randomForest","classif.RRF",
				"classif.randomForestSRC","classif.ranger","classif.rpart","classif.xgboost")) {
  learnerWithImportance=c(learnerWithImportance,paste0(learnerWithImportance,".featsel"))
  
	learnerNames<-names(bmr$learners)
	learnerNames<-intersect(learnerNames,learnerWithImportance)
	
	if (length(learnerNames)>0) {
		result<-NULL
		for (learnerName in learnerNames) {
			models<-getBMRModels(bmr, learner.ids =learnerName,drop = TRUE)	
			for (i in 1:length(models)) {
				temp<-getFeatureImportance(models[[i]])$res
				result<-rbind(result,data.frame(Feature=names(temp),Importance=as.numeric(unlist(temp)),Learner=learnerName,Replicate=i))
			}
		}
		return(result)
	} else {
		print("There is no learner which can be used to calculate a feature importance measure.")
		return()
	}
}

plotFeatureImportance<-function(dataForPlot,type="boxplot",relative=FALSE,freescale=TRUE) {
	allLearners = listLearners(warn.missing.packages=FALSE)
	row.names(allLearners)<-allLearners$class
	dataForPlot[,"Learner"]<-allLearners[gsub(".featsel$","",as.character(dataForPlot[,"Learner"])),"name"]
	
	type<-match.arg(type,c("boxplot","barplot"))
	if (relative) {
		dataForPlot<-dataForPlot[order(dataForPlot[,c("Learner")],dataForPlot[,c("Replicate")]),]
		temp<-tapply(dataForPlot[,"Importance"],dataForPlot[,c("Learner","Replicate")],function(x) x/sum(x))
		dataForPlot$Importance<-unlist(temp)
	}
	if (type=="boxplot") {
		p<-ggplot(dataForPlot,aes(x=Feature,y=Importance))
		p<-p+geom_boxplot()
	} else if (type=="barplot") {
		df2 <- data_summary(dataForPlot, varname="Importance",groupnames=c("Feature","Learner"))
		p<- ggplot(df2, aes(x=Feature, y=Importance)) + geom_bar(stat="identity", color="black", 
						position=position_dodge()) +
				geom_errorbar(aes(ymin=Importance-sd, ymax=Importance+sd), width=.2,position=position_dodge(.9)) 
	}
	if (relative) {
		p<-p+ylab("Relative Importance")
	}
	if (freescale) {
		return(p+facet_wrap(~Learner,scale="free"))
	} else {
		return(p+facet_wrap(~Learner))
	}
}


