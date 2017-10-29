subsetpred = cancer %>% dplyr::select(-binnedinc, -medianagefemale, -medianagemale, -geography, -pcths18_24, -pctnohs18_24, -pctsomecol18_24, -pctbachdeg18_24, -pctemployed16_over, -pctprivatecoveragealone, -pctempprivcoverage, -pctpubliccoverage, -pctpubliccoveragealone)
# Predict formula
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

# Model Selection by Cross Validation
set.seed(11)
folds = sample(rep(1:10,length=nrow(subsetpred)))
#folds
#table(folds)
cv.errors=matrix(NA,10,20)
for(k in 1:10){
  best.fit=regsubsets(target_deathrate~.,data=subsetpred[folds!=k,],nvmax=20,method="forward")
  for(i in 1:20){
    pred=predict(best.fit,subsetpred[folds==k,],id=i)
    cv.errors[k,i]=mean( (subsetpred$target_deathrate[folds==k]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
#cvplot <- plot(rmse.cv,pch=19,type="b")
