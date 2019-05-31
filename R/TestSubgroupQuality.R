#' Function compares different sets of patterns
#'   and verify how many of them a related to the goal class
#'
#' @param sets a list/vector of different sets, every set is a list of different pattern indices
#' @param labels a list/vector of target attribute(s) information. The attributes are aggregate by the function qfunc
#' @param qfunc is a function that takes labels of the target group a measure it relevancy to this attribute
#' @param qfunc.opt is an arbitrary data needed by qfunc
#' @param mode is the mode for computing extream values it can be one of 'small','large','both'.
#' @param n.bstrp is the number bootstrap trials
#' @param alpha is the significance level to be used for counting the class-related patterns
#'
#' @return For every set returns the probability that the quality value is within the confidence interval
#' @export
#'
#' @examples
#'
#' sets = list(list(SetName="TMP", Extents=list(c(1,5,7),c(2,3,4,5))))
#' labels = rbinom(10,1,0.5)
#' qfunc=function(labels, qfunc.opt=NULL) {
#'   return(mean(labels))
#' }
#' n.bstrp=100
#'
#' testSubgroupSetsQuality(sets,labels,qfunc,NULL,mode='b',n.bstrp)
testSubgroupSetsQuality=function(
  sets=list(),
  labels=c(),
  qfunc=function(labels, qfunc.opt){NULL},
  qfunc.opt=NULL,
  mode='both',
  n.bstrp=100,
  alpha=0.05)
{
  rnd.labels=NULL
  for(i in 1:n.bstrp) {
    rnd.labels=cbind(rnd.labels,sample(labels))
  }

  rslt=NULL
  rnd.rslt=NULL
  for(set in sets) {
    rnd.count=rep(0,ncol(rnd.labels))
    extNum=0
    for(ext in set$Extents) {
      if(length(ext)==0) {
        warning(paste0("Set ", extNum, " in ", set$SetName, " is empty."))
        next
      }
      extNum = extNum + 1
      extQ = qfunc(labels[ext],qfunc.opt)
      rndQs = apply(rnd.labels[ext,,drop=FALSE],2,qfunc,qfunc.opt)
      stopifnot(all(!is.na(rndQs)))

      largerValuesNum = sum(rndQs >= extQ)
      smallerValuesNum = sum(rndQs <= extQ)
      extreamValuesNum = -1
      if(mode == 'l' || mode == 'large') {
        extreamValuesNum = largerValuesNum
        rnd.count = rnd.count + (rndQs > quantile(rndQs, probs = 1-alpha))
      } else if(mode=='s' || mode=='small') {
        extreamValuesNum=smallerValuesNum
        rnd.count = rnd.count + (rndQs < quantile(rndQs, probs = alpha))
      } else if(mode=='b' || mode == 'both') {
        extreamValuesNum = min(largerValuesNum, smallerValuesNum)
        rnd.count = rnd.count + (quantile(rndQs, probs = alpha) > rndQs | rndQs > quantile(rndQs, probs = 1-alpha))
      } else {
        stopifnot(FALSE)
      }

      setRslt = data.frame(SetName = set$SetName, ExtNum=extNum, ExtreamValuesNum = extreamValuesNum)
      rslt=rbind(rslt,setRslt)
    }
    rnd.quantiles=quantile(rnd.count,probs = c(alpha,0.25,0.5,0.75,1-alpha))
    rnd.rslt=rbind(rnd.rslt,c(set$SetName,rnd.quantiles))
  }
  colnames(rnd.rslt)=c("SetName","Alpha","25%","50%","75%","1-Alpha")
  return(list(Data=rslt,Rnd=rnd.rslt))
}
