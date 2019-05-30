#' Function compares different sets of patterns
#'   and verify how many of them a related to the goal class
#'
#' @param sets a list/vector of different sets, every set is a list of different pattern indices
#' @param labels a list/vector of target attribute(s) information. The attributes are aggregate by the function qfunc
#' @param qfunc is a function that takes labels of the target group a measure it relevancy to this attribute
#' @param qfunc.opt is an arbitrary data needed by qfunc
#' @param n.bstrp is the number bootstrap trials
#'
#' @return For every set returns the probability that the quality value is within the confidence interval
#' @export
#'
#' @examples
#'
#' sets = list(list(SetsName="TMP", Extents=list(c(1,5,7),c(2,3,4,5))))
#' labels = rbinom(10,1,0.5)
#' qfunc=function(labels, qfunc.opt=NULL) {
#'   return(mean(labels))
#' }
#' n.bstrp=100
#'
#' testSubgroupSetsQuality(sets,labels,qfunc,NULL,n.bstrp)
testSubgroupSetsQuality=function(
  sets=list(),
  labels=c(),
  qfunc=function(labels, qfunc.opt){NULL},
  qfunc.opt=NULL,
  n.bstrp=100)
{
  rnd.labels=NULL
  for(i in 1:n.bstrp) {
    rnd.labels=cbind(rnd.labels,sample(labels))
  }

  rslt=NULL
  for(set in sets) {
    extNum=0
    for(ext in set$Extents) {
      extNum = extNum + 1
      extQ = qfunc(labels[ext],qfunc.opt)
      rndQs = apply(rnd.labels[ext,],2,qfunc,qfunc.opt)

      largerValuesNum = sum(rndQs >= extQ)
      smallerValuesNum = sum(rndQs <= extQ)
      extreamValuesNum = min(largerValuesNum, smallerValuesNum)

      setRslt = data.frame(SetsName = set$SetsName, ExtNum=extNum, ExtreamValuesNum = extreamValuesNum)
      rslt=rbind(rslt,setRslt)
    }
  }
  rslt
}
