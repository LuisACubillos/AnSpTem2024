bound2dom <- function(data=bound){
  npairs = dim(data$idx)[1]
  idx = idx1 = idx2= NULL
  for(i in 1:npairs){
    if(data$idx[i,2] < data$idx[i,1]){
      tmp = data$idx[i,2]
      idx1 = c(idx1,tmp)
      tmp = data$idx[i,1]
      idx2 = c(idx2,tmp)
    }
    idx = cbind(idx1,idx2)
  }
  npol = length(idx[,1])
  dom_coord <- data$loc
  itmp = seq(idx[1,1],idx[1,2])
  dom = data.frame(dom_coord[itmp,1:2],"ID"=1)
  for(i in 2:npol){
    itmp = seq(idx[i,1],idx[i,2])
    dom = data.frame(rbind(dom,data.frame(dom_coord[itmp,1:2],"ID"=i)))
  }
  colnames(dom) = c("X","Y","ID")
  return(dom)
}
