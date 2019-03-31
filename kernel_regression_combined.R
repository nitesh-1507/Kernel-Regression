Kernel_prediction_circ=function(test_xvar, test_xcirc, train_xvar, train_xcirc, train_y)
  {
  
  test_pred=rep(0,nrow(test_xvar))
  
  ##Bandwidth calculation
  bandwidth= function(train_xvar, train_y){
    
    feature_num1 = ncol(train_xvar)
    c=rep(1,length(feature_num1))
    for (i in 1:feature_num1){
      c[i]=dpill(train_xvar[,i], train_y)
    }
    return(c)
    
  }
  
  ##Kappa calculation
  kappa= function(train_xcirc,train_y){
    
    feature_num2 = ncol(train_xcirc)
    c=rep(1,length(feature_num2))
    for (i in 1:feature_num2){
      c[i]= 1/((dpill(train_xcirc[,i], train_y))^2)
      }
    return(c)
  }
  
  ##Kernel calculation
  kernel= function(yvar,ycirc,train_xvar, train_xcirc,train_y ){
    
    feature_num1 = ncol(train_xvar)
    feature_num2 = ncol(train_xcirc)
    lambda=bandwidth(train_xvar, train_y)
    kappa_value=kappa(train_xcirc, train_y)
    c=rep(1,nrow(train_xvar))
    d=rep(1,nrow(train_xcirc))
    for (i in 1:feature_num1){
      c=c*((exp(-(yvar[i]-train_xvar[,i])^2/(2*(lambda[i]^2))))/((lambda[i])*(sqrt(2*pi))))
    }
    for (j in 1:feature_num2){
      d=d*((exp(kappa_value[j]*(cos(ycirc[j]-train_xcirc[,j]))))/(2*pi*besselI(kappa_value[j],0)))
    }
    return(c*d)
  }
  
  
  
  ##Weight calculation
  weight= function(kernel){
    
    c = kernel/sum(kernel)
    return(c)
  }
 
  for (j in 1:nrow(test_xvar))
  { 
    
    kernel_voltage_power = kernel(test_xvar[j,],test_xcirc[j,],train_xvar,train_xcirc, train_y)
    weight_voltage_power= weight(kernel_voltage_power)
    
    test_pred[j]=weight_voltage_power%*%train_y
  }
  
  return(test_pred)
}