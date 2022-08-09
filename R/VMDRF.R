##Variational Mode Decomposition Based Random Forest Model
VMDRF=function(data,k,alpha,tau,K,DC,init,tol,m,n){

  data_org=as.matrix(data)
  xt=as.matrix(data_org)
  xt=as.vector(data_org)

  #code for display no.of imf and residual

  try=VMDecomp::vmd(xt,alpha = alpha,
                    tau = tau,
                    K = K,
                    DC = DC,
                    init = init,
                    tol = tol)

  imf_extr=try$u
  total_IMF=ncol(imf_extr)
  no_of_imf=ncol(imf_extr)
  len_extr_imf=length(imf_extr[,1])
  length_split=len_extr_imf-1
  test_data_l=ceiling(k*length_split)
  test_data_original=data_org[(test_data_l+2):length(data_org),]
  length_test_data=length(test_data_original)
  # dataset creation
  extr_imf=0
  model_svm=0
  predicted_out=matrix(nrow =length_test_data,ncol = no_of_imf)
  MSE_out=0
  RMSE_out=0
  MAPE_out=0
  MAD_out=0
  final_predict_imf=0
  for (i in 1:no_of_imf)
  {
    extr_imf=imf_extr[,i]

    yt=extr_imf[1:(len_extr_imf-1)]
    xt=extr_imf[2:len_extr_imf]
    data=data.frame(yt,xt)
    len_data=length(data[,1])
    split_train=k*len_data
    r_train=ceiling(split_train)
    traindata=data[1:r_train,]
    testdata=data[(r_train+1):len_data,]

    model_RF <- randomForest::randomForest(yt ~ ., data=traindata, mtry=m, ntree=n)

    print(model_RF)
    predicted_out[,i]<- stats::predict(model_RF,testdata)
    final_predict_imf=final_predict_imf+predicted_out[,i]
  }

  final_prediction=final_predict_imf

  # summarize accuracy
  MSE_out <- mean((test_data_original - final_prediction)^2)
  RMSE_out<- sqrt(MSE_out)


  #mean absolute deviation (MAD)
  MAD_out=mean(abs(test_data_original - final_prediction))


  #Mean absolute percent error (MAPE)
  MAPE_out=mean(abs((test_data_original-final_prediction)/test_data_original))


  #Maximum Error
  ME_out=max(abs(test_data_original-final_prediction))
  #accuracy
  prediction_accuracy=cbind(RMSE_out,MAD_out,MAPE_out,ME_out)


  TotalIMF =  K
  output_f=list(Total_No_IMF=TotalIMF, Prediction_Accuracy_VMDRF =prediction_accuracy, Final_Prediction_VMDRF =final_prediction)
  return(output_f)
}

