# server.R

library(fpp)

shinyServer(
  
  function(input, output) {
    
    output$ui_1 <- renderUI({
      if (is.null(input$input_type))
        return()
      
      sliderInput("period", "FORECASTing Period",
                  min = 1, max = 10, value = 1)
      
    })
    output$ui_2 <- renderUI({
      if (is.null(input$input_type))
        return()
      
      switch(input$input_type,
             "Moving Average" = sliderInput("interval", "Interval", min = 1, max = 15, value = 4),
             "ANN" = sliderInput("total_train", "Total Training", min = 0, max = 50, value = 7),
             "ARIMA" = sliderInput("train_set", "No. of Training data", min = 4, max = 24, value = 17)
      )
      
    })
    output$ui_3 <- renderUI({
      if (is.null(input$input_type))
        return()
      
      switch(input$input_type,
             "Exponential Smoothing" = sliderInput("alpha", "Alpha", min = 0, max = 1, value = 0.4),
             "Halt's Method" = sliderInput("alpha", "Alpha", min = 0, max = 1, value = 0.4),
             "Halt Winter's Method" = sliderInput("alpha", "Alpha", min = 0, max = 1, value = 0.4),
             "ANN" = sliderInput("LC", "Learning Constant", min = 0, max = 1, value = 0.15)
      )
      
    })
    output$ui_4 <- renderUI({
      if (is.null(input$input_type))
        return()
      
      switch(input$input_type,
             "Halt's Method" = sliderInput("beta", "Beta", min = 0, max = 1, value = 0.4),
             "Halt Winter's Method" = sliderInput("beta", "Beta", min = 0, max = 1, value = 0.4),
             "ANN" = sliderInput("bias", "Bias", min = 0, max = 1, value = 0.2)
      )
      
    })
    output$ui_5 <- renderUI({
      if (is.null(input$input_type))
        return()
      
      switch(input$input_type,
             "Halt Winter's Method" = sliderInput("gamma", "Gamma", min = 0, max = 1, value = 0.4),
             "ANN" = sliderInput("extra_waights", "increment in weekdays in %", min = 0, max = 100, value = 20)
      )
      
    })
    
    output$contents <- renderTable({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      
      x<-read.table(inFile$datapath, header=input$header, sep=input$sep)
      
      switch(input$input_type,
             "Mean" = f<-mean_func(x,input,output,input$period),
             "Moving Average" = f<-moving_avg_func(x,input,output,input$period,input$interval),
             "Exponential Smoothing" = f<-ses_func(x,input,output,input$period,input$alpha),
             "Halt's Method" = f<-halts_func(x,input,output,input$period,input$alpha,input$beta),
             "Halt Winter's Method" = f<-halt_winters_func(x,input,output,input$period,input$alpha,input$beta,input$gamma),
             "Simple Regression" = f<-simple_regr_func(x,input,output,input$period),
             "ARIMA" = f<-ARIMA(x,input,output,input$period,input$train_set),
             "ANN" = f<-neural_network(x,input,output,input$period, input$total_train, input$LC, input$bias, input$extra_waights)
      )
      row<-dim(x)[1]
      FORECAST<-f[1:row]
      x<- cbind(x,FORECAST)
      x
    })
  }
)
#-----------------------------------------------------------------------------------------------
mean_func <- function(data,input,output,period){
  row <- dim(data)[1]
  
  ANS <-data$Sales[1]  # first prediction <- first value
  FORECAST <- ANS
  
  FORECAST_PERIOD<- period
  counter<-1
  error_curr <- 0
  error <- 0
  error_sq <- 0
  error_abs <- 0
  error_perc <- 0
  
  while(counter<=row) {
    error_curr <- data$Sales[counter] - ANS;
    error <- error + error_curr;
    error_sq <- error_sq + error_curr*error_curr;
    error_abs <- error_abs + abs(error_curr);
    error_perc <- error_perc + (error_curr/data$Sales[counter])*100;
    
    ANS <- ((counter-1)*ANS + data$Sales[counter])/counter;
    FORECAST <- c(FORECAST,ANS);
    counter<-counter+1;
  }
  
  lapply(counter, function(counter) {
    output[[paste(counter)]] <- renderUI({
      strong(paste("UPCOMING FORECASTS"))
    })
  })
  
  # FORECASTing now :-
  index<-0
  while(index<FORECAST_PERIOD) {
    FORECAST <- c(FORECAST,ANS);
    lapply(counter+index, function(counter) {
      output[[paste(counter+1)]] <- renderUI({
        paste("For time: ",counter," , Prediction: ",FORECAST[counter],"\n")
      })
    })
    index<-index+1;
  }
  
  # calculate Theil's U-statistic
  counter <- 1;
  num <- 0;
  denom <- 0;
  while(counter<row) {
    num <- num +
      ((FORECAST[counter+1]-data$Sales[counter+1])/data$Sales[counter])**2;
    denom <- denom +
      ((data$Sales[counter+1]-data$Sales[counter])/data$Sales[counter])**2;
    counter <- counter +1
  }
  
  output$text1 <- renderUI({
    str1<-paste("Mean Error :",error/row,"\n");
    str2<-paste("Mean Absolute Error :",error_abs/row,"\n");
    str3<-paste("Mean Square Error :",error_sq/row,"\n");
    str4<-paste("Mean % Error :",error_perc/row,"%\n");
    str5<-paste("Theil's U-statistic : ", sqrt(num/denom),"\n");
    strong(HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>')))
  })
  
  output$dynamicPlot <- renderPlot({
    plot.ts(data$Sales, xlab="TIME", ylab="Sales in Thousands",xlim=c(1,30))
    lines(FORECAST,col="blue")
  })
  
  return(FORECAST)
}
#-------------------------------------------------------------------------------------------
moving_avg_func<- function(data,input,output,period,intervl){
  row <- dim(data)[1]
  interval <- intervl
  FORECAST<-NULL
  FORECAST_PERIOD<- period
  counter<-1
  while(counter<=interval) { # first interval FORECAST = 0
    FORECAST <- c(FORECAST,0);  
    counter <- counter+1;
  }
  error_curr <- 0
  error <- 0
  error_sq <- 0
  error_abs <- 0
  error_perc <- 0
  
  while(counter<=row) {
    ANS<-0
    inner_counter <- counter-interval;
    
    while(inner_counter<counter){
      ANS<-ANS+data$Sales[inner_counter];
      inner_counter<- inner_counter+1;
    }
    ANS<-ANS/interval
    error_curr <- data$Sales[counter] - ANS;
    error <- error + error_curr;
    error_sq <- error_sq + error_curr*error_curr;
    error_abs <- error_abs + abs(error_curr); 
    error_perc <- error_perc + (error_curr/data$Sales[counter])*100;
    FORECAST <- c(FORECAST,ANS);
    counter<-counter+1; 
  }
  
  lapply(counter, function(counter) {
    output[[paste(counter)]] <- renderUI({
      strong(paste("UPCOMING FORECASTS"))
    })
  })
  
  # FORECASTing now :-
  ANS<-0
  inner_counter <- counter-interval;
  while(inner_counter<counter){
    ANS<-ANS+data$Sales[inner_counter];
    inner_counter<- inner_counter+1;
  }
  ANS<-ANS/interval
  index<-0
  while(index<FORECAST_PERIOD) {
    FORECAST <- c(FORECAST,ANS);
    lapply(counter+index, function(counter) {
      output[[paste(counter+1)]] <- renderUI({
        paste("For time: ",counter," , Prediction: ",FORECAST[counter],"\n")
      })
    })
    index<-index+1;
  }
  
  # calculate Theil's U-statistic
  counter <- interval+1;
  num <- 0;
  denom <- 0;
  while(counter<row) {
    num <- num + ((FORECAST[counter+1]-data$Sales[counter+1])/data$Sales[counter])**2;
    denom <- denom + ((data$Sales[counter+1]-data$Sales[counter])/data$Sales[counter])**2;
    counter <- counter +1
  }
  
  # printing the errors
  output$text1 <- renderUI({
    str1<-paste("Mean Error :",error/row,"\n");
    str2<-paste("Mean Absolute Error :",error_abs/row,"\n");
    str3<-paste("Mean Square Error :",error_sq/row,"\n");
    str4<-paste("Mean % Error :",error_perc/row,"%\n");
    str5<-paste("Theil's U-statistic : ", sqrt(num/denom),"\n");
    strong(HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>')))
  })
  
  output$dynamicPlot <- renderPlot({
    plot.ts(data$Sales, xlab="TIME", ylab="Sales in Thousands",xlim=c(1,30))
    lines(FORECAST,col="blue")
  })
  
  return(FORECAST)  
}
#-----------------------------------------------------------------------------------------
ses_func<- function(data,input,output,period,alp){
  row <- dim(data)[1]
  
  alpha <- alp
  NO_PRED <- period
  
  FORECAST<-0  # first observation is NULL
  FORECAST<-c(FORECAST,data$Sales[1]) # 2nd observation is initialied as 1st
  counter<-2
  
  error_curr <- 0
  error <- 0
  error_sq <- 0
  error_abs <- 0
  error_perc <- 0
  
  while(counter<=row) {
    error_curr <- data$Sales[counter] - FORECAST[counter];
    error <- error + error_curr;
    error_sq <- error_sq + error_curr*error_curr;
    error_abs <- error_abs + abs(error_curr); 
    error_perc <- error_perc + (error_curr/data$Sales[counter])*100;
    
    FORECAST_NEXT<-alpha*data$Sales[counter]+(1-alpha)*FORECAST[counter]
    FORECAST <- c(FORECAST,FORECAST_NEXT);
    counter<-counter+1; 
  }
  
  lapply(counter, function(counter) {
    output[[paste(counter)]] <- renderUI({
      strong(paste("UPCOMING FORECASTS"))
    })
  })
  
  # Now predicting 
  index<-1
  while(index<=NO_PRED){
    
    lapply(row+index, function(counter) {
      output[[paste(counter+1)]] <- renderUI({
        paste("For time: ",counter," , Prediction: ",FORECAST[row+index],"\n")
      })
    })
    
    FORECAST <- c(FORECAST,FORECAST[row+index]);
    index<- index+1;
  }
  
  # calculate Theil's U-statistic
  
  counter <- 1;
  num <- 0;
  denom <- 0;
  while(counter<row) {
    num <- num + ((FORECAST[counter+1]-data$Sales[counter+1])/data$Sales[counter])**2;
    denom <- denom + ((data$Sales[counter+1]-data$Sales[counter])/data$Sales[counter])**2;
    counter <- counter +1
  }
  
  # printing the errors
  output$text1 <- renderUI({
    str1<-paste("Mean Error :",error/row,"\n");
    str2<-paste("Mean Absolute Error :",error_abs/row,"\n");
    str3<-paste("Mean Square Error :",error_sq/row,"\n");
    str4<-paste("Mean % Error :",error_perc/row,"%\n");
    str5<-paste("Theil's U-statistic : ", sqrt(num/denom),"\n");
    strong(HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>')))
  })
  
  output$dynamicPlot <- renderPlot({
    plot.ts(data$Sales, xlab="TIME", ylab="Sales in Thousands",xlim=c(1,30))
    lines(FORECAST,col="blue")
  })
  
  return(FORECAST)  
}
#-------------------------------------------------------------------------------------------
halts_func<- function(data,input,output,period,alp,bet){
  row <- dim(data)[1]
  
  alpha <- alp
  beta <- bet
  NO_PRED <- period
  
  FORECAST<-0  # first observation is NULL
  
  LEVEL<-data$Sales[1] #L1 = Y1
  
  if(beta==0) {
    TREND <- 0
  } else {
    TREND <-data$Sales[2]-data$Sales[1] #b1 = Y2-Y1
  }
  
  
  counter<-2
  
  error_curr <- 0
  error <- 0
  error_sq <- 0
  error_abs <- 0
  error_perc <- 0
  
  while(counter<=row) {
    
    FORECAST_NEXT<-LEVEL[counter-1]+TREND[counter-1]*1;
    FORECAST <- c(FORECAST,FORECAST_NEXT);
    
    error_curr <- data$Sales[counter] - FORECAST[counter];
    error <- error + error_curr;
    error_sq <- error_sq + error_curr*error_curr;
    error_abs <- error_abs + abs(error_curr); 
    error_perc <- error_perc + (error_curr/data$Sales[counter])*100;
    
    LEVEL_NEW <- alpha*data$Sales[counter]+(1-alpha)*(LEVEL[counter-1]+TREND[counter-1]);
    LEVEL<-c(LEVEL, LEVEL_NEW)
    
    TREND_NEW <- beta*(LEVEL[counter]-LEVEL[counter-1])+(1-beta)*TREND[counter-1]
    TREND<-c(TREND,TREND_NEW)
    
    counter<-counter+1; 
  }
  
  lapply(counter, function(counter) {
    output[[paste(counter)]] <- renderUI({
      strong(paste("UPCOMING FORECASTS"))
    })
  })
  
  # Now predicting 
  index <- 1
  while(index<=NO_PRED){
    FORECAST_NEXT<-LEVEL[counter-1]+TREND[counter-1]*index;
    FORECAST <- c(FORECAST,FORECAST_NEXT);
    
    lapply(row+index, function(counter) {
      output[[paste(counter+1)]] <- renderUI({
        paste("For time: ",counter," , Prediction: ",FORECAST[counter],"\n")
      })
    })
    
    index <- index+1
  }
  
  # calculate Theil's U-statistic
  
  counter <- 1;
  num <- 0;
  denom <- 0;
  
  while(counter<row) {
    num <- num + ((FORECAST[counter+1]-data$Sales[counter+1])/data$Sales[counter])**2;
    denom <- denom + ((data$Sales[counter+1]-data$Sales[counter])/data$Sales[counter])**2;
    counter <- counter +1
  }
  
  # printing the errors
  output$text1 <- renderUI({
    str1<-paste("Mean Error :",error/row,"\n");
    str2<-paste("Mean Absolute Error :",error_abs/row,"\n");
    str3<-paste("Mean Square Error :",error_sq/row,"\n");
    str4<-paste("Mean % Error :",error_perc/row,"%\n");
    str5<-paste("Theil's U-statistic : ", sqrt(num/denom),"\n");
    strong(HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>')))
  })
  
  output$dynamicPlot <- renderPlot({
    plot.ts(data$Sales, xlab="TIME", ylab="Sales in Thousands",xlim=c(1,30))
    lines(FORECAST,col="blue")
  })
  
  return(FORECAST)  
}
#------------------------------------------------------------------------------------------
halt_winters_func<- function(data,input,output,period,alp,bet,gama){
  row <- dim(data)[1]
  
  alpha <- alp
  beta <- bet
  gamma <- gama
  
  NO_PRED <- period
  season <- 4
  
  index<-1
  FORECAST<-NULL
  LEVEL<-NULL
  TREND<-NULL
  SEASN<-NULL
  SUM<-0
  while(index<=season){
    FORECAST<-c(FORECAST,0);  # first season observation is NULL
    LEVEL<-c(LEVEL,0);
    TREND<-c(TREND,0);
    SUM<-SUM+data$Sales[index];
    index<-index+1;
  }
  LEVEL[season]<-SUM/season  #Ls = summ(Yi)/s
  
  index<-1
  temp<-0
  while(index<=season){
    temp<-temp+(data$Sales[season+index]-data$Sales[index])/season;
    index<-index+1;
  }
  TREND[season]<-temp/season #bs = 1/s[(Ys+1 - Y1)/s + ... + (Ys+s - Ys)/s]
  index<-1
  while(index<=season){
    SEASN<-c(SEASN,data$Sales[index]/LEVEL[season]);
    index<-index+1;
  }
  
  counter<-season
  
  error_curr <- 0
  error <- 0
  error_sq <- 0
  error_abs <- 0
  error_perc <- 0
  
  while(counter<=row) {
    
    FORECAST_NEXT<-(LEVEL[counter-1]+TREND[counter-1]*1)*SEASN[counter-season];
    FORECAST <- c(FORECAST,FORECAST_NEXT);
    
    error_curr <- data$Sales[counter] - FORECAST[counter];
    error <- error + error_curr;
    error_sq <- error_sq + error_curr*error_curr;
    error_abs <- error_abs + abs(error_curr); 
    error_perc <- error_perc + (error_curr/data$Sales[counter])*100;
    
    LEVEL_NEW <- alpha*(data$Sales[counter]/SEASN[counter-season])+(1-alpha)*(LEVEL[counter-1]+TREND[counter-1]);
    LEVEL<-c(LEVEL, LEVEL_NEW);
    
    TREND_NEW <- beta*(LEVEL[counter]-LEVEL[counter-1])+(1-beta)*TREND[counter-1];
    TREND<-c(TREND,TREND_NEW);
    
    SEASN_NEW<- gamma*(data$Sales[counter]/LEVEL[counter])+(1-gamma)*SEASN[counter-season];
    SEASN<-c(SEASN,SEASN_NEW);
    
    counter<-counter+1; 
  }
  
  lapply(counter, function(counter) {
    output[[paste(counter)]] <- renderUI({
      strong(paste("UPCOMING FORECASTS"))
    })
  })
  
  # Now predicting 
  index <- 1
  index2<- 1
  while(index<=NO_PRED){
    FORECAST_NEXT<-(LEVEL[counter-1]+TREND[counter-1]*index)*SEASN[counter-1+index2-season];
    FORECAST <- c(FORECAST,FORECAST_NEXT);
    lapply(row+index, function(counter) {
      output[[paste(counter+1)]] <- renderUI({
        paste("For time: ",counter," , Prediction: ",FORECAST[counter],"\n")
      })
    })
    index <- index+1
    index2 <- index2+1
    if(index2>season) {index2=1;}
  }
  
  # calculate Theil's U-statistic
  
  counter <- season+1;
  num <- 0;
  denom <- 0;
  
  while(counter<row) {
    num <- num + ((FORECAST[counter+1]-data$Sales[counter+1])/data$Sales[counter])**2;
    denom <- denom + ((data$Sales[counter+1]-data$Sales[counter])/data$Sales[counter])**2;
    counter <- counter +1
  }
  
  # printing the errors
  output$text1 <- renderUI({
    str1<-paste("Mean Error :",error/row,"\n");
    str2<-paste("Mean Absolute Error :",error_abs/row,"\n");
    str3<-paste("Mean Square Error :",error_sq/row,"\n");
    str4<-paste("Mean % Error :",error_perc/row,"%\n");
    str5<-paste("Theil's U-statistic : ", sqrt(num/denom),"\n");
    strong(HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>')))
  })
  
  output$dynamicPlot <- renderPlot({
    plot.ts(data$Sales, xlab="TIME", ylab="Sales in Thousands",xlim=c(1,30))
    lines(FORECAST,col="blue")
  })
  
  return(FORECAST)  
}
#--------------------------------------------------------------------------------------------
simple_regr_func<- function(data,input,output,per){
  
  row <- dim(data)[1]
  period=per
  
  # Y = bX+a
  
  y_sum <- 0  # sales is y-axis
  x_sum <- 0   # time is x-axis
  counter<-1
  while(counter<=row) {
    y_sum <- y_sum+data$Sales[counter];
    x_sum <- x_sum+counter;
    counter<-counter+1; 
  }
  y_mean <- y_sum/row
  x_mean <- x_sum/row
  
  # finding 'b'
  num<-0
  denom<-0
  counter<-1
  while(counter<=row) {
    num<-num+(counter-x_mean)*(data$Sales[counter]-y_mean)
    denom<-denom+(counter-x_mean)*(counter-x_mean)
    counter<-counter+1; 
  }
  b<-num/denom
  a<-y_mean-b*x_mean
  
  FORECAST <- NULL
  counter<-1
  error_curr <- 0
  error <- 0
  error_sq <- 0
  error_abs <- 0
  error_perc <- 0
  num<-0
  denom<-0
  while(counter<=row) {
    ANS <- b*counter+a;
    FORECAST <- c(FORECAST,ANS);
    
    error_curr <- data$Sales[counter] - ANS;
    error <- error + error_curr;
    error_sq <- error_sq + error_curr*error_curr;
    error_abs <- error_abs + abs(error_curr); 
    error_perc <- error_perc + (error_curr/data$Sales[counter])*100;
    
    num<-num+(FORECAST[counter]-y_mean)**2;
    denom<-denom+(data$Sales[counter]-y_mean)**2;
    counter<-counter+1; 
  }
  
  R_Sq<-num/denom
  
  lapply(counter, function(counter) {
    output[[paste(counter)]] <- renderUI({
      strong(paste("UPCOMING FORECASTS"))
    })
  })
  
  index<- 1
  while(index<=period) {
    ANS <- b*counter+a;
    FORECAST <- c(FORECAST,ANS);
    lapply(counter, function(counter) {
      output[[paste(counter+1)]] <- renderUI({
        paste("For time: ",counter," , Prediction: ",FORECAST[counter],"\n")
      })
    })
    counter<-counter+1
    index<-index+1
  }
  
  # calculate Theil's U-statistic
  counter <- 1;
  num <- 0;
  denom <- 0;
  
  while(counter<row) {
    num <- num + ((FORECAST[counter+1]-data$Sales[counter+1])/data$Sales[counter])**2;
    denom <- denom + ((data$Sales[counter+1]-data$Sales[counter])/data$Sales[counter])**2;
    counter <- counter +1
  }
  
  # printing the errors
  output$text1 <- renderUI({
    str0<-paste("Equation : Sales = ",round(b,2),"*time + ",round(a,2),"\n");
    str01<-paste("This line fits the data with :",round(R_Sq*100,2),"% accuracy");
    str1<-paste("Mean Error :",error/row,"\n");
    str2<-paste("Mean Absolute Error :",error_abs/row,"\n");
    str3<-paste("Mean Square Error :",error_sq/row,"\n");
    str4<-paste("Mean % Error :",error_perc/row,"%\n");
    str5<-paste("Theil's U-statistic : ", sqrt(num/denom),"\n");
    strong(HTML(paste(str0, str01, str1, str2, str3, str4, str5, sep = '<br/>')))
  })
  
  output$dynamicPlot <- renderPlot({
    plot(data$Sales, xlab="TIME", ylab="Sales in Thousands",xlim=c(1,30))
    lines(FORECAST,col="blue")
  })
  return(FORECAST) 
}
#--------------------------------------------------------------------------------------------
ARIMA<- function(data,input,output,per,train){
  row <- dim(data)[1]
  
  FORECAST_PERIOD<- per
  season<-4
  trainset<-train
  
  tsobj<- ts(data$Sales[1:trainset], frequency = season)
  arimafit<- auto.arima(tsobj)
  forecasting<- forecast(arimafit, h=row-trainset+FORECAST_PERIOD)
  
  FORECAST<-NULL
  counter<- 1
  while(counter<=trainset){
    FORECAST<-c(FORECAST,fitted(arimafit)[counter])
    counter<-counter+1
  }
  counter<- trainset+1
  idx<-1
  error_curr <- 0
  error <- 0
  error_sq <- 0
  error_abs <- 0
  error_perc <- 0
  
  while(counter<=row) {
    error_curr <- data$Sales[counter] - forecasting$mean[idx];
    error <- error + error_curr;
    error_sq <- error_sq + error_curr*error_curr;
    error_abs <- error_abs + abs(error_curr);
    error_perc <- error_perc + (error_curr/data$Sales[counter])*100;
    
    FORECAST <- c(FORECAST,forecasting$mean[idx]);
    counter<-counter+1;
    idx<-idx+1;
  }
  
  lapply(counter, function(counter) {
    output[[paste(counter)]] <- renderUI({
      strong(paste("UPCOMING FORECASTS"))
    })
  })
  counter<-counter+1
  # FORECASTing now :-
  while(counter<=row+FORECAST_PERIOD+1) {
    FORECAST <- c(FORECAST,forecasting$mean[idx]);
    lapply(counter, function(counter) {
      output[[paste(counter+1)]] <- renderUI({
        paste("For time: ",counter," , Prediction: ",FORECAST[counter-1],"\n")
      })
    })
    counter<-counter+1;
    idx<-idx+1;
  }
  
  # calculate Theil's U-statistic
  counter <- trainset+1;
  num <- 0;
  denom <- 0;
  while(counter<row) {
    num <- num +
      ((FORECAST[counter+1]-data$Sales[counter+1])/data$Sales[counter])**2;
    denom <- denom +
      ((data$Sales[counter+1]-data$Sales[counter])/data$Sales[counter])**2;
    counter <- counter +1
  }
  
  output$text1 <- renderUI({
    str1<-paste("Mean Error :",error/row,"\n");
    str2<-paste("Mean Absolute Error :",error_abs/row,"\n");
    str3<-paste("Mean Square Error :",error_sq/row,"\n");
    str4<-paste("Mean % Error :",error_perc/row,"%\n");
    str5<-paste("Theil's U-statistic : ", sqrt(num/denom),"\n");
    strong(HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>')))
  })
  
  output$dynamicPlot <- renderPlot({
    plot.ts(data$Sales, xlab="TIME", ylab="Sales in Thousands",xlim=c(1,30))
    lines(FORECAST,col="blue")
  })
  
  return(FORECAST)
}
#--------------------------------------------------------------------------------------------
neural_network <- function(data,INPUT,OUTPUT,period, total_train, LC, bias, ex_wts){
  row <- dim(data)[1]
  input <- data$Sales
  
  total_input_neuron = 7
  total_pred = period
  tn = total_input_neuron
  FORECAST <- 0
  ac = 0.05
  learning_const = LC
  average = 0
  
  error_curr <- 0
  error <- 0
  error_sq <- 0
  error_abs <- 0
  error_perc <- 0
  xt<-NULL
  wt<-NULL
  
  for(i in 1:row){
    if(i <= tn){
      FORECAST[i] = input[i]
    }
    xt[i] = input[i]/1000.0
  }
  for(i in (row-tn): tn){
    average = average + input[i]
  }
  average = average/(tn)
  tmp = 0.0
  for(i in 1:row){
    tmp = tmp + (1.0/xt[i])
  }
  
  tmp = tmp/(5*row)
  for(i in 1:row){
    wt[i] = tmp
  }
  for(i in row+1:total_pred){
    input[i] = average
    wt[i] = tmp
    average = (average*7 + average - input[i-tn])/7
  }
  ul =row-tn
  for(xl in tn+1:ul){
    tt = 1
    xs = xl - tn
    #print(xs)
    while(tt <= total_train){
      
      tsum <- 0
      for(i in xs:tn){
        tsum = tsum +  (xt[i]*wt[i])
      }
      #ADDING BIAS 
      tsum = tsum + bias
      
      #ACTIVATION FUNCTION
      y = 1.0/(1 + exp(-ac * tsum))
      y = y*1000.0
      out = y
      err = input[xl] - out
      #---------------------------
      error_curr = err
      error <- error + error_curr
      error_sq <- error_sq + error_curr*error_curr
      error_abs <- error_abs + abs(error_curr)
      error_perc <- error_perc + (error_curr/input[xl])*100
      #---------------------------
      
      for(i in xs:tn){
        dw = 0.2* learning_const * err * xt[i]
        
        #adding extra waights to neurons on weekdays
        ex = i %% 8
        if(ex == 6 || ex == 7){
          dw = dw + (ex_wts/100)*dw  
        }
        
        wt[i] = wt[i] + dw;
      }
      tt = tt+1
      #print(wt)
      #print(err)
    }
    FORECAST[xl] = out
  }
  counter<-1
  
  #FUTURE PREDICTION
  
  for(xl in row+1:total_pred){
    tt = 1
    while(tt <= total_train){
      xs = xl-tn-1
      tsum <- 0
      for(i in xs:tn){
        tsum = tsum +  (xt[i]*wt[i])
      }
      #ADDING BIAS 
      tsum = tsum + bias
      
      #ACTIVATION FUNCTION
      y = 1.0/(1 + exp(-ac * tsum))
      y = y*1000.0
      out = y
      err = input[xl] - out
      for(i in xs:tn){
        dw = 0.2* learning_const * err * xt[i]
        wt[i] = wt[i] + dw;
      }
      tt = tt+1
      #print(wt)
      #print(err)
    }
    FORECAST[xl] = out
    input[xl] = out
    lapply(xl, function(counter) {
      OUTPUT[[paste(counter+1)]] <- renderUI({
        paste("For time: ",counter," , Prediction: ",FORECAST[counter],"\n")
      })
    })
  }
  # calculate Theil's U-statistic
  counter <- 1;
  num <- 0;
  denom <- 0;
  
  while(counter<row) {
    num <- num + ((FORECAST[counter+1]-data$Sales[counter+1])/data$Sales[counter])**2;
    denom <- denom + ((data$Sales[counter+1]-data$Sales[counter])/data$Sales[counter])**2;
    counter <- counter +1
  }
  
  # printing the errors
  OUTPUT$text1 <- renderUI({
    str1<-paste("Mean Error :",error/row,"\n");
    str2<-paste("Mean Absolute Error :",error_abs/row,"\n");
    str3<-paste("Mean Square Error :",error_sq/row,"\n");
    str4<-paste("Mean % Error :",error_perc/row,"%\n");
    str5<-paste("Theil's U-statistic : ", sqrt(num/denom),"\n");
    strong(HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>')))
  })
  
  OUTPUT$dynamicPlot <- renderPlot({
    plot(data$Sales, xlab="TIME", ylab="Sales in Thousands",xlim=c(1,30))
    lines(FORECAST,col="blue")
  })
  print(FORECAST)
  return (FORECAST)
}
#--------------------------------------------------------------------------------------------
