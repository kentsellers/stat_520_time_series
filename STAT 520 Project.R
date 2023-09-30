### Data prep

rm(list = ls())

library(readr)
library(Hmisc)

KentIm <- read_csv("C:/Users/wsellers/My Docs/R/KentIm.csv")
KentIm <- KentIm[,-1]

KentRe <- read_csv("C:/Users/wsellers/My Docs/R/KentRe.csv")
KentRe <- KentRe[,-1]



### For each time series of my sample, fit an MA(6) model

Im_MA6_matrix <- matrix(NA,nrow=0, ncol=7)
for (row in 1:nrow(KentIm)){
  model <- arima(as.ts(KentIm[row,])[1,], order=c(0,0,6),method='CSS')
  Im_MA6_matrix <- rbind(Im_MA6_matrix,as.vector(model$coef))
}

Re_MA6_matrix <- matrix(NA,nrow=0, ncol=7)
for (row in 1:nrow(KentRe)){
  model <- arima(as.ts(KentRe[row,])[1,], order=c(0,0,6),method='CSS')
  Re_MA6_matrix <- rbind(Re_MA6_matrix,as.vector(model$coef))
}



### For each time series of my sample, fit an AR(6) model

Im_AR6_matrix <- matrix(NA,nrow=0, ncol=7)
for (row in 1:nrow(KentIm)){
  model <- arima(as.ts(KentIm[row,])[1,], order=c(6,0,0),method='CSS')
  Im_AR6_matrix <- rbind(Im_AR6_matrix,as.vector(model$coef))
}

Re_AR6_matrix <- matrix(NA,nrow=0, ncol=7)
for (row in 1:nrow(KentRe)){
  model <- arima(as.ts(KentRe[row,])[1,], order=c(6,0,0),method='CSS')
  Re_AR6_matrix <- rbind(Re_AR6_matrix,as.vector(model$coef))
}



### For each time series of my sample, fit an ARMA(3,3) model

Im_ARMA33_matrix <- matrix(NA,nrow=0, ncol=7)
for (row in 1:nrow(KentIm)){
  model <- arima(as.ts(KentIm[row,])[1,], order=c(3,0,3),method='CSS')
  Im_ARMA33_matrix <- rbind(Im_ARMA33_matrix,as.vector(model$coef))
}

Re_ARMA33_matrix <- matrix(NA,nrow=0, ncol=7)
for (row in 1:nrow(KentRe)){
  model <- arima(as.ts(KentRe[row,])[1,], order=c(3,0,3),method='CSS')
  Re_ARMA33_matrix <- rbind(Re_ARMA33_matrix,as.vector(model$coef))
}



### Environment clean up
remove(model)
remove(row)



### Collect the parameters of the models fitted to each of the time series and plot the distribution. Then, make a box plot of the parameters of the MA(6), AR(6), and ARMA (3,3) models.


# Histograms and boxplots of Im_MA6 coefficients: theta1s,theta2s,...,theta6s
win.graph(width=60,height=30)
  hist.data.frame(as.data.frame(Im_MA6_matrix[,1:6]))

win.graph(width=60,height=30)
  boxplot(Im_MA6_matrix[,1],Im_MA6_matrix[,2],Im_MA6_matrix[,3],Im_MA6_matrix[,4],Im_MA6_matrix[,5],Im_MA6_matrix[,6], add=TRUE)

# Histograms and boxplots of Re_MA6 coefficients theta1s,theta2s,...,theta6s
win.graph(width=60,height=30)
  hist.data.frame(as.data.frame(Re_MA6_matrix[,1:6]))

win.graph(width=60,height=30)
  boxplot(Re_MA6_matrix[,1],Re_MA6_matrix[,2],Re_MA6_matrix[,3],Re_MA6_matrix[,4],Re_MA6_matrix[,5],Re_MA6_matrix[,6])

  
# Histograms and boxplots of Im_AR6 coefficients theta1s,theta2s,...,theta6s
win.graph(width=60,height=30)
  hist.data.frame(as.data.frame(Im_AR6_matrix[,1:6]))

win.graph(width=60,height=30)
  boxplot(Im_AR6_matrix[,1],Im_AR6_matrix[,2],Im_AR6_matrix[,3],Im_AR6_matrix[,4],Im_AR6_matrix[,5],Im_AR6_matrix[,6])

# Histograms and boxplots of Re_AR6 coefficients theta1s,theta2s,...,theta6s
win.graph(width=60,height=30)
  hist.data.frame(as.data.frame(Re_AR6_matrix[,1:6]))

win.graph(width=60,height=30)
  boxplot(Re_AR6_matrix[,1],Re_AR6_matrix[,2],Re_AR6_matrix[,3],Re_AR6_matrix[,4],Re_AR6_matrix[,5],Re_AR6_matrix[,6])

  
# Histograms and boxplots of Im_ARMA33 coefficients theta1s,theta2s,...,theta6s
win.graph(width=60,height=30)
  hist.data.frame(as.data.frame(Im_ARMA33_matrix[,1:6]))

win.graph(width=60,height=30)
  boxplot(Im_ARMA33_matrix[,1],Im_ARMA33_matrix[,2],Im_ARMA33_matrix[,3],Im_ARMA33_matrix[,4],Im_ARMA33_matrix[,5],Im_ARMA33_matrix[,6])

# Histograms and boxplots of Re_ARMA33 coefficients theta1s,theta2s,...,theta6s
win.graph(width=60,height=30)
  hist.data.frame(as.data.frame(Re_ARMA33_matrix[,1:6]))

win.graph(width=60,height=30)
  boxplot(Re_ARMA33_matrix[,1],Re_ARMA33_matrix[,2],Re_ARMA33_matrix[,3],Re_ARMA33_matrix[,4],Re_ARMA33_matrix[,5],Re_ARMA33_matrix[,6])


 