\name{VMDELM}
\alias{VMDELM}
\title{
Variational Mode Decomposition Based Extreme Learning Machine Model
}
\description{
The VMDELM function helps to fit the Variational Mode Decomposition based  Extreme Learning Machine Model. It will also provide you with accuracy measures along with an option to select the proportion of training and testing data sets. Users can choose among the available choices of parameters of regresion model for fitting the Variational Mode Decomposition based Extreme Learning Machine Model. In this package we have modelled the dependency of the study variable assuming first order autocorrelation. This package will help the researchers working in the area of hybrid machine learning models.
}
\usage{
VMDELM(data,k,alpha,tau,K,DC,init,tol)
}
\arguments{
  \item{data}{
Input univariate time series data.
}
  \item{k}{
Partition value for spliting the data set into training and testing.
}
  \item{alpha}{
a numeric value specifying the balancing parameter of the data-fidelity constraint.
}
  \item{tau}{
a numeric value specifying the time-step of the dual ascent ( pick 0 for noiseslack )
}
  \item{K}{
a numeric value specifying the number of modes to be recovered
}
  \item{DC}{
a boolean. If true the first mode is put and kept at DC (0-freq)
}
  \item{init}{
a numeric value. This parameter differs depending on the input ’data’ parameter
(1-dimensional and 2-dimensional)
}
  \item{tol}{
a numeric value specifying the tolerance of convergence criterion (typically this
parameter is around 1e-6 for the 1-dimensional and 1e-7 for the 2-dimensional
data)
}
}
\details{
Variational mode decomposition (VMD) is one of the latest signal decomposition techniques, similar to EMD, first proposed by Dragomiretskiy and Zosso (2014). This is a an entirely non-recursive variational mode decomposition model,where the modes are extracted concurrently. The algorithm generates an ensemble of modes and their respective center frequencies, such that the modes collectively reproduce the input signal. Further Extreme Learning Machine (ELM) model applied to each decomposed items to forecast them. Finally all forecasted values are aggregated to produce final forecast value (Das et al, 2020,2019,2022).
}
\value{
  \item{Total_No_IMF }{Total number of IMFs after decomposition by VMD method.}
  \item{Prediction_Accuracy_VMDELM }{List of performance measures of the fitted VMDELM model.}
  \item{Final_Prediction_VMDELM }{Final forecasted value of the VMD based ELM model. It is obtained by combining the forecasted value of all individual IMF and fresidue.}
}
\author{
Pankaj Das, Girish Kumar Jha, Tauqueer Ahmad and Achal Lama
}
\references{

Dragomiretskiy, K. and Zosso, D.(2014). Variational Mode Decomposition. IEEE Transactions on Signal Processing, 62(3):531-544. (doi: 10.1109/TSP.2013.2288675).

Das,P., Jha,G. K.,Lama,A., Parsad, R. and Mishra, D. (2020). Empirical Mode Decomposition based Support Vector Regression for Agricultural Price Forecasting. Indian Journal of Extension Education, 56(2): 7-12. (http://krishi.icar.gov.in/jspui/handle/123456789/44138).

Das, P., Jha, G.K., Lama, A. and Bharti (2022). “EMD-SVR” Hybrid Machine Learning Model and its Application in Agricultural Price Forecasting. Bhartiya Krishi Anusandhan Patrika. (DOI: 10.18805/BKAP385)

Das, P. Jha, G. K. and Lama, A. (2023). Empirical Mode Decomposition Based Ensemble Hybrid Machine Learning Models for Agricultural Commodity Price Forecasting. Statistics and Applications,21(1), 99-112.(http://krishi.icar.gov.in/jspui/handle/123456789/77772).

Das, P. (2019). Study On Machine Learning Techniques Based Hybrid Model for Forecasting in Agriculture. Published Ph.D. Thesis.

Choudhury, K., Jha, G. K., Das, P. and Chaturvedi, K. K. (2019). Forecasting Potato Price using Ensemble Artificial Neural Networks. Indian Journal of Extension Education, 55(1):71-77.
(http://krishi.icar.gov.in/jspui/handle/123456789/44873).
}


\seealso{
ELM,VMD,VMDecomp,VMDELM
}
\examples{
set.seed(6)
data3=rnorm(300,6.6,.36)
alpha = 2000
tau = 0
K= 3
DC = FALSE
init = 1
tol = 1e-6
#VMDELM(data3,0.8,alpha,tau,K,DC,init,tol)
}
\keyword{VMDELM}
\keyword{VMD}
\keyword{ELM}
