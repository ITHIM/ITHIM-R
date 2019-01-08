\name{ithim}
\alias{ithim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
ITHIM
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A function to run the Integrated Health and Transport Impact Modelling Tool.
}
\usage{
ithim(data, injury_model = "default", trips_model = "default", pollution_model = "default",\cr mean_bus_walk_time = 5, mmet_cycling = 4.63, mmet_walking = 2.53, pm_conc_base = 50, pm_trans_share = 0.225, \cr pa_dose_response_quantile = FALSE, ap_dose_response_quantile = FALSE, background_pa_scalar = 1, \cr safety_scalar = 1, chronic_disease_scalar = 1, nsamples = 1, uncertanty = FALSE, iterations = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{x} here~~
Input data for the model.
}
  \item{injury_model}{
%%     ~~Describe \code{x} here~~
The type of model to use for the injury data. Default is X. If you have a Who Hit Who matrix use the default. If you have individual level data use "Poisson".
}
  \item{trips_model}{
%%     ~~Describe \code{x} here~~
The type of model to use for the trips data. Default is X. GUIDANCE ABOUT WHICH MODEL TO RUN DEPENDING ON DATA. 
}
  \item{pollution_model}{
%%     ~~Describe \code{x} here~~
The type of model to use for the pollution data. Default is X. GUIDANCE ABOUT WHICH MODEL TO RUN DEPENDING ON DATA.
}
  \item{mean_bus_walk_time}{
%%     ~~Describe \code{x} here~~
Mean walking time to the bus. Default is 5. User can input value from their own data.
}
  \item{mmet_cycling}{
%%     ~~Describe \code{x} here~~
Marginal MET values for cycling. Default is 4.63. User can input value from their own data.
}
  \item{mmet_walking}{
%%     ~~Describe \code{x} here~~
Marginal MET values for cycling. Default is 2.53. User can input value from their own data.
}
  \item{pm_conc_base}{
%%     ~~Describe \code{x} here~~
Background concentration of PM2.5 for the study region. Default is 50. We strongly encourage users to provide local input data. 
}
  \item{pm_trans_share}{
%%     ~~Describe \code{x} here~~
Share of PM2.5 due to transportation. Default is 0.225. We strongly encourage users to provide local input data. 
}
  \item{pa_dose_response_quantile}{
%%     ~~Describe \code{x} here~~
Logical. Default = FALSE. Include physical activity dose response quantiles in estimation. 
}
  \item{ap_dose_response_quantile}{
%%     ~~Describe \code{x} here~~
Logical. Default = FALSE. Include air pollution dose response quantiles in estimation. 
}
  \item{background_pa_scalar}{
%%     ~~Describe \code{x} here~~
Information. 
}
  \item{safety_scalar}{
%%     ~~Describe \code{x} here~~
Information. 
}
  \item{chronic_disease_scalar}{
%%     ~~Describe \code{x} here~~
Information. 
}
  \item{uncertainty}{
%%     ~~Describe \code{x} here~~
Logical. Default = FALSE.
}
  \item{nsamples}{
%%     ~~Describe \code{x} here~~
Number of resamples for uncertainty estimation. Default is 1, meaning no uncertainty estimation. If uncertainty = TRUE, you should set nsamples = X. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
REFERENCES GO HERE
}
\author{
%%  ~~who you are~~
ITHIM R Team 

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
