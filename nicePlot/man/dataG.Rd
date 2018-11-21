\name{dataG}
\alias{dataG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  data set generator
}
\description{
  This function is used to create example of ggplot dataset
}
\usage{
dataG(nrow, ngroup = 2, nsecondary_group = NA, groupAsFactor = TRUE, forceDif = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nrow}{number of line in the final frame}
  \item{ngroup}{number of different group in the group variable}
  \item{nsecondary_group}{number of different group to put in another variable}
  \item{groupAsFactor}{A logical indicating wether or not the group variable must be in character or factor. Default is factor}
  \item{forceDif}{A logical to indicating weither or not you want to force the difference between the group}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame matching the input parameter.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Abel Garnier (Github : Mazklaus)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(nicePlot)

#generate a simple dataFrame
generatedData <- dataG(100)

#generate a complex dataFrame
generatedData <- dataG(100,3,4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
