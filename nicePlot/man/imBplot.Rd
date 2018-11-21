\name{imBplot}
\alias{imBplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  This function create a convinient boxplot
}
\description{
  This function is a wrapper for ggplot2 to facilitate the creation of boxplot.
}
\usage{
imBplot(data, values_name, group_name, aditional_grouping_name = NA, notched = FALSE, dot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{data}{data frame containing the column used for values_name and group_name}
  \item{values_name}{either the name or the place of the colmun containing values you want to plot in the data frame given in the data argument}
  \item{group_name}{either the name or the place of the colmun containing factors to indicates how to group values for the box_plot. It can contain more than two level.}
  \item{aditional_grouping_name}{another factor column to group for}
\item{notched}{A logical indicating wether or not you prefer get notched box rather than simple box plot. Default is FALSE}
\item{dot}{A logical indicating wether or not you want the point to appear on the graph. This help appreciating the distribution. Default is TRUE}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The output of the function is a ggplot object containing all the boxplot.You can stock it or just use the functionto print the plot in the plot panel.
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

#creating a simple box plot
imBplot(generatedData,1,2)
#or
base <- imBplot(generatedData,1,2)

#generate a three group dataFrame
generatedData <- dataG(100,3)

#creating a simple box plot
imBplot(generatedData,1,2)
#or
base <- imBplot(generatedData,1,2)

#generate a three group dataFrame
generatedData <- dataG(100,3,2)

#creating a simple box plot
imBplot(generatedData,1,2,3)
#or
base <- imBplot(generatedData,1,2,3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
