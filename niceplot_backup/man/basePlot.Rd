\name{basePlot}
\alias{basePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  generates spine of ggplot2 plot for the other function of the packages
}
\description{
  This function use ggplot2 library to setup a ggplot2 object containing all the base argument to be used later by te other function from this packages
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
basePlot(data, values_name, group_name, aditional_grouping_name = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data frame containing the column used for values_name and group_name}
  \item{values_name}{either the name or the place of the colmun containing values you want to plot in the data frame given in the data argument}
  \item{group_name}{either the name or the place of the colmun containing factors to indicates how to group values for the box_plot. It can contain more than two level.}
  \item{aditional_grouping_name}{another factor column to group for}
%%     ~~Describe \code{x} here~~
}
\details{
This function as been made to be used by the others function of the package. Thus it is not meant to be used alone since it result only in an unfinished ggplot item.
}
\value{

Return a ggplot item containing the base for the other function.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Abel Garnier
GitHub : Mazklaus
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

#creating a simple base
base <- basePlot(generatedData,1,2)

#adding some features to the base
base <- base + geom_point()

#Show the result (not made for this purpuse)
base

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
