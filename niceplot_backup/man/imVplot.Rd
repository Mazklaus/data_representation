\name{imVplot}
\alias{imVplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  function to create enhanced violin plot
}
\description{
  This function is a ggplot2 wrapper use to facilitate the creation of violin plot.
}
\usage{
imVplot(data, values_name, group_name, aditional_grouping_name = NA, box = TRUE, dot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{data}{data frame containing the column used for values_name and group_name}
  \item{values_name}{either the name or the place of the colmun containing values you want to plot in the data frame given in the data argument}
  \item{group_name}{either the name or the place of the colmun containing factors to indicates how to group values for the box_plot. It can contain more than two level.}
  \item{aditional_grouping_name}{another factor column to group for}
\item{box}{A logical indicating wether or not you want to show a box plot inside the violin. Default is TRUE}
\item{dot}{A logical indicating wether or not you want the point to appear on the graph. This information is redundant with the violin itself but can help understand what is a violin plot. Default is FALSE}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The output of the function is a ggplot object containing all the violin.You can stock it or just use the functionto print the plot in the plot panel.
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
imVplot(generatedData,1,2)
#or
base <- imVplot(generatedData,1,2)

#generate a three group dataFrame
generatedData <- dataG(100,3)

#creating a simple box plot
imVplot(generatedData,1,2)
#or
base <- imVplot(generatedData,1,2)

#generate a three group dataFrame
generatedData <- dataG(100,3,2)

#creating a simple box plot
imVplot(generatedData,1,2,3)
#or
base <- imVplot(generatedData,1,2,3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
