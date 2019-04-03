\name{changeStrip}
\alias{changeStrip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Change the color of the ggplot strip
}
\description{
  This function change the color of the strip of a ggplot passing in the argument. This step have to be done after have done the graph.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  changeStrip(gplot, color_swap, color_pallet = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gplot}{The ggplot from you want to change the strip color.}
  \item{color_swap}{a vector having the same length as the number of panel containing numerical (1 to x). This will be used to choose where to apply the color.}
  \item{color_pallet}{Optional argument to give colors. If not given they will be choose randomly.}
  %%     ~~Describe \code{x} here~~
}
\details{
  This function get in entry a ggplot object that contains strip (usually created by facet function). Then it will change the color of the strip given a vector.
}
\value{

  This function don't give a object in return but only print the plot with the rigth colors.
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
  library(reshape2) #used for facet_wrap function

  #generate a simple dataFrame
  generatedData <- dataG(100,2,3)

  #creating a simple plot using facet
  df <- ggplot(generatedData, aes(group, values)) +
        facet_wrap(~secondary_group)

  #updating the color
  changeStrip(df, c(1,2,3), c("red","blue","green"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
