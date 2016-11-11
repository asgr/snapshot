\name{snap.select.type.2}
\alias{snap.select.type.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns information from the common Gadget format 2 snapshot LABELS
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A convenience function. Given some commonly used Gadget format 2 snashot labels, it return the data type and dimensions.
}
\usage{
snap.select.type.2(what)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what}{
%%     ~~Describe \code{what} here~~
  What is the name of label that we are looking for.
}
}
\value{
   If the label is found the dimentions and the type of data.
   If not found Type=FALSE and Ndim=0. The same is returned if HEADER.
}
\author{
%%  ~~who you are~~
Federico Stasyszyn & Aaron Robotham
}

\seealso{
\code{\link{snap.write.block.2}}, \code{\link{snap.add.head.1}}, \code{\link{snap.write.head.2}}, \code{\link{snap.read.2}}, \code{\link{snap.gen.param}}
}
\examples{
    #None yet!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{gadget}
\keyword{snapshot}