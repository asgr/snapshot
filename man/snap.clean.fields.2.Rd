\name{snap.clean.fields.2}
\alias{snap.clean.fields.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean out a Gadget format 2 snapshot file
}
\description{
Function to read format 2 blocks and write back out. It is usefull to clean the DM particles.
}
\usage{
snap.clean.fields.2(file, gas)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
	INPUT file name to be read
}
  \item{gas}{
	OPTIONAL: write only the GAS particles.
}
}
\author{
Federico Stasyszyn
}

\seealso{
\code{\link{snap.write.block.2}}, \code{\link{snap.add.head.1}}, \code{\link{snap.write.head.2}}, \code{\link{snap.select.type.2}}, \code{\link{snap.gen.param}}
}
\examples{
    #None yet!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{clean}
\keyword{gadget}
\keyword{format2}
