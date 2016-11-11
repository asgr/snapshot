\name{snap.strip.2}
\alias{snap.strip.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Strip out particles from a Gadget format 2 snapshot.
}
\description{
A convenience function to strip out different particle types and arrange as separate list elements.
}
\usage{
snap.strip.2(snap, type=6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{snap}{
The Gadget format-2 snapshot data.
}
  \item{type}{
Vector; the particle types to strip out into separate lists.  
}
}

\value{
A list of particles data.frames as requested, named paste("T",type,sep='').
}
\author{
Federico Stasyszyn
}

\seealso{
\code{\link{snap.write.block.2}}, \code{\link{snap.write.head.2}}, \code{\link{snap.read.2}}, \code{\link{snap.add.head.1}}, \code{\link{snap.gen.param}}
}
\examples{
    #None yet!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{gadget}% use one of  RShowDoc("KEYWORDS")
\keyword{snapshot}% __ONLY ONE__ keyword per line
