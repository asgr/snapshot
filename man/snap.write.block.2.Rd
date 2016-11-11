\name{snap.write.block.2}
\alias{snap.write.block.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Function to add blocks to an existing Gadget snapshot format 2 file.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function to add blocks to an existing format 2 snapshot file. First it checks the data input and uses \code{\link{snap.select.type.2}} to define block properties. Then it writes the LABEL block and the DATA block.
}
\usage{
snap.write.block.2(file, label, inp, ndim, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{filename} here~~
	Name of the file to be opened to append the blocks.
}
  \item{label}{
%%     ~~Describe \code{label} here~~
	LABEL of the block to be added.
}
  \item{inp}{
%%     ~~Describe \code{inp} here~~
	Input array of data. In general a data frame if dim of 3, then x,y,z are expected in the data frame.
}
  \item{ndim}{
%%     ~~Describe \code{ndim} here~~
	OPTIONAL: dimensions of the input array usually 3 or 1 dimentions.
}
  \item{type}{
%%     ~~Describe \code{type} here~~
	OPTIONAL/DEPRECATED.
}
}
\value{
No value returned. Called for the side-effect of writing out a binary Gadget format-2 snapshot file.
}
\author{
Federico Stasyszyn
}
\note{
%%  ~~further notes~~
Note that you need already a file with a header. You can generate it with \code{\link{snap.write.head.2}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{snap.read.2}}, \code{\link{snap.add.head.1}}, \code{\link{snap.write.head.2}}, \code{\link{snap.gen.param}}
}
\examples{
    #None yet!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{gadget}
\keyword{snapshot}