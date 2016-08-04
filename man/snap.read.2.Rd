\name{snap.read.2}
\alias{snap.read.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in a Gadget format 2 snapshot file
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function opens the file and reads the header (also checks that the blocks are well written). 
Then goes trought the full file, checking the blocks. If the block label matches it returns that block.
}
\usage{
snap.read.2(file, what, ndim, type, debug, gas)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{filename} here~~
  	File name of the snapshot file to be read.
}
  \item{what}{
%%     ~~Describe \code{what} here~~
   	Name of the block to be read.
}
  \item{ndim}{
%%     ~~Describe \code{ndim} here~~
	OPTIONAL: Dimensions of the block.
}
  \item{type}{
%%     ~~Describe \code{type} here~~
	OPTIONAL: Type of data.
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
	OPTIONAL: add extra information. If debug = 1, then it shows all the blocks present in the file.
}
  \item{gas}{
%%     ~~Describe \code{gas} here~~
	OPTIONAL: read only gas files.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Opens the file, and read header (also checks that the blocks are well written). 
Then goes through the full file, checking the blocks. If the block label matches \option{what} then it returns that block DATA.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns the DATA block from the file.
}
\author{
%%  ~~who you are~~
Federico Stasyszyn & Aaron Robotham
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{snap.write.block.2}}, \code{\link{snap.add.head}}, \code{\link{snap.write.head.2}}, \code{\link{snap.select.type.2}}, \code{\link{snap.gen.param}}
}
\examples{
    #None yet!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{gadget}
\keyword{snapshot}