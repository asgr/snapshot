\name{snap.write.head.2}
\alias{snap.write.head.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Function to start creating a gadget format 2 snapshot file with a header
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function to start creating a Gadget format 2 snapshot file from a given header. Writes the label block and the header. 
}
\usage{
snap.write.head.2(head, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{head}{
A list containing various header information as list elements. These are:
\tabular{ll}{
Npart \tab Vector of length 6 containing the number of particles in this
snapshot file, where:\cr\tab gas [1] / collisionless particles [2:6]\cr
Massarr \tab Vector of length 6 containing the particle masses for the respective particle types\cr\tab in Npart\cr
Time \tab Time of snapshot in units of km/s and kpc so 1 unit is ~10 Gyrs\cr
z \tab Redshift of snapshot\cr
FlagSfr \tab Star formation turned on/off\cr
Nall \tab Vector of length 6 containing the number of particles in all
snapshot files, where:\cr\tab gas [1] / collisionless particles [2:6]\cr
FlagFeedback \tab Feedback turned on/off\cr
FlagCooling \tab Cooling turned on/off\cr
NumFiles \tab Number of files per snapshot- usually 1\cr
BoxSize \tab Size of simulation box edge length in units of kpc\cr
OmegaM \tab Omega matter of the simulation\cr
OmegaL \tab Omega lambda of the simulation\cr
h \tab Hubble constant divided by 100 used in the simulation\cr
FlagAge \tab Stellar ages on/off\cr
FlagMetals \tab Stellar metallacities on/off\cr
NallHW \tab Tell Gadget to use large integers for the respective particle types in Npart
\cr\tab- not usually necessary\cr
flag_entr_ics \tab Entropy for gas on/off
}
}
  \item{file}{
The full path to the Gadget snapshot to be created.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
	Basically, it opens the file to be written. Writes the label blocks and then the header itself.
}
\note{
	With this function one starts the process of writing a snapshot in Gadget format-2.
}

\author{
%%  ~~who you are~~
Federico Stasyszyn
}

\seealso{
\code{\link{snap.write.block.2}}, \code{\link{snap.add.head.1}}, \code{\link{snap.select.type.2}}, \code{\link{snap.read.2}}, \code{\link{snap.gen.param}}
}

\examples{
    #None yet!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{gadget}
\keyword{snapshot}