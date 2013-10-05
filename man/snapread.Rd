\name{snapread}
\alias{snapread}

\title{
Read in Gadget snapshots
}
\description{
This function allows the user to read in the standard format Gadget binaries. It keeps the particle information and header information in separate components of a list.
}
\usage{
snapread(file)
}

\arguments{
  \item{file}{
The full path to the Gadget snapshot to be read in.
}
}

\value{
  \item{part}{
A data.frame containing the main particle level information. Columns included are:
\tabular{ll}{
ID \tab particle ID\cr
x \tab x position in  units of Mpc\cr
y \tab y position in  units of Mpc\cr
z \tab z position in  units of Mpc\cr
vx \tab x velocity\cr
vy \tab y velocity\cr
vz \tab z velocity\cr
Mass \tab particle mass in units of Msun\cr
}
}
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
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{snapwrite}},\code{\link{addhead}},\code{\link{genparam}}
}

\examples{
\dontrun{
temp=snapread('somepath/snapshot_XXX')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{gadget}
\keyword{snapshot}
