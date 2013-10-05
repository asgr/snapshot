\name{addhead}
\alias{addhead}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add header information to particle data
}
\description{
Function to add required header information to a Gadget read particle dataframe. This has sensible defaults for a small galaxy merger style simulation
}
\usage{
addhead(part, Npart = 2, Massarr = 0, Time = 0, z = 0, FlagSfr = 0,
FlagFeedback = 0, FlagCooling = 0, BoxSize = 0, OmegaM = 0, OmegaL = 0,
h = 1, FlagAge = 0, FlagMetals = 0, NallHW = 0, flag_entr_ics = 0)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{part}{
Strictly speaking 'part' is passed through the function, but to make this a useful object 'part' should be a data.frame containing the main particle level information. Columns required are:
\tabular{ll}{
ID \tab particle ID\cr
x \tab x position in  units of Mpc\cr
y \tab y position in  units of Mpc\cr
z \tab z position in  units of Mpc\cr
vx \tab x velocity in units of km/s\cr
vy \tab y velocity in units of km/s\cr
vz \tab z velocity in units of km/s\cr
Mass \tab particle mass in units of Msun\cr
}
}
  \item{Npart}{
The index on the Npart vector that should contain the particle number, where: gas [1] / collisionless particles [2:6]. The actual value is calculated based on the part data.frame provided with 'part', Nall is also calculated based on this number and not given as an option since the same index as Npart must be used
}
  \item{Massarr}{
The mass of the particles in the particle index provided to Npart
}
  \item{Time}{
Time of snapshot in units of km/s and kpc so 1 unit is ~10 Gyrs
}
  \item{z}{
Redshift of snapshot
}
  \item{FlagSfr}{
Star formation turned on/off
}
  \item{FlagFeedback}{
Feedback turned on/off
}
  \item{FlagCooling}{
Cooling turned on/off
}
  \item{BoxSize}{
Size of simulation box edge length in units of kpc
}
  \item{OmegaM}{
Omega matter of the simulation
}
  \item{OmegaL}{
Omega lambda of the simulation
}
  \item{h}{
Hubble constant divided by 100 used in the simulation
}
  \item{FlagAge}{
Stellar ages on/off
}
  \item{FlagMetals}{
Stellar metallacities on/off
}
  \item{NallHW}{
Tell Gadget to use large integers in the particle index provided to Npart- not usually necessary
}
  \item{flag_entr_ics}{
Entropy for gas on/off
}
}
\details{
Nall is calculated based on Npart, and therfore it cannot be specified via an input argument. This increases the likelihood that a legal Gadget header will be produced.
}
\value{
  \item{part}{
Strictly speaking 'part' is passed through the function, but to make this a useful object 'part' should be a data.frame containing the main particle level information. Assuming 'part' has been given a sensible input, columns provided are:
\tabular{ll}{
ID \tab particle ID\cr
x \tab x position in  units of Mpc\cr
y \tab y position in  units of Mpc\cr
z \tab z position in  units of Mpc\cr
vx \tab x velocity in units of km/s\cr
vy \tab y velocity in units of km/s\cr
vz \tab z velocity in units of km/s\cr
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
\code{\link{snapwrite}},\code{\link{snapread}},\code{\link{genparam}}
}

\examples{
\dontrun{
tempadd=addhead(temp$part)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{gadget}

