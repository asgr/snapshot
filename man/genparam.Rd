\name{genparam}
\alias{genparam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a Gadget paramter file
}
\description{
Function to generator a legal Gadget paramter setup file. This has a sensible selection of defaults chosen for fairly small (non Cosmological) simulations.
}
\usage{
genparam(ParamFile = "galaxy.param", ParamBase = "./HernTest/",
InitCondFile = "./HernStart.gdt", OutputDir = "./HernTest/", EnergyFile = "energy.txt",
InfoFile = "info.txt", TimingsFile = "timings.txt", CpuFile = "cpu.txt",
RestartFile = "restart", SnapshotFileBase = "snapshot",
OutputListFilename = "parameterfiles/output_list.txt", TimeLimitCPU = 36000,
ResubmitOn = 0, ResubmitCommand = "my-scriptfile", ICFormat = 1, SnapFormat = 1,
ComovingIntegrationOn = 0, TypeOfTimestepCriterion = 0, OutputListOn = 0,
PeriodicBoundariesOn = 0, TimeBegin = 0, TimeMax = 0.001, Omega0 = 0, OmegaLambda = 0,
OmegaBaryon = 0, HubbleParam = 1, BoxSize = 0, TimeBetSnapshot = 1e-05,
TimeOfFirstSnapshot = 0, CpuTimeBetRestartFile = 36000, TimeBetStatistics = 0.05,
NumFilesPerSnapshot = 1, NumFilesWrittenInParallel = 1, ErrTolIntAccuracy = 0.025,
CourantFac = 0.3, MaxSizeTimestep = 0.1, MinSizeTimestep = 0, ErrTolTheta = 0.5,
TypeOfOpeningCriterion = 1, ErrTolForceAcc = 0.005, TreeDomainUpdateFrequency = 0.1,
DesNumNgb = 32, MaxNumNgbDeviation = 8, ArtBulkViscConst = 1, InitGasTemp = 0,
MinGasTemp = 100, PartAllocFactor = 3.0, TreeAllocFactor = 4.8, BufferSize = 25,
UnitLength_in_cm = 3.085678e+21, UnitMass_in_g = 1.989e+43,
UnitVelocity_in_cm_per_s = 1e+05, GravityConstantInternal = 0,
MinGasHsmlFractional = 0.25, SofteningGas = 1e-04, SofteningHalo = 1e-04,
SofteningDisk = 0.4, SofteningBulge = 0.8, SofteningStars = 0, SofteningBndry = 0.1,
SofteningGasMaxPhys = 1e-04, SofteningHaloMaxPhys = 1e-04, SofteningDiskMaxPhys = 0.4,
SofteningBulgeMaxPhys = 0.8, SofteningStarsMaxPhys = 0, SofteningBndryMaxPhys = 0.1,
MaxRMSDisplacementFac = 0.2, NFWConcentration = 10, VirialMass = 200, FlatRadius = 1e-05,
DeltaVir = 200, addNFW = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ParamFile}{
Name for the paramter file
}
  \item{ParamBase}{
Base file path for the paramter file
}
  \item{InitCondFile}{
Full path of file containing initial conditions
}
  \item{OutputDir}{
Base directory in which to put the major Gadget outputs, including snapshots etc
}
  \item{EnergyFile}{
Name to give energy file
}
  \item{InfoFile}{
Name to give info file
}
  \item{TimingsFile}{
Name to give timings file
}
  \item{CpuFile}{
Name to give CPU file
}
  \item{RestartFile}{
Name to give restart file
}
  \item{SnapshotFileBase}{
Base name for snapshots, appended by snapshot number
}
  \item{OutputListFilename}{
Name of file containing output times / expansion factors
}
  \item{TimeLimitCPU}{
Max CPU time to use for Gadget run
}
  \item{ResubmitOn}{
Flag to tell super-computer there is a resubmit file
}
  \item{ResubmitCommand}{
Specific to super-computer resubmit command
}
  \item{ICFormat}{
Initial conditions format: PUT OPTIONS IN TABLE HERE
}
  \item{SnapFormat}{
Snapshot format: PUT OPTIONS IN TABLE HERE
}
  \item{ComovingIntegrationOn}{
Allow for expansion of Universe
}
  \item{TypeOfTimestepCriterion}{
Type of particle integrator- leave at 0
}
  \item{OutputListOn}{
Flag to tell it to use OutputListFilename as input
}
  \item{PeriodicBoundariesOn}{
Flag to turn on/off periodic box boundaries, only needed for large cosmological runs
}
  \item{TimeBegin}{
Time at the beginning of simulation
}
  \item{TimeMax}{
Max time to evolve particles to
}
  \item{Omega0}{
Total energy density
}
  \item{OmegaLambda}{
Cosmological constant energy density
}
  \item{OmegaBaryon}{
Baryonic energy density
}
  \item{HubbleParam}{
Value of H0/100 to be used
}
  \item{BoxSize}{
Length of box edge (important for cosmological runs only)
}
  \item{TimeBetSnapshot}{
Time between snapshots
}
  \item{TimeOfFirstSnapshot}{
Time at which to output first snapshot
}
  \item{CpuTimeBetRestartFile}{
How often to output full restart file
}
  \item{TimeBetStatistics}{
Time between energy.txt updates
}
  \item{NumFilesPerSnapshot}{
How many files to split snapshots over
}
  \item{NumFilesWrittenInParallel}{
How many files to split snapshots over (probably ignore)
}
  \item{ErrTolIntAccuracy}{
Orbital integration accuracy
}
  \item{CourantFac}{
Limit on time step compared to sound crossing time for hydro runs
}
  \item{MaxSizeTimestep}{
Maximum time step allowed
}
  \item{MinSizeTimestep}{
Minimum time step allowed
}
  \item{ErrTolTheta}{
Controls the accurary of integration (smaller is closer to direct N-body)
}
  \item{TypeOfOpeningCriterion}{
Barnes-Hut or modified opening criteria (probably ignore)
}
  \item{ErrTolForceAcc}{
Only used for modified opening criterion (use default)
}
  \item{TreeDomainUpdateFrequency}{
How often should a tree be constructed
}
  \item{DesNumNgb}{
Number of neighbours to use for denisty estimation in SPH
}
  \item{MaxNumNgbDeviation}{
How much tolerance is allowed when finding neighbours
}
  \item{ArtBulkViscConst}{
Artificial viscosity term (use default)
}
  \item{InitGasTemp}{
Initial gas temperature
}
  \item{MinGasTemp}{
Minimum gas temperature allowed in the run
}
  \item{PartAllocFactor}{
Memory buffer per particle per processor
}
  \item{TreeAllocFactor}{
Memory buffer for tree calculation
}
  \item{BufferSize}{
Total memory buffer between processors
}
  \item{UnitLength_in_cm}{
Assumed IC distance units in cm (default assumes Kpc for input)
}
  \item{UnitMass_in_g}{
Assumed mass of provided IC mass units in grams (default assumes 1e10 Msun for input)
}
  \item{UnitVelocity_in_cm_per_s}{
Assumed velocity of provided units in cm/s (default assumes km/s)
}
  \item{GravityConstantInternal}{
Internal units for g
}
  \item{MinGasHsmlFractional}{
Minimum multiplicitive factor for smoothing length in hyrdo gas
}
  \item{SofteningGas}{
Softening to use for gas particles
}
  \item{SofteningHalo}{
Softening to use for halo particles
}
  \item{SofteningDisk}{
Softening to use for disk particles
}
  \item{SofteningBulge}{
Softening to use for bulge particles
}
  \item{SofteningStars}{
Softening to use for star particles
}
  \item{SofteningBndry}{
Softening to use for boundary particles
}
  \item{SofteningGasMaxPhys}{
Physical softening to use for gas particles (only relevant for Cosmo run)
}
  \item{SofteningHaloMaxPhys}{
Physical softening to use for halo particles (only relevant for Cosmo run)
}
  \item{SofteningDiskMaxPhys}{
Physical softening to use for disk particles (only relevant for Cosmo run)
}
  \item{SofteningBulgeMaxPhys}{
Physical softening to use for bulge particles (only relevant for Cosmo run)
}
  \item{SofteningStarsMaxPhys}{
Physical softening to use for star particles (only relevant for Cosmo run)
}
  \item{SofteningBndryMaxPhys}{
Physical softening to use for boundary particles (only relevant for Cosmo run)
}
  \item{MaxRMSDisplacementFac}{
Biggest distance that a particle can move in a time step
}
  \item{NFWConcentration}{
Concentration of analytic NFW profile, addNFW must be set to TRUE
}
  \item{VirialMass}{
Mass within virial radius of analytic NFW profile, addNFW must be set to TRUE
}
  \item{FlatRadius}{
Forces the NFW profile to be cored (not cusped), addNFW must be set to TRUE
}
  \item{DeltaVir}{
Virial overdensity of NFW profile, addNFW must be set to TRUE
}
  \item{addNFW}{
Logic determining whether the analyic NFW specific paramters be added to the setup file? See above
}
}

\value{
No value returned, called for the side-effect of writing out a Gadget paramter setup file.
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{snapwrite}},\code{\link{snapread}},\code{\link{addhead}}
}

\examples{
\dontrun{
genparam('example.param','Demo/Example1/')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{gadget}

