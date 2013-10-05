\name{snapshot-package}
\alias{snapshot-package}
\alias{snapshot}
\docType{package}
\title{
Gadget N-body cosmological simulation code snapshot I/O utilities
~~ package title ~~
}
\description{
Functions for reading and writing Gadget N-body snapshots. The Gadget code is popular in astronomy for running N-body / hydrodynamical cosmological and merger simulations. To find out more about Gadget see the main distribution page at www.mpa-garching.mpg.de/gadget/
}
\details{
\tabular{ll}{
Package: \tab snapshot\cr
Type: \tab Package\cr
Version: \tab 0.1.2\cr
Date: \tab 2013-10-04\cr
License: \tab GPL-2 \cr
}

}
\author{
Aaron Robotham

Maintainer: Aaron Robotham <aaron.robotham@uwa.edu.au>

}

\examples{
\dontrun{
temp=snapread('snapshot_XXX')
temp$part[,'x']=temp$part[,'x']+10
snapwrite(temp$part,temp$head,'snapshot_XXX_mod')
}
}
