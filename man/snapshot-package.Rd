\name{snapshot-package}
\alias{snapshot-package}
\alias{snapshot}
\docType{package}
\title{
Gadget N-body cosmological simulation code snapshot format 1 and 2 I/O utilities
}
\description{
Functions for reading and writing Gadget N-body format 1 and 2 snapshots. The Gadget code is popular in astronomy for running N-body / hydrodynamical cosmological and merger simulations. To find out more about Gadget see the main distribution page at www.mpa-garching.mpg.de/gadget/. Format 1 specific functions end with a ".1" suffix and format 2 specific functions end with a ".2" suffix. Generic functions have neither suffix.
}
\details{
\tabular{ll}{
Package: \tab snapshot\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2016-08-01\cr
License: \tab GPL-3 \cr
}

}
\author{
Aaron Robotham & Federico Stasyszyn

Maintainer: Aaron Robotham <aaron.robotham@uwa.edu.au>

}

\examples{
\dontrun{
temp=snap.read.1('snapshot_XXX')
temp$part[,'x']=temp$part[,'x']+10
snap.write.1(temp$part,temp$head,'snapshot_XXX_mod')
}
}
