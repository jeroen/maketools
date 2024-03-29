% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.R
\name{make}
\alias{make}
\alias{make_call}
\alias{make_echo}
\alias{make_info}
\title{Make}
\usage{
make(target = "all", makefile = r_makeconf_path())

make_call(cmd = "$(CC)", args = "--version")

make_echo(cmd = "$(CC)")

make_info()
}
\arguments{
\item{target}{name of output file that you want to make}

\item{makefile}{path to the \code{Makefile}. Defaults to the \code{Makeconf} which
R uses when building R packages.}

\item{cmd}{command to invoke (may be a variable)}

\item{args}{additional arguments for \code{cmd}}
}
\description{
Compile C / C++ / Fortran source files using the compiler configured
by your R \code{Makeconf} file.
}
\details{
The \code{make} function literally calls \verb{make yourfile.o -f /path/to/R/Makeconf}.
This is exactly what R does when building packages and hence the best
way to test if the compiler is working.
}
\examples{
# Test the CXX compiler
if(cxx_info()$available){
testprog <- '#include <iostream>\nint main() {std::cout << "Hello World!";}'
writeLines(testprog, con = 'testprog.cc')
make('testprog')

# Test and cleanup
system('./testprog')
unlink('testprog*', recursive = TRUE)
}

# Run a program from a make variable
make_call('$(CXX)', '--version')

# Where your makeconf is stored:
make_info()

}
\seealso{
Other maketools: 
\code{\link{diagnostics}},
\code{\link{pkgconfig}},
\code{\link{r_config}},
\code{\link{sysdeps}}
}
\concept{maketools}
