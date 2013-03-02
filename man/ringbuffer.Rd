\name{ringbuffer}
\alias{ringbuffer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ring buffers for R
}
\description{
A circular buffer, cyclic buffer or ring buffer is a data
structure that uses a single, fixed-size buffer as if it were connected
end-to-end. This structure lends itself easily to buffering data streams.

See http://en.wikipedia.org/wiki/Ring_buffer for more information.

This package implements a ring buffer as an R array.
}
\usage{
ringbuffer(size = 100, warn = FALSE)
}
\arguments{
  \item{size}{
     An integer.  Size of ring buffer.  Defaults to 100.
  }
  \item{warn}{
     A boolean.  FALSE by default.  If set to TRUE, enables overflow/underflow warnings.
  }
}
\value{

Returns a new \code{ringbuffer} environment.  It has the following functions
defined:

\item{write(items)}{writes items to buffer}
\item{read(n)}{reads n items from buffer}
\item{writable()}{returns number of items writable to buffer without overflowing it}
\item{readable()}{returns number of items readable from buffer without underflowing it}
\item{peek()}{returns items in buffer in ascending order}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://en.wikipedia.org/wiki/Ring_buffer
}
\author{
Allen Day, PhD
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

buf = ringbuffer(10)
buf$readable() == FALSE
buf$writable() == 10
buf$write(1:3)
buf$readable() == 3
buf$writable() == 7
buf$peek()
buf$read() == 1
buf$read() == 2
buf$read() == 3

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
