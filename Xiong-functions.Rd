\name{Xiong-functions}
\alias{Xiong-functions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Xiong-functions(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

clear_space <- function(x) {str_replace_all(x ," ","")}
find_lower_bound <- function(x) str_extract(x , "\\[[0-9]+")
find_upper_bound <- function(x) str_extract(x , "[0-9]+\\]")
clear_bracket1 <- function(x) {str_replace_all(find_lower_bound(x) , "\\[" , "")}
clear_bracket1 <- function(x) {str_replace_all(find_upper_bound(x) , "\\]" , "")}

fix_who_column <- function(x) {
library(tidyverse)
library(stringr)
health <- read_csv("C:/Users/Administrator/Desktop/wrangling hw/Xiong/xmart.csv")
    clear_space <- function(x) {str_replace_all(x ," ","")}
    find_lower_bound <- function(x) str_extract(clear_space(x) , "\\[[0-9]+")
    find_upper_bound <- function(x) str_extract(clear_space(x) , "[0-9]+\\]")
    find_value <- function(x) str_extract(clear_space(x) , "[0-9]+")
    clear_bracket1 <- function(x) {str_replace_all(x , "\\[" , "")}
    clear_bracket2 <- function(x) {str_replace_all(x , "\\]" , "")}
        health$lower <- clear_bracket1(find_lower_bound(x))
        health$upper <- clear_bracket2(find_upper_bound(x))
        health$Mvalue <- find_value(x)
    health
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
