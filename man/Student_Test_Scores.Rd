\name{Student_Test_Scores}
\alias{Student_Test_Scores}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(Student_Test_Scores)}
\format{
  A data frame with 100705 observations on the following 10 variables.
  \describe{
    \item{\code{sid}}{a numeric vector}
    \item{\code{test_type}}{a factor with levels \code{ACT} \code{SAT} \code{State}}
    \item{\code{school_year}}{a numeric vector}
    \item{\code{test_date}}{a factor with levels \code{05apr2007} \code{05feb2007} \code{05mar2008} \code{06apr2008} \code{06jun2008} \code{07dec2006} \code{07dec2007} \code{07feb2006} \code{07feb2008} \code{07jan2008} \code{07jun2007} \code{07nov2007} \code{07oct2006} \code{07oct2007} \code{07sep2007} \code{15apr2004} \code{15apr2005} \code{15apr2006} \code{15apr2007} \code{15apr2008} \code{15apr2009} \code{18mar2004} \code{18mar2005} \code{18mar2006} \code{18mar2007} \code{18mar2008} \code{18mar2009} \code{23nov2005} \code{25feb2007} \code{25feb2008} \code{27apr2007} \code{27apr2008} \code{27nov2006} \code{27nov2007} \code{27sep2006} \code{27sep2007} \code{28dec2006} \code{28dec2007} \code{28may2006} \code{28may2007} \code{28oct2006} \code{28oct2007}}
    \item{\code{grade_level}}{a numeric vector}
    \item{\code{test_subject}}{a factor with levels \code{ACT Composite} \code{ELA} \code{Math} \code{Writing}}
    \item{\code{scaled_score}}{a numeric vector}
    \item{\code{performance_level}}{a numeric vector}
    \item{\code{performance_level_desc}}{a factor with levels \code{} \code{Advanced} \code{Not Proficient} \code{On the Way to Proficient} \code{Proficient}}
    \item{\code{raw_score}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Student_Test_Scores)
## maybe str(Student_Test_Scores) ; plot(Student_Test_Scores) ...
}
\keyword{datasets}
