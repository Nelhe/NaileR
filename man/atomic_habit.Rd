% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{atomic_habit}
\alias{atomic_habit}
\title{Atomic habits survey}
\format{
A data frame with 167 rows and 50 columns:
\itemize{
\item columns 1-10, do you feel able to...
\item columns 11-20, from 0 to 5 how restrictive...
\item columns 21-30, is it restrictive, yes or no...
\item columns 31-40, justify your answers
\item columns 41-50, a combination of able and restrictive
}
}
\source{
Applied mathematics department, Institut Agro Rennes-Angers
}
\usage{
atomic_habit
}
\description{
People think they need to make big changes to change
the course of their lives. But in James Clear's book,
Atomic Habits, they will discover that the smallest of changes,
coupled with a good knowledge of psychology and neuroscience,
can have a revolutionary effect on their lives and relationships.
To understand this concept of atomic habits, we interviewed 167 people
and asked them if they were able to never take their car alone again,
to buy local products...
We also asked them how restrictive they found this and why.
}
\examples{
\dontrun{
# Processing time is often longer than ten seconds
# because the function uses a large language model.

library(FactoMineR)
library(NaileR)
data(atomic_habit)

res_mfa <- MFA(atomic_habit[,1:30],
              group = c(10,10,10),
              type = c("n","s","n"),
              num.group.sup = 3,
              name.group = c("capable","restrictive", "restrictive binary"),
              graph = FALSE)

plot.MFA(res_mfa, choix = "ind", invisible = c("quali","quali.sup"),
        lab.ind = FALSE,
        title = "MFA based on being capable and restrictiveness data")

res_hcpc <- HCPC(res_mfa, nb.clust = 3, graph = FALSE)
plot.HCPC(res_hcpc, choice = "map",
         draw.tree = FALSE,
         ind.names = FALSE,
         title = "Atomic habits - typology")
summary(res_hcpc$data.clust)
}
}
\keyword{datasets}
