#' Prostate Cancer Dataset
#'
#' a prostate carcinoma dataset from a clinical trial David P. Byar and
#' Sylvan B. Green. The choice of treatment for cancer patients based on
#' covariate information: application to prostate cancer.
#' Bulletin du Cancer, 67:477–490, 1980.
#' The data was downloaded from the web \url{http://portal.uni-freiburg.de/imbi/Royston-Sauerbrei-book/index.html}
#' and modified to keep relevant variables.
#'
#' @format A data frame with 475 rows and 15 variables:
#' \describe{
#'   \item{survtime}{survival time. Response variable}
#'   \item{cens}{The status indicator, 0=alive, 1=dead}
#'   \item{rx}{treatment received, 0=control, 1=treatment}
#'   \item{bm}{existence of bone metastasis}
#'   \item{hx}{history of cardiovascular events}
#'   \item{stage}{disease stage (3 or 4)}
#'   \item{pf}{performance}
#'   \item{age}{age}
#'   \item{weight}{weight in kg.}
#'   \item{age1}{dummy variable for 65 <= age < 75}
#'   \item{age2}{dummy variable for age >= 75}
#'   \item{wt1}{dummy variable for 90 <= weight < 110}
#'   \item{wt2}{dummy variable for weight >= 110}
#'   \item{age_group}{age categorized in 3 groups}
#'   \item{weight_group}{weight categorized in 3 groups}
#' }
#' @source \url{http://portal.uni-freiburg.de/imbi/Royston-Sauerbrei-book/index.html}
"prca"

# head(prca0)
#
# # Select the variables that we use for the analysis
# prca <- prca0[,c("SURVTIME","CENS","RX","BM","HX","STAGE","PF", "AGE", "WT")]
# head(prca) # Preview dataset
# names(prca)<- c("survtime","cens","rx","bm",
#                 "hx","stage","pf","age", "wt")  # Change names of variables to lower case
#
# # Create subgroups for Age and Weight and Stage
# prca$age1 <- 1 * (prca$age > 65 & prca$age <= 75)
# prca$age2 <- 1 * (prca$age > 75)
# prca$wt1  <- 1 * (prca$wt > 90 & prca$wt <= 110)
# prca$wt2  <- 1 * (prca$wt > 110)
# # Create subgroups for Age and Weight and Stage with (-1,1) coding
# prca$agegroup <- 1 + (1 * (prca$age > 65) + 1 * (prca$age > 75))
# prca$wtgroup  <- 1 + (1 * (prca$wt > 90) + 1 * (prca$wt > 110))
# dat = prca
# dat$agegroup = factor(dat$agegroup)
# dat$wtgroup = factor(dat$wtgroup)
# range(dat$age)
# range(dat$wt)
# levels(dat$agegroup) = c("[48,65]","(65,75]","(75,89]")
# levels(dat$wtgroup)  = c("[69,90]","(90,110]","(110,152]")
# ## We need variables as factors
# dat$bm    = factor(dat$bm)
# dat$hx    = factor(dat$hx)
# dat$stage = factor(dat$stage)
# dat$pf    = factor(dat$pf)
# dat$rx    = factor(dat$rx) # Treatment
#
# # Put labels to the variables so that they appear in the plot
# names(dat)<- c("survtime",
#                "cens",
#                "rx",
#                "bm",
#                "hx",
#                "stage",
#                "pf",
#                "age",
#                "weight",
#                "age1",
#                "age2",
#                "wt1",
#                "wt2",
#                "age_group",
#                "weight_group")
# prca <- dat
# devtools::use_data(prca, overwrite = T)
