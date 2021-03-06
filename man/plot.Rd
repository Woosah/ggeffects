% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot}
\alias{plot}
\alias{plot.ggeffects}
\title{Plot ggeffects-objects}
\usage{
\method{plot}{ggeffects}(x, ci = TRUE, facets, rawdata = FALSE,
  colors = "Set1", alpha = 0.15, dodge = 0.1, use.theme = TRUE,
  dot.alpha = 0.5, jitter = TRUE, ...)
}
\arguments{
\item{x}{An object of class \code{ggeffects}, as returned by the functions
from this package.}

\item{ci}{Logical, if \code{TRUE}, confidence bands (for continuous variables
at x-axis) resp. error bars (for factors at x-axis) are plotted.
For \code{ggeffects}-objects from \code{ggpredict()} with argument
\code{full.data = TRUE}, \code{ci} is automatically set to \code{FALSE}.}

\item{facets}{Logical, defaults to \code{TRUE}, if \code{x} has a column named
\code{facet}, and defaults to \code{FALSE}, if \code{x} has no such
column. Set \code{facets = TRUE} to wrap the plot into facets even
for grouping variables (see 'Examples').}

\item{rawdata}{Logical, if \code{TRUE}, a layer with raw data from response by
predictor on the x-axis, plotted as point-geoms, is added to the plot.}

\item{colors}{Character vector with color values in hex-format, valid
color value names (see \code{demo("colors")} or a name of a
\href{http://colorbrewer2.org}{color brewer} palette.
Following options are valid for \code{colors}:
\itemize{
  \item If not specified, the color brewer palette "Set1" will be used.
  \item If \code{"gs"}, a greyscale will be used.
  \item If \code{"bw"}, the plot is black/white and uses different line types to distinguish groups.
  \item If \code{colors} is any valid color brewer palette name, the related palette will be used. Use \code{\link[RColorBrewer]{display.brewer.all}} to view all available palette names.
  \item Else specify own color values or names as vector (e.g. \code{colors = c("#f00000", "#00ff00")}).
}}

\item{alpha}{Alpha value for the confidence bands.}

\item{dodge}{Value for offsetting or shifting error bars, to avoid overlapping.
Only applies, if a factor is plotted at the x-axis; in such cases,
the confidence bands are replaced by error bars.}

\item{use.theme}{Logical, if \code{TRUE}, a slightly tweaked version of ggplot's
minimal-theme is applied to the plot. If \code{FALSE}, no theme-modifications
are applied.}

\item{dot.alpha}{Alpha value for data points, when \code{rawdata = TRUE}.}

\item{jitter}{Logical, if \code{TRUE} and \code{rawdata = TRUE}, adds a small
amount of random variation to the location of data points dots, to
avoid overplotting. Hence the points don't reflect exact
values in the data. For binary outcomes, raw data is never jittered
to avoid that data points exceed the axis limits.}

\item{...}{Currently not used.}
}
\value{
A ggplot2-object.
}
\description{
A generic plot-method for \code{ggeffects}-objects.
}
\details{
\code{ggpredict()} with argument \code{full.data = FALSE} computes
         marginal effects at the mean, where covariates are held constant. In
         this case, the slope between groups does not vary and the standard
         errors and confidence intervals have the same "trend" as the predicted
         values. Hence, plotting confidence bands or error bars is possible.
         However, \code{ggpredict()} with argument \code{full.data = TRUE},
         covariates and standard errors vary between groups, so plotting
         confidence bands and error bars would follow a "winding" shape,
         while the predicted values are smoothened by \code{\link[ggplot2]{geom_smooth}}.
         Predicted values and confidence bands or error bars would no
         longer match, thus, \code{ci} is automatically set to \code{FALSE}
         in such cases. You still may want to plot objects returned by
         \code{ggpredict()} with argument \code{full.data = TRUE} to additionally
         plot the raw data points, which is automatically done.
         \cr \cr
         For \code{ggaverage()}, which computes average marginal effects,
         the same problem with standard errors and confidence bands would
         apply. However, the standard errors for the average marginal effects
         are taken from the marginal effects at the mean, and the predicted
         values from the average marginal effects are used to compute
         another regression on these values, to get the "smoothened" values
         that are used to compute standard errors and confidence intervals
         that match the predicted values of the average marginal effects
         (maybe, at this point, it is helpful to inspect the code to better
         understand what is happening...).
         \cr \cr
         For proportional odds logistic regression (see \code{\link[MASS]{polr}}),
         plots are automatically facetted by \code{response.level}, which indicates
         the grouping of predictions based on the level of the model's response.
}
\examples{
library(sjmisc)
data(efc)
efc$c172code <- to_label(efc$c172code)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

dat <- ggpredict(fit, terms = "c12hour")
plot(dat)

dat <- ggpredict(fit, terms = "c12hour", full.data = TRUE)
plot(dat)

dat <- ggaverage(fit, terms = "neg_c_7")
plot(dat)

# facet by group
dat <- ggpredict(fit, terms = c("c12hour", "c172code"))
plot(dat, facet = TRUE)

# don't use facets, b/w figure, w/o confidence bands
dat <- ggaverage(fit, terms = c("c12hour", "c172code"))
plot(dat, colors = "bw", ci = FALSE)

# factor at x axis, plot exact data points and error bars
dat <- ggpredict(fit, terms = c("c172code", "c161sex"))
plot(dat)

# for three variables, automatic facetting
dat <- ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
plot(dat)


}
