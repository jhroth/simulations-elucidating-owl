# Trying to replicate the simulation study from Zhao et al. (2012), using implementation of OWL from "DynTxRegime" package
rm(list=ls())
library(DevTreatRules)
source("Functions/Motivation_OWL.R")
Expit <- function(x) exp(x) / (1 + exp(x))

set.seed(123)
one.simple.simulation.propensity_0.67 <- SimulateSimpleData(n=500, p.informative=1, p.noisy=5, prop.on.treatment=0.67)
scenario.1.df <- data.frame("X"=one.simple.simulation.propensity_0.67$X.scenario.logistic.1, response=one.simple.simulation.propensity_0.67$Y.scenario.logistic.1, treatment=one.simple.simulation.propensity_0.67$T)

png("Plots/pi_0.67_first.png")
PlotOWLSummaries(X=one.simple.simulation.propensity_0.67$X.informative, treatment=one.simple.simulation.propensity_0.67$T, response=one.simple.simulation.propensity_0.67$Y.scenario.logistic.1,
                   true.response.prob=one.simple.simulation.propensity_0.67$prob.scenario.logistic.1, plot.true.response.prob=TRUE,
                   population.treatment.prob.among.responders=one.simple.simulation.propensity_0.67$population.treatment.prob.among.responders.scenario.logistic.1,
                   plot.population.treatment.prob.among.responders=FALSE,
                   propensity.score=one.simple.simulation.propensity_0.67$propensity.score, plot.propensity.score=FALSE)
dev.off()

png("Plots/pi_0.67_second.png")
PlotOWLSummaries(X=one.simple.simulation.propensity_0.67$X.informative, treatment=one.simple.simulation.propensity_0.67$T, response=one.simple.simulation.propensity_0.67$Y.scenario.logistic.1,
                   true.response.prob=one.simple.simulation.propensity_0.67$prob.scenario.logistic.1, plot.true.response.prob=TRUE,
                   population.treatment.prob.among.responders=one.simple.simulation.propensity_0.67$population.treatment.prob.among.responders.scenario.logistic.1,
                   plot.population.treatment.prob.among.responders=TRUE,
                   propensity.score=one.simple.simulation.propensity_0.67$propensity.score, plot.propensity.score=FALSE)
dev.off()

png("Plots/pi_0.67_third.png")
PlotOWLSummaries(X=one.simple.simulation.propensity_0.67$X.informative, treatment=one.simple.simulation.propensity_0.67$T, response=one.simple.simulation.propensity_0.67$Y.scenario.logistic.1,
                   true.response.prob=one.simple.simulation.propensity_0.67$prob.scenario.logistic.1, plot.true.response.prob=TRUE,
                   population.treatment.prob.among.responders=one.simple.simulation.propensity_0.67$population.treatment.prob.among.responders.scenario.logistic.1,
                   plot.population.treatment.prob.among.responders=TRUE,
                   propensity.score=one.simple.simulation.propensity_0.67$propensity.score, plot.propensity.score=TRUE)
dev.off()

png("Plots/pi_0.67_fourth.png")
PlotOWLSummaries(X=one.simple.simulation.propensity_0.67$X.informative, treatment=one.simple.simulation.propensity_0.67$T, response=one.simple.simulation.propensity_0.67$Y.scenario.logistic.1,
                   true.response.prob=one.simple.simulation.propensity_0.67$prob.scenario.logistic.1, plot.true.response.prob=TRUE,
                   population.treatment.prob.among.responders=one.simple.simulation.propensity_0.67$population.treatment.prob.among.responders.scenario.logistic.1,
                   plot.population.treatment.prob.among.responders=TRUE,
                   propensity.score=one.simple.simulation.propensity_0.67$propensity.score, plot.propensity.score=TRUE)
abline(v=-0.9, col="black", lwd=2.5, lty="dashed")
dev.off()


## make common legend
png("Plots/motivation_legend.png", width=480, height=480)
plot(0, 0, type = "n", ann = F, axes = F)
legend("center", cex=2.5, legend=c("P(Y=1 | T=1, X)", "P(Y=1 | T=0, X)", "P(T=1 | Y=1, X)", "P(T=1)"),
                      col=c(adjustcolor(col="blue", alpha=0.8), adjustcolor(col="orange", alpha=0.8), "black", "forestgreen"),
                      lty=c("dashed", "dashed", "solid", "dotted"), lwd=rep(5, 4))
dev.off()

set.seed(123)
example.n <- 5e+05
biomarker <- seq(from=0, to=1, length.out=example.n)
treatment <- rbinom(n=example.n, size=1, prob=0.5)
# non-qualitative interaction
survival.time.non.crossing <- rep(NA, example.n)
survival.time.non.crossing[treatment == 0] <- 8.5 + 1.5 * biomarker[treatment == 0]
survival.time.non.crossing[treatment == 1] <- 7 + 0.8 * biomarker[treatment == 1] + 2 * biomarker[treatment == 1]^2
# qualitative interaction
survival.time.qualitative <- rep(NA, example.n)
survival.time.qualitative[treatment == 0] <- 8.5 + 1.5 * biomarker[treatment == 0]
survival.time.qualitative[treatment == 1] <- 7 + 1 * biomarker[treatment == 1] + 4 * biomarker[treatment == 1]^2

# make plots
min.y <- min(c(survival.time.qualitative, survival.time.non.crossing))
max.y <- max(c(survival.time.qualitative, survival.time.non.crossing))
png("Plots/hypothetical_qualitative.png")
par(mar=c(6, 5, 4, 2))
plot(sort(biomarker[treatment==1]) + 1, sort(survival.time.qualitative[treatment==1]), type="l", lwd=5, lty="solid", col="blue",
       xlab="X", cex.lab=2.5, cex.axis=2, ylim=c(5, 15), yaxt="n", ylab="P(Y | X, T)")
points(sort(biomarker[treatment==0]) + 1, sort(survival.time.qualitative[treatment==0]), type="l",lwd=5, lty="dashed", col="DarkOrange", cex.lab=2.5, cex.axis=2)
axis(2, at=seq(from=5, to=15, by=2), labels=as.character(seq(from=0, to=1, by=0.2)), cex.lab=2.5, cex.axis=2)
dev.off()

png("Plots/hypothetical_noncrossing.png")
par(mar=c(6, 5, 4, 2))
plot(sort(biomarker[treatment==1]) + 1, sort(survival.time.non.crossing[treatment==1]), type="l", lwd=5, lty="solid", col="blue",
       xlab="X", ylim=c(5, 15), cex.lab=2.5, cex.axis=2, yaxt="n", ylab="P(Y | X, T)")
points(sort(biomarker[treatment==0]) + 1, sort(survival.time.non.crossing[treatment==0]), type="l",lwd=5, lty="dashed", col="DarkOrange", cex.lab=2.5, cex.axis=2)
axis(2, at=seq(from=5, to=15, by=2), labels=as.character(seq(from=0, to=1, by=0.2)), cex.lab=2.5, cex.axis=2)
dev.off()
