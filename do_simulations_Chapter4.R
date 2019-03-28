# Jeremy Roth
rm(list=ls())
library(DevTreatRules)
source("Functions/Simulations_Chapter4.R")

# set general parameters that I'll use for every simulation scenraio
vec.approaches <-  c("OWL.framework", "direct.interactions", "split.regression", "split.regression.naive")
results.dir <- "Results/"
plots.dir <- "Plots/"
n.test <- 10000

# SCENARIO: DEFAULT
set.seed(123)
checking.default <- FormatSimulation(n=500, scenario="default",
                                          shift.for.continuous="min",
                                          unif.min=0, unif.max=2, noise.mu=0, noise.sigma=1,
                                          epsilon.mu=0, epsilon.sigma=1)

png(paste0(plots.dir, "default_X1_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
plot(checking.default$one.design$X[checking.default$one.design$T == 0 & checking.default$one.design$L == 0, 1],
      checking.default$one.response$S[checking.default$one.design$T == 0 & checking.default$one.design$L == 0],
      pch=1, col="blue", ylim=c(-3, 3), xlab=expression(X[1]), ylab="S",
      cex.axis=1.5, cex.lab=2)
points(checking.default$one.design$X[checking.default$one.design$T == 0 & checking.default$one.design$L == 1, 1],
       checking.default$one.response$S[checking.default$one.design$T == 0 & checking.default$one.design$L == 1],
       pch=2, col="blue")
points(checking.default$one.design$X[checking.default$one.design$T == 1 & checking.default$one.design$L == 0, 1],
       checking.default$one.response$S[checking.default$one.design$T == 1 & checking.default$one.design$L == 0],
       pch=1, col="orange")
points(checking.default$one.design$X[checking.default$one.design$T == 1 & checking.default$one.design$L == 1, 1],
       checking.default$one.response$S[checking.default$one.design$T == 1 & checking.default$one.design$L == 1],
       pch=2, col="orange")
dev.off()

png(paste0(plots.dir, "default_legend_", Sys.Date(), ".png"))
plot(0, 0, type = "n", ann = F, axes = F)
legend("center", cex=3, legend=c("T=0, L=0", "T=0, L=1", "T=1, L=0", "T=1, L=1"),
          col=c("blue", "orange", "blue", "orange"), pch=c(1, 1, 2, 2), pt.cex=2, pt.lwd=rep(5, 4))
dev.off()

# SCENARIO: DEFAULT
n.reps.default <- 50
n.training.vec.default <- c(50, 100, 200, 500, 1000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.default))
colnames(mat.ABR) <- n.training.vec.default

for (i in 1:length(n.training.vec.default)) {
    one.n.training <- n.training.vec.default[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.default, scenario="default", n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "default_means_n_reps_", n.reps.default, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "default_sds_n_reps_", n.reps.default, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "default_mean_ABR_n_reps_", n.reps.default, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "default_legend_ABR_n_reps_", n.reps.default, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "default_mat_mean_ABR_n_reps_", n.reps.default, "_", Sys.Date(), ".csv"))

# SCENARIO: main effect added
n.reps.main.effect.added <- 50
n.training.vec.main.effect.added <- c(50, 100, 500, 1000, 2000, 5000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.main.effect.added))
colnames(mat.ABR) <- n.training.vec.main.effect.added

set.seed(123)
checking.main.effect.added <- FormatSimulation(n=500, scenario="main.effect.added",
                                          shift.for.continuous="min",
                                          unif.min=0, unif.max=2, noise.mu=0, noise.sigma=1,
                                          epsilon.mu=0, epsilon.sigma=1)


png(paste0(plots.dir, "main.effect.added_X1_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
plot(checking.main.effect.added$one.design$X[checking.main.effect.added$one.design$T == 0 & checking.main.effect.added$one.design$L == 0, 1],
      checking.main.effect.added$one.response$S[checking.main.effect.added$one.design$T == 0 & checking.main.effect.added$one.design$L == 0],
       pch=1, col="blue", ylim=c(-3, 3), xlab=expression(X[1]), ylab="S",
       cex.axis=1.5, cex.lab=2)
points(checking.main.effect.added$one.design$X[checking.main.effect.added$one.design$T == 0 & checking.main.effect.added$one.design$L == 1, 1],
       checking.main.effect.added$one.response$S[checking.main.effect.added$one.design$T == 0 & checking.main.effect.added$one.design$L == 1],
       pch=2, col="blue")
points(checking.main.effect.added$one.design$X[checking.main.effect.added$one.design$T == 1 & checking.main.effect.added$one.design$L == 0, 1],
       checking.main.effect.added$one.response$S[checking.main.effect.added$one.design$T == 1 & checking.main.effect.added$one.design$L == 0],
       pch=1, col="orange")
points(checking.main.effect.added$one.design$X[checking.main.effect.added$one.design$T == 1 & checking.main.effect.added$one.design$L == 1, 1],
       checking.main.effect.added$one.response$S[checking.main.effect.added$one.design$T == 1 & checking.main.effect.added$one.design$L == 1],
       pch=2, col="orange")
dev.off()

for (i in 1:length(n.training.vec.main.effect.added)) {
    one.n.training <- n.training.vec.main.effect.added[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.main.effect.added, scenario="main.effect.added", n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "main.effect.added_means_n_reps_", n.reps.main.effect.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "main.effect.added_sds_n_reps_", n.reps.main.effect.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])
png(file=paste0(plots.dir, "main.effect.added_mean_ABR_n_reps_", n.reps.main.effect.added, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "main.effect.added_legend_ABR_n_reps_", n.reps.main.effect.added, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "default_mat_mean_ABR_n_reps_", n.reps.main.effect.added, "_", Sys.Date(), ".csv"))

# SCENARIO: quadratic interaction added
n.reps.quadratic.interaction.added <- 50
n.training.vec.quadratic.interaction.added <- c(50, 100, 500, 1000, 2000, 5000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.quadratic.interaction.added))
colnames(mat.ABR) <- n.training.vec.quadratic.interaction.added

set.seed(123)
checking.quadratic.interaction.added <- FormatSimulation(n=500, scenario="quadratic.interaction.added",
                                          shift.for.continuous="min",
                                          unif.min=0, unif.max=2, noise.mu=0, noise.sigma=1,
                                          epsilon.mu=0, epsilon.sigma=1)
png(paste0(plots.dir, "quadratic.interaction.added_X1_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
plot(checking.quadratic.interaction.added$one.design$X[checking.quadratic.interaction.added$one.design$T == 0 & checking.quadratic.interaction.added$one.design$L == 0, 1],
      checking.quadratic.interaction.added$one.response$S[checking.quadratic.interaction.added$one.design$T == 0 & checking.quadratic.interaction.added$one.design$L == 0],
       pch=1, col="blue", ylim=c(-3, 3), xlab=expression(X[1]), ylab="S",
      cex.axis=1.5, cex.lab=2)
points(checking.quadratic.interaction.added$one.design$X[checking.quadratic.interaction.added$one.design$T == 0 & checking.quadratic.interaction.added$one.design$L == 1, 1],
       checking.quadratic.interaction.added$one.response$S[checking.quadratic.interaction.added$one.design$T == 0 & checking.quadratic.interaction.added$one.design$L == 1],
       pch=2, col="blue")
points(checking.quadratic.interaction.added$one.design$X[checking.quadratic.interaction.added$one.design$T == 1 & checking.quadratic.interaction.added$one.design$L == 0, 1],
       checking.quadratic.interaction.added$one.response$S[checking.quadratic.interaction.added$one.design$T == 1 & checking.quadratic.interaction.added$one.design$L == 0],
       pch=1, col="orange")
points(checking.quadratic.interaction.added$one.design$X[checking.quadratic.interaction.added$one.design$T == 1 & checking.quadratic.interaction.added$one.design$L == 1, 1],
       checking.quadratic.interaction.added$one.response$S[checking.quadratic.interaction.added$one.design$T == 1 & checking.quadratic.interaction.added$one.design$L == 1],
       pch=2, col="orange")
dev.off()

for (i in 1:length(n.training.vec.quadratic.interaction.added)) {
    one.n.training <- n.training.vec.quadratic.interaction.added[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.quadratic.interaction.added, scenario="quadratic.interaction.added", n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "quadratic.interaction.added_means_n_reps_", n.reps.quadratic.interaction.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "quadratic.interaction.added_sds_n_reps_", n.reps.quadratic.interaction.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "quadratic.interaction.added_mean_ABR_n_reps_", n.reps.quadratic.interaction.added, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "quadratic.interaction.added_legend_ABR_n_reps_", n.reps.quadratic.interaction.added, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "default_mat_mean_ABR_n_reps_", n.reps.quadratic.interaction.added, "_", Sys.Date(), ".csv"))

# SCENARIO: cubic interaction added
n.reps.piecewise.cubic.interaction.added <- 50
n.training.vec.piecewise.cubic.interaction.added <- c(50, 100, 500, 1000, 2000, 5000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.piecewise.cubic.interaction.added))
colnames(mat.ABR) <- n.training.vec.piecewise.cubic.interaction.added

set.seed(123)
checking.piecewise.cubic.interaction.added <- FormatSimulation(n=500, scenario="piecewise.cubic.interaction.added",
                                          shift.for.continuous="min",
                                          unif.min=0, unif.max=2, noise.mu=0, noise.sigma=1,
                                          epsilon.mu=0, epsilon.sigma=1)
png(paste0(plots.dir, "cubic.interaction.added_X1_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
plot(checking.piecewise.cubic.interaction.added$one.design$X[checking.piecewise.cubic.interaction.added$one.design$T == 0 & checking.piecewise.cubic.interaction.added$one.design$L == 0, 1],
      checking.piecewise.cubic.interaction.added$one.response$S[checking.piecewise.cubic.interaction.added$one.design$T == 0 & checking.piecewise.cubic.interaction.added$one.design$L == 0],
       pch=1, col="blue", ylim=c(-3, 3), xlab=expression(X[1]), ylab="S",
     cex.axis=1.5, cex.lab=2)
points(checking.piecewise.cubic.interaction.added$one.design$X[checking.piecewise.cubic.interaction.added$one.design$T == 0 & checking.piecewise.cubic.interaction.added$one.design$L == 1, 1],
       checking.piecewise.cubic.interaction.added$one.response$S[checking.piecewise.cubic.interaction.added$one.design$T == 0 & checking.piecewise.cubic.interaction.added$one.design$L == 1],
       pch=2, col="blue")
points(checking.piecewise.cubic.interaction.added$one.design$X[checking.piecewise.cubic.interaction.added$one.design$T == 1 & checking.piecewise.cubic.interaction.added$one.design$L == 0, 1],
       checking.piecewise.cubic.interaction.added$one.response$S[checking.piecewise.cubic.interaction.added$one.design$T == 1 & checking.piecewise.cubic.interaction.added$one.design$L == 0],
       pch=1, col="orange")
points(checking.piecewise.cubic.interaction.added$one.design$X[checking.piecewise.cubic.interaction.added$one.design$T == 1 & checking.piecewise.cubic.interaction.added$one.design$L == 1, 1],
       checking.piecewise.cubic.interaction.added$one.response$S[checking.piecewise.cubic.interaction.added$one.design$T == 1 & checking.piecewise.cubic.interaction.added$one.design$L == 1],
       pch=2, col="orange")
dev.off()

for (i in 1:length(n.training.vec.piecewise.cubic.interaction.added)) {
    one.n.training <- n.training.vec.piecewise.cubic.interaction.added[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.piecewise.cubic.interaction.added, scenario="piecewise.cubic.interaction.added", n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "piecewise.cubic.interaction.added_means_n_reps_", n.reps.piecewise.cubic.interaction.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "piecewise.cubic.interaction.added_sds_n_reps_", n.reps.piecewise.cubic.interaction.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "piecewise.cubic.interaction.added_mean_ABR_n_reps_", n.reps.piecewise.cubic.interaction.added, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "piecewise.cubic.interaction.added_legend_ABR_n_reps_", n.reps.piecewise.cubic.interaction.added, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "default_mat_mean_ABR_n_reps_", n.reps.piecewise.cubic.interaction.added, "_", Sys.Date(), ".csv"))

# SCENARIO: noise added
n.reps.noise.added <- 50
n.training.vec.noise.added <- c(100, 500, 1000, 2000, 5000, 10000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.noise.added))
colnames(mat.ABR) <- n.training.vec.noise.added

set.seed(123)
checking.noise.added <- FormatSimulation(n=500, scenario="noise.added",
                                          shift.for.continuous="min",
                                          unif.min=0, unif.max=2, noise.mu=0, noise.sigma=1,
                                          epsilon.mu=0, epsilon.sigma=1)
png(paste0(plots.dir, "noise.added_X1_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
plot(checking.noise.added$one.design$X[checking.noise.added$one.design$T == 0 & checking.noise.added$one.design$L == 0, 1],
      checking.noise.added$one.response$S[checking.noise.added$one.design$T == 0 & checking.noise.added$one.design$L == 0],
       pch=1, col="blue", ylim=c(-3, 3), xlab=expression(X[1]), ylab="S",
      cex.axis=1.5, cex.lab=2)
points(checking.noise.added$one.design$X[checking.noise.added$one.design$T == 0 & checking.noise.added$one.design$L == 1, 1],
       checking.noise.added$one.response$S[checking.noise.added$one.design$T == 0 & checking.noise.added$one.design$L == 1],
       pch=2, col="blue")
points(checking.noise.added$one.design$X[checking.noise.added$one.design$T == 1 & checking.noise.added$one.design$L == 0, 1],
       checking.noise.added$one.response$S[checking.noise.added$one.design$T == 1 & checking.noise.added$one.design$L == 0],
       pch=1, col="orange")
points(checking.noise.added$one.design$X[checking.noise.added$one.design$T == 1 & checking.noise.added$one.design$L == 1, 1],
       checking.noise.added$one.response$S[checking.noise.added$one.design$T == 1 & checking.noise.added$one.design$L == 1],
       pch=2, col="orange")
dev.off()

for (i in 1:length(n.training.vec.noise.added)) {
    one.n.training <- n.training.vec.noise.added[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.noise.added, scenario="noise.added", n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "noise.added_means_n_reps_", n.reps.noise.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "noise.added_sds_n_reps_", n.reps.noise.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "noise.added_mean_ABR_n_reps_", n.reps.noise.added, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "noise.added_legend_ABR_n_reps_", n.reps.noise.added, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "noise.added_mat_mean_ABR_n_reps_", n.reps.noise.added, "_", Sys.Date(), ".csv"))

# SCENARIO: high.dimensional.noise.added
n.reps.high.dimensional.noise.added <- 50
n.training.vec.high.dimensional.noise.added <- c(1000, 2000, 5000, 10000) #c(500, 1000, 2000, 5000, 10000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.high.dimensional.noise.added))
colnames(mat.ABR) <- n.training.vec.high.dimensional.noise.added

set.seed(123)
checking.high.dimensional.noise.added <- FormatSimulation(n=500, scenario="high.dimensional.noise.added",
                                          shift.for.continuous="min",
                                          unif.min=0, unif.max=2, noise.mu=0, noise.sigma=1,
                                          epsilon.mu=0, epsilon.sigma=1)
png(paste0(plots.dir, "high.dimensional.noise.added_X1_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
plot(checking.high.dimensional.noise.added$one.design$X[checking.high.dimensional.noise.added$one.design$T == 0 & checking.high.dimensional.noise.added$one.design$L == 0, 1],
      checking.high.dimensional.noise.added$one.response$S[checking.high.dimensional.noise.added$one.design$T == 0 & checking.high.dimensional.noise.added$one.design$L == 0],
       pch=1, col="blue", ylim=c(-3, 3), xlab=expression(X[1]), ylab="S",
      cex.axis=1.5, cex.lab=2)
points(checking.high.dimensional.noise.added$one.design$X[checking.high.dimensional.noise.added$one.design$T == 0 & checking.high.dimensional.noise.added$one.design$L == 1, 1],
       checking.high.dimensional.noise.added$one.response$S[checking.high.dimensional.noise.added$one.design$T == 0 & checking.high.dimensional.noise.added$one.design$L == 1],
       pch=2, col="blue")
points(checking.high.dimensional.noise.added$one.design$X[checking.high.dimensional.noise.added$one.design$T == 1 & checking.high.dimensional.noise.added$one.design$L == 0, 1],
       checking.high.dimensional.noise.added$one.response$S[checking.high.dimensional.noise.added$one.design$T == 1 & checking.high.dimensional.noise.added$one.design$L == 0],
       pch=1, col="orange")
points(checking.high.dimensional.noise.added$one.design$X[checking.high.dimensional.noise.added$one.design$T == 1 & checking.high.dimensional.noise.added$one.design$L == 1, 1],
       checking.high.dimensional.noise.added$one.response$S[checking.high.dimensional.noise.added$one.design$T == 1 & checking.high.dimensional.noise.added$one.design$L == 1],
       pch=2, col="orange")
dev.off()

for (i in 1:length(n.training.vec.high.dimensional.noise.added)) {
    one.n.training <- n.training.vec.high.dimensional.noise.added[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.high.dimensional.noise.added, scenario="high.dimensional.noise.added", n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "high.dimensional.noise.added_means_n_reps_", n.reps.high.dimensional.noise.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "high.dimensional.noise.added_sds_n_reps_", n.reps.high.dimensional.noise.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "high.dimensional.noise.added_mean_ABR_n_reps_", n.reps.high.dimensional.noise.added, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "high.dimensional.noise.added_legend_ABR_n_reps_", n.reps.high.dimensional.noise.added, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "high.dimensional.noise.added_mat_mean_ABR_n_reps_", n.reps.high.dimensional.noise.added, "_", Sys.Date(), ".csv"))


# SCENARIO: high.dimensional.noise.and.prognostic.added
n.reps.high.dimensional.noise.and.prognostic.added <- 50
n.training.vec.high.dimensional.noise.and.prognostic.added <- c(1000, 2000, 5000, 10000) #c(500, 1000, 2000, 5000, 10000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.high.dimensional.noise.and.prognostic.added))
colnames(mat.ABR) <- n.training.vec.high.dimensional.noise.and.prognostic.added

set.seed(123)
checking.high.dimensional.noise.and.prognostic.added <- FormatSimulation(n=500, scenario="high.dimensional.noise.and.prognostic.added",
                                          shift.for.continuous="min",
                                          unif.min=0, unif.max=2, noise.mu=0, noise.sigma=1,
                                          epsilon.mu=0, epsilon.sigma=1)
png(paste0(plots.dir, "high.dimensional.noise.and.prognostic.added_X1_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
plot(checking.high.dimensional.noise.and.prognostic.added$one.design$X[checking.high.dimensional.noise.and.prognostic.added$one.design$T == 0 & checking.high.dimensional.noise.and.prognostic.added$one.design$L == 0, 1],
      checking.high.dimensional.noise.and.prognostic.added$one.response$S[checking.high.dimensional.noise.and.prognostic.added$one.design$T == 0 & checking.high.dimensional.noise.and.prognostic.added$one.design$L == 0],
       pch=1, col="blue", ylim=c(-3, 3), xlab=expression(X[1]), ylab="S",
      cex.axis=1.5, cex.lab=2)
points(checking.high.dimensional.noise.and.prognostic.added$one.design$X[checking.high.dimensional.noise.and.prognostic.added$one.design$T == 0 & checking.high.dimensional.noise.and.prognostic.added$one.design$L == 1, 1],
       checking.high.dimensional.noise.and.prognostic.added$one.response$S[checking.high.dimensional.noise.and.prognostic.added$one.design$T == 0 & checking.high.dimensional.noise.and.prognostic.added$one.design$L == 1],
       pch=2, col="blue")
points(checking.high.dimensional.noise.and.prognostic.added$one.design$X[checking.high.dimensional.noise.and.prognostic.added$one.design$T == 1 & checking.high.dimensional.noise.and.prognostic.added$one.design$L == 0, 1],
       checking.high.dimensional.noise.and.prognostic.added$one.response$S[checking.high.dimensional.noise.and.prognostic.added$one.design$T == 1 & checking.high.dimensional.noise.and.prognostic.added$one.design$L == 0],
       pch=1, col="orange")
points(checking.high.dimensional.noise.and.prognostic.added$one.design$X[checking.high.dimensional.noise.and.prognostic.added$one.design$T == 1 & checking.high.dimensional.noise.and.prognostic.added$one.design$L == 1, 1],
       checking.high.dimensional.noise.and.prognostic.added$one.response$S[checking.high.dimensional.noise.and.prognostic.added$one.design$T == 1 & checking.high.dimensional.noise.and.prognostic.added$one.design$L == 1],
       pch=2, col="orange")
dev.off()

for (i in 1:length(n.training.vec.high.dimensional.noise.and.prognostic.added)) {
    one.n.training <- n.training.vec.high.dimensional.noise.and.prognostic.added[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.high.dimensional.noise.and.prognostic.added, scenario="high.dimensional.noise.and.prognostic.added", n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "high.dimensional.noise.and.prognostic.added_means_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "high.dimensional.noise.and.prognostic.added_sds_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "high.dimensional.noise.and.prognostic.added_mean_ABR_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "high.dimensional.noise.and.prognostic.added_legend_ABR_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "high.dimensional.noise.and.prognostic.added_mat_mean_ABR_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added, "_", Sys.Date(), ".csv"))

# SCENARIO: DEFAULT.CONTINUOUS
## make plots showing x1->f(x1), x2->f(x2), x3->f(x3) for the piecewise constant scenario
n.reps.default.continuous <- 50
vec.shift.default.continuous <- c("50") #c(0, 10, 50)
n.training.vec.default.continuous <- c(50, 100, 200, 500, 1000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.default.continuous))
colnames(mat.ABR) <- n.training.vec.default.continuous

for (s in 1:length(vec.shift.default.continuous)) {
    my.shift.continuous <- vec.shift.default.continuous[s]
for (i in 1:length(n.training.vec.default.continuous)) {
    one.n.training <- n.training.vec.default.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.default.continuous, scenario="default.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE,
                                                                  OWL.framework.shift.by.min=TRUE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "default.continuous_mean_ABR_n_reps_", n.reps.default.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "default.continuous_legend_ABR_n_reps_", n.reps.default.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "default.continuous_mat_mean_ABR_n_reps_", n.reps.default.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: MAIN.EFFECT.ADDED.CONTINUOUS
n.reps.main.effect.added.continuous <- 50
vec.shift.main.effect.added.continuous <-  c("50") #c(0, 10, 50)
n.training.vec.main.effect.added.continuous <- c(50, 100, 200, 500, 1000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.main.effect.added.continuous))
colnames(mat.ABR) <- n.training.vec.main.effect.added.continuous

for (s in 1:length(vec.shift.main.effect.added.continuous)) {
    my.shift.continuous <- vec.shift.main.effect.added.continuous[s]
for (i in 1:length(n.training.vec.main.effect.added.continuous)) {
    one.n.training <- n.training.vec.main.effect.added.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.main.effect.added.continuous, scenario="main.effect.added.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "main.effect.added.continuous_means_n_reps_", n.reps.main.effect.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "main.effect.added.continuous_sds_n_reps_", n.reps.main.effect.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "main.effect.added.continuous_mean_ABR_n_reps_", n.reps.main.effect.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "main.effect.added.continuous_legend_ABR_n_reps_", n.reps.main.effect.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "main.effect.added.continuous_mat_mean_ABR_n_reps_", n.reps.main.effect.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: QUADRATIC.INTERACTION.ADDED.CONTINUOUS
n.reps.quadratic.interaction.added.continuous <- 50
vec.shift.quadratic.interaction.added.continuous <- c("50") #c(0, 10, 50)
n.training.vec.quadratic.interaction.added.continuous <- c(50, 100, 200, 500, 1000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.quadratic.interaction.added.continuous))
colnames(mat.ABR) <- n.training.vec.quadratic.interaction.added.continuous

for (s in 1:length(vec.shift.quadratic.interaction.added.continuous)) {
    my.shift.continuous <- vec.shift.quadratic.interaction.added.continuous[s]
for (i in 1:length(n.training.vec.quadratic.interaction.added.continuous)) {
    one.n.training <- n.training.vec.quadratic.interaction.added.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.quadratic.interaction.added.continuous, scenario="quadratic.interaction.added.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "quadratic.interaction.added.continuous_means_n_reps_", n.reps.quadratic.interaction.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "quadratic.interaction.added.continuous_sds_n_reps_", n.reps.quadratic.interaction.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])
png(file=paste0(plots.dir, "quadratic.interaction.added.continuous_mean_ABR_n_reps_", n.reps.quadratic.interaction.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "quadratic.interaction.added.continuous_legend_ABR_n_reps_", n.reps.quadratic.interaction.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "quadratic.interaction.added.continuous_mat_mean_ABR_n_reps_", n.reps.quadratic.interaction.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: PIECEWISE.CUBIC.INTERACTION.ADDED.CONTINUOUS
n.reps.piecewise.cubic.interaction.added.continuous <- 50
vec.shift.piecewise.cubic.interaction.added.continuous <- c("50") #c(0, 10, 50)
n.training.vec.piecewise.cubic.interaction.added.continuous <- c(50, 100, 200, 500, 1000, 2000, 5000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.piecewise.cubic.interaction.added.continuous))
colnames(mat.ABR) <- n.training.vec.piecewise.cubic.interaction.added.continuous

for (s in 1:length(vec.shift.piecewise.cubic.interaction.added.continuous)) {
    my.shift.continuous <- vec.shift.piecewise.cubic.interaction.added.continuous[s]
for (i in 1:length(n.training.vec.piecewise.cubic.interaction.added.continuous)) {
    one.n.training <- n.training.vec.piecewise.cubic.interaction.added.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.piecewise.cubic.interaction.added.continuous,
                                                                  scenario="piecewise.cubic.interaction.added.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "piecewise.cubic.interaction.added.continuous_means_n_reps_", n.reps.piecewise.cubic.interaction.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "piecewise.cubic.interaction.added.continuous_sds_n_reps_", n.reps.piecewise.cubic.interaction.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "piecewise.cubic.interaction.added.continuous_mean_ABR_n_reps_", n.reps.piecewise.cubic.interaction.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "piecewise.cubic.interaction.added.continuous_legend_ABR_n_reps_", n.reps.piecewise.cubic.interaction.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "piecewise.cubic.interaction.added.continuous_mat_mean_ABR_n_reps_", n.reps.piecewise.cubic.interaction.added.continuous, "_shift_", my.sphift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: NOISE.ADDED.CONTINUOUS
n.reps.noise.added.continuous <- 50
vec.shift.noise.added.continuous <- c("50") # c("none", "min", "10", "50")
n.training.vec.noise.added.continuous <- c(200, 500, 1000, 2000, 5000, 10000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.noise.added.continuous))
colnames(mat.ABR) <- n.training.vec.noise.added.continuous

for (s in 1:length(vec.shift.noise.added.continuous)) {
    my.shift.continuous <- vec.shift.noise.added.continuous[s]
for (i in 1:length(n.training.vec.noise.added.continuous)) {
    one.n.training <- n.training.vec.noise.added.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.noise.added.continuous, scenario="noise.added.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "noise.added.continuous_means_n_reps_", n.reps.noise.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "noise.added.continuous_sds_n_reps_", n.reps.noise.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "noise.added.continuous_mean_ABR_n_reps_", n.reps.noise.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "noise.added.continuous_legend_ABR_n_reps_", n.reps.noise.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "noise.added.continuous_mat_mean_ABR_n_reps_", n.reps.noise.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}


# SCENARIO: HIGH.DIMENSIONAL.NOISE.ADDED.CONTINUOUS
n.reps.high.dimensional.noise.added.continuous <- 50
vec.shift.high.dimensional.noise.added.continuous <- c("50")
n.training.vec.high.dimensional.noise.added.continuous <- c(500, 1000, 2000, 5000, 10000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.high.dimensional.noise.added.continuous))
colnames(mat.ABR) <- n.training.vec.high.dimensional.noise.added.continuous

for (s in 1:length(vec.shift.high.dimensional.noise.added.continuous)) {
    my.shift.continuous <- vec.shift.high.dimensional.noise.added.continuous[s]
for (i in 1:length(n.training.vec.high.dimensional.noise.added.continuous)) {
    one.n.training <- n.training.vec.high.dimensional.noise.added.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.high.dimensional.noise.added.continuous, scenario="high.dimensional.noise.added.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "high.dimensional.noise.added.continuous_means_n_reps_", n.reps.high.dimensional.noise.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "high.dimensional.noise.added.continuous_sds_n_reps_", n.reps.high.dimensional.noise.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "high.dimensional.noise.added.continuous_mean_ABR_n_reps_", n.reps.high.dimensional.noise.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "high.dimensional.noise.added.continuous_legend_ABR_n_reps_", n.reps.high.dimensional.noise.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "high.dimensional.noise.added.continuous_mat_mean_ABR_n_reps_", n.reps.high.dimensional.noise.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: HIGH.DIMENSIONAL.NOISE.AND.PROGNOSTIC.ADDED.CONTINUOUS
n.reps.high.dimensional.noise.and.prognostic.added.continuous <- 50
vec.shift.high.dimensional.noise.and.prognostic.added.continuous <- c("50") #c("none", "min", "10", "50")
n.training.vec.high.dimensional.noise.and.prognostic.added.continuous <- c(500, 1000, 2000, 5000, 10000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.high.dimensional.noise.and.prognostic.added.continuous))
colnames(mat.ABR) <- n.training.vec.high.dimensional.noise.and.prognostic.added.continuous

for (s in 1:length(vec.shift.high.dimensional.noise.and.prognostic.added.continuous)) {
    my.shift.continuous <- vec.shift.high.dimensional.noise.and.prognostic.added.continuous[s]
for (i in 1:length(n.training.vec.high.dimensional.noise.and.prognostic.added.continuous)) {
    one.n.training <- n.training.vec.high.dimensional.noise.and.prognostic.added.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.high.dimensional.noise.and.prognostic.added.continuous, scenario="high.dimensional.noise.and.prognostic.added.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "high.dimensional.noise.and.prognostic.added.continuous_means_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "high.dimensional.noise.and.prognostic.added.continuous_sds_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "high.dimensional.noise.and.prognostic.added.continuous_mean_ABR_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "high.dimensional.noise.and.prognostic.added.continuous_legend_ABR_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "high.dimensional.noise.and.prognostic.added.continuous_mat_mean_ABR_n_reps_", n.reps.high.dimensional.noise.and.prognostic.added.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}


# SCENARIO: DEFAULT.PIECEWISE.CONSTANT
n.reps.default.piecewise.constant <- 50
n.training.vec.default.piecewise.constant <- c(50, 100, 200, 500, 1000, 2000, 5000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.default.piecewise.constant))
colnames(mat.ABR) <- n.training.vec.default.piecewise.constant

set.seed(123)
checking.default.piecewise.constant <- FormatSimulation(n=20000, scenario="default.piecewise.constant",
                                          shift.for.continuous="min",
                                          unif.min=0, unif.max=2, noise.mu=0, noise.sigma=1,
                                          epsilon.mu=0, epsilon.sigma=1)
png(paste0(plots.dir, "default.piecewise.constant_X1_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
ord.x1.t0 <- order(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 0, 1])
plot(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 0, 1][ord.x1.t0],
      checking.default.piecewise.constant$one.response$S.mat[checking.default.piecewise.constant$one.design$T == 0, 1][ord.x1.t0],
       pch=1, col="blue", ylim=c(-2, 2), xlab=expression(X[1]), ylab=expression(f(X[1])),
       cex.axis=1.5, cex.lab=2,
       lwd=5, type="l")
ord.x1.t1 <- order(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 1, 1])
lines(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 1, 1][ord.x1.t1],
       checking.default.piecewise.constant$one.response$S.mat[checking.default.piecewise.constant$one.design$T == 1, 1][ord.x1.t1],
       pch=1, col="orange",
       lwd=5)
dev.off()

png(paste0(plots.dir, "default.piecewise.constant_X2_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
ord.x2.t0 <- order(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 0, 2])
plot(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 0, 2][ord.x2.t0],
      checking.default.piecewise.constant$one.response$S.mat[checking.default.piecewise.constant$one.design$T == 0, 2][ord.x2.t0],
      pch=1, col="blue", ylim=c(-2, 2), xlab=expression(X[2]), ylab=expression(f(X[2])),
      cex.axis=1.5, cex.lab=2,
      type="l", lwd=5)
ord.x2.t1 <- order(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 1, 2])
lines(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 1, 2][ord.x2.t1],
         checking.default.piecewise.constant$one.response$S.mat[checking.default.piecewise.constant$one.design$T == 1, 2][ord.x2.t1],
         pch=1, col="orange",
         lwd=5)
dev.off()

png(paste0(plots.dir, "default.piecewise.constant_X3_", Sys.Date(), ".png"))
par(mar=c(5.1,5.1,4.1,2.1))
ord.x3.t0 <- order(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 0, 3])
plot(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 0, 3][ord.x3.t0],
      checking.default.piecewise.constant$one.response$S.mat[checking.default.piecewise.constant$one.design$T == 0, 3][ord.x3.t0],
       pch=1, col="blue", ylim=c(-2, 2), xlab=expression(X[3]), ylab=expression(f(X[3])),
      cex.axis=1.5, cex.lab=2,
      type="l", lwd=5)
ord.x3.t1 <- order(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 1, 3])
lines(checking.default.piecewise.constant$one.design$X[checking.default.piecewise.constant$one.design$T == 1, 3][ord.x3.t1],
       checking.default.piecewise.constant$one.response$S.mat[checking.default.piecewise.constant$one.design$T == 1, 3][ord.x3.t1],
       pch=1, col="orange",
       lwd=5)
dev.off()

for (i in 1:length(n.training.vec.default.piecewise.constant)) {
    one.n.training <- n.training.vec.default.piecewise.constant[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.default.piecewise.constant, scenario="default.piecewise.constant",
                                                                  n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "default.piecewise.constant_means_n_reps_", n.reps.default.piecewise.constant, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "default.piecewise.constant_sds_n_reps_", n.reps.default.piecewise.constant, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "default.piecewise.constant_mean_ABR_n_reps_", n.reps.default.piecewise.constant, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "default.piecewise.constant_legend_ABR_n_reps_", n.reps.default.piecewise.constant, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "default.piecewise.constant_mat_mean_ABR_n_reps_", n.reps.default.piecewise.constant, "_", Sys.Date(), ".csv"))

# SCENARIO: TWO.WAY.INTERACTIONS.PIECEWISE.CONSTANT
n.reps.two.way.interactions.piecewise.constant <- 50
n.training.vec.two.way.interactions.piecewise.constant <- c(50, 100, 200, 500, 1000, 2000, 5000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.two.way.interactions.piecewise.constant))
colnames(mat.ABR) <- n.training.vec.two.way.interactions.piecewise.constant

for (i in 1:length(n.training.vec.two.way.interactions.piecewise.constant)) {
    one.n.training <- n.training.vec.two.way.interactions.piecewise.constant[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.two.way.interactions.piecewise.constant, scenario="two.way.interactions.piecewise.constant",
                                                                  n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "two.way.interactions.piecewise.constant_means_n_reps_", n.reps.two.way.interactions.piecewise.constant, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "two.way.interactions.piecewise.constant_sds_n_reps_", n.reps.two.way.interactions.piecewise.constant, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "two.way.interactions.piecewise.constant_mean_ABR_n_reps_", n.reps.two.way.interactions.piecewise.constant, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "two.way.interactions.piecewise.constant_legend_ABR_n_reps_", n.reps.two.way.interactions.piecewise.constant, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "two.way.interactions.piecewise.constant_mat_mean_ABR_n_reps_", n.reps.two.way.interactions.piecewise.constant, "_", Sys.Date(), ".csv"))

# SCENARIO: THREE.WAY.INTERACTIONS.PIECEWISE.CONSTANT
n.reps.three.way.interactions.piecewise.constant <- 50
n.training.vec.three.way.interactions.piecewise.constant <- c(50, 100, 200, 500, 1000, 2000, 5000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.three.way.interactions.piecewise.constant))
colnames(mat.ABR) <- n.training.vec.three.way.interactions.piecewise.constant

for (i in 1:length(n.training.vec.three.way.interactions.piecewise.constant)) {
    one.n.training <- n.training.vec.three.way.interactions.piecewise.constant[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.three.way.interactions.piecewise.constant, scenario="three.way.interactions.piecewise.constant",
                                                                  n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "three.way.interactions.piecewise.constant_means_n_reps_", n.reps.three.way.interactions.piecewise.constant, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "three.way.interactions.piecewise.constant_sds_n_reps_", n.reps.three.way.interactions.piecewise.constant, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "three.way.interactions.piecewise.constant_mean_ABR_n_reps_", n.reps.three.way.interactions.piecewise.constant, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "three.way.interactions.piecewise.constant_legend_ABR_n_reps_", n.reps.three.way.interactions.piecewise.constant, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "three.way.interactions.piecewise.constant_mat_mean_ABR_n_reps_", n.reps.three.way.interactions.piecewise.constant, "_", Sys.Date(), ".csv"))

# SCENARIO: NONLINEAR.INTERACTIONS.PIECEWISE.CONSTANT
n.reps.nonlinear.interactions.piecewise.constant <- 50
n.training.vec.nonlinear.interactions.piecewise.constant <- c(50, 100, 200, 500, 1000, 2000, 5000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.nonlinear.interactions.piecewise.constant))
colnames(mat.ABR) <- n.training.vec.nonlinear.interactions.piecewise.constant

for (i in 1:length(n.training.vec.nonlinear.interactions.piecewise.constant)) {
    one.n.training <- n.training.vec.nonlinear.interactions.piecewise.constant[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.nonlinear.interactions.piecewise.constant, scenario="nonlinear.interactions.piecewise.constant",
                                                                  n.training=one.n.training, n.test=n.test, vec.approaches=vec.approaches,
                                                                  bootstrap.CI=FALSE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "nonlinear.interactions.piecewise.constant_means_n_reps_", n.reps.nonlinear.interactions.piecewise.constant, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "nonlinear.interactions.piecewise.constant_sds_n_reps_", n.reps.nonlinear.interactions.piecewise.constant, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "nonlinear.interactions.piecewise.constant_mean_ABR_n_reps_", n.reps.nonlinear.interactions.piecewise.constant, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "nonlinear.interactions.piecewise.constant_legend_ABR_n_reps_", n.reps.nonlinear.interactions.piecewise.constant, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "nonlinear.interactions.piecewise.constant_mat_mean_ABR_n_reps_", n.reps.nonlinear.interactions.piecewise.constant, "_", Sys.Date(), ".csv"))


# SCENARIO: DEFAULT.PIECEWISE.CONSTANT.CONTINUOUS
n.reps.default.piecewise.constant.continuous <- 50
vec.shift.default.piecewise.constant.continuous <- c("50") #c("none", "min", "10", "50")
n.training.vec.default.piecewise.constant.continuous <- c(50, 100, 200, 500, 1000, 2000, 5000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.default.piecewise.constant.continuous))
colnames(mat.ABR) <- n.training.vec.default.piecewise.constant.continuous

for (s in 1:length(vec.shift.default.piecewise.constant.continuous)) {
    my.shift.continuous <- vec.shift.default.piecewise.constant.continuous[s]
for (i in 1:length(n.training.vec.default.piecewise.constant.continuous)) {
    one.n.training <- n.training.vec.default.piecewise.constant.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.default.piecewise.constant.continuous, scenario="default.piecewise.constant.continuous",
                                                                  n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "default.piecewise.constant.continuous_means_n_reps_", n.reps.default.piecewise.constant.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "default.piecewise.constant.continuous_sds_n_reps_", n.reps.default.piecewise.constant.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "default.piecewise.constant.continuous_mean_ABR_n_reps_", n.reps.default.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "default.piecewise.constant.continuous_legend_ABR_n_reps_", n.reps.default.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "default.piecewise.constant.continuous_mat_mean_ABR_n_reps_", n.reps.default.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: TWO.WAY.INTERACTIONS.PIECEWISE.CONSTANT.CONTINUOUS
n.reps.two.way.interactions.piecewise.constant.continuous <- 50
vec.shift.two.way.interactions.piecewise.constant.continuous <- c("50")  #c("none", "min", "10", "50")
n.training.vec.two.way.interactions.piecewise.constant.continuous <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.two.way.interactions.piecewise.constant.continuous))
colnames(mat.ABR) <- n.training.vec.two.way.interactions.piecewise.constant.continuous

for (s in 1:length(vec.shift.two.way.interactions.piecewise.constant.continuous)) {
    my.shift.continuous <- vec.shift.two.way.interactions.piecewise.constant.continuous[s]
for (i in 1:length(n.training.vec.two.way.interactions.piecewise.constant.continuous)) {
    one.n.training <- n.training.vec.two.way.interactions.piecewise.constant.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.two.way.interactions.piecewise.constant.continuous, scenario="two.way.interactions.piecewise.constant.continuous",
                                                                  n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "two.way.interactions.piecewise.constant.continuous_means_n_reps_", n.reps.two.way.interactions.piecewise.constant.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "two.way.interactions.piecewise.constant.continuous_sds_n_reps_", n.reps.two.way.interactions.piecewise.constant.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "two.way.interactions.piecewise.constant.continuous_mean_ABR_n_reps_", n.reps.two.way.interactions.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "two.way.interactions.piecewise.constant.continuous_legend_ABR_n_reps_", n.reps.two.way.interactions.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "two.way.interactions.piecewise.constant.continuous_mat_mean_ABR_n_reps_", n.reps.two.way.interactions.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: THREE.WAY.INTERACTIONS.PIECEWISE.CONSTANT.CONTINUOUS
n.reps.three.way.interactions.piecewise.constant.continuous <- 50
vec.shift.three.way.interactions.piecewise.constant.continuous <- c("50")  #c("none", "min", "10", "50")
n.training.vec.three.way.interactions.piecewise.constant.continuous <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.three.way.interactions.piecewise.constant.continuous))
colnames(mat.ABR) <- n.training.vec.three.way.interactions.piecewise.constant.continuous

for (s in 1:length(vec.shift.three.way.interactions.piecewise.constant.continuous)) {
    my.shift.continuous <- vec.shift.three.way.interactions.piecewise.constant.continuous[s]
for (i in 1:length(n.training.vec.three.way.interactions.piecewise.constant.continuous)) {
    one.n.training <- n.training.vec.three.way.interactions.piecewise.constant.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.three.way.interactions.piecewise.constant.continuous, scenario="three.way.interactions.piecewise.constant.continuous",
                                                                  n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE,
                                                                  direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "three.way.interactions.piecewise.constant.continuous_means_n_reps_", n.reps.three.way.interactions.piecewise.constant.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "three.way.interactions.piecewise.constant.continuous_sds_n_reps_", n.reps.three.way.interactions.piecewise.constant.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "three.way.interactions.piecewise.constant.continuous_mean_ABR_n_reps_", n.reps.three.way.interactions.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "three.way.interactions.piecewise.constant.continuous_legend_ABR_n_reps_", n.reps.three.way.interactions.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "three.way.interactions.piecewise.constant.continuous_mat_mean_ABR_n_reps_", n.reps.three.way.interactions.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: NONLINEAR.INTERACTIONS.PIECEWISE.CONSTANT.CONTINUOUS
n.reps.nonlinear.interactions.piecewise.constant.continuous <- 50
vec.shift.nonlinear.interactions.piecewise.constant.continuous <- c("50")  #c("none", "min", "10", "50")
n.training.vec.nonlinear.interactions.piecewise.constant.continuous <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.nonlinear.interactions.piecewise.constant.continuous))
colnames(mat.ABR) <- n.training.vec.nonlinear.interactions.piecewise.constant.continuous

for (s in 1:length(vec.shift.nonlinear.interactions.piecewise.constant.continuous)) {
    my.shift.continuous <- vec.shift.nonlinear.interactions.piecewise.constant.continuous[s]
for (i in 1:length(n.training.vec.nonlinear.interactions.piecewise.constant.continuous)) {
    one.n.training <- n.training.vec.nonlinear.interactions.piecewise.constant.continuous[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.nonlinear.interactions.piecewise.constant.continuous, scenario="nonlinear.interactions.piecewise.constant.continuous",
                                                                  n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE, direct.interactions.exclude.A.from.penalty=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "nonlinear.interactions.piecewise.constant.continuous_means_n_reps_", n.reps.nonlinear.interactions.piecewise.constant.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "nonlinear.interactions.piecewise.constant.continuous_sds_n_reps_", n.reps.nonlinear.interactions.piecewise.constant.continuous, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "nonlinear.interactions.piecewise.constant.continuous_mean_ABR_n_reps_", n.reps.nonlinear.interactions.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "nonlinear.interactions.piecewise.constant.continuous_legend_ABR_n_reps_", n.reps.nonlinear.interactions.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "nonlinear.interactions.piecewise.constant.continuous_mat_mean_ABR_n_reps_", n.reps.nonlinear.interactions.piecewise.constant.continuous, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}


## make plots showing x1->f(x1), x2->f(x2), x3->f(x3) for the piecewise constant scenario
n.reps.default.continuous.debugging.DI <- 50
vec.shift.default.continuous.debugging.DI <- c("min", "10")  #c("none", "min", "10", "50")
n.training.vec.default.continuous.debugging.DI <- c(50, 100, 200, 500, 1000)
mat.ABR <- matrix(NA, nrow=length(vec.approaches)+3, ncol=length(n.training.vec.default.continuous.debugging.DI))
colnames(mat.ABR) <- n.training.vec.default.continuous.debugging.DI

for (s in 1:length(vec.shift.default.continuous.debugging.DI)) {
    my.shift.continuous <- vec.shift.default.continuous.debugging.DI[s]
for (i in 1:length(n.training.vec.default.continuous.debugging.DI)) {
    one.n.training <- n.training.vec.default.continuous.debugging.DI[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.default.continuous.debugging.DI, scenario="default.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches, 
                                                                  bootstrap.CI=FALSE,
                                                                  debugging.DI=TRUE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "default.continuous.debugging.DI_means_n_reps_", n.reps.default.continuous.debugging.DI, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "default.continuous.debugging.DI_sds_n_reps_", n.reps.default.continuous.debugging.DI, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "default.continuous.debugging.DI_mean_ABR_n_reps_", n.reps.default.continuous.debugging.DI, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR, my.title="")
dev.off()
png(file=paste0(plots.dir, "default.continuous.debugging.DI_legend_ABR_n_reps_", n.reps.default.continuous.debugging.DI, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR)
dev.off()
write.csv(mat.ABR, file=paste0(results.dir, "default.continuous.debugging.DI_mat_mean_ABR_n_reps_", n.reps.default.continuous.debugging.DI, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

