rm(list=ls())
library(DevTreatRules)
library(glmnet)
source("Functions/Simulations_Chapter4.R")
results.dir <- "Results/"
plots.dir <- "Plots/"
n.test <- 10000

# SCENARIO: DEFAULT.CONTINUOUS.TUNING.OWL
n.reps.default.continuous.tuning.OWL <- 5
vec.shift.default.continuous.tuning.OWL <- c("min", "50")
n.training.vec.default.continuous.tuning.OWL <- c(50, 100, 200, 500, 1000)
vec.approaches.tuning.OWL <-  c("OWL.framework", "split.regression") 
mat.ABR <- matrix(NA, nrow=length(vec.approaches.tuning.OWL)+3, ncol=length(n.training.vec.default.continuous.tuning.OWL))
colnames(mat.ABR) <- n.training.vec.default.continuous.tuning.OWL

for (s in 1:length(vec.shift.default.continuous.tuning.OWL)) {
    my.shift.continuous <- vec.shift.default.continuous.tuning.OWL[s]
for (i in 1:length(n.training.vec.default.continuous.tuning.OWL)) {
    one.n.training <- n.training.vec.default.continuous.tuning.OWL[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.default.continuous.tuning.OWL, scenario="default.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches.tuning.OWL, 
                                                                  bootstrap.CI=FALSE,
                                                                  OWL.framework.shift.by.min=FALSE,
                                                                  direct.interactions.center.continuous.Y=TRUE,
                                                                  direct.interactions.exclude.A.from.penalty=FALSE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "default.continuous.tuning.OWL_means_n_reps_", n.reps.default.continuous.tuning.OWL, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "default.continuous.tuning.OWL_sds_n_reps_", n.reps.default.continuous.tuning.OWL, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.counts, file=paste0(results.dir, "default.continuous.tuning.OWL_counts_n_reps_", n.reps.default.continuous.tuning.OWL, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])

png(file=paste0(plots.dir, "default.continuous.tuning.OWL_mean_ABR_n_reps_", n.reps.default.continuous.tuning.OWL, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR[c("OWL.framework", "split.regression", "optimal.rule"), ], my.title="")
dev.off()
png(file=paste0(plots.dir, "default.continuous.tuning.OWL_legend_ABR_n_reps_", n.reps.default.continuous.tuning.OWL, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR[c("OWL.framework", "split.regression", "optimal.rule"), ])
dev.off()
write.csv(mat.ABR[c("OWL.framework", "split.regression", "optimal.rule"), ], file=paste0(results.dir, "default.continuous.tuning.OWL_mat_mean_ABR_n_reps_", n.reps.default.continuous.tuning.OWL, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: NOISE.ADDED.CONTINUOUS.TUNING.OWL
n.reps.noise.added.continuous.tuning.OWL <- 5
vec.shift.noise.added.continuous.tuning.OWL <- c("min", "50") 
n.training.vec.noise.added.continuous.tuning.OWL <- n.training.vec.noise.added.continuous <- c(200, 500, 1000, 2000, 5000, 10000)
vec.approaches.tuning.OWL <-  c("OWL.framework", "split.regression") 
mat.ABR <- matrix(NA, nrow=length(vec.approaches.tuning.OWL)+3, ncol=length(n.training.vec.noise.added.continuous.tuning.OWL))
colnames(mat.ABR) <- n.training.vec.noise.added.continuous.tuning.OWL

for (s in 1:length(vec.shift.noise.added.continuous.tuning.OWL)) {
    my.shift.continuous <- vec.shift.noise.added.continuous.tuning.OWL[s]
for (i in 1:length(n.training.vec.noise.added.continuous.tuning.OWL)) {
    one.n.training <- n.training.vec.noise.added.continuous.tuning.OWL[i]
    set.seed(one.n.training)
    one.simulation.set <- DoAllSimulations(n.reps=n.reps.noise.added.continuous.tuning.OWL, scenario="noise.added.continuous", n.training=one.n.training, n.test=n.test,
                                                                  shift.for.continuous=my.shift.continuous,
                                                                  vec.approaches=vec.approaches.tuning.OWL, 
                                                                  bootstrap.CI=FALSE,
                                                                  OWL.framework.shift.by.min=FALSE,
                                                                  direct.interactions.center.continuous.Y=TRUE,
                                                                  direct.interactions.exclude.A.from.penalty=FALSE)
    mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
    write.csv(one.simulation.set$mat.means, file=paste0(results.dir, "noise.added.continuous.tuning.OWL_means_n_reps_", n.reps.noise.added.continuous.tuning.OWL, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.sds, file=paste0(results.dir, "noise.added.continuous.tuning.OWL_sds_n_reps_", n.reps.noise.added.continuous.tuning.OWL, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
    write.csv(one.simulation.set$mat.counts, file=paste0(results.dir, "noise.added.continuous.tuning.OWL_counts_n_reps_", n.reps.noise.added.continuous.tuning.OWL, "_n_training_", one.n.training, "_", Sys.Date(), ".csv"))
}
rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])
png(file=paste0(plots.dir, "noise.added.continuous.tuning.OWL_mean_ABR_n_reps_", n.reps.noise.added.continuous.tuning.OWL, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotMeanSummary(mat.ABR[c("OWL.framework", "split.regression", "optimal.rule"), ], my.title="")
dev.off()
png(file=paste0(plots.dir, "noise.added.continuous.tuning.OWL_legend_ABR_n_reps_", n.reps.noise.added.continuous.tuning.OWL, "_shift_", my.shift.continuous, "_", Sys.Date(), ".png"))
PlotLegendMeanSummary(mat.ABR[c("OWL.framework", "split.regression", "optimal.rule"), ])
dev.off()
write.csv(mat.ABR[c("OWL.framework", "split.regression", "optimal.rule"), ], file=paste0(results.dir, "noise.added.continuous.tuning.OWL_mat_mean_ABR_n_reps_", n.reps.noise.added.continuous.tuning.OWL, "_shift_", my.shift.continuous, "_", Sys.Date(), ".csv"))
}

# SCENARIO: DEFAULT.CONTINUOUS.TUNING.DI
n.reps.default.continuous.tuning.DI <- 5
vec.shift.default.continuous.tuning.DI <- c("min", "50") 
n.training.vec.default.continuous.tuning.DI <- c(50, 100, 200, 500, 1000)
vec.approaches.tuning.DI <-  c("direct.interactions", "split.regression") 
mat.ABR <- matrix(NA, nrow=length(vec.approaches.tuning.DI)+3, ncol=length(n.training.vec.default.continuous.tuning.DI))
colnames(mat.ABR) <- n.training.vec.default.continuous.tuning.DI
vec.DI.center.continuous.Y <- c(FALSE, TRUE)
vec.DI.exclude.A.from.penalty <- c(FALSE, TRUE) 
for (s in 1:length(vec.shift.default.continuous.tuning.DI)) {
    my.shift.continuous <- vec.shift.default.continuous.tuning.DI[s]
    for (c in 1:length(vec.DI.center.continuous.Y)) {
        my.center.continuous.Y <- vec.DI.center.continuous.Y[c]
        for (e in 1:length(vec.DI.exclude.A.from.penalty)) {
            my.exclude.A <- vec.DI.exclude.A.from.penalty[e]
            for (i in 1:length(n.training.vec.default.continuous.tuning.DI)) {
                one.n.training <- n.training.vec.default.continuous.tuning.DI[i]
                set.seed(one.n.training)
                one.simulation.set <- DoAllSimulations(n.reps=n.reps.default.continuous.tuning.DI, scenario="default.continuous", n.training=one.n.training, n.test=n.test,
                                                               shift.for.continuous=my.shift.continuous,
                                                               vec.approaches=vec.approaches.tuning.DI, 
                                                               bootstrap.CI=FALSE,
                                                               OWL.framework.shift.by.min=FALSE,
                                                               direct.interactions.center.continuous.Y=my.center.continuous.Y,
                                                               direct.interactions.exclude.A.from.penalty=my.exclude.A)
                mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
            }
            rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])
            png(file=paste0(plots.dir, "default.continuous.tuning.DI_mean_ABR_n_reps_", n.reps.default.continuous.tuning.DI, "_shift_", my.shift.continuous,
                                   "_center_Y_", my.center.continuous.Y, "_exclude_A_", my.exclude.A, "_", Sys.Date(), ".png"))
            PlotMeanSummary(mat.ABR[c("direct.interactions", "split.regression", "optimal.rule"), ], my.title="")
            dev.off()
            png(file=paste0(plots.dir, "default.continuous.tuning.DI_legend_ABR_n_reps_", n.reps.default.continuous.tuning.DI, "_shift_", my.shift.continuous,
                                  "_center_Y_", my.center.continuous.Y, "_exclude_A_", my.exclude.A, "_", Sys.Date(), ".png"))
            PlotLegendMeanSummary(mat.ABR[c("direct.interactions", "split.regression", "optimal.rule"), ])
            dev.off()
            write.csv(mat.ABR[c("direct.interactions", "split.regression", "optimal.rule"), ],
                         file=paste0(results.dir, "default.continuous.tuning.DI_mat_mean_ABR_n_reps_", n.reps.default.continuous.tuning.DI,
                                         "_shift_", my.shift.continuous, "_center_Y_", my.center.continuous.Y, "_exclude_A_", my.exclude.A, "_", Sys.Date(), ".csv"))
        }
    }
}

# SCENARIO: NOISE.ADDED.CONTINUOUS.TUNING.DI
n.reps.noise.added.continuous.tuning.DI <- 5
vec.shift.noise.added.continuous.tuning.DI <- c("50", "min")
n.training.vec.noise.added.continuous.tuning.DI <- c(200, 500, 1000, 2000, 5000, 10000)
vec.approaches.tuning.DI <-  c("direct.interactions", "split.regression") 
mat.ABR <- matrix(NA, nrow=length(vec.approaches.tuning.DI)+3, ncol=length(n.training.vec.noise.added.continuous.tuning.DI))
colnames(mat.ABR) <- n.training.vec.noise.added.continuous.tuning.DI
vec.DI.center.continuous.Y <- c(FALSE, TRUE)
vec.DI.exclude.A.from.penalty <- c(FALSE, TRUE)
for (s in 1:length(vec.shift.noise.added.continuous.tuning.DI)) {
    my.shift.continuous <- vec.shift.noise.added.continuous.tuning.DI[s]
    for (c in 1:length(vec.DI.center.continuous.Y)) {
        my.center.continuous.Y <- vec.DI.center.continuous.Y[c]
        for (e in 1:length(vec.DI.exclude.A.from.penalty)) {
            my.exclude.A <- vec.DI.exclude.A.from.penalty[e]
            for (i in 1:length(n.training.vec.noise.added.continuous.tuning.DI)) {
                one.n.training <- n.training.vec.noise.added.continuous.tuning.DI[i]
                set.seed(one.n.training)
                one.simulation.set <- DoAllSimulations(n.reps=n.reps.noise.added.continuous.tuning.DI, scenario="noise.added.continuous", n.training=one.n.training, n.test=n.test,
                                                               shift.for.continuous=my.shift.continuous,
                                                               vec.approaches=vec.approaches.tuning.DI, 
                                                               bootstrap.CI=FALSE,
                                                               OWL.framework.shift.by.min=FALSE,
                                                               direct.interactions.center.continuous.Y=my.center.continuous.Y,
                                                               direct.interactions.exclude.A.from.penalty=my.exclude.A)
                mat.ABR[, as.character(one.n.training)] <- one.simulation.set$mat.means[, "exact.ABR"]
            }
            rownames(mat.ABR) <- names(one.simulation.set$mat.means[, "exact.ABR"])
            png(file=paste0(plots.dir, "noise.added.continuous.tuning.DI_mean_ABR_n_reps_", n.reps.noise.added.continuous.tuning.DI, "_shift_", my.shift.continuous,
                                   "_center_Y_", my.center.continuous.Y, "_exclude_A_", my.exclude.A, "_", Sys.Date(), ".png"))
            PlotMeanSummary(mat.ABR[c("direct.interactions", "split.regression", "optimal.rule"), ], my.title="")
            dev.off()
            png(file=paste0(plots.dir, "noise.added.continuous.tuning.DI_legend_ABR_n_reps_", n.reps.noise.added.continuous.tuning.DI, "_shift_", my.shift.continuous,
                                  "_center_Y_", my.center.continuous.Y, "_exclude_A_", my.exclude.A, "_", Sys.Date(), ".png"))
            PlotLegendMeanSummary(mat.ABR[c("direct.interactions", "split.regression", "optimal.rule"), ])
            dev.off()
            write.csv(mat.ABR[c("direct.interactions", "split.regression", "optimal.rule"), ],
                         file=paste0(results.dir, "noise.added.continuous.tuning.DI_mat_mean_ABR_n_reps_", n.reps.noise.added.continuous.tuning.DI,
                                         "_shift_", my.shift.continuous, "_center_Y_", my.center.continuous.Y, "_exclude_A_", my.exclude.A, "_", Sys.Date(), ".csv"))
        }
    }
}
