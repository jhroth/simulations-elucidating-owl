GetTrueResponseProb <- function(intercept, beta.1, X, link) {
    stopifnot(link%in% c("logistic", "poisson"))
    if (link == "logistic") {
        response.prob.linear <- Expit(intercept + beta.1 * X)
        response.prob.quadratic <- Expit(intercept + beta.1 * X + + beta.1 * X^2)
    } else if (link == "poisson") {
        if ((beta.1 - 4 < intercept) & (intercept < -beta.1) == FALSE & ((beta.1/4 - 4 < intercept) & (intercept < -2*beta.1) == FALSE)) {
            stop("invalid choice of coefficients for poisson model")
        }
        response.prob.linear <- exp(intercept + beta.1 * X)
        response.prob.quadratic <- exp(intercept + beta.1 * X + + beta.1 * X^2)
    }
        return(list("response.prob.linear"=response.prob.linear, "response.prob.quadratic"=response.prob.quadratic))
}

GetObservationWeights <- function(Y, T, propensity.score) {
    IPW.weights <- Y / (T * propensity.score + (1 - T) * propensity.score)
    return(list("IPW.weights"=IPW.weights))
}

SimulateSimpleData <- function(n, p.informative=1, p.noisy=5, prop.on.treatment=0.5, response.type="binary", is.rct=TRUE, 
            intercept.control.linear.logistic=0.45, beta.1.control.linear.logistic=0.5, intercept.treatment.linear.logistic=0.5, beta.1.treatment.linear.logistic=2,
            intercept.control.quadratic.logistic=-0.5, beta.1.control.quadratic.logistic=0.5, intercept.treatment.quadratic.logistic=-1, beta.1.treatment.quadratic.logistic=1.5,
            intercept.control.linear.poisson=-0.9, beta.1.control.linear.poisson=0.3, intercept.treatment.linear.poisson=-1, beta.1.treatment.linear.poisson=0.85,
            intercept.control.quadratic.poisson=-1, beta.1.control.quadratic.poisson=0.25, intercept.treatment.quadratic.poisson=-1.3, beta.1.treatment.quadratic.poisson=0.6) {
    p <- p.informative + p.noisy
    if (is.rct == TRUE) {
        n.treatment <- ceiling(n * prop.on.treatment)
        n.control <- n - n.treatment
        T <- rep(0, n)
        idx.treatment <- sort(sample(x=1:n, size=n.treatment, replace=FALSE)) ##making sure the number of T is actually the same as data-generating mechanism would suggest (so sample proportion on treatment is equal to propensity score)
        T[idx.treatment] <- 1
        propensity.score <- mean(T)  ## the propensity score will just be equal to the randomization ratio here
        X.informative <- matrix(runif(n=n*p.informative, min=-1, max=1), nrow=n, ncol=p.informative)
        X.noisy <- matrix(runif(n=n*p.noisy, min=-1, max=1), nrow=n, ncol=p.noisy)
        if (response.type=="binary") {
            # logistic scenario 1
            X.scenario.logistic.1 <- as.matrix(X.informative[, 1]) ## no noise features
            prob.scenario.logistic.1 <- rep(NA, n)
            prob.scenario.logistic.1[T==1] <- GetTrueResponseProb(intercept=intercept.treatment.linear.logistic, beta.1=beta.1.treatment.linear.logistic, X=X.informative[T==1, 1], link="logistic")$response.prob.linear
            prob.scenario.logistic.1[T==0] <- GetTrueResponseProb(intercept=intercept.control.linear.logistic, beta.1=beta.1.control.linear.logistic, X=X.informative[T==0, 1], link="logistic")$response.prob.linear
            Y.scenario.logistic.1 <- rbinom(n=n, size=1, prob=prob.scenario.logistic.1)
            OWL.IPW.weight.scenario.logistic.1 <- GetObservationWeights(Y=Y.scenario.logistic.1, T=T, propensity.score=propensity.score)$IPW.weights
            population.averaged.response.prob.scenario.logistic.1 <- propensity.score * GetTrueResponseProb(intercept=intercept.treatment.linear.logistic, beta.1=beta.1.treatment.linear.logistic, X=X.informative[, 1], link="logistic")$response.prob.linear +
                                                                                                                                                                                                                     (1 - propensity.score) * GetTrueResponseProb(intercept=intercept.control.linear.logistic, beta.1=beta.1.control.linear.logistic, X=X.informative[, 1], link="logistic")$response.prob.linear
            population.treatment.prob.among.responders.scenario.logistic.1 <-
                propensity.score * GetTrueResponseProb(intercept=intercept.treatment.linear.logistic, beta.1=beta.1.treatment.linear.logistic, X=X.informative[, 1], link="logistic")$response.prob.linear / population.averaged.response.prob.scenario.logistic.1 
            # logistic scenario 2
            X.scenario.logistic.2 <- as.matrix(X.informative[, 1]) # no noise features
            prob.scenario.logistic.2 <- rep(NA, n)
            prob.scenario.logistic.2[T==1] <- GetTrueResponseProb(intercept=intercept.treatment.quadratic.logistic, beta.1=beta.1.treatment.quadratic.logistic, X=X.informative[T==1, 1], link="logistic")$response.prob.quadratic
            prob.scenario.logistic.2[T==0] <- GetTrueResponseProb(intercept=intercept.control.quadratic.logistic, beta.1=beta.1.control.quadratic.logistic, X=X.informative[T==0, 1], link="logistic")$response.prob.quadratic
            Y.scenario.logistic.2 <- rbinom(n=n, size=1, prob=prob.scenario.logistic.2)
            OWL.IPW.weight.scenario.logistic.2 <- GetObservationWeights(Y=Y.scenario.logistic.2, T=T, propensity.score=propensity.score)$IPW.weights
            population.averaged.response.prob.scenario.logistic.2 <- propensity.score * GetTrueResponseProb(intercept=intercept.treatment.quadratic.logistic, beta.1=beta.1.treatment.quadratic.logistic, X=X.informative[, 1], link="logistic")$response.prob.quadratic +
                                                                                                                                                                                                                           (1 - propensity.score) * GetTrueResponseProb(intercept=intercept.control.quadratic.logistic, beta.1=beta.1.control.quadratic.logistic, X=X.informative[, 1], link="logistic")$response.prob.quadratic
            population.treatment.prob.among.responders.scenario.logistic.2 <-
                propensity.score * GetTrueResponseProb(intercept=intercept.treatment.quadratic.logistic, beta.1=beta.1.treatment.quadratic.logistic, X=X.informative[, 1], link="logistic")$response.prob.quadratic / population.averaged.response.prob.scenario.logistic.2
            # logistic scenario 3
            prob.scenario.logistic.3 <- prob.scenario.logistic.1
            X.scenario.logistic.3 <- as.matrix(cbind(X.informative, X.noisy))
            Y.scenario.logistic.3 <- Y.scenario.logistic.1 ## the additional features are just noise
            OWL.IPW.weight.scenario.logistic.3 <- GetObservationWeights(Y=Y.scenario.logistic.3, T=T, propensity.score=propensity.score)$IPW.weights
            population.averaged.response.prob.scenario.logistic.3 <- population.averaged.response.prob.scenario.logistic.1
            population.treatment.prob.among.responders.scenario.logistic.3 <- population.treatment.prob.among.responders.scenario.logistic.1
            # logistic scenario 4
            prob.scenario.logistic.4 <- prob.scenario.logistic.2
            X.scenario.logistic.4 <- as.matrix(cbind(X.informative, X.noisy))
            Y.scenario.logistic.4 <- Y.scenario.logistic.2 ## the additional features are just noise
            OWL.IPW.weight.scenario.logistic.4 <- GetObservationWeights(Y=Y.scenario.logistic.4, T=T, propensity.score=propensity.score)$IPW.weights
            population.averaged.response.prob.scenario.logistic.4 <- population.averaged.response.prob.scenario.logistic.1
            population.treatment.prob.among.responders.scenario.logistic.4 <- population.treatment.prob.among.responders.scenario.logistic.2
            
            # Poisson scenario 1
            X.scenario.poisson.1 <- as.matrix(X.informative[, 1]) ## no noise features
            prob.scenario.poisson.1 <- rep(NA, n)
            prob.scenario.poisson.1[T==1] <- GetTrueResponseProb(intercept=intercept.treatment.linear.poisson, beta.1=beta.1.treatment.linear.poisson, X=X.informative[T==1, 1], link="poisson")$response.prob.linear
            prob.scenario.poisson.1[T==0] <- GetTrueResponseProb(intercept=intercept.control.linear.poisson, beta.1=beta.1.control.linear.poisson, X=X.informative[T==0, 1], link="poisson")$response.prob.linear
            Y.scenario.poisson.1 <- rbinom(n=n, size=1, prob=prob.scenario.poisson.1)
            OWL.IPW.weight.scenario.poisson.1 <- GetObservationWeights(Y=Y.scenario.poisson.1, T=T, propensity.score=propensity.score)$IPW.weights
            population.averaged.response.prob.scenario.poisson.1 <- propensity.score * GetTrueResponseProb(intercept=intercept.treatment.linear.poisson, beta.1=beta.1.treatment.linear.poisson, X=X.informative[, 1], link="poisson")$response.prob.linear +
                                                                                                                                                                                                                     (1 - propensity.score) * GetTrueResponseProb(intercept=intercept.control.linear.poisson, beta.1=beta.1.control.linear.poisson, X=X.informative[, 1], link="poisson")$response.prob.linear
            population.treatment.prob.among.responders.scenario.poisson.1 <-
                propensity.score * GetTrueResponseProb(intercept=intercept.treatment.linear.poisson, beta.1=beta.1.treatment.linear.poisson, X=X.informative[, 1], link="poisson")$response.prob.linear / population.averaged.response.prob.scenario.poisson.1

            # Poisson scenario 2
            X.scenario.poisson.2 <- as.matrix(X.informative[, 1]) # no noise features
            prob.scenario.poisson.2 <- rep(NA, n)
            prob.scenario.poisson.2[T==1] <- GetTrueResponseProb(intercept=intercept.treatment.quadratic.poisson, beta.1=beta.1.treatment.quadratic.poisson, X=X.informative[T==1, 1], link="poisson")$response.prob.quadratic
            prob.scenario.poisson.2[T==0] <- GetTrueResponseProb(intercept=intercept.control.quadratic.poisson, beta.1=beta.1.control.quadratic.poisson, X=X.informative[T==0, 1], link="poisson")$response.prob.quadratic
            Y.scenario.poisson.2 <- rbinom(n=n, size=1, prob=prob.scenario.poisson.2)
            OWL.IPW.weight.scenario.poisson.2 <- GetObservationWeights(Y=Y.scenario.poisson.2, T=T, propensity.score=propensity.score)$IPW.weights
            population.averaged.response.prob.scenario.poisson.2 <- propensity.score * GetTrueResponseProb(intercept=intercept.treatment.quadratic.poisson, beta.1=beta.1.treatment.quadratic.poisson, X=X.informative[, 1], link="poisson")$response.prob.quadratic +
                                                                                                                                                                                                                           (1 - propensity.score) * GetTrueResponseProb(intercept=intercept.control.quadratic.poisson, beta.1=beta.1.control.quadratic.poisson, X=X.informative[, 1], link="poisson")$response.prob.quadratic
            population.treatment.prob.among.responders.scenario.poisson.2 <-
                propensity.score * GetTrueResponseProb(intercept=intercept.treatment.quadratic.poisson, beta.1=beta.1.treatment.quadratic.poisson, X=X.informative[, 1], link="poisson")$response.prob.quadratic / population.averaged.response.prob.scenario.poisson.2

            # Poisson scenario 3
            prob.scenario.poisson.3 <- prob.scenario.poisson.1
            X.scenario.poisson.3 <- as.matrix(cbind(X.informative, X.noisy))
            Y.scenario.poisson.3 <- Y.scenario.poisson.1 ## the additional features are just noise
            OWL.IPW.weight.scenario.poisson.3 <- GetObservationWeights(Y=Y.scenario.poisson.3, T=T, propensity.score=propensity.score)$IPW.weights
            population.averaged.response.prob.scenario.poisson.3 <- population.averaged.response.prob.scenario.poisson.1
            population.treatment.prob.among.responders.scenario.poisson.3 <- population.treatment.prob.among.responders.scenario.poisson.1

            # Poisson scenario 4
            prob.scenario.poisson.4 <- prob.scenario.poisson.2
            X.scenario.poisson.4 <- as.matrix(cbind(X.informative, X.noisy))
            Y.scenario.poisson.4 <- Y.scenario.poisson.2 ## the additional features are just noise
            OWL.IPW.weight.scenario.poisson.4 <- GetObservationWeights(Y=Y.scenario.poisson.4, T=T, propensity.score=propensity.score)$IPW.weights
            population.averaged.response.prob.scenario.poisson.4 <- population.averaged.response.prob.scenario.poisson.1
            population.treatment.prob.among.responders.scenario.poisson.4 <- population.treatment.prob.among.responders.scenario.poisson.2
2
        }
        else {
            stop("only using a binary response for now")
        }
     return(list("T"=T, "idx.treatment"=idx.treatment,"X.informative"=X.informative, "X.noisy"=X.noisy, "propensity.score"=propensity.score,
                    "prob.scenario.logistic.1"=prob.scenario.logistic.1, "X.scenario.logistic.1"=X.scenario.logistic.1, "Y.scenario.logistic.1"=Y.scenario.logistic.1, "OWL.IPW.weight.scenario.logistic.1"=OWL.IPW.weight.scenario.logistic.1,
                    "population.averaged.response.prob.scenario.logistic.1"=population.averaged.response.prob.scenario.logistic.1, "population.treatment.prob.among.responders.scenario.logistic.1"=population.treatment.prob.among.responders.scenario.logistic.1,
                   "intercept.control.scenario.logistic.1"= intercept.control.linear.logistic, "beta.1.control.scenario.logistic.1"= beta.1.control.linear.logistic,
                   "intercept.treatment.scenario.logistic.1"= intercept.treatment.linear.logistic, "beta.1.treatment.scenario.logistic.1"= beta.1.treatment.linear.logistic,
                   "prob.scenario.logistic.2"=prob.scenario.logistic.2, "X.scenario.logistic.2"=X.scenario.logistic.2, "Y.scenario.logistic.2"=Y.scenario.logistic.2, "OWL.IPW.weight.scenario.logistic.2"=OWL.IPW.weight.scenario.logistic.2,
                   "population.averaged.response.prob.scenario.logistic.2"=population.averaged.response.prob.scenario.logistic.2, "population.treatment.prob.among.responders.scenario.logistic.2"=population.treatment.prob.among.responders.scenario.logistic.2,
                   "intercept.control.scenario.logistic.2"= intercept.control.quadratic.logistic, "beta.1.control.scenario.logistic.2"= beta.1.control.quadratic.logistic,
                   "intercept.treatment.scenario.logistic.2"= intercept.treatment.quadratic.logistic, "beta.1.treatment.scenario.logistic.2"= beta.1.treatment.quadratic.logistic,
                   "prob.scenario.logistic.3"=prob.scenario.logistic.3, "X.scenario.logistic.3"=X.scenario.logistic.3, "Y.scenario.logistic.3"=Y.scenario.logistic.3, "OWL.IPW.weight.scenario.logistic.3"=OWL.IPW.weight.scenario.logistic.3,
                   "population.averaged.response.prob.scenario.logistic.3"=population.averaged.response.prob.scenario.logistic.3, "population.treatment.prob.among.responders.scenario.logistic.3"=population.treatment.prob.among.responders.scenario.logistic.3,
                   "intercept.control.scenario.logistic.3"= intercept.control.linear.logistic, "beta.1.control.scenario.logistic.3"= beta.1.control.linear.logistic,
                   "intercept.treatment.scenario.logistic.3"= intercept.treatment.linear.logistic, "beta.1.treatment.scenario.logistic.3"= beta.1.treatment.linear.logistic,
                   "prob.scenario.logistic.4"=prob.scenario.logistic.4, "X.scenario.logistic.4"=X.scenario.logistic.4, "Y.scenario.logistic.4"=Y.scenario.logistic.4, "OWL.IPW.weight.scenario.logistic.4"=OWL.IPW.weight.scenario.logistic.4,
                   "population.averaged.response.prob.scenario.logistic.4"=population.averaged.response.prob.scenario.logistic.4, "population.treatment.prob.among.responders.scenario.logistic.4"=population.treatment.prob.among.responders.scenario.logistic.4,
                   "intercept.control.scenario.logistic.4"= intercept.control.quadratic.logistic, "beta.1.control.scenario.logistic.4"= beta.1.control.quadratic.logistic,
                   "intercept.treatment.scenario.logistic.4"= intercept.treatment.quadratic.logistic, "beta.1.treatment.scenario.logistic.4"= beta.1.treatment.quadratic.logistic,

                    "prob.scenario.poisson.1"=prob.scenario.poisson.1, "X.scenario.poisson.1"=X.scenario.poisson.1, "Y.scenario.poisson.1"=Y.scenario.poisson.1, "OWL.IPW.weight.scenario.poisson.1"=OWL.IPW.weight.scenario.poisson.1,
                    "population.averaged.response.prob.scenario.poisson.1"=population.averaged.response.prob.scenario.poisson.1, "population.treatment.prob.among.responders.scenario.poisson.1"=population.treatment.prob.among.responders.scenario.poisson.1,
                   "intercept.control.scenario.poisson.1"= intercept.control.linear.poisson, "beta.1.control.scenario.poisson.1"= beta.1.control.linear.poisson,
                   "intercept.treatment.scenario.poisson.1"= intercept.treatment.linear.poisson, "beta.1.treatment.scenario.poisson.1"= beta.1.treatment.linear.poisson,
                   "prob.scenario.poisson.2"=prob.scenario.poisson.2, "X.scenario.poisson.2"=X.scenario.poisson.2, "Y.scenario.poisson.2"=Y.scenario.poisson.2, "OWL.IPW.weight.scenario.poisson.2"=OWL.IPW.weight.scenario.poisson.2,
                   "population.averaged.response.prob.scenario.poisson.2"=population.averaged.response.prob.scenario.poisson.2, "population.treatment.prob.among.responders.scenario.poisson.2"=population.treatment.prob.among.responders.scenario.poisson.2,
                   "intercept.control.scenario.poisson.2"= intercept.control.quadratic.poisson, "beta.1.control.scenario.poisson.2"= beta.1.control.quadratic.poisson,
                   "intercept.treatment.scenario.poisson.2"= intercept.treatment.quadratic.poisson, "beta.1.treatment.scenario.poisson.2"= beta.1.treatment.quadratic.poisson,
                   "prob.scenario.poisson.3"=prob.scenario.poisson.3, "X.scenario.poisson.3"=X.scenario.poisson.3, "Y.scenario.poisson.3"=Y.scenario.poisson.3, "OWL.IPW.weight.scenario.poisson.3"=OWL.IPW.weight.scenario.poisson.3,
                   "population.averaged.response.prob.scenario.poisson.3"=population.averaged.response.prob.scenario.poisson.3, "population.treatment.prob.among.responders.scenario.poisson.3"=population.treatment.prob.among.responders.scenario.poisson.3,
                   "intercept.control.scenario.poisson.3"= intercept.control.linear.poisson, "beta.1.control.scenario.poisson.3"= beta.1.control.linear.poisson,
                   "intercept.treatment.scenario.poisson.3"= intercept.treatment.linear.poisson, "beta.1.treatment.scenario.poisson.3"= beta.1.treatment.linear.poisson,
                   "prob.scenario.poisson.4"=prob.scenario.poisson.4, "X.scenario.poisson.4"=X.scenario.poisson.4, "Y.scenario.poisson.4"=Y.scenario.poisson.4, "OWL.IPW.weight.scenario.poisson.4"=OWL.IPW.weight.scenario.poisson.4,
                   "population.averaged.response.prob.scenario.poisson.4"=population.averaged.response.prob.scenario.poisson.4, "population.treatment.prob.among.responders.scenario.poisson.4"=population.treatment.prob.among.responders.scenario.poisson.4,
                   "intercept.control.scenario.poisson.4"= intercept.control.quadratic.poisson, "beta.1.control.scenario.poisson.4"= beta.1.control.quadratic.poisson,
                   "intercept.treatment.scenario.poisson.4"= intercept.treatment.quadratic.poisson, "beta.1.treatment.scenario.poisson.4"= beta.1.treatment.quadratic.poisson))
    } else {
        stop("need to add observational setting, where treatment assignment depends on X")
    }
}

PlotOWLSummaries <- function(X, treatment, response, 
                                               true.response.prob, plot.true.response.prob=TRUE,
                                               population.treatment.prob.among.responders, plot.population.treatment.prob.among.responders=TRUE,
                                               propensity.score, plot.propensity.score=FALSE,
                                              include.legend=FALSE) {
    get.n <- length(treatment)
    ord.X <- order(X)
    ord.X.treat.1 <- order(X[treatment==1])
    ord.X.treat.0 <- order(X[treatment==0])
    par(mar=c(6, 5, 4, 2))
    plot(X[treatment == 1][ord.X.treat.1], true.response.prob[treatment == 1][ord.X.treat.1],
         type="l", lwd=3, xlab="X", ylab="Probability", ylim=c(0, 1), lty="dashed", cex.lab=2, cex.axis=1.5, col=adjustcolor(col="blue", alpha=0.8))
    lines(X[treatment == 0][ord.X.treat.0], true.response.prob[treatment == 0][ord.X.treat.0],
          type="l", lwd=3, lty="dashed", col=adjustcolor(col="orange", alpha=0.8))
    legend.stuff <- c("P(Y=1 | T=1, X=x)", "P(Y=1 | T=0, X=x)")
    lty.stuff <- c("dashed", "dashed")
    col.stuff <- c(adjustcolor(col="blue", alpha=0.8),
                     adjustcolor(col="orange", alpha=0.8))
    lwd.stuff <- c(3, 3)
    if (plot.population.treatment.prob.among.responders == TRUE) {
        lines(X[ord.X], population.treatment.prob.among.responders[ord.X], type="l", lwd=6, col="black", lty="solid")
        legend.stuff <- c(legend.stuff, "P(T=1 | Y=1, X=x)")
        lty.stuff <- c(lty.stuff, "solid")
        col.stuff <- c(col.stuff, "black")
        lwd.stuff <- c(lwd.stuff, 3)
    }
    if (plot.propensity.score == TRUE) {
        if (length(propensity.score) == 1) {
            abline(a=propensity.score, b=0, col="forestgreen", lwd=5, lty="dotted")
        } else {
            stop("need to add")
        }
        legend.stuff <- c(legend.stuff, "P(T=1)")
        lty.stuff <- c(lty.stuff, "dotted")
        col.stuff <- c(col.stuff, "forestgreen")
        lwd.stuff <- c(lwd.stuff, 3)
    }
    if (include.legend==TRUE) {
        legend("bottomright",
               legend.stuff,
               lty=lty.stuff,
               col=col.stuff,
               lwd=lwd.stuff,
               cex=1.1)
    }
}
