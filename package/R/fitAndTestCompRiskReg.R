
#' @title Regression Models for Competing Risks.
#' @name compRiskReg
#' @description Fitting Cox model (Regression model) for competing risks.
#' @description The function fits survival curves for each risk treating other events as censoring.
#' @param time vector with times of the first event or follow-up, must be numeric.
#' @param risk vector with type of event, can be numeric or factor/character.
#' @param group vector with group variable, can be numeric or factor/character.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @return a list of length n, where n is number of different types of events. Each element of a list contains a result of crr function from cmprsk package for given type of event.
#' @export
#' @examples fitReg(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive")
#' @importFrom dplyr filter
#' @importFrom cmprsk crr
#' @importFrom gridExtra tableGrob

fitReg <- function(time,
                   risk,
                   group,
                   cens = 0){

    #preparing data
    risks <- riskVec(risk, cens)
    nrOfRisks <- as.numeric(length(risks))

    groupMatrix <- as.matrix(group)
    covMatrix <- model.matrix(~groupMatrix)[,-1]

    reg <- lapply(1:nrOfRisks, function(x) {
               localCode <- risks[x]
               crr(ftime = time,
               fstatus = risk,
               cov1 = covMatrix,
               failcode = localCode)}
               )

    names(reg) <- risks
    reg
}


#' @title Regresion models difference testing
#' @name regTest
#' @description Testing differences in Competing Risks Regression Models between groups.
#' @param reg a result of fitReg function.
#' @param conf.int level of two-sided confidence interval (default 0.95).
#' @return a data.frame containing p-values of Modified Log-Rank Test for each type of event. The test compares differences between groups in Competing Risks Cox Models.
#' @export
#' @examples fitR <- fitReg(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive")
#' regTest(fitR)
#' @importFrom dplyr filter
#' @importFrom cmprsk crr
#' @importFrom gridExtra tableGrob grid.arrange

regTest <- function(reg, conf.int = 0.95){
    #counting lrt statistic for each risk
    #df = nr of groups - 1
    nrOfRisks <- length(reg)
    risks <- names(reg)

    stat <- sapply(1:nrOfRisks, function(x){
         tmp <- summary(reg[[x]], conf.int = conf.int)
         tmp$logtest
    })

    p <- sapply(1:nrOfRisks, function(x){
        pchisq(stat[1,x], stat[2,x], lower.tail = FALSE)
    })

    p <- round(p, digits = 4)


    p <- as.data.frame(t(p))


    colnames(p) <- risks
    rownames(p) <- "CompRisk LRT"

    as.data.frame(p)

    }
