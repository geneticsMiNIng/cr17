
fullAnalysis("time", "risk", "group", data, 5)

#' @title full analysis of data
#' @name fullAnalysis
#' @description The functionn
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can
#'  be data frame or matrix
#' @param type "kaplan-meier", "fleming-harrington" or "fh2"
#' @param conf.int level of two sided conf int
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @param error "greenwood (default)', "tsiatis", "aalen
#' @export
#' @importFrom gridExtra grid.arrange


fullAnalysis <- function(time,
                        risk,
                        group,
                        data,
                        target,
                        cens = 0,
                        rho = 0,
                        type = "kaplan-meier",
                        conf.int = 0.95,
                        conf.type = "log"
){
    fit <- fitSurvival(time, risk, group, data, cens, type, conf.int, conf.type)

    plotSurvCurves <- plotSurvival(fit, target)

    lrtSurvTest <- lrtSurvival(time, risk, group, data, cens, rho)

    CoxSurvTest <- simpleCox(time, risk, group, data, cens)

    CoxCompTest <-  compRiskReg(time, risk, group, data, cens, conf.int)

    cumincTest <- testCuminc(time, risk, group, data, rho, cens)

    plotCumFun <- plotCuminc(time, risk, group, data, target, rho, cens)

    lay <- rbind(c(1,1,3,3),
                 c(1,1,4,4),
                 c(2,2,5,5),
                 c(2,2,6,6))

    grid.arrange(plotSurvCurves,plotCumFun, lrtSurvTest, CoxSurvTest, CoxCompTest, cumincTest,
                 layout_matrix = lay)


}
