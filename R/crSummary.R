#fit <- fitSurvival("time", "risk", "group", data)


#crSummary("time", "event", "gender", LUAD, 1200, "alive")
#crSummary("time", "risk", "group", data, 4)
#crSummary("CI_time", "CI_status", "Heart", dt, 10)

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
#' @importFrom gridExtra grid.arrange rbind.gtable tableGrob
#' @importFrom grid textGrob


crSummary <- function(time,
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
    ci <- fitCuminc(time, risk, group, data, cens, rho)

    lrtSurvTest <- lrtSurvival(time, risk, group, data, cens, rho)
    cumincTest <- testCuminc(ci)


    fitCox <- fitCox(time, risk, group, data, cens)
    CoxSurvTest <- testCox(fitCox)

    fitReg <- fitReg(time, risk, group, data, cens)
    CoxCompTest <-  regTest(fitReg, conf.int)

    #Plots
    plotSurvCurves <- plotSurvival(fit, target)
    plotCumFun <- plotCuminc(ci, risk, group, target)

    #tables
    riskTable <- riskTab(fit)
    # for(i in 1:length(riskTable)){
    #     assign(paste("riskTab", i, sep = ""), riskTable[[i]])
    # }
    eventTable <- eventTab(fit, ci, risk, group)
    # for(i in 1:length(eventTable)){
    #     assign(paste("eventTab", i, sep = ""), eventTable[[i]])
    # }



    Test1 <- rbind(lrtSurvTest, CoxSurvTest)
    Test2 <- rbind(cumincTest, CoxCompTest)

    #Tests
    Tests <- arrangeGrob(tableGrob(Test1, theme = ttheme_minimal()),
                         tableGrob(Test2, theme = ttheme_minimal()),
                         top =  textGrob("Tests of differences betweeen groups",
                                         vjust = 3,
                                         gp = gpar(fontface = "bold")),
                         ncol = 2)


    lay <- rbind(c(1,2),
                 c(1,2),
                 c(1,2),
                 c(3,4),
                 c(5,5),
                 c(5,5))

    grid.arrange(plotSurvCurves,
                 plotCumFun,
                 riskTable,
                 eventTable,
                 Tests,
                 layout_matrix = lay)

}
