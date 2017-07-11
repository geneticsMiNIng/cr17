

#' @title Competing Risks Models Summary.
#' @name crSummary
#' @description The function generates summarized report including
#' p-values of testing differences between groups and visualisation of survival
#' and cumulative incidences curves.
#' @param time name of a column indicating time of an event or follow-up, must be numeric.
#' @param risk name of a column indicating type of event, can be numeric or factor/character.
#' @param group name of a column indicating grouping variable, can be numeric or factor/character.
#' @param rho rho parameter from Fleming-Harrington Test.
#' @param data data.frame, data.table or matrix containing time, risk and group columns.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @param target point in time, in which the confidence bounds should be plotted (default NULL, no confidence bounds plotted).
#' @param type type of survival curve to be fitted. Possible values are "kaplan-meier" (default), "fleming-harrington" or "fh2".
#' @param conf.int level of two-sided confidence interval.
#' @param conf.type type of confidence interval. Possilble values: "none", "plain", "log" (default), "log-log".
#' @param ggtheme ggtheme to be used in plots (default: theme_minimal()).
#' @param titleSurv a title of a survival curves plot (default: "Survival curves").
#' @param titleCuminc a title of a cumulative incidences plot (default: "Cumulative incidence function").
#' @param xtitle a title of x axis of survival curves and cumulative incidences plots(default: "Time").
#' @param ytitleSurv a title of y axis of survial curves plot (default: "Probability of survivng up to time t").
#' @param ytitleCuminc a title of y axis (default: "Cumulative incidences").
#' @param legendtitle a title of a legend (default: "Group").
#' @return Results of functions implemented in the package summarised in a one-page raport.
#' @export
#' @examples crSummary(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive", target = 1200, type = "kaplan-meier",  conf.int = 0.95, conf.type = "log", ggtheme = theme_minimal(), titleSurv = "Survival curves", titleCuminc = "Cumulative incidence function", xtitle = "Time", ytitleSurv = "Probability of survivng up to time t", ytitleCuminc = "Cumulative incidences", legendtitle = "Group")
#' @importFrom gridExtra grid.arrange rbind.gtable tableGrob
#' @importFrom grid textGrob


crSummary <- function(time,
                      risk,
                      group,
                      data,
                      target = NULL,
                      cens = 0,
                      rho = 0,
                      type = "kaplan-meier",
                      conf.int = 0.95,
                      conf.type = "log",
                      ggtheme = theme_minimal(),
                      titleSurv = "Survival curves",
                      titleCuminc = "Cumulative incidence function",
                      xtitle = "Time",
                      ytitleSurv = "Probability of survivng up to time t",
                      ytitleCuminc = "Cumulative incidences",
                      legendtitle = "Group"

){

    fit <- fitSurvival(time, risk, group, data, cens, type, conf.int, conf.type)
    ci <- fitCuminc(time, risk, group, data, cens)

    lrtSurvTest <- lrtSurvival(time, risk, group, data, cens, rho)
    cumincTest <- testCuminc(ci)


    fitCox <- fitCox(time, risk, group, data, cens)
    CoxSurvTest <- testCox(fitCox)

    fitReg <- fitReg(time, risk, group, data, cens)
    CoxCompTest <-  regTest(fitReg, conf.int)

    #Plots
    plotSurvCurves <- plotSurvival(fit,
                                   target,
                                   ggtheme,
                                   titleSurv,
                                   xtitle,
                                   ytitleSurv,
                                   legendtitle)

    plotCumFun <- plotCuminc(ci,
                             target,
                             ggtheme,
                             titleCuminc,
                             xtitle,
                             ytitleCuminc,
                             legendtitle)

    #tables
    riskTable <- riskTab(time, risk, group, data, cens)
    # for(i in 1:length(riskTable)){
    #     assign(paste("riskTab", i, sep = ""), riskTable[[i]])
    # }
    eventTable <- eventTab(time, risk, group, data, cens)
    # for(i in 1:length(eventTable)){
    #     assign(paste("eventTab", i, sep = ""), eventTable[[i]])
    # }


    Test1 <- rbind(lrtSurvTest, CoxSurvTest)
    Test2 <- rbind(cumincTest, CoxCompTest)

    #Tests
    Tests <- arrangeGrob(tableGrob(Test1, theme = ttheme_minimal()),
                         tableGrob(Test2, theme = ttheme_minimal()),
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
