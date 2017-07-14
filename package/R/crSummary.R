
#' @title Competing Risks Models Summary.
#' @name summarizeCR
#' @description The function generates summarized report including
#' p-values of testing differenzes between groups and visualisation of survival
#' and cumulative incidences curves.
#' @param time vector with times of an event or follow-up, must be numeric.
#' @param risk vector with type of event, can be numeric or factor/character.
#' @param group vector with group variable, can be numeric or factor/character.
#' @param cens value of 'risk' indicating censored observation (if NULL, the first value of 'risk' vector will be taken).
#' @param rho rho parameter from Fleming-Harrington Test.
#' @param target point in time, in which the confidence bounds should be plotted (default NULL, no confidence bounds plotted).
#' @param type type of survival curve to be fitted. Possible values are "kaplan-meier" (default), "fleming-harrington" or "fh2".
#' @param conf.int conf.int level of two-sided confidence interval (default = 0.95).
#' @param conf.type type of confidence interval. Possilble values: "none", "plain", "log" (default), "log-log".
#' @param ggtheme ggtheme to be used in plots (default: theme_minimal()).
#' @param titleSurv a title of a survival curves plot (default: "Survival curves").
#' @param titleCuminc a title of a cumulative incidences plot (default: "Cumulative incidence functions").
#' @param xtitle a title of x axis of survival curves and cumulative incidences plots(default: "Time").
#' @param ytitleSurv a title of y axis of survial curves plot (default: "Probability of survivng up to time t").
#' @param ytitleCuminc a title of y axis (default: "Cumulative incidences").
#' @param legendtitle a title of a legend (default: "Group").
#' @param riskTabTitle a title of table with number at risk.
#' @param eventTabTitle a title of table with number of events.
#' @return Results of functions implemented in the package summarised in a one-page raport.
#' @export
#' @examples
#' summarizeCR(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive")
#'
#' summarizeCR(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive",
#' target = 1200, type = "fleming-harrington",  conf.int = 0.99, conf.type = "log-log",
#' ggtheme = theme_bw())
#'
#' summarizeCR(time = LUAD$time/365, risk = LUAD$event, group = LUAD$gender, cens = "alive",
#' conf.type = "plain", rho = 1)
#'
#' summarizeCR(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive",
#' target = 800, type = "kaplan-meier", ggtheme = theme_gray(), titleSurv = "Survival analysis",
#' titleCuminc = "Competing risks models", xtitle = "Days", ytitleSurv = "Survival curves",
#' ytitleCuminc = "Cumulative incidence functions", legendtitle = "Gender")
#'
#' @importFrom gridExtra grid.arrange tableGrob arrangeGrob
#' @importFrom gtable gtable_add_grob
#' @importFrom grid segmentsGrob unit


summarizeCR <- function(time,
                      risk,
                      group,
                      cens = NULL,
                      rho = 0,
                      target = NULL,
                      type = "kaplan-meier",
                      conf.int = 0.95,
                      conf.type = "log",
                      ggtheme = theme_minimal(),
                      titleSurv = "Survival curves",
                      titleCuminc = "Cumulative incidence functions",
                      xtitle = "Time",
                      ytitleSurv = "Probability of survivng up to time t",
                      ytitleCuminc = "Cumulative incidences",
                      legendtitle = "Group",
                      riskTabTitle = "Number at risk",
                      eventTabTitle = "Number of events"

){


    fit <- fitSurvival(time, risk, group, cens, type, conf.int, conf.type)
    ci <- fitCuminc(time, risk, group, cens)

    lrtSurvTest <- testSurvival(time, risk, group, cens, rho)
    cumincTest <- testCuminc(ci)


    fitCox <- fitCox(time, risk, group, cens, conf.int = conf.int)
    CoxSurvTest <- testCox(fitCox)

    fitReg <- fitReg(time, risk, group, cens)
    CoxCompTest <-  testReg(fitReg, conf.int)

    #Plots
    plotSurvCurves <- plotSurvival(fit,
                                   target,
                                   ggtheme,
                                   titleSurv,
                                   xtitle,
                                   ytitleSurv,
                                   legendtitle)

    plotCumFun <- plotCuminc(ci,
                             cens,
                             target,
                             ggtheme,
                             titleCuminc,
                             xtitle,
                             ytitleCuminc,
                             legendtitle)

    #tables
    riskTable <- riskTab(time, risk, group, cens, riskTabTitle)
    # for(i in 1:length(riskTable)){
    #     assign(paste("riskTab", i, sep = ""), riskTable[[i]])
    # }
    eventTable <- eventTab(time, risk, group, cens, eventTabTitle)
    # for(i in 1:length(eventTable)){
    #     assign(paste("eventTab", i, sep = ""), eventTable[[i]])
    # }


    Tests <- rbind(lrtSurvTest, CoxSurvTest, cumincTest, CoxCompTest)

    #Tests
    Tests <- tableGrob(Tests, theme = ttheme_minimal())

    Tests <- gtable_add_grob(Tests,
                             grobs = segmentsGrob(
                                 x0 = unit(0, "npc"),
                                 y0 = unit(0, "npc"),
                                 x1 = unit(1, "npc"),
                                 y1 = unit(0, "npc"),
                                 gp = gpar(lwd = 2)),
                             t = 5, b = 5, l = 1, r = 3)


    lay <- rbind(c(1,2),
                 c(1,2),
                 c(1,2),
                 c(3,4),
                 c(3,5))

    grid.arrange(plotSurvCurves,
                 plotCumFun,
                 Tests,
                 riskTable,
                 eventTable,
                 layout_matrix = lay)

}
