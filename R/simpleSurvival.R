

#do survfit można jeszcze dodać weights, subset, na.action,
#pozwalam tylko na right censoring
#risk powinny móc być factor, przetestuj to

#fit <- fitSurvival("time", "risk", "group", data)
#fit <- fitSurvival("CI_time", "CI_status", "DM", dt)

#' @title Preparing data for plots and tables
#' @name fitSurvival
#' @description The function fits Kaplan-Mayer models for each risk
#' treating other events as censoring
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
#' @importFrom dplyr filter
#' @importFrom survival Surv survfit

fitSurvival <- function(time,
                  risk,
                  group,
                  data,
                  cens = 0,
                  type = "kaplan-meier",
                  conf.int = 0.95,
                  conf.type = "log"
                  )

    {

    #errors
    stopifnot(type %in% c("kaplan-meier", "fleming-harrington", "fh2"))
    stopifnot(conf.type %in% c("none", "plain", "log" , "log-log"))

    #data preparation
    data <- as.data.frame(data)
    timeVec <- data[, time]
    groups <- factor(data[, group])
    #timeVec <- as.numeric(as.data.frame(timeVec[,1]))

    risks <- as.data.frame(unique(data[, risk]))
    risks <- filter(risks, risks != cens)
    risks <- risks[,1]
    nrOfRisks <- as.numeric(nrow(risks))

    #for each risk we fit a survfit object, trating other events as censoring
    #fit is a list of survfit objects assuming other events as censoring
    fit <- lapply(risks, function(x) {
        localStatus <- {data[,risk] == x}
        summary(survfit(Surv(timeVec, localStatus)~groups,
                type = type,
                conf.int = conf.int,
                conf.type = conf.type
                ))
    })

    names(fit) <- risks
    fit
}

#jak zrobić te errorbars na każdym z osobna dla niewiadomej liczby wykresów???
#' @title ploting
#' @name plotSurvival
#' @description The function plots the survival curves for both risks and within all groups
#' @param time time must be numeric
#' @param status can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param type "kaplan-meier", "fleming-harrington" or "fh2"
#' @param conf.int level of two sided conf int
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @param error "greenwood (default)', "tsiatis", "aalen
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr filter


plotSurvival <- function(fit, target){

    #defining risks
    risks <- names(fit)
    risks <- factor(risks)

    #dealing with factor names of strata
    badGroupNames <- levels(fit[[1]]$strata)
    strataMapping <- 1:length(badGroupNames)
    #ISSUE nazwy grup nie moga mieć w środku '='
    groups <- sapply(as.character(badGroupNames), function(x) strsplit(x, split = "=")[[1]][2])
    strataMapping <- cbind(strataMapping, groups)
    colnames(strataMapping) <- c("strata", "group")

    toPlot <- c()
    for(i in risks){
        tmp <- cbind(fit[[i]]$time,
                     fit[[i]]$surv,
                     fit[[i]]$strata,
                     fit[[i]]$lower,
                     fit[[i]]$upper,
                     rep(i, times = length(fit[[i]]$time)))

        tmp <- as.data.frame(tmp)
        toPlot <- as.data.frame(rbind(toPlot, tmp))
    }

    colnames(toPlot) <- c("time", "prob", "strata", "lowerBound", "upperBound", "risk")
    toPlot <- merge(toPlot, strataMapping, by = "strata")

    toPlot$time <- as.numeric(as.character(toPlot$time))
    toPlot$prob <- as.numeric(as.character(toPlot$prob))
    toPlot$lowerBound <- as.numeric(as.character(toPlot$lowerBound))
    toPlot$upperBound <- as.numeric(as.character(toPlot$upperBound))
    toPlot <- toPlot[, !names(toPlot) %in% "strata"]


    #adding starting points
    zeros <- expand.grid(risks, groups)
    colnames(zeros) <- c("risk", "group")
    zeros$time <- 0
    zeros$prob <- 1
    zeros$lowerBound <- 1
    zeros$upperBound <- 1

    zeros <- zeros[, colnames(toPlot)]

    toPlot <- rbind(toPlot, zeros)

    barsData <- expand.grid(risks, groups)

    #function making confidence bounds for each risk and each group
    bounds <- function(ri, gr, target){
        ri <- as.character(ri)
        gr <- as.character(gr)
        tmp <- as.data.frame(filter(toPlot, toPlot$risk == ri & toPlot$group == gr))
        whichTime <- which(tmp$time <= target)
        nr <- length(whichTime)
        lower <- tmp$lowerBound[nr]
        upper  <- tmp$upperBound[nr]
        prob <- tmp$prob[nr]
        c(lower, prob, upper)
    }

    low <- c()
    up <- c()
    prob <- c()
    for(i in 1:nrow(barsData)){
        tmpBounds <- as.numeric(bounds(barsData[i,1],barsData[i,2],target))
        low <- c(low, tmpBounds[1])
        prob <- c(prob, tmpBounds[2])
        up <- c(up, tmpBounds[3])

    }

    barsData <- cbind(barsData, low, prob, up)
    colnames(barsData)[1:2] <- c("risk", "group")
    rm(low,up, prob)



    #making a plot
    plot1 <- ggplot(data = toPlot, aes(time, prob, color = group)) +
        geom_step(size=1) +
        facet_grid(~risk, scales = "free")

    #adding errorbars
    plot1 <- plot1 +
        geom_errorbar(data = barsData, aes(x = target, ymin = low, ymax = up), size = 0.75, alpha = 0.7)

    #making it beauty
    plot1 <- plot1 +
        theme_minimal() +
        ggtitle("Survival curves") +
        theme(plot.title = element_text(size=13, face="bold", hjust = 0.5)) +
        scale_y_continuous("Probability of survivng up to time t", limits = c(0,1)) +
        scale_x_continuous("Time")+
        theme(legend.title = element_text(size=10, face="bold"))+
        scale_color_discrete(name="Group", labels = groups)

}
