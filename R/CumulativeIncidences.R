# plotCuminc("time", "risk", "group", data, 10) -> x
# plotCuminc("time", "event", "gender", LUAD, 500, cens = "alive") -> x
# plotCuminc("CI_time", "CI_status", "DM", dt, 5) -> x

#' @title making cuminc data for plotting and testing
#' @name plotCuminc
#' @description list with cumulative incidences and a test of differences between them
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param conf.int level of two sided conf int
#' @param target conf intervals point
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @export
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc


plotCuminc <- function(time,
                        risk,
                        group,
                        data,
                        target,
                        rho = 0,
                        cens = 0){



    data <- as.data.frame(data)

    risks <- as.data.frame(unique(data[, risk]))
    risks <- filter(risks, risks != cens)
    uniRisks <- 1:nrow(risks)
    mapRisks <- cbind(risks, uniRisks)
    colnames(mapRisks)[1] <- risk

    groups <- as.data.frame(unique(data[, group]))
    groups <- filter(groups, !is.na(groups))
    uniGroups <- letters[1:nrow(groups)]
    mapGroups <- cbind(groups, uniGroups)
    colnames(mapGroups)[1] <- group

    data <- merge(data, mapRisks, by = risk)
    data <- merge(data, mapGroups, by = group)

    ci <- cuminc(ftime = data[, time],
                 fstatus = data[, "uniRisks"],
                 group = data[, "uniGroups"],
                 rho = rho,
                 cencode = cens)
    #delete testing
    ci <- ci[-length(ci)]

    #make long format
    aggNames <- names(ci)
    toPlot <- c()

    for(i in aggNames){
        tmp <- as.data.frame(ci[[i]])
        tmp$name <- i
        toPlot <- as.data.frame(rbind(toPlot, tmp))
        }

    riskGroup <- sapply(toPlot$name, function(x){
        unlist(strsplit(x, split = " "))
    })


    riskGroup <- as.data.frame(riskGroup)
    riskGroup <- t(riskGroup)
    colnames(riskGroup) <- c("uniGroups", "uniRisks")
    rownames(riskGroup) <- NULL

    toPlot <- cbind(toPlot, riskGroup)
    toPlot <- toPlot[, !names(toPlot) %in% "name"]

    toPlot <- merge(toPlot, mapRisks, by = "uniRisks")
    toPlot <- merge(toPlot, mapGroups, by = "uniGroups")

    toPlot <- toPlot[, !names(toPlot) %in% c("uniGroups", "uniRisks")]
    colnames(toPlot)[4:5] <-  c(risk, group)


    #adding conf intervals
    toPlot$lowerBound <- sapply(1:nrow(toPlot), function(x){
        est <- toPlot[x, "est"]
        var <- toPlot[x, "var"]
        exp(log(est) - 1.96*sqrt(var)/est)
    })

    toPlot$upperBound <- sapply(1:nrow(toPlot), function(x){
        est <- toPlot[x, "est"]
        var <- toPlot[x, "var"]
        exp(log(est) + 1.96*sqrt(var)/est)
    })

    barsData <- expand.grid(risks[,1], groups[,1])
    colnames(barsData) <- c("fac", "col")
    barsData <- as.data.frame(barsData)


    colnames(toPlot)[which(colnames(toPlot) == risk)] <- "fac"
    colnames(toPlot)[which(colnames(toPlot) == group)] <- "col"


    #looking for needed conf bounds
    bounds <- function(ri, gr, target){
        ri <- as.character(ri)
        gr <- as.character(gr)
        tmp <- as.data.frame(filter(toPlot, fac == ri & col == gr))
        whichTime <- which(tmp$time <= target)
        nr <- length(whichTime)
        lower <- tmp$lowerBound[nr]
        upper  <- tmp$upperBound[nr]
        est <- tmp$est[nr]
        c(lower, est, upper)

    }

    low <- c()
    up <- c()
    est <- c()
    for(i in 1:nrow(barsData)){
        tmpBounds <- as.numeric(bounds(barsData[i,1],barsData[i,2], target))
        low <- c(low, tmpBounds[1])
        est <- c(est, tmpBounds[2])
        up <- c(up, tmpBounds[3])

    }

    barsData <- cbind(barsData, low, est, up)
    rm(low,up, est)


    #making a plot
    plot1 <- ggplot(data = toPlot, aes(time, est, color = col)) +
        geom_step(size=1) +
        facet_grid(~fac, scales = "free")

    #adding errorbars
    plot1 <- plot1 +
        geom_errorbar(data = barsData, aes(x = target, ymin = low, ymax = up), size = 0.75, alpha = 0.7)

    #making it beauty
    plot1 <- plot1 +
        theme_minimal() +
        ggtitle("Cumulative incidence function") +
        theme(plot.title = element_text(size=13, face="bold", hjust = 0.5)) +
        scale_y_continuous("Cumulative incidences", limits = c(0,1)) +
        scale_x_continuous("Time")+
        theme(legend.title = element_text(size=10, face="bold"))+
        scale_color_discrete(name="Group", labels = groups)

}




#' @title Log rank test for competing risks
#' @name testCuminc
#' @description Log rank test for competing risks
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param conf.int level of two sided conf int
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @export
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc
#' @importFrom gridExtra tableGrob

testCuminc <- function(time,
                       risk,
                       group,
                       data,
                       rho = 0,
                       cens = 0){

    data <- as.data.frame(data)

    ci <- cuminc(ftime = data[,time],
                 fstatus = data[, risk],
                 group = data[,group],
                 rho = rho,
                 cencode = cens)

    tab <- as.data.frame(ci[[length(ci)]])
    p <- as.data.frame(t(tab$pv))
    colnames(p) <- rownames(tab)
    rownames(p) <- "Log rank test for competing risks"

    tableGrob(p)


    # kable(p, digits = 4,
    #       caption ="Log rank test for competing risks" ,
    #       format.args = list(scientific = FALSE))
    }
