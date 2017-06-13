
#' @title making cuminc data for plotting and testing
#' @name compRiskReg
#' @description cox models for competing risks and differences in them between groups
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param conf.int level of two sided conf int

#' @export
#' @importFrom dplyr filter
#' @importFrom cmprsk crr
#' @importFrom gridExtra tableGrob

compRiskReg <- function(time,
                        risk,
                        group,
                        data,
                        cens = 0,
                        conf.int = 0.95){

    #preparing data
    data <- as.data.frame(data)
    timeVec <- data[,time]


    gr <- as.matrix(data[,group])
    cov <- model.matrix(~gr)[,-1]

    risks <- as.data.frame(unique(data[, risk]))
    risks <- filter(risks, risks != cens)
    risks <- risks[,1]
    nrOfRisks <- as.numeric(length(risks))

    reg <- lapply(1:nrOfRisks, function(x) {
               localStatus <- data[,risk]
               localCode <- risks[x]
               crr(ftime = timeVec,
               fstatus = localStatus,
               cov1 = cov,
               failcode = localCode)}
               )

    #counting lrt statistic for each risk
    #df = nr of groups - 1

    stat <- sapply(1:nrOfRisks, function(x){
         summary(reg[[x]], conf.int = conf.int)$logtest
    })

    p <- sapply(1:nrOfRisks, function(x){
        pchisq(stat[1,x], stat[2,x], lower.tail = FALSE)
    })

    p <- as.data.frame(t(p))
    colnames(p) <- risks
    rownames(p) <- "Modified LRT"

    tableGrob(p)

    # kable(p, digits = 4,
    #       caption = "Modified looglikelihood ratio test for competing risks",
    #       format.args = list(scientific = FALSE))
    }


#x <- compRiskReg("time", "risk", "group", data)
