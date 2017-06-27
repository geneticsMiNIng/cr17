
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

fitReg <- function(time,
                   risk,
                   group,
                   data,
                   cens = 0){

    #preparing data
    data <- as.data.frame(data)
    timeVec <- data[,time]


    gr <- as.matrix(data[,group])
    cov <- model.matrix(~gr)[,-1]

    risks <- riskVec(data, risk, cens)

    nrOfRisks <- as.numeric(length(risks))

    reg <- lapply(1:nrOfRisks, function(x) {
               localStatus <- data[,risk]
               localCode <- risks[x]
               crr(ftime = timeVec,
               fstatus = localStatus,
               cov1 = cov,
               failcode = localCode)}
               )

    names(reg) <- risks
    reg
}


#' @title making cuminc data for plotting and testing
#' @name regTest
#' @description cox models for competing risks and differences in them between groups
#' @param reg time must be numeric
#' @param conf.int level of two sided conf int
#' @export
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
    rownames(p) <- "Modified LRT"

    as.data.frame(p)

    }


#x <- compRiskReg("time", "risk", "group", data)
