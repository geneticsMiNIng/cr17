#' @title simple cox model
#' @name simpleCox
#' @description The function fits cos models for each risk
#' treating other events as censoring
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param type "kaplan-meier", "fleming-harrington" or "fh2"
#' @param conf.int level of two sided conf int
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @param error "greenwood (default)', "tsiatis", "aalen
#' @export
#' @importFrom dplyr filter
#' @importFrom gridExtra tableGrob
#' @importFrom survival Surv coxph
simpleCox <- function(time,
                      risk,
                      group,
                      data,
                      cens = 0)
    {
    #data preparation
    data <- as.data.frame(data)
    timeVec <- data[,time]

    #risks - a vector indicating possible risk values
    risks <- as.data.frame(unique(data[, risk]))
    risks <- filter(risks, risks != cens)
    risks <- as.vector(risks[,1])
    nr_of_risks <- as.numeric(length(risks))

    fit <- lapply(risks, function(x){
        localrisk <- as.numeric(data[,risk] == x)
        localGroup <- factor(data[,group])
        summary(coxph(Surv(timeVec, localrisk)~localGroup))
        }
    )


    tab <- sapply(1:nr_of_risks, function(x){
       c(fit[[x]]$logtest[3], fit[[x]]$waldtest[3], fit[[x]]$sctest[3])
    })


    colnames(tab) <- risks
    rownames(tab) <- c("Likelihood ratio test", "Wald test", "Logrank test")

    tableGrob(tab)
#     kable(tab, digits = 4, caption = "Tests of differences in survival curves between groups using simple Cox model",
#         format.args = list(scientific = FALSE))

}
