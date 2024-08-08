pacman::p_load(
    tidyverse,
    ggplot2,
    lme4
)

# create example demand curve for comparison
demand_data <- tibble(
    x = rep(c(1:100), 100),
    rfact = abs(rep(rnorm(n=100, mean=rep(c(10,5), each = 1), sd=1), each=100)),
    y = rep(exp(-0.05*1:100), 100)*rfact,
    id = rep(1:50, each=200),
    group = rep(rep(c("trt", "ctrl"), each = 100), 50)
)
p1 <- demand_data %>% 
    ggplot(aes(
        x, y, color = group, group = interaction(id, group)
    )) +
    geom_line() +
    facet_wrap(~id)
p1


# formula
k <- beezdemand::GetK(demand_data)
demand_data[, "k"] <- k
fo <- y ~ q0 * 10^(k * (exp(-alpha * q0 * x) - 1))
startq0 <- max(demand_data$y)
startalpha <- 0.01
fit <- demand_data %>% 
    group_by(id) %>% 
    group_split() %>% 
    map_dfr(
        ., function(X){
            fit_ <- nlsr::wrapnlsr(formula = fo,
                                  start = list(q0 = startq0, alpha = startalpha),
                                  lower = c(-Inf, -Inf),
                                  upper = c(Inf, Inf), data = X)
            return(
                tibble(
                    Q0d = coefficients(fit_)[1],
                    Alpha = coefficients(fit_)[2]
                    ))
        }
    )

# non linear mixed effects model
# fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
#             data = Loblolly,
#             fixed = Asym + R0 + lrc ~ 1,
#             random = Asym ~ 1,
#             start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
demand_function <- function(x, alpha, q0, alpha2, q02, group){
    (q0*q02+group * 10^(2.78294 * (exp(-alpha*alpha2+group * q0*q02+group * x) - 1)))
}
nfun <- deriv(
    body(demand_function)[[2]],
    namevec = c("alpha", "q0", "alpha2", "q02"),
    function.arg = demand_function
)

me_data <- demand_data %>% 
    mutate(
        # y = scales::rescale(y, to = c(0, 1)),
        # x = scales::rescale(x, to = c(0, 1)),
        group = as.numeric(as.factor(group))-1,
        id = as.factor(id)
    )

mefit <- lme4::nlmer(
    y ~ nfun(x=x, alpha, q0, alpha2, q02, group=group) ~ (q0|id) + (alpha|id),
    data = me_data,
    verbose = TRUE,
    start = c(alpha = 0.001, q0 = 0.5, alpha2 = 0.001, q02 = 0.5),
    control = nlmerControl(optimizer = "bobyqa" ,
                           optCtrl = list(maxfun= 100000))
)

# fit beezdemand as reference
source("~/repos_sync/lickometer-library/src/lickometer_functions_compilate.R")
beez_fit <- beezdemand::FitCurves(
    as.data.frame(demand_data) %>% 
        mutate(id = interaction(id, group)),
    equation = "koff",
    xcol = "x",
    ycol = "y",
    idcol = "id",
    detailed = TRUE
)$dfres

lib <- tibble(
    Q0d = beez_fit$Q0d,
    Alpha = beez_fit$Alpha,
    type = "lib",
    id = beez_fit$id
)

lib_long <- lib %>% 
    pivot_longer(
        cols = c("Q0d", "Alpha")
    )

lib_long %>% 
    group_by(name, stringr::str_extract(id, "[a-z]+")) %>% 
    summarise(
        m = mean(value),
        se = sd(value)/sqrt(n())
    )
summary(mefit[[1]])
# q0     0.50252    0.28911
# alpha  2.28109    0.04756

mdl <- lm(
    data = lib %>%
        mutate(
            group = stringr::str_extract(id, "[a-z]+"),
            id = stringr::str_extract(id, "[0-9]+")
        ),
    Q0d ~ group
)
summary(mdl)

emmeans::emmeans(
    mdl, ~ group
)


lib_long %>% 
    ggplot(aes(
        group, value, color = group
    )) +
    stat_summary(
        fun.data = "mean_se",
        aes(group = group),
        geom = "errorbar",
        width = 0.2
    ) +
    facet_wrap(~name, scales = "free") +
    theme(legend.position = "none")
