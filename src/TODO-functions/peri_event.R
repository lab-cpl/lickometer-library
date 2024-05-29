pacman::p_load(
  ggplot2,
  tidyverse,
  magrittr,
  furrr
)

# make fake data to test function

data <- read_csv("~/repos_sync/lickometer-library/src/TODO-functions/data.csv") %>%
  filter(
    droga %in% c("oxa_acsf_canulauni_pvn", "tcs_veh_canulauni_pvn"),
    outlier != "Excluido"
  )


# first step is to detect when an event was made and return
# a vector with the positions

peri_event <- function(data, win_size) {
  # get event locations
  events_loc <- which(dplyr::if_else(lag(data$evento, default = NA) != data$evento, 1, 0)==1)
  wins <-
    events_loc %>%
    imap(
      ., function(X, idx) {
        ts <- data[X, ]$tiempo
	# this vector contains the start and end of the window
        v <- c(ts - win_size, ts + win_size)
        dat <- data %>%
          filter(
            tiempo >= v[1] & tiempo <= v[2]
          ) %>%
          mutate(
            idx = idx,
            tiempo = tiempo - ts)
        return(dat)
      }, .progress = TRUE
    )
  return(wins)
}

plan(multisession, workers = 8)
wins <-
    data %>% 
    group_by(ID, tipo_recompensa, dosis, droga) %>% 
    group_split() %>% 
    future_map(., ~peri_event(.x, 1000*20)) %>% 
    bind_rows()

mdl_data <-
    wins %>% 
    filter(tipo_recompensa == "sac", tiempo > 0) %>% 
    group_by(ID, dosis, droga, idx) %>% 
    mutate(dosis = as.numeric(str_extract(dosis, "[0-9]+"))) %>%
    summarise(
        time_out_licks = n()
    ) %>% 
    ungroup()

mdl_data %>% 
    ggplot(aes(
        dosis, time_out_licks, color = droga
    )) +
    geom_hline(yintercept = 0) +
    stat_summary(
        fun.data = "mean_se",
        geom = "pointrange",
        aes(group = dosis)
    ) +
    facet_wrap(~droga, scales = "free")

mdl <- lmerTest::lmer(
    (time_out_licks) ~ (dosis) + (1 | ID),
    data = mdl_data %>%
        filter(droga == "tcs_veh_canulauni_pvn") %>% 
        mutate(dosis = (dosis))
)
summary(mdl)

qqnorm(residuals(mdl, type = "pearson"))
qqline(residuals(mdl, type = "pearson"))

mdl_emm <-
    emmeans::emmeans(
    mdl,
    pairwise ~ dosis,
    regrid = "response",
    at = list(dosis = c(0, 62, 125, 250))
    ) %>% 
    emmeans::test(., adjust = "mvt")
mdl_emm

mdl_raw <-
    mdl_data %>% 
    filter(droga == "tcs_veh_canulauni_pvn") %>% 
    group_by(ID, droga, dosis) %>% 
    summarise(response = mean(time_out_licks)) 

mdl_emm$emmeans %>% 
    as_tibble() %>% 
    ggplot(aes(
        dosis, response
    )) +
    geom_point(
        shape = 21,
        size = 5,
        color = "purple"
    ) +
    geom_point(
        data = mdl_raw,
        aes(color = as.factor(ID))
    ) +
    stat_summary(
        data = mdl_raw,
        fun.data = "mean_cl_boot",
        geom = "errorbar",
        aes(group = dosis)
    )


wins %>% 
    bind_rows %>% 
    filter(tipo_recompensa == "sac", tiempo > 0) %>% 
    group_by(ID, dosis, droga) %>% 
    mutate(dosis = as.numeric(str_extract(dosis, "[0-9]+"))) %>%
    summarise(
        hz = if_else(length(u_act)==0, 0, max(u_act)/20000)
    ) %>% 
    ungroup() %>% 
    group_by(ID, droga) %>% 
    ggplot(aes(
        dosis, hz, group = ID
    )) +
    geom_point() +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "errorbar",
        width = 20,
        lwd = 0.5,
        aes(group = dosis)
    ) +
    stat_summary(
        fun.data = "mean_cl_boot",
        geom = "line",
        lwd = 0.5,
        aes(group = 1)
    ) +
    geom_smooth(method = "lm", formula = y ~ x + splines::bs(x, 4), se = FALSE,
                aes(group = 1)) +
    facet_wrap(~droga, scales = "free")

