
if(secondary) {

  library(exuber)

  # Estimation and critical values
radf_price <- 
  price %>% 
  radf(lag = 1)
radf_income <- 
  price_income %>% 
  radf(lag = 1)

mc_con <- 
  mc_cv(NROW(price), opt_bsadf = "conservative")

summary_price <- 
  radf_price %>% 
  summary(cv = mc_con)
summary_income <- 
  radf_income %>% 
  summary(cv = mc_con)

# we will need diagnostics to rename the plot output
diagnostics_price <- 
  radf_price %>% 
  diagnostics(cv = mc_con)
diagnostics_income <- 
  radf_income %>% 
  diagnostics(cv = mc_con)

cnames <- union(diagnostics_price$accepted, diagnostics_income$accepted)

datestamp_price <- 
  radf_price %>% 
  datestamp(cv = mc_con)
datestamp_income <- 
  radf_income %>% 
  datestamp(cv = mc_con)

# Individual plots
library(purrr)
library(ggplot2)


# autoplot ----------------------------------------------------------------


autoplot_price <- 
  radf_price %>% 
 autoplot(cv = mc_con) %>% 
  map( ~.x + ggtitle(""))

# for (dg in diagnostics_price$rejected) {
#   autoplot_price[[dg]] <- 
#     radf_price %>% 
#     with(bsadf) %>% 
#     as.tibble() %>% 
#     select(dg) %>% 
#     bind_cols(index = index(radf_price, trunc = TRUE),
#               cv = mc_cov %>% "[["(NROW(price)) %>% with(bsadf_cv) %>% "["(, 2)) %>%  
#     ggplot() + 
#     geom_line(aes_string(x = "index", y = as.name(dg)), size = 0.7, colour = "blue") +
#     geom_line(aes(x = index, y = cv), colour = "red", size = 0.8, linetype = "dashed") + 
#     ggtitle("") + theme_light() + ylab("") + xlab("")
#     
# }

autoplot_income <- 
  radf_income %>% 
  autoplot(cv = mc_con) %>% 
  map( ~.x + ggtitle(""))


# for (dg in diagnostics_income$rejected) {
#   autoplot_income[[dg]] <- radf_price %>% 
#     with(bsadf) %>% 
#     as.tibble() %>% 
#     select(dg) %>% 
#     bind_cols(index = index(radf_price, trunc = TRUE),
#               cv = crit %>% "[["(NROW(price)) %>% with(bsadf_cv) %>% "["(, 2)) %>%  
#     ggplot() + 
#     geom_line(aes_string(x = "index", y = as.name(dg)), size = 0.7, colour = "blue") +
#     geom_line(aes(x = index, y = cv), colour = "red", size = 0.8, linetype = "dashed") + 
#     ggtitle("") + theme_light() + ylab("") + xlab("")
#   
# }


# autoplot datestamp ------------------------------------------------------

autoplot_datestamp_price <- 
  datestamp_price %>% 
  autoplot()

autoplot_datestamp_income <- 
  datestamp_income %>% 
  autoplot()


# Plot -------------------------------------------------------------------

# House Prices plots
plot_price <- list()
for (i in seq_along(cnames)) {
  plot_price[[i]] <- ggplot(aes_string("Date", as.name(cnames[i])), 
                         data = price) +
    geom_line() + ylab("") + xlab("") +
    theme_light()
}
names(plot_price) <- cnames

# Personal Income plots
plot_income <- list()
for (i in seq_along(cnames)) {
  plot_income[[i]] <- ggplot(aes_string("Date", as.name(cnames[i])), data = price_income) +
    geom_line() + ylab("") + xlab("") +
    theme_light()
}
names(plot_income) <- cnames


# data export -------------------------------------------------------------

estimation_price <- 
  radf_price %>%
  pluck("bsadf") %>% 
  as_tibble() %>% 
  mutate(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())
  
estimation_income <- 
  radf_income %>%
  pluck("bsadf") %>% 
  as_tibble() %>% 
  mutate(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())




cv_seq <- mc_con %>% 
  pluck("bsadf_cv") %>% 
  as_tibble() %>% 
  "["(-1,) %>% 
  bind_cols(Date = index(radf_price, trunc = TRUE)) %>% 
  select(Date, everything())
# cv <- crit[[NROW(price)]] %>% 
#   "["(c("adf_cv", "sadf_cv", "gsadf_cv")) %>% 
#   reduce(rbind) %>% 
#   as_tibble() %>% 
#   cbind(tstat = c("ADF", "SADF", "GSADF")) %>% 
#   select(tstat, everything()) %>% 
#   as_tibble()

# save data ---------------------------------------------------------------

store <- c((items <- c("price", "income")),
           c("cnames"),
           paste0("summary_", items),
           paste0("datestamp_", items),
           paste0("plot_", items),
           paste0("autoplot_", items),
           paste0("autoplot_datestamp_", items))

path_store <- paste0("data/", store, ".rds")

for (i in seq_along(store)) saveRDS(get(store[i]), file = path_store[i])

}






