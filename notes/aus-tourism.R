# 11.1 Hierarchical and grouped time series





tourism <- tsibble::tourism %>%
  mutate(State = recode(State,
                        `New South Wales` = "NSW",
                        `Northern Territory` = "NT",
                        `Queensland` = "QLD",
                        `South Australia` = "SA",
                        `Tasmania` = "TAS",
                        `Victoria` = "VIC",
                        `Western Australia` = "WA"
  ))


tourism_hts <- tourism %>%
  aggregate_key(State / Region, Trips = sum(Trips))


tourism_hts

tourism_hts %>%
  filter(is_aggregated(Region)) %>%
  autoplot(Trips) +
  labs(
    y = "Trips ('000)",
    title = "Australian tourism: national and states"
  ) +
  facet_wrap(vars(State), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")


tourism_full <- tourism %>%
  aggregate_key((State / Region) * Purpose, Trips = sum(Trips))


# 11.2 Single level approaches





tourism_states <- tourism %>%
  aggregate_key(State, Trips = sum(Trips))

fcasts_state <- tourism_states %>%
  filter(!is_aggregated(State)) %>%
  model(ets = ETS(Trips)) %>%
  forecast()

fcasts_state

# Sum bottom-level forecasts to get top-level forecasts
fcasts_national <- fcasts_state %>%
  summarise(value = sum(Trips), .mean = mean(value))

tourism_states %>%
  model(ets = ETS(Trips)) %>%
  reconcile(bu = bottom_up(ets)) %>%
  forecast()



# 11.4 Forecasting Australian domestic tourism





tourism_full <- tourism %>%
  aggregate_key((State / Region) * Purpose, Trips = sum(Trips))

fit <- tourism_full %>%
  filter(year(Quarter) <= 2015) %>%
  model(base = ETS(Trips)) %>%
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink"),
  )


fc <- fit %>% forecast(h = "2 years")

library(highcharter)
fc %>% hchart("line",hcaes(Quarter,Trips,group = .model))

fc %>%
  filter(is_aggregated(Region), is_aggregated(Purpose)) %>%
  autoplot(
    tourism_full %>% filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(State), scales = "free_y")


plot
plotly::ggplotly(plot)

fc %>%
  filter(is_aggregated(State), !is_aggregated(Purpose)) %>%
  autoplot(
    tourism_full %>% filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(Purpose), scales = "free_y")

fc %>%
  filter(is_aggregated(State), is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase))


