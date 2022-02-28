
retail_sales <- get_retail_sales() %>%
  arrange(retail_category,date) %>%
  mutate(date = ceiling_date(date,"month")-1)


retail_sales %>%
  select(retail_category:value) %>%
  arrange(retail_category, date) %>%
  hchart("area", hcaes(date, value, group = retail_category)) %>%
  hc_plotOptions(
    area = list(stacking = "normal")
  )


retail_tsi <- retail_sales %>%
  mutate(date = tsibble::yearmonth(date)) %>%
  tsibble(key = retail_category,index = date) %>%
  group_by_key() %>%
  model(x11 = X_13ARIMA_SEATS(value ~ x11())) %>%
  components() %>%
  select(retail_category,date,value,season_adjust)



retail_hts <- retail_tsi %>%
  aggregate_key(retail_category, seas = sum(season_adjust,na.rm = TRUE))



fit <- retail_hts %>%
  model(base = ETS(seas),
        arm = ARIMA(seas)) %>%
  reconcile(
    bu = bottom_up(base),
    bu_ar = bottom_up(arm),
    mint = min_trace(base, method = "mint_shrink"),
    mint_ar = min_trace(arm, method = "mint_shrink")
  )

retail_hts


fc <- fit %>% forecast(h = "2 years")

fc %>%  class()
  as_tibble() %>%
  mutate(date = as_date(date),retail_category = as.character(retail_category)) %>%
  janitor::clean_names() %>%
  select(-seas) %>%
  split(.$retail_category) %>%
  imap(~{
    .x %>%
      hchart("line",hcaes(date,mean,group = model)) %>%
      hc_title(text = .y) %>%
      hc_tooltip(shared = TRUE)
  }) %>%
  hw_grid()

