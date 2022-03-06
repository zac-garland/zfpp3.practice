get_retail_sales()

sector_px <- sector_ticks %>%
  gather(etf_family,ticker,-(1:2)) %>%
  select(etf_family,sector,ticker,weight) %>%
  mutate(prices = map(ticker,tq_get))

sector_rets <- sector_px %>%
  unnest() %>%
  select(-symbol) %>%
  select(ticker,sector,date,weight,adjusted) %>%
  group_by(ticker) %>%
  mutate(adjusted_contr = adjusted*weight) %>%
  tidyquant::tq_mutate(select = adjusted,mutate_fun = dailyReturn) %>%
  # fill(weekly,monthly,quarterly,yearly) %>%
  na.omit() %>%
  ungroup()




sector_rets %>%
  group_by(sector,date) %>%
  summarize(total_ret = sum(adjusted,na.rm=TRUE)/2)

sector_rets %>%
  group_by(date) %>%
  summarize(total_ret = mean(daily.returns,na.rm=TRUE))

sector_index_chart <- function(df,start_date = (Sys.Date() %m-% years(3))){
  df %>%
    unnest() %>%
    select(sector,ticker,date,adjusted) %>%
    filter(date >= as_date(start_date)) %>%
    mutate(key = glue::glue("{sector} | {ticker}")) %>%
    mutate(year_mon = paste(week(date),year(date))) %>%
    group_by(key,year_mon) %>%
    filter(date == max(date,na.rm=TRUE)) %>%
    group_by(key) %>%
    arrange(key,date) %>%
    mutate(index = adjusted/head(adjusted) - 1) %>%
    group_by(sector,date) %>%
    summarize(index = mean(index,na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(sector,date) %>%
    hchart("line",hcaes(date,index,group = sector)) %>%
    hc_add_theme(hc_theme_darkunica()) %>%
    hc_tooltip(shared = TRUE,table = TRUE,valueDecimals = 2,sort = TRUE) %>%
    hc_rangeSelector(enabled = TRUE)

}


plot_index_splines <- function(df = sector_px){
  seq.Date((Sys.Date() - years(6)),Sys.Date(),by = "year") %>%
    floor_date("year") %>%
    rev() %>%
    map(~{
      df %>%
        sector_index_chart(.x)

    }) %>%
    hw_grid()

}

plot_index_splines(sector_px)


plot_stocks <- function(df){
  df %>% hchart("area",hcaes(date,total_ret,group = sector)) %>%
    hc_plotOptions(
      area = list(stacking = "normal")
    ) %>%
    highcharter::hc_rangeSelector(
      enabled = TRUE
    ) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_yAxis(
      title = list(text = "")
    ) %>%
    highcharter::hc_add_theme(highcharter::hc_theme_darkunica()) %>%
    hc_exporting(
      enabled = TRUE, filename = glue::glue("sp-zg"),
      sourceWidth = 800, sourceHeight = 500, scale = 10
    ) %>%
    hc_tooltip(valueDecimals = 2) %>%
    hc_plotOptions(series = list(connectNulls = TRUE))

}
