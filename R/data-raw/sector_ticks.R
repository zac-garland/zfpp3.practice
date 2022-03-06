sector_ticks <- sector_ticks %>%
  read_html("") %>%
  html_table() %>%
  as.data.frame() %>%
  as_tibble() %>%
  slice(2:12) %>%
  setNames(c("sector", "weight", "vanguard",
             "spdr")) %>%
  mutate(weight = parse_double(weight %>% str_remove("%")) %>% {./100})

usethis::use_data(sector_ticks)
