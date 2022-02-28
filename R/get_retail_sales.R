get_retail_sales <- function(){
  tmp_xl <- tempfile(fileext = ".xls")
  download.file("https://www.census.gov/retail/mrts/www/mrtssales92-present.xls", tmp_xl)
  on.exit(file.remove(tmp_xl),add=TRUE)
  excel_sheets <- readxl::excel_sheets(tmp_xl)

  excel_sheets %>% map_dfr(~{
    dat <- readxl::read_xls(tmp_xl, skip = 4,sheet = .x)

    dat %>%
      rename(naics_code = 1, retail_category = 2) %>%
      mutate(adjusted = case_when(retail_category == "ADJUSTED(2)" ~ TRUE, TRUE ~ NA)) %>%
      fill(adjusted) %>%
      mutate(adjusted = case_when(is.na(adjusted) ~ FALSE, TRUE ~ adjusted)) %>%
      filter(!adjusted) %>%
      gather(date, value, -(1:2)) %>%
      filter(str_length(naics_code) == 3) %>%
      mutate(
        date = str_replace(date, " ", " 01 ") %>% mdy(),
        value = parse_double(as.character(value))
      ) %>%
      filter(!is.na(date), !is.na(value))
  })

}
