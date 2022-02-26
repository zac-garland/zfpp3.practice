library(tidyverse)
library(rvest)
"https://otexts.com/fpp3/" %>%
  read_html() -> pg

sections <- pg %>%
  html_nodes("a") %>%
  map_dfr(~{
    tibble(
      section = .x %>% html_text(),
      href = .x %>% html_attr("href")
    ) %>%
      mutate(href = glue::glue("https://otexts.com/fpp3/{href}"))
  })

sections %>% slice(1:135)rowid_to_column() %>% tail(n = 25)

fpp3_out <- sections %>%
  slice(1:135) %>%
  rowid_to_column() %>%
  split(.$rowid) %>%
  map(~{
    htm <- .x  %>%
      pull(href) %>%
      read_html()

    code <- htm %>%
      html_nodes(".sourceCode") %>%
      html_nodes("code") %>%
      html_text()
    print(.x$section)
    if(length(code)>0){
      c(paste("#",unique(.x$section),"\n\n"),"```{r}\n\n",code,"\n\n```")
    }else{
      ""
    }




  })

fpp3_out %>% unlist() %>% clipr::write_clip()



