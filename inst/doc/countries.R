## -----------------------------------------------------------------------------
#| include: false
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width="100%",
  fig.width = 10,
  fig.align = 'center',
  message = FALSE,
  warning = FALSE
)


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
library(dplyr)
library(ggplot2)


## -----------------------------------------------------------------------------
library(necountries)


## -----------------------------------------------------------------------------
#| echo: false
sf::st_geometry(necountries:::Spain) %>% plot


## -----------------------------------------------------------------------------
ne_countries %>% as_tibble %>% select(- polygon, - point) %>%
    print(n = 2, width = Inf)


## -----------------------------------------------------------------------------
ne_towns %>% print(n = 2)


## -----------------------------------------------------------------------------
#| eval: false
## countries()


## -----------------------------------------------------------------------------
countries("France") %>% as_tibble %>% select(1:5)


## -----------------------------------------------------------------------------
fr_parts <- countries("France", part = TRUE)
fr_parts %>% pull(country)
countries("France", dependency = TRUE) %>% pull(country)


## -----------------------------------------------------------------------------
fr_parts %>% attr("bb") %>% plot(border = "red")
fr_parts %>% attr("bg") %>% plot(add = TRUE)


## -----------------------------------------------------------------------------
countries("Western Europe", part = TRUE) %>% pull(country)


## -----------------------------------------------------------------------------
countries(exclude = "Antarctica", coastlines = FALSE) %>% plot


## -----------------------------------------------------------------------------
countries(exclude = "Antarctica", coastlines = FALSE,
          include = c("Alaska", "Greenland")) %>% plot


## -----------------------------------------------------------------------------
we <- countries(c("France", "Spain"))
towns(we) %>% print(n = 2)


## -----------------------------------------------------------------------------
#| eval: false
## towns(c("France", "Spain"))


## -----------------------------------------------------------------------------
towns("Australia", size = 2E06)


## -----------------------------------------------------------------------------
towns("Australia", size = 2E06, capital = TRUE)


## -----------------------------------------------------------------------------
aus <- countries("Australia", towns = 2E06, capital = TRUE)


## -----------------------------------------------------------------------------
attr(aus, "towns")


## -----------------------------------------------------------------------------
countries(c("Portugal", "Spain"), towns = 1E06, capital = TRUE) %>%
    labels(var = c("country", "towns", "capital"))


## -----------------------------------------------------------------------------
countries("Russia", coastlines = FALSE) %>% plot


## -----------------------------------------------------------------------------
countries("Russia", coastlines = FALSE, shift = TRUE) %>% plot


## -----------------------------------------------------------------------------
countries("Europe", utm = TRUE, extend = 1.1,
          include = c("Turkey", "Cyprus", "Northern Cyprus"),
          exclude = "Russia") %>% plot


## -----------------------------------------------------------------------------
countries("Europe", crs = 3034, extend = 1.5,
          include = c("Turkey", "Cyprus", "Northern Cyprus"),
          exclude = "Russia") %>% plot


## -----------------------------------------------------------------------------
countries("Asia", exclude = "Russia") %>% plot(fill = "economy")


## -----------------------------------------------------------------------------
#| eval: false
## countries(c("Asia"), exclude = "Russia") %>%
##     plot(fill = "economy", palette = "Dark2")


## -----------------------------------------------------------------------------
countries("Europe", exclude = "Russia") %>%
    plot(fill = "pop", bks = c(0, 1E06, 5E06, 1E07, 5E07, 1E08, Inf))


## -----------------------------------------------------------------------------
#| eval: false
## countries("Europe", exclude = "Russia") %>%
##     plot(fill = "pop", bks = c(0, 1E06, 5E06, 1E07, 5E07, 1E08, Inf),
##          palette = "PuOr")


## -----------------------------------------------------------------------------
countries("Europe", exclude = "Russia") %>%
    plot(fill = "pop", n = 10, style = "pretty",
         palette = "Oranges")


## -----------------------------------------------------------------------------
countries("Europe", exclude = "Russia", capital = TRUE, towns = 1E06) %>%
    plot(fill = "pop", n = 10, style = "pretty",
         palette = "Oranges")


## -----------------------------------------------------------------------------
countries("Europe", exclude = "Russia") %>%
    plot(fill = "pop", centroid = "gdp", n = 10, style = "pretty",
         palette = "Oranges")


## -----------------------------------------------------------------------------
countries("Europe", exclude = "Russia", capital = TRUE) %>%
    plot(fill = "pop", capital = "income", n = 10, style = "pretty",
         palette = "Oranges")


## -----------------------------------------------------------------------------
#| fig.height: 10
#| output.width: "70%"
countries("Europe", exclude = "Russia", capital = TRUE, lang = "es") %>%
    plot(fill = "pop", capital = "income", n = 10, style = "pretty",
         palette = "Oranges", labels = "country") +
    labs(x = NULL, y = NULL) +
    guides(fill = "none", shape = "none")


## -----------------------------------------------------------------------------
#| fig.height: 10
#| output.width: "70%"
countries("Western Europe", capital = TRUE, towns = 1E06) %>%
    plot(fill = "pop", capital = "income", n = 4, style = "pretty",
         palette = "Oranges", labels = c("country", "capital", "towns")) +
    labs(x = NULL, y = NULL) +
    guides(fill = "none", shape = "none")


## -----------------------------------------------------------------------------
slave_trade <- slave_trade %>%
    mutate(slaves = slaves / pop) %>%
    select(country, slaves, gdp, colony)


## -----------------------------------------------------------------------------
#| eval: true
countries("Africa") %>%
    check_join(slave_trade, by = "country", side = "both")


## -----------------------------------------------------------------------------
#| eval: TRUE
slave_trade <- slave_trade %>%
    mutate(country = case_when(country == "Democratic Republic of Congo" ~ "D.R. Congo",
                               country == "Cape Verde Islands" ~ "Cabo Verde",
                               country == "Sao Tome & Principe" ~ "Sao Tome and Principe",
                               country == "Swaziland" ~ "eSwatini",
                               .default = country))


## -----------------------------------------------------------------------------
#| eval: true
strade <- countries("Africa", capital = TRUE) %>% select(iso2:status, point) %>% 
    left_join(slave_trade, "country")
strade %>% plot(fill = "slaves", n = 5, type = "pretty", capital = "gdp")


## -----------------------------------------------------------------------------
sp_solow %>% print(n = 3)


## -----------------------------------------------------------------------------
sp_solow <- sp_solow %>%
    mutate(growth = (gdp95 / gdp60) ^ (1 / 35) - 1)


## -----------------------------------------------------------------------------
#| eval: true
countries() %>% check_join(sp_solow, by = "code")


## -----------------------------------------------------------------------------
#| eval: true
sp_solow <- sp_solow %>% mutate(code = ifelse(code == "ZAR", "COD", code))
sps <- countries(include = "Hong Kong", exclude = "Antarctica") %>%
    select(iso2:status, point) %>% 
    left_join(sp_solow, by = "code")


## -----------------------------------------------------------------------------
#| eval: true
sps %>% plot(fill = "growth", centroid = "gdp60")


## -----------------------------------------------------------------------------
#| echo: false
#| results: 'asis'
deps <- anti_join(necountries:::countries_list, necountries:::sovereignty_list, by = "country") %>%
    select(country, sovereign) %>%
    filter(sovereign != "Israel") %>% 
    tidyr::nest(.by = sovereign)
names_deps <- deps[[1]]
deps <- purrr::map(deps[[2]], ~ paste(.x[[1]], collapse = ", "))
names(deps) <- names_deps
for (i in 1:length(deps)){
    sov <- names(deps)[i]
    adep <- deps[i]
    cat(paste("- **", sov, "**: ", adep, "\n", sep = ""))
}

