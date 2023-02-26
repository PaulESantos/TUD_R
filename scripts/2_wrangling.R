################################################################################
#                                                                              #
#                     "Reproducible Data Analysis with R"                      #
#                     Cédric Scherer | Feb 27-Mar 3, 2023                      #
#                                                                              #
#                              --- Session 2 ---                               #
#                    "Data Wrangling with the {tidyverse}"                     #
#                                                                              #
################################################################################


## PREPARATION #################################################################

## This session makes use of the following packages:
##  - tibble
##  - tidyr
##  - dplyr
##  - forcats
##  - stringr
##  - lubridate
##  - hms
##  - ggplot2
##  - glue
##  - babynames
##  - nycflights13

## Please install the missing packages by running the following:
pkgs <- c("tibble", "tidyr", "dplyr", "forcats", "stringr", "lubridate", 
          "hms", "ggplot2", "glue", "babynames", "nycflights13")
unavailable <- setdiff(pkgs, rownames(installed.packages()))
install.packages(unavailable)


## That's it---let's start!




## -----------------------------------------------------------------------------
# install.packages("babynames")
library(babynames)


## -----------------------------------------------------------------------------
tibble::glimpse(babynames)


## -----------------------------------------------------------------------------
1 %/% 10
11 %/% 10
17 %/% 10


## -----------------------------------------------------------------------------
2019 %/% 10
2020 %/% 10


## -----------------------------------------------------------------------------
2019 %/% 10 * 10
2020 %/% 10 * 10


## -----------------------------------------------------------------------------
# install.packages("tidyverse")


## -----------------------------------------------------------------------------
# install.packages("dplyr")
library(dplyr)


## -----------------------------------------------------------------------------
filter(babynames, year == 2000, n > 25000)


## -----------------------------------------------------------------------------
babynames[babynames$year == 2000 & babynames$n > 25000, ]


## -----------------------------------------------------------------------------
filter(babynames, (name == "Cedric" | name == "Clarissa"))


## -----------------------------------------------------------------------------
filter(babynames, (name == "Cedric" | name == "Clarissa") & year %in% 1900:1905)


## -----------------------------------------------------------------------------
select(babynames, name, year, n)


## -----------------------------------------------------------------------------
babynames[, c("name", "year", "n")]


## -----------------------------------------------------------------------------
select(babynames, name, year, n)


## -----------------------------------------------------------------------------
select(babynames, -prop, -sex)


## -----------------------------------------------------------------------------
arrange(babynames, prop)


## -----------------------------------------------------------------------------
babynames[order(babynames$prop), ]


## -----------------------------------------------------------------------------
arrange(babynames, year, sex, -prop)


## -----------------------------------------------------------------------------
mutate(babynames, decade = year %/% 10 * 10)


## -----------------------------------------------------------------------------
head(transform(babynames, decade = year %/% 10 * 10))


## -----------------------------------------------------------------------------
mutate(babynames, name = stringr::str_to_lower(name))


## -----------------------------------------------------------------------------
mutate(babynames, note = "Please check!")


## -----------------------------------------------------------------------------
mutate(babynames, note = if_else(year < 1900, "Please check!", "Ok"))


## -----------------------------------------------------------------------------
mutate(babynames, id = row_number())


## -----------------------------------------------------------------------------
# install.packages("ggplot2")  ## contains the `mpg` data set
mpg <- ggplot2::mpg  ## we need some more numeric columns


## -----------------------------------------------------------------------------
mpg


## -----------------------------------------------------------------------------
mpg2 <- select(mpg, model, hwy, cty) ## only to fit my slide


## -----------------------------------------------------------------------------
mpg2


## -----------------------------------------------------------------------------
mutate(mpg2, diff = hwy - cty, perc = diff / hwy)


## -----------------------------------------------------------------------------
mpg_diff <- transform(mpg2, diff = hwy - cty)
head(transform(mpg_diff, perc = diff / hwy))


## -----------------------------------------------------------------------------
mutate(mpg2, across(everything(), mean))


## -----------------------------------------------------------------------------
mutate(mpg2, across(everything(), mean, na.rm = TRUE))


## -----------------------------------------------------------------------------
mutate(mpg2, across(everything(), ~mean(.x, na.rm = TRUE)))


## -----------------------------------------------------------------------------
mutate(mpg2, across(where(is.numeric), as.character))


## -----------------------------------------------------------------------------
mutate(mpg2, across(where(is.numeric), list(avg = mean, log_2 = log2)))


## -----------------------------------------------------------------------------
mutate(mpg2, across(where(is.numeric), list(~mean(., na.rm = TRUE), ~log2(.))))


## -----------------------------------------------------------------------------
mutate(mpg2, across(c("hwy", "cty"), mean, na.rm = TRUE))


## -----------------------------------------------------------------------------
mutate(mpg2, across(everything(), list(~mean(., na.rm = TRUE), ~nchar(.))))


## -----------------------------------------------------------------------------
summarize(babynames, unique_children = sum(n, na.rm = TRUE))


## -----------------------------------------------------------------------------
sum(babynames$n, na.rm = TRUE)


## -----------------------------------------------------------------------------
summarize(babynames, n = n())


## -----------------------------------------------------------------------------
group_by(babynames, sex)


## -----------------------------------------------------------------------------
group_by(babynames, sex, year)


## -----------------------------------------------------------------------------
grouped <- group_by(babynames, sex, year)
ungroup(grouped)


## -----------------------------------------------------------------------------
(names_group <- group_by(babynames, sex))


## -----------------------------------------------------------------------------
summarize(names_group, sum = sum(n))


## -----------------------------------------------------------------------------
babies1 <- filter(babynames, year %in% range(year))


## -----------------------------------------------------------------------------
babies2 <- group_by(babies1, year)


## -----------------------------------------------------------------------------
babies2


## -----------------------------------------------------------------------------
babies3 <- mutate(babies2, n_year = sum(n))


## -----------------------------------------------------------------------------
babies3


## -----------------------------------------------------------------------------
summarize(babies2, n_year = sum(n))


## -----------------------------------------------------------------------------
babies4 <- group_by(babies3, sex, year)


## -----------------------------------------------------------------------------
babies4


## -----------------------------------------------------------------------------
babies5 <- mutate(babies4, n_sex_year = sum(n), prop_sex_year = n_sex_year / n_year)


## -----------------------------------------------------------------------------
babies5


## -----------------------------------------------------------------------------
babies5 <- summarize(babies4, n_sex_year = sum(n), prop_sex_year = n_sex_year / n_year)


## -----------------------------------------------------------------------------
babies5


## -----------------------------------------------------------------------------
babies5 <- summarize(babies4, n_sex_year = sum(n), prop_sex_year = n_sex_year / unique(n_year))


## -----------------------------------------------------------------------------
babies5


## -----------------------------------------------------------------------------
babies5 <- unique(summarize(babies4, n_sex_year = sum(n), prop_sex_year = n_sex_year / n_year))


## -----------------------------------------------------------------------------
babies5


## -----------------------------------------------------------------------------
babies1 <- filter(babynames, year %in% range(year))
babies2 <- group_by(babies1, year)
babies3 <- mutate(babies2, n_year = sum(n))
babies4 <- group_by(babies3, sex, year)
babies5 <- summarize(babies4, n_sex_year = sum(n), prop_sex_year = n_sex_year / unique(n_year))


## -----------------------------------------------------------------------------
babies3 <- mutate(group_by(filter(babynames, year %in% range(year)), year), n_year = sum(n))
babies5 <- summarize(group_by(babies3, sex, year), n_sex_year = sum(n), prop_sex_year = n_sex_year / unique(n_year))


## -----------------------------------------------------------------------------
babies3 <-
  mutate(
    group_by(
      filter(
        babynames, year %in% range(year)
      ),
      year
    ),
    n_year = sum(n)
  )


## -----------------------------------------------------------------------------
babies5 <-
  summarize(
    group_by(
      babies3, sex, year
    ),
    n_sex_year = sum(n),
    prop_sex_year = n_sex_year / unique(n_year)
  )


## -----------------------------------------------------------------------------
babies5 <- 
  babynames %>% 
  filter(year %in% range(year)) %>% 
  group_by(year) %>% 
  mutate(n_year = sum(n)) %>% 
  group_by(sex, year) %>% 
  summarize(
    n_sex_year = sum(n), 
    prop_sex_year = n_sex_year / unique(n_year)
  )


## -----------------------------------------------------------------------------
babies5


## -----------------------------------------------------------------------------
# babies5 <- 
  babynames %>% 
  filter(year %in% range(year)) %>% 
  group_by(year) %>% 
  mutate(n_year = sum(n)) %>% 
  group_by(sex, year) %>% 
  summarize(
    n_sex_year = sum(n), 
    prop_sex_year = n_sex_year / unique(n_year)
  )


## -----------------------------------------------------------------------------
# babies5 <- 
  babynames %>% 
  filter(year %in% range(year)) %>% 
  group_by(year) %>% 
  mutate(n_year = sum(n)) # %>% 
  # group_by(sex, year) %>% 
  # summarize(
  #   n_sex_year = sum(n), 
  #   prop_sex_year = n_sex_year / unique(n_year)
  # )


## -----------------------------------------------------------------------------
# babies5 <- 
  babynames %>% 
  filter(year %in% range(year)) %>% 
  group_by(year) %>% 
  mutate(n_year = sum(n)) %>% 
  # group_by(sex, year) %>% 
  # summarize(
  #   n_sex_year = sum(n), 
  #   prop_sex_year = n_sex_year / unique(n_year)
  # ) %>% 
  {}  ## some love it—some hate it


## -----------------------------------------------------------------------------
#babies5 <- 
  babynames %>% 
  filter(year %in% range(year)) %>% 
  group_by(year) %>% 
  mutate(n_year = sum(n)) %>% 
  #group_by(sex, year) %>% 
  summarize(
    n_sex_year = sum(n), 
    #prop_sex_year = n_sex_year / unique(n_year)
  )


## -----------------------------------------------------------------------------
distinct(mpg, manufacturer)


## -----------------------------------------------------------------------------
distinct(mpg, hwy)


## -----------------------------------------------------------------------------
n_distinct(mpg$model)


## -----------------------------------------------------------------------------
slice(mpg, 100:120)


## -----------------------------------------------------------------------------
mpg %>% group_by(manufacturer) %>% slice(1)


## -----------------------------------------------------------------------------
slice_sample(mpg, n = 5)


## -----------------------------------------------------------------------------
mpg %>% group_by(year, manufacturer) %>% slice_sample(n = 1)


## -----------------------------------------------------------------------------
slice_sample(mpg, prop = .05)


## -----------------------------------------------------------------------------
slice_max(mpg, n = 5, order_by = hwy)


## -----------------------------------------------------------------------------
slice_max(mpg, n = 5, order_by = hwy, with_ties = FALSE)


## -----------------------------------------------------------------------------
slice_min(mpg, n = 5, order_by = hwy)


## -----------------------------------------------------------------------------
mpg %>% group_by(manufacturer) %>% slice_max(n = 1, order_by = hwy)


## -----------------------------------------------------------------------------
slice_max(mpg, prop = .01, order_by = displ)


## -----------------------------------------------------------------------------
count(mpg)


## -----------------------------------------------------------------------------
nrow(mpg)


## -----------------------------------------------------------------------------
count(mpg)


## -----------------------------------------------------------------------------
summarize(mpg, n = n())


## -----------------------------------------------------------------------------
count(mpg, manufacturer)


## -----------------------------------------------------------------------------
mpg %>% group_by(manufacturer) %>% summarize(n = n())


## -----------------------------------------------------------------------------
count(mpg, manufacturer, sort = TRUE)


## -----------------------------------------------------------------------------
mpg %>% group_by(manufacturer) %>% summarize(n = n()) %>% arrange(-n)


## -----------------------------------------------------------------------------
add_count(mpg2)


## -----------------------------------------------------------------------------
add_count(mpg2)


## -----------------------------------------------------------------------------
mutate(mpg2, n = n())


## -----------------------------------------------------------------------------
add_count(mpg2, model)


## -----------------------------------------------------------------------------
mpg2 %>% group_by(model) %>% mutate(n = n())


## -----------------------------------------------------------------------------
band_members


## -----------------------------------------------------------------------------
band_instruments


## -----------------------------------------------------------------------------
left_join(band_members, band_instruments)


## -----------------------------------------------------------------------------
right_join(band_members, band_instruments)


## -----------------------------------------------------------------------------
inner_join(band_members, band_instruments)


## -----------------------------------------------------------------------------
full_join(band_members, band_instruments)


## -----------------------------------------------------------------------------
full_join(band_members, band_instruments, by = "name")


## -----------------------------------------------------------------------------
band_members


## -----------------------------------------------------------------------------
band_instruments2


## -----------------------------------------------------------------------------
full_join(band_members, band_instruments2, by = c("name" = "artist"))


## -----------------------------------------------------------------------------
# install.packages("tidyr")
library(tidyr)


## -----------------------------------------------------------------------------
mpg3 <- count(mpg, manufacturer, class)
mpg3


## -----------------------------------------------------------------------------
mpg3 <- count(mpg, manufacturer, class)
complete(mpg3, manufacturer, nesting(class))


## -----------------------------------------------------------------------------
mpg3 <- count(mpg, manufacturer, class)
complete(mpg3, manufacturer, nesting(class), fill = list(n = 0))


## -----------------------------------------------------------------------------
mpg4 <-
  mpg %>% 
  group_by(manufacturer, model, trans, cyl, year) %>% 
  summarize(cty = mean(cty, na.rm = TRUE))


## -----------------------------------------------------------------------------
mpg4


## -----------------------------------------------------------------------------
mpg_wide <- pivot_wider(
  mpg4,
  id_cols = c(manufacturer, model, trans, cyl),
  names_from = year,
  values_from = cty
)


## -----------------------------------------------------------------------------
mpg_wide


## -----------------------------------------------------------------------------
mpg_wide <- pivot_wider(
  mpg4,
  id_cols = c(manufacturer, model, trans, cyl),
  names_from = year,
  values_from = cty,
  names_prefix = "cty_"
)


## -----------------------------------------------------------------------------
mpg_wide


## -----------------------------------------------------------------------------
pivot_wider(
  mpg4,
  id_cols = c(manufacturer, trans, cyl), ## removed model
  names_from = year,
  values_from = cty,
  names_prefix = "cty_"
) 


## -----------------------------------------------------------------------------
mpg_wide %>% 
  pivot_longer(
    cols = starts_with("cty_"),
    names_to = "year",
    values_to = "cty",
    names_prefix = "cty_"
  )


## -----------------------------------------------------------------------------
mpg_wide %>% 
  pivot_longer(
    cols = starts_with("cty_"),
    names_to = "year",
    values_to = "cty",
    names_prefix = "cty_",
    values_drop_na = TRUE
  )


## -----------------------------------------------------------------------------
# install.packages("forcats")
library(forcats)


## -----------------------------------------------------------------------------
mpg_fct <- mutate(mpg, man_fct = factor(manufacturer))
levels(mpg_fct$man_fct)


## -----------------------------------------------------------------------------
f1 <- mpg_fct %>% mutate(man_fct = fct_inorder(man_fct))
levels(f1$man_fct)


## -----------------------------------------------------------------------------
f2 <- mpg_fct %>% mutate(man_fct = fct_infreq(man_fct))
levels(f2$man_fct)


## -----------------------------------------------------------------------------
f3 <- mpg_fct %>% mutate(man_fct = fct_rev(man_fct))
levels(f3$man_fct)


## -----------------------------------------------------------------------------
f4 <- mpg_fct %>% 
  mutate(man_fct = fct_relevel(man_fct, "volkswagen", after = 1))
levels(f4$man_fct)


## -----------------------------------------------------------------------------
f5 <- mpg_fct %>% mutate(man_fct = fct_relevel(man_fct, "audi", after = Inf))
levels(f5$man_fct)


## -----------------------------------------------------------------------------
mpg_fct <- mpg_fct %>% 
  group_by(man_fct) %>% mutate(avg_displ = mean(displ)) %>% ungroup()
mpg_fct


## -----------------------------------------------------------------------------
f6 <- mpg_fct %>% 
  mutate(man_fct = fct_reorder(man_fct, avg_displ))  ### also `fct_reorder2()`
levels(f6$man_fct)


## -----------------------------------------------------------------------------
f7 <- mpg_fct %>% mutate(man_fct = fct_lump_n(man_fct, 10))
levels(f7$man_fct)


## -----------------------------------------------------------------------------
f8 <- mpg_fct %>% 
  mutate(man_fct = fct_lump_n(man_fct, 10, other_level = "other manufacturers"))
levels(f8$man_fct)


## -----------------------------------------------------------------------------
f8 <- mpg_fct %>% mutate(man_fct = fct_lump_lowfreq(man_fct))
levels(f8$man_fct)


## -----------------------------------------------------------------------------
# install.packages("stringr")
library(stringr)


## -----------------------------------------------------------------------------
manufacturers <- mpg %>% distinct(manufacturer) %>% pull(manufacturer)


## -----------------------------------------------------------------------------
manufacturers


## -----------------------------------------------------------------------------
typeof(manufacturers)


## -----------------------------------------------------------------------------
str_length(manufacturers)


## -----------------------------------------------------------------------------
str_sub(manufacturers, 1, 3)


## -----------------------------------------------------------------------------
str_subset(manufacturers, "y")


## -----------------------------------------------------------------------------
str_subset(manufacturers, "[ab]")


## -----------------------------------------------------------------------------
str_detect(manufacturers, "[ab]")


## -----------------------------------------------------------------------------
str_starts(manufacturers, "[ab]")  ## also `str_ends()`


## -----------------------------------------------------------------------------
str_count(manufacturers, "[aeiou]")


## -----------------------------------------------------------------------------
str_extract(manufacturers, "[aeiou]")


## -----------------------------------------------------------------------------
str_extract_all(manufacturers, "[aeiou]")


## -----------------------------------------------------------------------------
str_replace(manufacturers, "[aeiou]", "X")


## -----------------------------------------------------------------------------
str_replace_all(manufacturers, "[aeiou]", "X")


## -----------------------------------------------------------------------------
str_remove(manufacturers, "[lrs]")


## -----------------------------------------------------------------------------
str_remove_all(manufacturers, "[lrs]")


## -----------------------------------------------------------------------------
str_trim(c("we", "do not", "like ", " malformatted variables "))


## -----------------------------------------------------------------------------
str_wrap(manufacturers, 8)


## -----------------------------------------------------------------------------
str_trunc(manufacturers, 6)


## -----------------------------------------------------------------------------
str_to_upper(manufacturers)


## -----------------------------------------------------------------------------
str_to_title(manufacturers)


## -----------------------------------------------------------------------------
str_to_lower(c("yes", "Maybe", "NO"))


## -----------------------------------------------------------------------------
# install.packages("lubridate")
library(lubridate)


## -----------------------------------------------------------------------------
flights_selected <- 
  flights %>% 
  mutate(
    date = format(time_hour, format = "%d.%m.%Y"),  ## turn into date as string
    datetime = str_remove_all(time_hour, ":|-")  ## turn into custom formatted datetime string
  ) %>%
  select(flight, date, datetime) %>% 
  slice_sample(n = 100) %>%  ## just use random subset
  arrange(flight)


## -----------------------------------------------------------------------------
flights_selected


## -----------------------------------------------------------------------------
flights_dates <- flights_selected %>% 
  mutate(datetime = ymd_hms(datetime), date = dmy(date))  ## also `ymd*`, `ydm*`, `mdy*`, ...


## -----------------------------------------------------------------------------
flights_dates


## -----------------------------------------------------------------------------
head(flights_dates$datetime)


## -----------------------------------------------------------------------------
flights_dates <- flights_selected %>% 
  mutate(datetime = ymd_hms(datetime, tz = "America/New_York"), date = dmy(date))


## -----------------------------------------------------------------------------
head(flights_dates$datetime)


## -----------------------------------------------------------------------------
flights_dates %>% 
  mutate(
    dt_west = with_tz(datetime, "America/Los_Angeles"),
    dt_cet  = with_tz(datetime, "CET"),
  )


## -----------------------------------------------------------------------------
flights_dates


## -----------------------------------------------------------------------------
flights_dates %>% 
  mutate(
    dt_west = force_tz(datetime, "America/Los_Angeles"),
    dt_cet  = force_tz(datetime, "CET"),
  )


## -----------------------------------------------------------------------------
flights_dates %>% 
  mutate(year = year(date), month = month(date))


## -----------------------------------------------------------------------------
flights_dates %>% 
  mutate(yday = yday(date), week = week(date), wday = wday(date))


## -----------------------------------------------------------------------------

flights_dates %>% 
  mutate(month = month(date, label = TRUE), wday = wday(date, label = TRUE))

## if labels are not in English, run this and rerun the line before
# invisible(Sys.setlocale("LC_TIME", "C")) 


## -----------------------------------------------------------------------------
flights_dates %>% 
  mutate(month = month(date, label = TRUE), wday = wday(date, label = TRUE, abbr = FALSE))


## -----------------------------------------------------------------------------
# Sys.setlocale("LC_TIME", "de_DE.UTF-8") ## on Mac
# Sys.setlocale("LC_TIME", "German")    ## on Win


## -----------------------------------------------------------------------------
flights_dates %>% 
  mutate(month = month(date, label = TRUE), wday = wday(date, label = TRUE, abbr = FALSE))


## -----------------------------------------------------------------------------
flights_dates %>% 
  mutate(date_ahead = date + 30)


## -----------------------------------------------------------------------------
flights_dates %>% 
  mutate(date_ahead = date + weeks(6))


## -----------------------------------------------------------------------------
# install.packages("hms")
data.frame(hours = 1:3, hms = hms::hms(hours = 1:3))  ## do not confuse with `lubridate::hms()`


## -----------------------------------------------------------------------------
flights_time <- flights_dates %>% 
  mutate(time = hms::as_hms(datetime))


## -----------------------------------------------------------------------------
flights_time


## -----------------------------------------------------------------------------
flights_time %>% 
  mutate(time_num = as.numeric(time))


## -----------------------------------------------------------------------------
# install.packages("glue")
library(glue)


## -----------------------------------------------------------------------------
mpg %>% 
  select(manufacturer, model, year, trans) %>% 
  mutate(
    text = glue("The model {manufacturer} {model} with {trans} as transmission was build in {year}.")
  )


## -----------------------------------------------------------------------------
mpg %>% 
  select(manufacturer, model, year, trans) %>% 
  mutate(text = 
    glue("The {str_to_title(manufacturer)} {str_to_title(model)} with {trans} as transmission was build in {year}.")
  ) 


## -----------------------------------------------------------------------------
date <- Sys.Date()
wday <- wday(date, label = TRUE, abbr = FALSE)


## -----------------------------------------------------------------------------
paste("Today is", wday, ",", date)


## -----------------------------------------------------------------------------
paste("Today is", wday, ",", date, sep = "*")


## -----------------------------------------------------------------------------
paste("Today is ", wday, ", ", date, sep = "")


## -----------------------------------------------------------------------------
paste0("Today is ", wday, ", ", date)


## -----------------------------------------------------------------------------
mpg %>% 
  select(manufacturer, model, year, trans) %>% 
  mutate(text = 
    paste0("The ", str_to_title(manufacturer), " ", str_to_title(model), 
           " with ", trans, " as transmission was build in ", year, ".")
  ) 

