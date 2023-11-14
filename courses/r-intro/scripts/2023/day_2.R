# day_2.R
# ~~~~~~~

library(dplyr)
library(lubridate)
library(stringr)


files = list.files(path = "courses/r-intro/data/taught/part_2/", 
                  full.names = TRUE)

siteNames = basename(files) |>
  str_remove(pattern = "_2018.csv")
  tolower()
# OR
siteNames = basename(files) |>
  word(1, 1, sep = "_") |>
  tolower()

c112 = read.csv(files[1]) |>
  tibble() |>
  select(date, no, no2, o3)|>
  mutate(date = ymd_hms(date),
          site = "c112")

hors = read.csv(files[2]) |>
  tibble() |>
  select(-X) |>
  mutate(date = ymd_hms(date),
          site = "hors")

kc1 = read.csv(files[3]) |>
  tibble() |>
  select(-X) |>
  mutate(date = ymd_hms(date),
          site = "kc1",
          no = ifelse(no == "missing", NA, no),
          no = as.numeric(no))

lon6 = read.csv(files[4]) |>
  tibble() |>
  select(-X) |>
  #mutate(date = as_datetime(date))
  mutate(date =as.POSIXct(date, origin = "1970-01-01 00:00:00"),
          site = "lon6")

my1 = read.csv(files[5]) |>
  tibble() |>
  select(-X) |>
  mutate(date = ymd_hms(date),
          site = "my1")

left_join(x = c112,y = my1, by = "date", suffix = c("_c112","_my1"))

bind_rows(c112, my1, hors, kc1, lon6)



# # -----------------------------------------------------------------------



siteList = list()

for(i in 1:length(files)){
  siteList[[i]] = read.csv(files[i]) |>
    tibble() |>
    mutate(site = siteNames[i]) |>
    select(-X)
  
  if(siteNames[i] == "lon6"){
    siteList[[i]] = siteList[[i]] |>
      mutate(date = as.POSIXct(date, origin = "1970-01-01 00:00:00"))
  }else{
    siteList[[i]] = siteList[[i]] |>
      mutate(date = ymd_hms(date))
  }
  
  if(siteNames[i] == "kc1"){
    siteList[[i]] = siteList[[i]] |>
      mutate(no = ifelse(no == "missing", NA, no), no = as.numeric(no))
  }
}

sites_middle = bind_rows(siteList)
sites_middle |> count(site)

# approach 1
mean(sites_middle$no2, na.rm=TRUE)

# summarise/summarize approach 2, returns data frame/tibble
sites_middle |>
  summarise(mean_no2 = mean(no2, na.rm=TRUE),
            sd_no2 = sd(no2, na.rm=T))
sites_middle |>
  group_by(site) |>
  summarise(mean_no2 = mean(no2, na.rm=TRUE),
            sd_no2 = sd(no2, na.rm=TRUE))

# select, rename, mutate, group_by, summarise, arrange
sites_middle |>
  group_by(site) |>
  summarise(mean_no2 = mean(no2, na.rm=TRUE),
            sd_no2 = sd(no2, na.rm=TRUE),
            mean_no = mean(no, na.rm=TRUE),
            sd_no = mean(no, na.rm=TRUE),
            mean_o3 = mean(o3, na.rm=TRUE),
            sd_o3 = mean(o3, na.rm=TRUE)) |>
  arrange(mean_no2) #desc = descending

library(tidyr)
# pivot_wider and pivot_longer
sites_wide = sites_middle |>
                pivot_wider(names_from=site, values_from=c(no2, no, o3))

sites_wide |>
  summarise(
    mean_no2_c112 = mean(no2_c112, na.rm=TRUE),
    mean_no2_hors = mean(no2_hors, na.rm=TRUE)
    # ... x 15 per species
  )

sites_long = sites_middle |>
  pivot_longer(c(no, no2, o3), names_to = "species", values_to="conc")

sites_long |>
  group_by(site, species) |>
  summarise(mean_conc = mean(conc, na.rm=TRUE),
            sd_conc = sd(conc, na.rm=TRUE)) |>
  ungroup() #remove explicit groups from tibble

# mutate also works with group_by
# flag values > 95% quantile - naive outlier detection
sites_long |>
  group_by(site, species) |>
  mutate(p_95 = quantile(conc, 0.95, na.rm=TRUE),
         flag = conc > p_95) |>
  ungroup()

# filter
sites_long |>
  filter(site == 'c112', # comma combines conditions as ANDs
         species == 'no') # double == for equality

# combine with OR by using '|'
sites_long |>
  filter(site == 'c112' | site == 'kc1') |>
  count(site)

sites_long |>
  filter(site != 'c112') |> # not equals !=
  
sites_long |>
  filter(site %in% c('c112', 'kc1', 'my1')) |> # better than using OR
  count(site)

sites_long |>
  filter(species == 'no2',
         conc > 100) # <, <=, >, >=

# can also use conditions on datetime
sites_long |>
  filter(#date >= as_datetime("2018-12-25 00:00:00"),
        #date < as_datetime("2018-12-31 00:00:00"),
        between(date, as_datetime("2018-12-25 00:00:00"), as_datetime("2018-12-26 00:00:00"))) |>
  tail()

sites_wide |>
  filter(no2_c112 > 100 | no2_hors > 100) # lots of typing and duplication

# filter also works with group_by
sites_long |>
  group_by(site, species) |>
  filter(conc >= quantile(conc, 0.95, na.rm=TRUE)) |>
  ungroup()

# more time-based functionality
# time averaging from hour to daily
sites_long |>
  mutate(date = floor_date(date, "day")) |>
  group_by(date, site, species) |>
  summarise(conc = mean(conc, na.rm=TRUE)) |>
  ungroup()

# find hour of maximum concentration
sites_long |>
  mutate(hour_of_day = hour(date)) |>
  group_by(hour_of_day, site, species) |>
  summarise(conc = mean(conc, na.rm=TRUE)) |>
  ungroup() |>
  group_by(site, species) |>
  filter(conc == max(conc, na.rm=TRUE)) |>
  ungroup()

# wday and yday get day of week/year
sites_long |>
  mutate(week_day = wday(date)) # 1 = sunday

# today()

# datetime arithmetic
sites_long |>
  select(date) |>
  mutate(
    plus_one = date + days(1),
    plus_month = date + months(1)
  )

#lag gets previous row values
sites_long |>
  group_by(site, species) |>
  arrange(date) |>
  mutate(prev_conc = lag(conc),
         conc_diff = conc - prev_conc) |>
  slice(8755:8765)
