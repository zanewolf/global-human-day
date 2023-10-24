# Housekeeping 
library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(colorspace)
library(lubridate)

#import data and clean

# learned about the clean_names function, it takes all sort of 'cases' and default is snake: "snake_case"
all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')|> clean_names(case='lower_camel')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv')|> clean_names(case='lower_camel')
global_human_day <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_human_day.csv')|> clean_names(case='lower_camel')
global_economic_activity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_economic_activity.csv')|> clean_names(case='lower_camel')

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
txt1 <- "white"
bg <- "white"
accent <- "grey20"

pal <- c('#4a166a', '#ce5068', '#f0d35b', '#61aa6f', "#2f3c68")
pal <- colorRampPalette(pal)(8) # creates a color palette using colors given as steps.
# pal  <- c("")
# show_pal(pal) show_pal function not defined, so I googled one to use

# font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
# font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
showtext_auto()

# defining functions
show_palette <- function(colors) {
  image(1:n, 1, as.matrix(1:n), col = colors, 
        xlab = "", ylab = "", xaxt = "n", 
        yaxt = "n", bty = "n")
}

# this function calculates what height the different x-axis labels need to be at so they don't overlap
breathing_space_on_x <- function(x, d, y0, dy, eps = 0.01) {
  n <- length(x)
  dd <- c(0, diff(x))
  y_spaced <- rep(0, n)
  for(k in 1:n) {
    if(y_spaced[k] == 0) {
      dk <- cumsum(dd[(k+1):(k+min(n, k+5))])
      id <- which(dk < d)
      if(length(id > 0)) {
        y_spaced[(k+1):(k+length(id))] <- id
      }
    }
  }
  y_new <- y0 - seq(0, 10*dy, dy)
  y_new[y_spaced+1]
}

# converts 1.07hours to 1h 04m
to_hm <- function(seconds, fmt = "hm") {
  hrs <- floor(seconds/3600)
  mins <- floor(seconds/60) - hrs*60
  secs <- seconds - mins*60 - hrs*3600
  mins <- str_pad(mins, width = 2, pad = 0) #increases single digit to two, more sophisticated than my ifelse
  secs <- str_pad(secs, width = 2, pad = 0)
  ifelse(hrs == 0, paste0(mins, "m"), paste0(hrs, "h ", mins, "m"))
}

# make_caption <- function(accent, data) {
#   mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
#   twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
#   github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
#   floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
#   threads <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xe618;</span>")
#   space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
#   space2 <- glue("<span style='color:{bg}'>-</span>") # can't believe I'm doing this
#   glue("{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{threads}{space2}@danoehm{space2}{mastodon}{space2}@danoehm@{space}fosstodon.org")
# }


#  I initially couldn't find the to_hm function so I reverse-engineered it based on the format of the time in his final viz. I was troubleshooting mins when i found his original, so whomp whomp
# 
# to_hm2 <- function(x){
#     hours = floor(x/3600)
#     minRem = floor(x/60)-hours*60
#     mins = ifelse(minRem<10,paste(c('0',minRem),collapse = ""),minRem)
#     paste0(hours, "h ", min, "m")
# 
#   }



n <- 8 # number of colors in palette > magic number, would be better to go through and define N by the number of distinct categories
show_palette(pal)

# come back and add custom fonts later

# font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
# font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
# font_add_google("Barlow", "bar")
# ft <- "bar"
# showtext_auto()

# wrangle ----

lighten_amount <- seq(0, 1, length = 7)[2:6] # it's a way to modify the colors between category and subcategory by "lighten_amount"
print(lighten_amount)

df_cats <- all_countries |>
  distinct(category, subcategory)

# grab the 20 highest and 20 lowest countries on the Human Development Index list

highest20 <- c("AUS","BEL","DNK","FIN","DEU","HKG","ISL","IRL","JPN","KOR","LIE","LUX","NLD","NZL","NOR","SGP","SWE","CHE","GBR","CAN")

lowest20 <- c("AFG","BFA","BDI","CAF","TCD","COD","ERI","ETH","GMB","GIN","GNB","LBR","MDG","MLI","MOZ","NER","SEL","SSD","SDN","YEM")


a <- 0


df_global <- global_human_day |>
  left_join(df_cats, by = "subcategory") |> # add in the column 'category' to match the corresponding subcategory
  arrange(category, desc(hoursPerDay)) |> # sort the df by hours per day per category, not subcategory
  mutate(
    hrs = cumsum(hoursPerDay), # just adds the value of the previous rows to the current row value, last row should be 24
    x = ifelse(row_number() == 1, 0, lag(hrs)) + hoursPerDay/2, #scaling the starting x value to a 24-hr clock, lag() shifts the time value of a time-series data back by a given number of observations, but why add hoursPerDay/2? - used for label placement, midway of time block
    xmin = ifelse(row_number() == 1, 0, lag(hrs)), # corresponds to previous row's cumsum value
    xmax = hrs, # corresponds to current row's cumsum value
    ymin = 0.29-a, #0
    ymax = 0.49-a, #0.2
    cat_num = as.numeric(factor(category)), #just assigns a number to the categories as a factor, useful for plotting
    col = pal[cat_num],# assigns color to category based on the number of the cateogry
    y_lab = breathing_space_on_x(x, 1.3, .15, 0.25),
    sub_lab = str_wrap(subcategory, 15), #good for wrapping long strings into short paragraphs
    # hrs_lab = paste0(str_wrap(subcategory, 10), "\n", to_hm(hoursPerDay*3600)),
    hrs_lab = to_hm(hoursPerDay*3600),
    y_hrs = breathing_space_on_x(x, 0.5, 0.126, 0.06)
  )|> 
  group_by(category) |>
  mutate(
    subcat_num = 1:n(), #numbers the subcategories for each category
    sub_col = lighten(col, lighten_amount[subcat_num]) #progressively lightens main category color for each additional subcategory color
  )  |>
  ungroup()

df_global_cat <- global_human_day |>
  left_join(df_cats, by = "subcategory") |>
  group_by(category) |>
  summarise(
    hoursPerDay = sum(hoursPerDay)
  ) |>
  mutate(
    hrs = cumsum(hoursPerDay),
    x = ifelse(row_number() == 1, 0, lag(hrs)) + hoursPerDay/2,
    xmin = ifelse(row_number() == 1, 0, lag(hrs)),
    xmax = hrs,
    ymin = 0.51-a, #0.21
    ymax = 0.61-a, #0.41
    cat_num = as.numeric(factor(category)),
    col = pal[cat_num],
    y_lab = breathing_space_on_x(x, 1.2, 0.75, -0.25),
    cat_lab = str_wrap(category, 15),
    hrs_lab = to_hm(hoursPerDay*3600)
  )


df_highest20 <- all_countries |>
  filter(countryIso3 %in% highest20) |>
  rename(hoursPerDay = hoursPerDayCombined) |>
  group_by(subcategory)|>
  summarize(avgHours = mean(hoursPerDay))|>
  left_join(
    global_human_day |>
      select(subcategory, globalHoursPerDay = hoursPerDay),
    by = "subcategory"
  ) |>
  right_join(df_cats, by ="subcategory") |>  #add categories back in 
  arrange(category, desc(avgHours)) |>
  mutate(
    # avgHours = mean(),
    change = avgHours - globalHoursPerDay,
    hrs = cumsum(avgHours),
    x = ifelse(row_number() == 1, 0, lag(hrs)) + avgHours/2,
    xmin = ifelse(row_number() == 1, 0, lag(hrs)),
    xmax = hrs,
    ymin = -2.21-a, #-1.8-a
    ymax = -2.01-a, #-1.6-a
    cat_num = as.numeric(factor(category)),
    col = pal[cat_num],
    hrs_lab = to_hm(avgHours*3600),
    change_lab = to_hm(abs(change)*3600),
    change_lab = ifelse(change < 0, paste0("-", change_lab), paste0("+", change_lab)),
    y_lab = breathing_space_on_x(x, 0.6, -2.35, 0.25),
  )  |>
  group_by(category) |>
  mutate(
    subcat_num = 1:n(),
    sub_col = lighten(col, lighten_amount[subcat_num])
  ) |>
  ungroup()


df_highest20_cat <- all_countries |>
  filter(countryIso3  %in% highest20) |>
  rename(hoursPerDay = hoursPerDayCombined) |>
  group_by(subcategory)|>
  summarize(avgHours = round(mean(hoursPerDay),2)) |>
  right_join(df_cats, by ="subcategory")|>
  ungroup() |>
  group_by(category)|>
  summarize(hoursPerDay = sum(avgHours))|>  #add categories back in
  arrange(category, desc(hoursPerDay)) |>
  left_join(
    df_global_cat |>
      select(category, mean = hoursPerDay),
    by = "category"
  ) |>
  group_by(category) |>
  summarise(
    hoursPerDay = sum(hoursPerDay),
    mean = sum(mean)
  ) |>
  mutate(
    change = hoursPerDay - mean,
    hrs = cumsum(hoursPerDay),
    x = ifelse(row_number() == 1, 0, lag(hrs)) + hoursPerDay/2,
    xmin = ifelse(row_number() == 1, 0, lag(hrs)),
    xmax = hrs,
    ymin = -1.99-a, #-1.59-a
    ymax = -1.79-a, #-1.39-a
    cat_num = as.numeric(factor(category)),
    col = pal[cat_num],
    y_lab = breathing_space_on_x(x, 0.6, -1.65, -0.25),
    hrs_lab = to_hm(hoursPerDay*3600),
    change_lab = to_hm(abs(change)*3600),
    change_lab = ifelse(change < 0, paste0("-", change_lab), paste0("+", change_lab)),
  )


df_lowest20 <- all_countries |>
  filter(countryIso3 %in% lowest20) |>
  rename(hoursPerDay = hoursPerDayCombined) |>
  group_by(subcategory)|>
  summarize(avgHours = mean(hoursPerDay))|>
  left_join(
    global_human_day |>
      select(subcategory, globalHoursPerDay = hoursPerDay),
    by = "subcategory"
  ) |>
  right_join(df_cats, by ="subcategory") |>  #add categories back in 
  arrange(category, desc(avgHours)) |>
  mutate(
    # avgHours = mean(),
    change = avgHours - globalHoursPerDay,
    hrs = cumsum(avgHours),
    x = ifelse(row_number() == 1, 0, lag(hrs)) + avgHours/2,
    xmin = ifelse(row_number() == 1, 0, lag(hrs)),
    xmax = hrs,
    ymin = -4.71-a, #-1.8-a
    ymax = -4.51-a, #-1.6-a
    cat_num = as.numeric(factor(category)),
    col = pal[cat_num],
    hrs_lab = to_hm(avgHours*3600),
    change_lab = to_hm(abs(change)*3600),
    change_lab = ifelse(change < 0, paste0("-", change_lab), paste0("+", change_lab)),
    y_lab = breathing_space_on_x(x, 0.6, -4.85, 0.25),
  )  |>
  group_by(category) |>
  mutate(
    subcat_num = 1:n(),
    sub_col = lighten(col, lighten_amount[subcat_num])
  ) |>
  ungroup()


df_lowest20_cat <- all_countries |>
  filter(countryIso3  %in% lowest20) |>
  rename(hoursPerDay = hoursPerDayCombined) |>
  group_by(subcategory)|>
  summarize(avgHours = round(mean(hoursPerDay),2)) |>
  right_join(df_cats, by ="subcategory")|>
  ungroup() |>
  group_by(category)|>
  summarize(hoursPerDay = sum(avgHours))|>  #add categories back in
  arrange(category, desc(hoursPerDay)) |>
  left_join(
    df_global_cat |>
      select(category, mean = hoursPerDay),
    by = "category"
  ) |>
  group_by(category) |>
  summarise(
    hoursPerDay = sum(hoursPerDay),
    mean = sum(mean)
  ) |>
  mutate(
    change = hoursPerDay - mean,
    hrs = cumsum(hoursPerDay),
    x = ifelse(row_number() == 1, 0, lag(hrs)) + hoursPerDay/2,
    xmin = ifelse(row_number() == 1, 0, lag(hrs)),
    xmax = hrs,
    ymin = -4.49-a, #-1.59-a
    ymax = -4.29-a, #-1.39-a
    cat_num = as.numeric(factor(category)),
    col = pal[cat_num],
    y_lab = breathing_space_on_x(x, 0.6, -4.15, -0.25),
    hrs_lab = to_hm(hoursPerDay*3600),
    change_lab = to_hm(abs(change)*3600),
    change_lab = ifelse(change < 0, paste0("-", change_lab), paste0("+", change_lab)),
  )

df_global_cat2 <- df_global_cat |>
  bind_rows(df_highest20_cat) |>
  bind_rows(df_lowest20_cat)


# df <- df_highest20_cat |>
#   bind_rows(df_global_cat) |>
#   bind_rows(df_lo)


# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "Data")


title_bar <- tribble(
  ~x, ~y, ~title,
  0, -0.95, "Global average",
  0, -3.45, "Top 20",
  0,-5.95, "Bottom 20"
)

title <- "GLOBAL HUMAN DAY: HOW WE SPEND OUR TIME"
subtitle <-
  "           Global Human Day estimates how all humans spend their time on any given day using a generalized, physical outcome–based categorisation that facilitates the integration of data from hundreds of diverse datasets.
  
Compared with the global average, people living in the 20 countries with the highest Human Development Index (HDI) spend about a half hour more in passive/interactive/social experiences (or more casually reframed as hobbies and/or socialization) and resource allocation, and about forty minutes less in food growth and collection. 

Meanwhile, people living in the 20 countries with the lowest HDI spend roughly 20 minutes more in both school and food preparation each day, and twenty minutes less in hobbies and/or socialization experiences. "

# 📊 plot --------------------------------------------------------------------

pt_size <- 2
pt_dist <- 0.08
txt_dist <- 0.08

df_global |>
  bind_rows(df_highest20) |>
  bind_rows(df_lowest20) |>
  ggplot() +
  
  # shading rect
  annotate("rect", xmin = -0.5, xmax = 24.5, ymin = -0.99, ymax = 1.5, fill = "grey95",color="darkgrey") +
  annotate("rect", xmin = -0.5, xmax = 24.5, ymin = -3.49, ymax = -1, fill = "white",color="lightblue") +
  annotate("rect", xmin = -0.5, xmax = 24.5, ymin = -5.99, ymax = -3.5, fill = "white",color="lightgreen") +
  
  
  # title text
  geom_text(aes(x = x, y = y, label = toupper(title)), title_bar, family = ft, colour = txt, size = 32, vjust = 0, hjust = 0, fontface = "bold") +
  
  # little lines down
  geom_segment(aes(x = x, xend = x, y = ymin, yend = y_lab+pt_dist, colour = sub_col)) +
  geom_point(aes(x = x, y = ymin, colour = sub_col), size = pt_size) +
  geom_point(aes(x = x, y = y_lab+pt_dist, colour = sub_col), size = pt_size) +
  
  # little lines up
  geom_segment(aes(x = x, xend = x, y = ymax, yend = y_lab-pt_dist, colour = col), df_global_cat2) +
  geom_point(aes(x = x, y = ymax, colour = col), df_global_cat2, size = pt_size) +
  geom_point(aes(x = x, y = y_lab-pt_dist, colour = col), df_global_cat2, size = pt_size) +
  
  # text
  geom_text(aes(x = x, y = y_lab-txt_dist, label = sub_lab), family = ft, size = 10, colour = txt, vjust = 1, lineheight = 0.3) + #global_cat subcategory labels
  geom_text(aes(x = x, y = y_lab, label = hrs_lab), family = ft, size = 12, colour = txt, vjust = 1, lineheight = 0.3, fontface = "bold") + #subcategory total time values for highest and lowest
  
  # cat text
  geom_text(aes(x = x, y = y_lab+txt_dist, label = cat_lab), df_global_cat, family = ft, size = 10, colour = txt, hjust = 0.5, lineheight = 0.3, vjust = 0) +
  geom_text(aes(x = x, y = y_lab, label = hrs_lab), filter(df_global_cat, !is.na(cat_lab)), family = ft, size = 12, colour = txt, hjust = 0.5, lineheight = 0.3, vjust = 0, fontface = "bold") +
  geom_text(aes(x = x, y = y_lab+txt_dist, label = hrs_lab), df_highest20_cat, family = ft, size = 12, colour = txt, hjust = 0.5, lineheight = 0.3, vjust = 0, fontface = "bold") +
  geom_text(aes(x = x, y = y_lab+txt_dist, label = hrs_lab), df_lowest20_cat, family = ft, size = 12, colour = txt, hjust = 0.5, lineheight = 0.3, vjust = 0, fontface = "bold") +
  
  # change text
  geom_text(aes(x = x, y = y_lab-txt_dist, label = change_lab), family = ft, size =10, colour = txt, vjust = 1, lineheight = 0.3) +
  geom_text(aes(x = x, y = y_lab, label = change_lab), df_highest20_cat, family = ft, size = 10, colour = txt, vjust = 0, lineheight = 0.3) +
  geom_text(aes(x = x, y = y_lab, label = change_lab), df_lowest20_cat, family = ft, size = 10, colour = txt, vjust = 0, lineheight = 0.3) +
  
  # stacked bars
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sub_col)) + #bottom rectangle of subcategories
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col), df_global_cat2) + #top rectangle of major categories
  # by binding the data together, you only have to plot one for the other data sets to plot? 
  # 
  
  scale_fill_identity() +
  scale_colour_identity() +
  
  ylim(min(-6), 2) +
  coord_cartesian(clip = "off") +
  labs(
    # caption = caption,
    title = title,
    subtitle = str_wrap(subtitle, width = 175)
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 150, hjust = 0.1, face = "bold"),
    plot.subtitle = element_text(hjust = 0.3,size=60),
    # plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 10)),
    plot.margin = margin(b = 0, t = 0, r = 0, l = 0)
  )

ggsave("global-human-day.png", height = 16, width = 24)
