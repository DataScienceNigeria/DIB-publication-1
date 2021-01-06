# Load packages

textmining <- c("rtweet", "tidytext","topicmodels", "quanteda", "tm", "quanteda", "stm", "janeaustenr", "gutenbergr", "wordcloud", "wordcloud2", "SnowballC", "textstem", "textshape", "lexicon", "textclean")

formating <- c("RColorBrewer", "mdthemes")

data_mining <- c("tidyverse", "readxl", "widyr", "lubridate", "stringr", "janitor", "inspectdf", "scales", "readxl", "ggchicklet", "xts", "anytime")


install.load::install_load(c(textmining, formating, data_mining))

theme_set(theme_bw())


# Import data

covid_sheet1 <- read_excel("Data/COVID-19 and economic data in Nigeria vs3.xlsx", skip = 1, .name_repair = make_clean_names)

covid_lab <- covid_sheet1 %>% 
  select(states, geopolitical_zones, covid_19_lab) %>%   drop_na() 

# COVID-19 lab by geopolitical zone

covid_lab %>% 
  count(geopolitical_zones, wt = covid_19_lab) %>% 
  mutate(pct = percent(n/sum(n))) %>% 
  ggplot(aes(x = reorder(geopolitical_zones, n), y = n, fill = geopolitical_zones, label = pct))+ geom_chicklet(width = 0.3, show.legend = F) + geom_text(position = position_dodge(0.9), vjust = 0, size = 2.5) + labs(x = "Geopolitical zones", y = "Number of laboratories") + scale_color_brewer() + theme(axis.ticks.x = element_blank())

ggsave(filename = "Charts/COVID19_lab_geozone.png", width = 6.26, height = 4.48)
       
# COVID-19 lab by state

covid_lab %>% 
  ggplot(aes(x = reorder(states, covid_19_lab), y = covid_19_lab, fill = states, label = covid_19_lab))+ geom_chicklet(show.legend = FALSE) + 
  labs(x = "States", y = "Number of laboratories") + coord_flip() + 
  scale_y_continuous(expand = c(0, 0))+ 
  theme(axis.ticks.y = element_blank())+
  theme_minimal()

ggsave("Charts/Covid_19_lab_by_state.png", width = 6.74, height = 4.54)

# Budget analysis

covid_sheet1 %>% 
  select(geopolitical_zones, budget = x2020_initial_budget_bn_presented) %>% 
  count(geopolitical_zones, wt = budget) %>%  ggplot(aes(reorder(geopolitical_zones, n), y = n, fill = geopolitical_zones, label = n)) + 
  geom_col(show.legend = F) + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_flip() + 
  theme( axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_text(size = 8))+
  geom_text(position = position_dodge(0.9), vjust = "inward", hjust = "inward",
 size = 3) + 
  labs(x = "Geopolitical zones", y = "Amount in billion naira") + scale_color_brewer() + 
  theme(axis.ticks.x = element_blank()) + theme_minimal()


ggsave(filename = "Charts/budgetbygeo.png", width = 6.26, height = 4.48)


## Initial and revised budget 

init_rev_budget <- covid_sheet1 %>% 
  select(states, initial = x2020_initial_budget_bn_presented, revised= x2020_revised_budget_bn_due_to_covid_19) %>% 
  mutate(revised = -revised) %>% pivot_longer(cols = 2:3, names_to = "time", values_to = "budget") %>% mutate(states = factor(states, levels = rev(unique(states))), time = fct_relevel(time, "revised")) %>% 
  mutate(budget2 = abs(budget)) 

init_rev_budget %>% 
  ggplot(aes(states, y = budget, fill = time, label = budget2)) + geom_col() +theme_minimal() + scale_y_continuous(expand = c(0, 0)) + geom_hline(yintercept = 0, col = 'maroon') + labs(y = "Amount in naira", x = "States", fill = NULL) + coord_flip() + theme(legend.key = element_rect(fill = "white", colour = "black"), legend.position = "top", axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_text(size = 8)) + scale_fill_brewer(palette = 'Dark2', direction = -1) +  scale_fill_discrete(labels = c("revised budget", "initial budget"))

ggsave("Charts/Covid_19_budget_diff.png", width = 7, height = 5)


# COVID-19 Laboratories by ownership

data <- tibble::tribble(~Laboratories, ~count, "Fee Paying Private", 21, "Government", 68, "Corporate", 6)

## Steps to plot Pie chart of laboratories by ownership

## Compute percentages
data$fraction <- data$count / sum(data$count)

## Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

## Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

## Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

## Compute a good label
data$label <- str_c("(",data$count, ", ", scales::percent(data$fraction),")")

## Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Laboratories)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Laboratories), size=6) + ## x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
     theme(legend.key = element_rect(fill = "white", colour = "black"), legend.title = element_text(face = "bold"))

ggsave(filename = "Charts/COVID-19 lab by ownership.png", width = 6.26, height = 4.48)

# Time series trend analysis

covid_sheet2 <- read_excel("Data/COVID-19 and economic data in Nigeria vs3.xlsx", sheet = 2, .name_repair = make_clean_names) %>% clean_names() %>% mutate(date = anytime::anydate(date))

covid_time_series <- covid_sheet2[, -1] # remove date column

covid_time_series <- xts(covid_time_series, order.by =  covid_sheet2$date) # converted to xts object

## COVID-19 daily trend analysis

plot(covid_time_series[, 4], main = "Daily confirmed cases", col= "red")

plot(covid_time_series[, 5], main = "Daily recovered cases", col= "green")

plot(covid_time_series[, 6], main = "Daily death cases", col= "darkred")


# Scrape NCDC timeline for COVID-19 updates

ncdc_tweets <- get_timelines(user = "@NCDCgov", n = 3200)

## Data cleaning

ncdc_tweets_clean <- ncdc_tweets %>% 
  select(3, 5, 7, 12, 14, 17, 22, 31) %>% 
  rename("date" = "created_at", "tweet" = "text", "mentions_twitter_handle" = "mentions_screen_name", "tweet_type" = "is_retweet") %>%  
  mutate(tweet_type = ifelse(tweet_type == TRUE,  "retweet", "tweet"), hashtags = as.character(hashtags) %>%  str_remove_all(., pattern = '[\\"|c()]'), mentions_twitter_handle = as.character(mentions_twitter_handle) %>% str_remove_all(., pattern = '[\\"|c()]'), media_url = as.character(media_url))


## removing foreign characters and url

ncdc_tweets_clean <- ncdc_tweets_clean %>% mutate(tweet = str_remove_all(tweet, "&amp;|&lt;|&gt;"), tweet = str_replace_all(tweet, "http.*",""))

## NCDC tweets from December 1, 2020 to October 30, 2020

ncdc_COVID_19_tweets <- ncdc_tweets_clean %>% 
  filter(date>= ymd("2019-12-01"))

## Word variant in NCDC tweet according to COVID-19 phases

ncdc_tweets_variant <- ncdc_COVID_19_tweets  %>% 
  mutate(covid19_time_period = factor(
  case_when(
   as_date(date) <= ymd("2020-3-29") ~ "Pre-lockdown", between(as_date(date), ymd("2020-3-30"), ymd("2020-05-4")) ~ "Lockdown",
    TRUE ~ "Easing"), levels = c ("Pre-lockdown", "Lockdown", "Easing"))) %>% 
  relocate(covid19_time_period, .after = date)

## Tweets data were saved 
ncdc_tweets_variant %>% 
  writexl::write_xlsx("Data/COVID-19 and economic data in Nigeria vs3.xlsx")

## Tweets data were saved and now imported

ncdc_COVID_19_tweets <- read_excel("Data/COVID-19 and economic data in Nigeria vs3.xlsx", sheet = 5, .name_repair = make_clean_names) %>% clean_names() %>% mutate(date = anytime::anytime(date), covid19_time_period =
  fct_relevel(covid19_time_period, "Pre-lockdown", "Lockdown", "Easing"))


## Line plot of NCDC tweets

ncdc_COVID_19_tweets %>% 
  group_by(tweet_type) %>% 
    ts_plot(by = "days") +
  facet_grid(tweet_type~.)+
  theme_minimal()+
  labs(title = "NCDC timelines per day", x="", y = "Daily tweets") +
  theme(legend.position = "none") 


## Tokenization

ncdc_tweets_token <- ncdc_COVID_19_tweets %>% 
  unnest_tokens(output = tweets, input = tweet) 

## Remove digit (using regex)

ncdc_tweets_token <- ncdc_tweets_token %>% filter(!str_detect(tweets, "\\b\\d+\\b"))


## remove stopwords

custom_stop_words <- bind_rows(tibble(word = c("ncdc", "chikwe_i", "lassafever", "dg", "dr", "fmohnigeria"), lexicon = c("custom")), tidytext::stop_words)


ncdc_tweets_token <- ncdc_tweets_token %>%
  anti_join(custom_stop_words, by = c("tweets"= "word"))


## Lemmatization

ncdc_tweets_token <- ncdc_tweets_token %>%
  mutate_at("tweets", ~lemmatize_words(.))

## Wordcloud

ncdc_tweets_token %>%
  count(tweets) %>%
  with(wordcloud(tweets, n, min.freq = 5,           max.words= 150, random.order= FALSE, rot.per = 0.35, colors=brewer.pal(8, "Dark2")))

## Word variant in NCDC tweet according to COVID-19 phases


ncdc_tweets_token %>% 
  group_by(covid19_time_period) %>% 
  count(tweets, sort = TRUE) %>%
  slice_max(order_by = n, n = 15) %>% ungroup() %>% 
  ggplot(aes(x = reorder_within(tweets, n, covid19_time_period), y = n, fill = covid19_time_period)) + geom_col(show.legend = FALSE) + facet_wrap(~covid19_time_period, scales = "free") + scale_x_reordered() + coord_flip() + 
  labs(y = "Frequency of words", x = NULL, title = NULL)+ theme(axis.text.y = element_text(face = "bold"))
