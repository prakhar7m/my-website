---
title: "Analyzing Deforestation"
description: "Description"
categories:
  - r
  - Data Analysis
tags:
  - r
  - statistics
---
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/cover.jpg" alt="Untitled" />

```ruby
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())
```


```ruby
tt <- tt_load("2021-04-06")
tt$forest %>%
  count(entity, sort = TRUE) %>%
  View()

```

```ruby
forest_area_country <- tt$forest_area %>%
  filter(str_length(code) == 3,
         year >= 1992) %>%
  rename(country = entity) %>%
  mutate(forest_area = forest_area / 100)
forest <- tt$forest %>%
  filter(str_length(code) == 3) %>%
  rename(country = entity)
forest_area_country %>%
  filter(country %in% c("United States", "Canada", "China", "India",
                        "Senegal", "Germany", "Brazil", "Poland")) %>%
  mutate(country = fct_reorder(country, -forest_area)) %>%
  ggplot(aes(year, forest_area, color = country)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  expand_limits(y = 0) +
  labs(x = "Year",
       y = "% of global forest area")
```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-2-1.png" alt="Untitled" />


```ruby
forest_area_country %>%
  mutate(country = fct_lump(country, 9, w = forest_area)) %>%
  group_by(country, year) %>%
  summarize(forest_area = sum(forest_area),
            .groups = "drop") %>%
  mutate(country = fct_reorder(country, -forest_area)) %>%
  ggplot(aes(year, forest_area, fill = country)) +
  geom_area() +
  scale_y_continuous(labels = percent) +
  expand_limits(y = 0) +
  labs(x = "Year",
       y = "% of global forest area")
forest_area_country %>%
  filter(year %in% c(1992, 2020)) %>%
  mutate(year = paste0("forest_area_", year)) %>%
  spread(year, forest_area) %>%
  arrange(desc(forest_area_1992))
```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-3-1.png" alt="Untitled" />


```ruby
forest %>%
  group_by(year) %>%
  summarize(net_forest_conversion = sum(net_forest_conversion))
forest %>%
  filter(year == 2015) %>%
  arrange((net_forest_conversion)) %>%
  slice_max(abs(net_forest_conversion), n = 20) %>%
  mutate(country = fct_reorder(country, net_forest_conversion)) %>%
  ggplot(aes(net_forest_conversion, country,
             fill = net_forest_conversion > 0)) +
  geom_col() +
  scale_x_continuous(labels = comma) +
  theme(legend.position = "none") +
  labs(x = "Net change in forest in 2015 (hectares)",
       y = "")

```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-4-1.png" alt="Untitled" />


```ruby
library(tidytext)
forest %>%
  group_by(year) %>%
  slice_max(abs(net_forest_conversion), n = 10) %>%
  ungroup() %>%
  mutate(country = reorder_within(country, net_forest_conversion, year)) %>%
  ggplot(aes(net_forest_conversion, country,
             fill = net_forest_conversion > 0)) +
  geom_col() +
  facet_wrap(~ year, scales = "free_y") +
  scale_x_continuous(label = comma) +
  scale_y_reordered() +
  theme(legend.position = "none") +
  labs(x = "Net change in forest in 2015 (hectares)",
       y = "")

```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-5-1.png" alt="Untitled" />


```ruby
forest %>%
  mutate(country = fct_lump(country, 8, w = abs(net_forest_conversion))) %>%
  group_by(country, year) %>%
  summarize(net_forest_conversion = sum(net_forest_conversion),
            .groups = "drop") %>%
  mutate(country = fct_reorder(country, -net_forest_conversion)) %>%
  ggplot(aes(year, net_forest_conversion, color = country)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(y = "Net change in forest (hectares)")
```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-6-1.png" alt="Untitled" />


```ruby
brazil_loss <- tt$brazil_loss %>%
  pivot_longer(commercial_crops:small_scale_clearing,
               names_to = "cause",
               values_to = "loss") %>%
  mutate(cause = str_to_sentence(str_replace_all(cause, "_", " ")))
brazil_loss %>%
  filter(year == max(year)) %>%
  arrange(desc(loss)) %>%
  mutate(cause = fct_reorder(cause, loss)) %>%
  ggplot(aes(loss, cause)) +
  geom_col() +
  scale_x_continuous(labels = comma) +
  labs(x = "Loss of forest in 2013 (hectares)",
       y = "")

```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-7-1.png" alt="Untitled" />


```ruby
brazil_loss %>%
  mutate(cause = fct_reorder(cause, -loss)) %>%
  ggplot(aes(year, loss, color = cause)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(y = "Loss of forest (hectares)",
       x = "")
```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-8-1.png" alt="Untitled" />


```ruby
brazil_loss %>%
  mutate(cause = fct_lump(cause, 6, w = loss)) %>%
  group_by(cause, year) %>%
  summarize(loss = sum(loss), .groups = "drop") %>%
  mutate(cause = fct_reorder(cause, loss)) %>%
  ggplot(aes(year, loss, fill = cause)) +
  geom_area() +
  scale_y_continuous(labels = comma) +
  labs(y = "Loss of forest (hectares)",
       x = "")
```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-9-1.png" alt="Untitled" />


```ruby
soybean_use <- tt$soybean_use %>%
  filter(str_length(code) == 3) %>%
  rename(country = entity) %>%
  mutate(total = human_food + animal_feed + processed) %>%
  pivot_longer(human_food:processed,
               names_to = "use",
               values_to = "amount") %>%
  replace_na(list(amount = 0)) %>%
  arrange(desc(total)) %>%
  mutate(use = str_to_sentence(str_replace_all(use, "_", " ")))
vegetable_oil <- tt$vegetable_oil %>%
  rename(country = entity)
soybean_use %>%
  filter(country %in% c("Brazil", "United States", "China", "Indonesia")) %>%
  ggplot(aes(year, amount, fill = use)) +
  geom_area() +
  scale_y_continuous(labels = comma) +
  labs(y = "Soybeans (tonnes)",
       fill = "") +
  facet_wrap(~ country)
```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-10-1.png" alt="Untitled" />


```ruby
vegetable_oil %>%
  filter(!is.na(production)) %>%
  mutate(crop_oil = fct_lump(crop_oil, 5, w = production)) %>%
  group_by(country, crop_oil, year) %>%
  summarize(production = sum(production)) %>%
  filter(country %in% c("United States", "India", "China", "Indonesia")) %>%
  ggplot(aes(year, production, fill = crop_oil)) +
  geom_area() +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ country)
```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-11-1.png" alt="Untitled" />


```ruby
library(fuzzyjoin)
library(ggthemes)
country_data <- forest %>%
  filter(year == 2010) %>%
  inner_join(maps::iso3166, by = c(code = "a3"))
map_data("world") %>%
  as_tibble() %>%
  filter(region != "Antarctica") %>%
  regex_left_join(country_data, by = c(region = "mapname")) %>%
  ggplot(aes(long, lat, group = group, fill = net_forest_conversion)) +
  geom_polygon(color = "black", size = .05) +
  scale_fill_gradient2(low = "red", high = "green",
                       labels = comma) +
  theme_map() +
  labs(fill = "Net forest change (2010)")
```
<img src="{{site.baseurl | prepend: site.url}}images/deforestation/unnamed-chunk-12-1.png" alt="Untitled" />

