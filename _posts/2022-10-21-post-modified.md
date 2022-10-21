---
title: "R Notebook"
output: html_notebook
editor_options: 
  chunk_output_type: inline
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(tidytuesdayR)
library(scales)
library(janitor)
theme_set(theme_light())
```


```{r}
data <- tt_load("2021-01-12")
```


```{r}
artwork<-data$artwork %>%
  filter(artist != "Turner, Joseph Mallord William")%>%
  clean_names()
  

artists<-data$artists %>%
  clean_names()
```


```{r}
artwork %>%
  separate(medium, c("medium", "surface"), sep = " on ",
           fill = "right", extra = "merge")%>%
  count(medium, sort =TRUE) %>%
  head(50) %>%
  replace_na(list(medium = "Missing"))%>%
  mutate(medium = fct_reorder(medium,n)) %>%
  ggplot(aes(n,medium))+
  geom_col() +
  labs(y = 'Medium(ignoring "on ..")',
       x = "# of pieces")

```


```{r}
artwork%>%
  filter(fct_lump(artist, 16)!= 'Other')%>%
  mutate(medium = fct_lump(medium,11))%>%
  count(artist, medium, sort =TRUE)%>%
  mutate(artist= fct_reorder(artist,n,sum))%>%
  ggplot(aes(n, artist, fill=medium))+
  geom_col()
```


```{r}
by_decade_medium <- artwork %>%
  separate(medium, c("medium", "surface"), sep = " on ",
           fill = "right", extra = "merge")%>%
  filter(year >= 1700)%>%
  count(decade = round(year, -1),
        medium = fct_lump(medium, 11))%>%
  complete(decade, medium, fill = list(n=0))%>%
  group_by(decade)%>%
  mutate(pct=n/sum(n))%>%
  ungroup()
 
```


```{r}
 by_decade_medium%>%
  ggplot(aes(decade, n , fill=medium))+
  geom_area()
```


```{r}
by_decade_medium%>%
  filter(!is.na(medium))%>%
  mutate(medium=fct_reorder(medium,n,sum))%>%
  ggplot(aes(decade, pct, fill= medium))+
  geom_area()+
  scale_y_continuous(labels = percent)+
  labs(x= "decade",
       y= "% of Tate Modern pieces",
       fill = "Medium(without on..)")
```


```{r}
by_decade_medium%>%
  filter(!is.na(medium))%>%
  mutate(medium=fct_reorder(medium,pct,sum, .desc = TRUE))%>%
  ggplot(aes(decade, pct, fill= medium))+
  geom_col()+
  facet_wrap(~medium, scales = "free_y")+
  scale_y_continuous(labels = percent)+
  theme(legend.position = "None")+
  labs(x= "decade",
       y= "% of Tate Modern pieces",
       fill = "Medium(without on..)")
```


```{r}
artwork_size<-artwork %>%
  filter(!is.na(units),
         !is.na(height),
         !is.na(width))%>%
  mutate(area=(height/1000) * (width/1000),
         ratio= width/height)

artwork_size%>%
  ggplot(aes(area))+
  geom_histogram()+
  scale_x_log10(labels = comma)+
  labs(x="Area(square metres")
```

```{r}
artwork_size%>%
  filter(ratio>.2, ratio<5)%>%
  ggplot(aes(ratio))+
  geom_histogram()+
  scale_x_log10(labels=comma)+
  geom_vline(xintercept= c(3/4,4/3),color='red', linetype='dashed')+
  labs(x="Width/Height ratio",
       y="What are typical shapes of Tate Artwork?",
       subtitle = "Red lines show 3:4 and 4:3 ratios")
```

```{r}
artwork_size%>%
  mutate(shape= case_when(
    ratio > 1.05 ~ "Landscape",
    ratio <.95 ~ "Portrait",
    TRUE ~ "Square"
    ))%>%
  count(shape, decade= round(year, -1))%>%
  filter(decade >=1700)%>%
  complete(decade, shape, fill= list(n=0))%>%
  group_by(decade)%>%
  mutate(pct=n/sum(n))%>%
  ggplot(aes(decade,pct, fill=shape))+
  geom_area()

```
```{r}
size_by_decade<- artwork_size%>%
  group_by(decade=round(year,-1))%>%
  summarize(median_ratio=median(ratio),
            median_area=median(area),
            n=n())%>%
  filter(decade>=1700)
  
size_by_decade%>%
  ggplot(aes(decade,median_ratio))+
  geom_line()+
  geom_point(aes(size=n))+
  labs(x="decade",
       y="median ratio(width/height")
  
```
```{r}
size_by_decade%>%
  ggplot(aes(decade,median_area))+
  geom_line()+
  geom_point(aes(size=n))+
  labs(x="decade",
       y="median area in metres^2 ")
```
```{r}
artwork_size%>%
  group_by(artist)%>%
  summarize(n_pieces=n(),
            avg_year= mean(year, na.rm=TRUE),
            first_year= min(year, na.rm = TRUE),
            last_year= max(year, na.rm = TRUE),
            median_area=median(area),
            median_ratio=median(ratio))%>%
  arrange(desc(n_pieces))

artwork_size %>%
  extract(medium, "medium_on", " on (.*)", remove = FALSE) %>%
  filter(fct_lump(artist, 25) != "Other") %>%
  add_count(artist) %>%
  mutate(artist = glue("{ artist } ({ n })")) %>%
  mutate(artist = fct_reorder(artist, ratio)) %>%
  ggplot(aes(ratio, artist)) +
  geom_boxplot() +
  geom_vline(xintercept = c(3 / 4, 4 / 3),
             linetype = "dashed", color = "red") +
  scale_x_log10(limits = c(.25, 4)) +
  labs(x = "Ratio (width / height)",
       y = "",
       title = "What aspect ratio do artists work in?",
       subtitle = "For the 25 most common artists in the Tate")

```
```{r}
library(ggridges)
artwork_size %>%
  extract(medium, "medium_on", " on (.*)", remove = FALSE) %>%
  filter(fct_lump(artist, 25) != "Other") %>%
  add_count(artist) %>%
  mutate(artist = glue("{ artist } ({ n })")) %>%
  mutate(artist = fct_reorder(artist, ratio)) %>%
  ggplot(aes(ratio, artist)) +
  geom_density_ridges() +
  geom_vline(xintercept = c(3 / 4, 4 / 3),
             linetype = "dashed", color = "red") +
  scale_x_log10(limits = c(.33, 3)) +
  labs(x = "Ratio (width / height)",
       y = "",
       title = "What aspect ratio do artists work in?",
       subtitle = "For the 25 most common artists in the Tate")
```
