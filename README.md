
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#4 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday (after Thanksgiving) to polish things.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 4: Scraping (into) the Hall of Fame

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# Function of above work made into a function for a given year
Year_BBHOF <- function(url) {
  html <- read_html(url)
  tables <- html_table(html)

  data <- tables[[1]]
  
  actual_col_names <- data[1, ]
  colnames(data) <- actual_col_names
  data <- data[-1, ]
  
  data$Votes <- as.numeric(data$Votes)
  data <- data %>% select(
    Name, Votes, 
  ) %>% mutate(
    yearID = 2024,
    votedBy = "BBWAA",
    ballots = 385,
    needed = 289,
    inducted = ifelse(Votes>=289, "Y", "N"),
    category = NA, # don't know yet
    needed_note = NA # not sure what would go here
  ) %>% rename(
    votes = Votes, 
    player_name = Name
  )
  
  data$player_name <- gsub("X-", "", data$player_name)
  data$votes <- as.integer(data$votes)
  data$inducted <- factor(data$inducted)
  data$needed_note <- as.character(data$needed_note)
  data$category <- factor(data$category, levels = c("Executive", "Manager", "Pioneer", "Pioneer/Executive", "Player", "Umpire"))

  player_id <- People %>% 
  mutate(
    player_name = paste(nameFirst, nameLast)
  ) %>% 
  select(playerID, player_name)

  data <- data %>% 
    mutate(
      player_name = str_replace_all(player_name,
                                c("\\á" = "a", "\\é" = "e", "\\í" = "i", "\\ó" = "o"))
    )
  
  player_id <- player_id %>% 
  mutate(
    player_name = str_replace(player_name,
                              "\\. ",
                              ".")
  )
  
  data <- data %>% left_join(player_id, by = "player_name") %>% 
    select(
      names(HallOfFame)
    )
  
  data
}

hof24 <- Year_BBHOF("https://www.baseball-reference.com/awards/hof_2024.shtml")

HoF_Complete <- rbind(hof24, HallOfFame)
HoF_Complete
```

    ## # A tibble: 6,411 × 9
    ##    playerID  yearID votedBy ballots needed votes inducted category needed_note
    ##    <chr>      <dbl> <chr>     <dbl>  <dbl> <dbl> <fct>    <fct>    <chr>      
    ##  1 beltrad01   2024 BBWAA       385    289   366 Y        <NA>     <NA>       
    ##  2 heltoto01   2024 BBWAA       385    289   307 Y        <NA>     <NA>       
    ##  3 mauerjo01   2024 BBWAA       385    289   293 Y        <NA>     <NA>       
    ##  4 wagnebi02   2024 BBWAA       385    289   284 N        <NA>     <NA>       
    ##  5 sheffga01   2024 BBWAA       385    289   246 N        <NA>     <NA>       
    ##  6 jonesan01   2024 BBWAA       385    289   237 N        <NA>     <NA>       
    ##  7 beltrca01   2024 BBWAA       385    289   220 N        <NA>     <NA>       
    ##  8 rodrial01   2024 BBWAA       385    289   134 N        <NA>     <NA>       
    ##  9 ramirma02   2024 BBWAA       385    289   125 N        <NA>     <NA>       
    ## 10 utleych01   2024 BBWAA       385    289   111 N        <NA>     <NA>       
    ## # ℹ 6,401 more rows

``` r
HoF_Complete %>% 
  ggplot(aes(x = yearID, fill = inducted)) +
  geom_bar()
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
