HW1
================
Elisaveta
2023-11-04

# Task 1

``` r
insurance <- read.csv('insurance_cost.csv',
                 stringsAsFactors = T)
```

# Task 2

``` r
ggplot(data = insurance, 
       aes(x = bmi)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](HW1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(data = insurance, 
       aes(x = charges)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](HW1_files/figure-gfm/unnamed-chunk-2-2.png)<!-- --> \# Task 3

``` r
charges_mean <- round(mean(insurance$charges),1)
charges_median <- round(median(insurance$charges),1)


ggplot(data = insurance, 
       aes(x = charges)) +
  geom_density() +
  geom_vline(aes(xintercept = charges_median), color = 'red') +
  annotate("text", 
           x= charges_median -3000, 
           y= 0,
           label=paste0("Median=", charges_median),
           color = 'red') +
  geom_vline(aes(xintercept = charges_mean), color = 'blue') +
  annotate("text", 
           x= charges_mean+3500, 
           y= 0,
           label=paste0("Mean=", charges_mean),
           color = 'blue')
```

![](HW1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
  theme_minimal() + 
  labs(y = 'Плотность', x = 'Расходы')
```

    ## List of 99
    ##  $ line                      :List of 6
    ##   ..$ colour       : chr "black"
    ##   ..$ linewidth    : num 0.5
    ##   ..$ linetype     : num 1
    ##   ..$ lineend      : chr "butt"
    ##   ..$ arrow        : logi FALSE
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
    ##  $ rect                      :List of 5
    ##   ..$ fill         : chr "white"
    ##   ..$ colour       : chr "black"
    ##   ..$ linewidth    : num 0.5
    ##   ..$ linetype     : num 1
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
    ##  $ text                      :List of 11
    ##   ..$ family       : chr ""
    ##   ..$ face         : chr "plain"
    ##   ..$ colour       : chr "black"
    ##   ..$ size         : num 11
    ##   ..$ hjust        : num 0.5
    ##   ..$ vjust        : num 0.5
    ##   ..$ angle        : num 0
    ##   ..$ lineheight   : num 0.9
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : logi FALSE
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ title                     : NULL
    ##  $ aspect.ratio              : NULL
    ##  $ axis.title                : NULL
    ##  $ axis.title.x              :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 2.75points 0points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.title.x.top          :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 0
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 2.75points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.title.x.bottom       : NULL
    ##  $ axis.title.y              :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : num 90
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 2.75points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.title.y.left         : NULL
    ##  $ axis.title.y.right        :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 0
    ##   ..$ angle        : num -90
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.75points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text                 :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : chr "grey30"
    ##   ..$ size         : 'rel' num 0.8
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text.x               :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 2.2points 0points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text.x.top           :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : num 0
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 2.2points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text.x.bottom        : NULL
    ##  $ axis.text.y               :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 1
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text.y.left          : NULL
    ##  $ axis.text.y.right         :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 0
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.2points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.ticks                : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ axis.ticks.x              : NULL
    ##  $ axis.ticks.x.top          : NULL
    ##  $ axis.ticks.x.bottom       : NULL
    ##  $ axis.ticks.y              : NULL
    ##  $ axis.ticks.y.left         : NULL
    ##  $ axis.ticks.y.right        : NULL
    ##  $ axis.ticks.length         : 'simpleUnit' num 2.75points
    ##   ..- attr(*, "unit")= int 8
    ##  $ axis.ticks.length.x       : NULL
    ##  $ axis.ticks.length.x.top   : NULL
    ##  $ axis.ticks.length.x.bottom: NULL
    ##  $ axis.ticks.length.y       : NULL
    ##  $ axis.ticks.length.y.left  : NULL
    ##  $ axis.ticks.length.y.right : NULL
    ##  $ axis.line                 : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ axis.line.x               : NULL
    ##  $ axis.line.x.top           : NULL
    ##  $ axis.line.x.bottom        : NULL
    ##  $ axis.line.y               : NULL
    ##  $ axis.line.y.left          : NULL
    ##  $ axis.line.y.right         : NULL
    ##  $ legend.background         : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ legend.margin             : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
    ##   ..- attr(*, "unit")= int 8
    ##  $ legend.spacing            : 'simpleUnit' num 11points
    ##   ..- attr(*, "unit")= int 8
    ##  $ legend.spacing.x          : NULL
    ##  $ legend.spacing.y          : NULL
    ##  $ legend.key                : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ legend.key.size           : 'simpleUnit' num 1.2lines
    ##   ..- attr(*, "unit")= int 3
    ##  $ legend.key.height         : NULL
    ##  $ legend.key.width          : NULL
    ##  $ legend.text               :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : 'rel' num 0.8
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ legend.text.align         : NULL
    ##  $ legend.title              :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 0
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ legend.title.align        : NULL
    ##  $ legend.position           : chr "right"
    ##  $ legend.direction          : NULL
    ##  $ legend.justification      : chr "center"
    ##  $ legend.box                : NULL
    ##  $ legend.box.just           : NULL
    ##  $ legend.box.margin         : 'margin' num [1:4] 0cm 0cm 0cm 0cm
    ##   ..- attr(*, "unit")= int 1
    ##  $ legend.box.background     : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ legend.box.spacing        : 'simpleUnit' num 11points
    ##   ..- attr(*, "unit")= int 8
    ##  $ panel.background          : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ panel.border              : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ panel.spacing             : 'simpleUnit' num 5.5points
    ##   ..- attr(*, "unit")= int 8
    ##  $ panel.spacing.x           : NULL
    ##  $ panel.spacing.y           : NULL
    ##  $ panel.grid                :List of 6
    ##   ..$ colour       : chr "grey92"
    ##   ..$ linewidth    : NULL
    ##   ..$ linetype     : NULL
    ##   ..$ lineend      : NULL
    ##   ..$ arrow        : logi FALSE
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
    ##  $ panel.grid.major          : NULL
    ##  $ panel.grid.minor          :List of 6
    ##   ..$ colour       : NULL
    ##   ..$ linewidth    : 'rel' num 0.5
    ##   ..$ linetype     : NULL
    ##   ..$ lineend      : NULL
    ##   ..$ arrow        : logi FALSE
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
    ##  $ panel.grid.major.x        : NULL
    ##  $ panel.grid.major.y        : NULL
    ##  $ panel.grid.minor.x        : NULL
    ##  $ panel.grid.minor.y        : NULL
    ##  $ panel.ontop               : logi FALSE
    ##  $ plot.background           : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ plot.title                :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : 'rel' num 1.2
    ##   ..$ hjust        : num 0
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 5.5points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ plot.title.position       : chr "panel"
    ##  $ plot.subtitle             :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 0
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 0points 0points 5.5points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ plot.caption              :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : 'rel' num 0.8
    ##   ..$ hjust        : num 1
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 5.5points 0points 0points 0points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ plot.caption.position     : chr "panel"
    ##  $ plot.tag                  :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : 'rel' num 1.2
    ##   ..$ hjust        : num 0.5
    ##   ..$ vjust        : num 0.5
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ plot.tag.position         : chr "topleft"
    ##  $ plot.margin               : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
    ##   ..- attr(*, "unit")= int 8
    ##  $ strip.background          : list()
    ##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
    ##  $ strip.background.x        : NULL
    ##  $ strip.background.y        : NULL
    ##  $ strip.clip                : chr "inherit"
    ##  $ strip.placement           : chr "inside"
    ##  $ strip.text                :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : chr "grey10"
    ##   ..$ size         : 'rel' num 0.8
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : 'margin' num [1:4] 4.4points 4.4points 4.4points 4.4points
    ##   .. ..- attr(*, "unit")= int 8
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ strip.text.x              : NULL
    ##  $ strip.text.x.bottom       : NULL
    ##  $ strip.text.x.top          : NULL
    ##  $ strip.text.y              :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : num -90
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ strip.text.y.left         :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : num 90
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi TRUE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ strip.text.y.right        : NULL
    ##  $ strip.switch.pad.grid     : 'simpleUnit' num 2.75points
    ##   ..- attr(*, "unit")= int 8
    ##  $ strip.switch.pad.wrap     : 'simpleUnit' num 2.75points
    ##   ..- attr(*, "unit")= int 8
    ##  $ y                         : chr "Плотность"
    ##  $ x                         : chr "Расходы"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi TRUE
    ##  - attr(*, "validate")= logi TRUE

# Task 4

``` r
ggplot() +
  geom_boxplot(data = insurance, 
               aes(x = charges, y = sex)) +
  theme_minimal() + 
  labs(y = 'Пол', x = 'Расходы')
```

![](HW1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot() +
  geom_boxplot(data = insurance, 
               aes(x = charges, y = smoker)) +
  theme_minimal() + 
  labs(y = 'Курение', x = 'Расходы')
```

![](HW1_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
ggplot() +
  geom_boxplot(data = insurance, 
               aes(x = charges, y = region)) +
  theme_minimal() + 
  labs(y = 'Регион', x = 'Расходы')
```

![](HW1_files/figure-gfm/unnamed-chunk-4-3.png)<!-- --> \# Task 5

``` r
dencity_plot <- ggplot(data = insurance, 
       aes(x = charges)) +
  geom_density() +
  geom_vline(aes(xintercept = charges_median), color = 'red') +
  annotate("text", 
           x= charges_median -3000, 
           y= 0,
           label=paste0("Median=", charges_median),
           color = 'red') +
  geom_vline(aes(xintercept = charges_mean), color = 'blue') +
  annotate("text", 
           x= charges_mean+3500, 
           y= 0,
           label=paste0("Mean=", charges_mean),
           color = 'blue') +
  theme_minimal() + 
  labs(y = 'Плотность', x = 'Расходы')
bx1 <- ggplot() +
  geom_boxplot(data = insurance, 
               aes(x = charges, y = sex)) +
  theme_minimal() + 
  labs(y = 'Пол', x = 'Расходы')
bx2 <- ggplot() +
  geom_boxplot(data = insurance, 
               aes(x = charges, y = smoker)) +
  theme_minimal() + 
  labs(y = 'Курение', x = 'Расходы')
bx3 <- ggplot() +
  geom_boxplot(data = insurance, 
               aes(x = charges, y = region)) +
  theme_minimal() + 
  labs(y = 'Регион', x = 'Расходы')

ggpubr::ggarrange(dencity_plot, bx1, bx2, bx3,
                  labels = c("График плотности", "Боксплот 1", "Боксплот 2", "Боксплот 3"),
                  ncol = 1, nrow = 4) +
  ggtitle('4 графика')
```

![](HW1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Task 6

``` r
ggplot(data = insurance, 
       aes(x = charges, color = region, fill = region, group = region)) +
  geom_density() +
  facet_grid(. ~ region) +
  theme_minimal() + 
  labs(y = 'Плотность', x = 'Расходы')
```

![](HW1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Task 7

``` r
insurance %>% 
  ggplot(aes(x=age, y=charges)) + 
    geom_point(size=3) +
  theme_minimal() +
  ggtitle('scatter plot отношения переменных age и charges') +
  labs(y = 'Расходы', x = 'Возраст') +
  theme(
    axis.text.x = element_text(size = 14))
```

![](HW1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Task 8

``` r
insurance %>% 
  ggplot(aes(x=age, y=charges)) + 
    geom_point(size=3) +
  theme_minimal() +
  ggtitle('scatter plot отношения переменных age и charges') +
  labs(y = 'Расходы', x = 'Возраст') +
  theme(
    axis.text.x = element_text(size = 14))+
  geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HW1_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Task 9

``` r
insurance %>% 
  ggplot(aes(x=age, y=charges, collor = smoker)) + 
    geom_point(size=3) +
  theme_minimal() +
  ggtitle('scatter plot отношения переменных age и charges') +
  labs(y = 'Расходы', x = 'Возраст') +
  theme(
    axis.text.x = element_text(size = 14))+
  geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HW1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Task 10

``` r
insurance %>% 
  ggplot(aes(x=bmi, y=charges)) + 
    geom_point(size=3) +
  theme_minimal() +
  ggtitle('scatter plot отношения переменных bmi и charges') +
  labs(y = 'Расходы', x = 'bmi') +
  theme(
    axis.text.x = element_text(size = 14))
```

![](HW1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
insurance %>% 
  ggplot(aes(x=bmi, y=charges)) + 
    geom_point(size=3) +
  theme_minimal() +
  ggtitle('scatter plot отношения переменных bmi и charges') +
  labs(y = 'Расходы', x = 'bmi') +
  theme(
    axis.text.x = element_text(size = 14))+
  geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HW1_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
insurance %>% 
  ggplot(aes(x=bmi, y=charges, collor = smoker)) + 
    geom_point(size=3) +
  theme_minimal() +
  ggtitle('scatter plot отношения переменных bmi и charges') +
  labs(y = 'Расходы', x = 'bmi') +
  theme(
    axis.text.x = element_text(size = 14))+
  geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HW1_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

# Task 11

Вопрос: сравнить расходы среди курящих в зависимости от пола.

Аргументация выбора типа графика. Боксплот показывает распределение
значений в выборке и основные статистические показатели: медиану,
верхний и нижний квартили, наблюдаемые минимумы и максимумы, а также
выбросы. Удобно использовать для быстрой оценки данных.

``` r
insurance %>%
  filter(smoker == "yes") %>%
  ggplot(aes(x = charges, y = sex)) +
  geom_boxplot() +
  theme_minimal() + 
  labs(y = 'Пол', x = 'Расходы')+
  ggtitle('расходы среди курящих в зависимости от пола')
```

![](HW1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> \# Task 12

Вопрос: существует ли взаимосвязь bmi и возраста среди женщин.

Аргументация выбора типа графика. Диаграммы рассеяния удобно
использовать для демонстрации наличия или отсутствия корреляции между
двумя переменными.

``` r
insurance %>%
  filter(sex == "female") %>%
  ggplot(aes(x = bmi, y = age)) +
  geom_point(size=3) +
  theme_minimal() +
  ggtitle('Диаграмма рассеяния bmi и возраста') +
  labs(y = 'Возраст', x = 'bmi') + 
  geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HW1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

# Task 13

Вопрос: представить визуализацию распределения данных расходы в
зависимости от количества детей, у проживающих в ренионе northeast

Аргументация выбора типа графика. График плотности удобный инструмент
визуализации распределения данных/

``` r
insurance %>%
  filter(region == "northeast") %>%
  ggplot(aes(x = charges, color = children, fill = children, group = children)) +
  geom_density() +
  facet_grid(. ~ children) +
  theme_minimal() + 
  labs(y = 'Плотность', x = 'Расходы') +
  ggtitle('График плотности вероятности') 
```

![](HW1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# Task 14

``` r
insurance <- insurance %>% 
  mutate(
    age_group = case_when(
      age < 35 ~ "21-34",
      age >= 35 & age < 50 ~ "35-49",
      age >= 50 ~ "50+"
    ))

insurance %>%
  ggplot(aes(x = bmi, y = log(charges))) +
  geom_point(size=3, color = "#926EAE", alpha=0.7) +
  theme_minimal() +
  ggtitle('Отношение индекса массы тела к логарифму трат по возрастным группам') +
  labs(y = 'log(charges)', x = 'bmi') + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ age_group)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HW1_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
