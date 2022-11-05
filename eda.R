Dat <- fakeAPI
Dat$uid <- paste("USD",as.character(Dat$uid))
Dat$schooltype <- ifelse(Dat$schooltype=="E","Elementary",
                         ifelse(Dat$schooltype=="M","Middle School","High School"))
TopCounties <- Dat %>% group_by(county) %>% summarize(n=n()) %>% arrange(desc(n)) %>% .[1:15,]

Dat <- Dat %>% filter(county %in% TopCounties$county)

First <- Dat %>%
  group_by(county) %>%
  summarize(minapi=min(api),
            diffmeals=max(api)-min(api))

## All should have a simple table

## Chris Ward has the min api, so we should have them as the one wiht the "wall of shame"
First %>%
  arrange(minapi) %>%
  select(county)

Dat %>% filter(county=="Chris Ward") %>%
  arrange(desc(api))

# ## Chris Spielman & Randy Gradishar have the biggest difference in meals, so we should have them with the box and whispers plots looking for outliers
# First %>%
#   arrange(desc(diffmeals)) %>%
#   select(county)

## Jack Tatum and Chris Spielman are two counties that were put on notice for the inequities between their urban and non-urban school performance
## For this reason, they will need box and whisker plots to see outliers
Dat %>%
  filter(county %in% TopCounties$county) %>%
  mutate(community=ifelse(community=="Urban","Urban","NonUrban")) %>%
  group_by(county,community) %>%
  summarize(api=mean(api)) %>%
  pivot_wider(names_from=community,values_from=api) %>%
  transmute(Diff=NonUrban-Urban) %>%
  arrange(desc(Diff))

BoxPlots <- Dat %>% 
  filter(county %in% c("Jack Tatum","Chris Spielman")) %>%
  mutate(community=ifelse(community=="Urban","Urban","Non-Urban"))
  
ggplot(BoxPlots[BoxPlots$county=="Jack Tatum",],aes(x=meals,fill=community)) +
  geom_boxplot() +
  theme_pander()


