library(mgcv)
library(lme4)
library(tidyverse)
library(ggthemes)
theme_set(theme_bw())
theme_update(panel.grid = element_blank())
set.seed(2) ## simulate some data... 
dat <- gamSim(1,n=400,dist="normal",scale=2)

mod <- gam(y ~ s(x2, k = 6, bs = "tp"), data = dat)
dat <- dat %>% 
  mutate(pred = predict(mod))

ggplot(dat, aes(x = x2)) + 
  geom_point(aes(y = y), alpha = .3) + 
  geom_line(aes(y = pred), size = 1)

sm <- smoothCon(s(x2, k = 6, bs = "tp"), data = dat)[[1]]
pm <- PredictMat(sm, data = dat)
colnames(pm) <- paste0("s", 1:ncol(pm))

grid <- as_tibble(pm) %>% 
  mutate(x2 = dat$x2) %>% 
  pivot_longer(cols = -x2) %>% 
  mutate(
    type = case_when(
      str_detect(name, "[1-4]") ~ "Random effects",
      TRUE ~ "Fixed effects"
    )
  )

ggplot(grid, aes(x = x2, y = value, group = name, color = name)) + 
  geom_line() + 
  facet_wrap(vars(type)) +
  scale_color_colorblind()


