# ===========================================================
# R CODE
# UMHB BLOG, DECEMBER 2016
# AARON R. BAGGETT, PH.D.
# ===========================================================

# Load and/or install package libraries
library(ggplot2)
library(dplyr)
library(lsr)

# Read in data
gss <- read.csv("http://aaronbaggett.com/data/gss.csv")

# Descriptive/summary statistics for GSS data
gss_summ <- gss %>%
  group_by(life) %>% 
  summarize(n = length(tv),
    min = min(tv),
    max = max(tv),
    mean = mean(tv),
    sd = sd(tv),
    ci = 1.96 * (sd/(sqrt(n))))

# Mean and 95% CI plot
ggplot(data = gss_summ, aes(x = life, y = mean)) + geom_point(size = 3) +
  geom_errorbar(aes(x = life, ymax = mean + ci, ymin = mean - ci),
    width = 0, size = 0.5) + scale_x_discrete(name = "\nOutlook on Life") +
  scale_y_continuous(name = "Mean Hours of TV Nightly\n",
    limits = c(1, 6), breaks = seq(0, 20, 1)) + theme_bw()

# Calculate and print ANOVA results
m1 <- aov(tv ~ life, data = gss)
summary(m1)
TukeyHSD(m1)
etaSquared(m1)
