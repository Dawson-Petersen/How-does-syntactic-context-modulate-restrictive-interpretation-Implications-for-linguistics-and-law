####Packages####
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)
library(lmerTest)
library(extrafont)

options(contrasts = c("contr.treatment", "contr.poly"))

#### Load and Prepare Data ####
df = read.csv("clean_data.csv")

df = df %>%
  mutate(across(c(participants,
                  items,
                  syntactic_condition,
                  restricted,
                  legal_background,
                  politics
                  ), as.factor)) %>%
  mutate(syntactic_condition = relevel(as.factor(syntactic_condition), ref = "baseline"))


#### Descriptive Stats ####

table(df$items)
table(df$syntactic_condition)
table(df$items, df$syntactic_condition)


aggregate(restricted ~ syntactic_condition, data = df, FUN = function(x) sum(x == "Restricted") / (sum(x == "Restricted") + sum(x == "Broad")))


#### Regression Analysis ####
model = glmer(restricted ~ syntactic_condition + (1 | items), data = df, family = binomial)
summary(model)

#### Post hoc tests 
subset = df[df$syntactic_condition != "baseline", ]
subset$syntactic_condition = factor(subset$syntactic_condition)
chisq.test(table(subset$syntactic_condition, subset$restricted))


df$type = ifelse(as.numeric(df$items) <= 6, "noscitur", "ejusdem")
df$type = as.factor(df$type)
model2 = glmer(restricted ~ syntactic_condition + type + (1 | items), data = df, family = binomial)
summary(model2)

anova(model, model2)


#### Graphs
aggr_interp <- df %>%
  group_by(syntactic_condition) %>%
  summarise(
    n = sum(restricted %in% c("Restricted", "Broad")),
    restricted = sum(restricted == "Restricted") / n,
    se = sqrt((restricted * (1 - restricted)) / n),
    .groups = "drop"
  )


ggplot(aggr_interp, aes(x = syntactic_condition, fill = syntactic_condition, y = restricted)) + 
  geom_col(position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(
    aes(ymin = restricted - se, ymax = restricted + se),
    position = position_dodge(width = 0.9, preserve = "single"),
    width = 0.25 
  ) +
  labs(y = "Proportion of 'Restricted' Interpretations", x = "Syntactic Condition", title = "n = 1438") +
  scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 1.01), clip = "off") + 
  scale_fill_manual(
    values = c("#D6D2C4", "#00B5E2", "#041E42"),
    labels = c("Basline", "Phrasal Coordination", "Simple List")) +
  scale_x_discrete(drop = FALSE, labels = c("Baseline", "Phrasal\nCoordination", "Simple\nList")) +
    scale_fill_manual(
    values = c("#D6D2C4", "#00B5E2", "#041E42"),
    labels = c("Baseline", "Phrasal Coordination", "Simple List")) +
  theme(
    panel.background = element_rect(fill = "#ffffff", color = NA), 
    plot.background = element_rect(fill = "#ffffff", color = NA),
    text = element_text(family = "Georgia", color = "#000000", size = 28),
    axis.text.y = element_text(color = "#000000", size = 32),
    axis.text.x = element_text(color = "#000000", size = 28, margin = margin(t = 35)), 
    axis.title.x = element_text(margin = margin(t = 3), size = 32),
    axis.title.y = element_text(size = 26),
    plot.title = element_text(size = 22, hjust = 0.5),
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(1, 1, 1.1, 1), "lines"), 
    legend.position = "none"
  )


aggr_items_interp <- df %>%
  group_by(syntactic_condition, items) %>%
  summarise(
    n = sum(restricted %in% c("Restricted", "Broad")),
    restricted = sum(restricted == "Restricted") / n,
    se = sqrt((restricted * (1 - restricted)) / n),
    .groups = "drop"
  )

aggr_items_interp$type = ifelse(as.numeric(aggr_items_interp$items) <= 6, "Noscitur", "Ejusdem")
aggr_items_interp$type = factor(aggr_items_interp$type, levels = c("Noscitur", "Ejusdem"))

ggplot(aggr_items_interp, aes(x = items, y = restricted, fill = syntactic_condition)) + 
  geom_col(position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(
    aes(ymin = restricted - se, ymax = restricted + se),
    position = position_dodge(width = 0.9, preserve = "single"),
    width = 0.25 
  ) +
  labs(y = "Proportion of 'Restricted' Interpretations", x = "Item", 
       fill = "Syntactic Condition", title = "") +
  scale_fill_manual(
    values = c("#D6D2C4", "#00B5E2", "#041E42"),
    labels = c("Baseline", "Phrasal Coordination", "Simple List")) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 1.01), clip = "off") + 
  facet_wrap(~type, scales = "free_x") +
  theme(
    panel.background = element_rect(fill = "#ffffff", color = NA), 
    plot.background = element_rect(fill = "#ffffff", color = NA),
    text = element_text(family = "Georgia", color = "#000000", size = 28),
    axis.text.y = element_text(color = "#000000", size = 32),
    axis.text.x = element_text(color = "#000000", size = 28, margin = margin(t = 35)), 
    axis.title.x = element_text(margin = margin(t = 3), size = 32),
    axis.title.y = element_text(size = 26),
    plot.title = element_text(size = 22, hjust = 0.5),
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(1, 1, 1.1, 1), "lines"), 
    legend.position = "none",
    strip.text = element_text(size = 32),
    strip.background = element_blank()
  )

