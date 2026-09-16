# Import libraries
library(fGarch)
library(simr)
library(effectsize)
library(tidyr)
library(dplyr)


####### Generate Data #######
#set seed
set.seed(618)

#design
n.items = 12
conditions = c(0,1,2)


nn.items = n.items * length(conditions)

## Sample Size
n = 1440
trials = 1
obs = n * trials

obs_per_condition = round(obs / nn.items)

## Sanity check
obs %% nn.items == 0

## IVs
## Items
items = seq(1, n.items, 1)
items = rep(items, each = (obs / n.items))

## syntactic
syntactic_condition = rep(conditions, (obs / length(conditions)))


## Participants
participants = rep(seq(1, n, 1), trials)


## Create Dataframe
df = data.frame(participants = as.factor(participants), 
                items = as.factor(items), 
                syntactic_condition = as.factor(syntactic_condition)) %>% 
  mutate(syntactic_condition = relevel(as.factor(syntactic_condition), ref = "0"))

## Sanity Check 2
nrow(unique.data.frame(df)) == nrow(df)

## DV
list0 = c(rep(0, round((obs / 3) * .55)),  rep(1, round((obs / 3) * (1 - .55))))
list1 = c(rep(0, round((obs / 3) * .45)),  rep(1, round((obs / 3) * (1 - .45))))
list2 = c(rep(0, round((obs / 3) * .4)),  rep(1, round((obs / 3) * (1 - .4))))

df$restricted[df$syntactic_condition == "0"] = sample(list0)
df$restricted[df$syntactic_condition == "1"] = sample(list1)
df$restricted[df$syntactic_condition == "2"] = sample(list2)

effects = aggregate(restricted ~ syntactic_condition, data = df, FUN = function(x) mean(x))
print(effects)

#calculate simulated odds ratios 
effects$restricted[effects$syntactic_condition == 1] / effects$restricted[effects$syntactic_condition == 0] 
effects$restricted[effects$syntactic_condition == 2] / effects$restricted[effects$syntactic_condition == 0] 

####### Regression Model #######
model = glmer(restricted ~ syntactic_condition + (1 | items), data = df, family = binomial)
summary(model)


VarCorr(model) <- list("items" = diag(c(1.5)))

####### Power Analysis #######

## Simulation   
sim1 = powerSim(model, simr::fixed("syntactic_condition1"), nsim = 1000)
print(sim1)

sim2 = powerSim(model, simr::fixed("syntactic_condition2"), nsim = 1000)
print(sim2)

