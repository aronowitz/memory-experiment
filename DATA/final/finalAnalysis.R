### Memory Museum Pilot Study Analysis
# Trevor James Swanson
# 09/22/2020

### CONDITIONS:
# 1) Space + Semantics + Time
# 2) Space + Time
# 3) Space + Semantics
# 4) Time + Semantics
# 5) As much chaos as possible (control)


### ------------------------------------------------------------------------ ###
### --------------------------- DATA PREPARATION --------------------------- ###
### ------------------------------------------------------------------------ ###
source('../../extractData.R')
out <- getData(getJSONS(), matchPs = TRUE)
sorts <- lapply(out, lapply, getSorts)

sorts$dat1 <- lapply(sorts$dat1, function(z){
  z1 <- z[-(1:18), ]
  z1$trial <- z1$trial - 2
  return(z1)
})

simple <- lapply(sorts, function(z){
  k <- seq(1, unique(sapply(z, nrow)), by = 9)
  lapply(z, function(i) i[k, 1:3])
})

### Create datasets:
# Full data: all items, all IDs, all trials, all conditions
# Simple data: all IDs, all trials, all conditions

for(i in 1:3){
  for(j in 1:length(sorts[[i]])){
    sorts[[i]][[j]] <- cbind.data.frame(ID = j, sorts[[i]][[j]])
    simple[[i]][[j]] <- cbind.data.frame(ID = j, simple[[i]][[j]])
  }
  sorts[[i]] <- data.frame(do.call(rbind, sorts[[i]]))
  simple[[i]] <- data.frame(do.call(rbind, simple[[i]]))
}

sorts$dat2$trial <- sorts$dat2$trial + 30
simple$dat2$trial <- simple$dat2$trial + 30
sorts$dat3$trial <- sorts$dat3$trial + 60
simple$dat3$trial <- simple$dat3$trial + 60

for(i in 1:3){
  sorts[[i]] <- data.frame(ID = sorts[[i]]$ID, session = i, sorts[[i]][, -1])
  simple[[i]] <- data.frame(ID = simple[[i]]$ID, session = i, simple[[i]][, -1])
}

sorts <- data.frame(do.call(rbind, sorts))
simple <- data.frame(do.call(rbind, simple))

for(i in c('ID', 'session', 'condition')){
  sorts[[i]] <- factor(sorts[[i]])
  simple[[i]] <- factor(simple[[i]])
}

conditions <- c('SpaceSemTime', 'SpaceTime', 'SpaceSem', 'TimeSem', 'Control')
levels(sorts$condition) <- rev(levels(sorts$condition))
sorts$condition <- as.numeric(as.character(sorts$condition))
sorts$condition <- factor(sorts$condition)
levels(sorts$condition) <- rev(conditions)

levels(simple$condition) <- rev(levels(simple$condition))
simple$condition <- as.numeric(as.character(simple$condition))
simple$condition <- factor(simple$condition)
levels(simple$condition) <- rev(conditions)

sorts$correct <- sorts$correct/9
simple$correct <- simple$correct/9

rownames(sorts) <- 1:nrow(sorts)
rownames(simple) <- 1:nrow(simple)
write.csv(sorts, 'fullData.csv', row.names = FALSE)
write.csv(simple, 'simpleData.csv', row.names = FALSE)


### ------------------------------------------------------------------------ ###
### ------------------------------- ANALYSES ------------------------------- ###
### ------------------------------------------------------------------------ ###

### Analysis 1: Simple dataset
if(!require(lme4)){install.packages('lme4', dependencies = TRUE)} # v1.1-19
if(!require(lsmeans)){install.packages('lsmeans', dependencies = TRUE)}

m0 <- lmer(correct ~ condition + (1|ID) + (1|session),
           data = simple, REML = FALSE)
m1 <- lmer(correct ~ trial + (1|ID) + (1|session),
           data = simple, REML = FALSE)
m2 <- lmer(correct ~ condition + trial + (1|ID) + (1|session),
           data = simple, REML = FALSE)
m3 <- lmer(correct ~ condition * trial + (1|ID) + (1|session), 
           data = simple, REML = FALSE)

anova(m0, m2) # Is condition necessary, or only trial?
anova(m1, m2) # Is trial necessary, or only condition?
anova(m2, m3) # Are there interactions between trial and condition?

lsmeans(m2, pairwise ~ condition, adjust = 'tukey')

