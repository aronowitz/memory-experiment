### Memory Museum Pilot Study Analysis
# Trevor James Swanson
# 09/22/2020


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

m1 <- lmer(correct ~ condition + trial + (1|ID) + (1|session), 
           data = simple, REML = FALSE)

m2 <- lmer(correct ~ condition * trial + (1|ID) + (1|session), 
           data = simple, REML = FALSE)

anova(m1, m2)



