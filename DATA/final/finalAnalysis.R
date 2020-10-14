### Memory Museum Pilot Study Analysis
# Trevor James Swanson
# 09/22/2020

### CONDITIONS: (numbering only relevant to original format)
# 1) Space + Semantics + Time
# 2) Space + Time
# 3) Space + Semantics
# 4) Time + Semantics
# 5) As much chaos as possible (control)


### ------------------------------------------------------------------------ ###
### --------------------------- DATA PREPARATION --------------------------- ###
### ------------------------------------------------------------------------ ###
if(!require(ggplot2)){install.packages('ggplot2', dependencies = TRUE)}
source('extractData.R')
out <- getData(getJSONS(), matchPs = TRUE)
sorts <- lapply(out, lapply, getSorts)
inits <- data.frame(do.call(rbind, lapply(
  lapply(out, lapply, getInits), function(z) data.frame(do.call(rbind, z)))
))

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
sorts <- cbind(sorts, inits)
# write.csv(sorts, 'fullData.csv', row.names = FALSE)
# write.csv(simple, 'simpleData.csv', row.names = FALSE)


### Coding Error Patterns
ids <- split(sorts, sorts$ID)
ids <- lapply(ids, function(z) split(z, z$trial))

for(i in seq_along(ids)){
  for(j in seq_along(ids[[i]])){
    k <- ids[[i]][[j]]
    s1 <- apply(k[, grep('1$', colnames(k))[1:4]], 1, function(zz) paste0(zz, collapse = '.'))
    s2 <- apply(k[, grep('0$', colnames(k))[1:4]], 1, function(zz) paste0(zz, collapse = '.'))
    
    s3 <- apply(k[, c('cat1', 'src1', 'room1')], 1, function(zz) paste0(zz, collapse = '.'))
    s4 <- apply(k[, c('cat0', 'src0', 'room0')], 1, function(zz) paste0(zz, collapse = '.'))
    
    s5 <- apply(k[, c('cat1', 'loc1')], 1, function(zz) paste0(zz, collapse = '.'))
    s6 <- apply(k[, c('cat0', 'loc0')], 1, function(zz) paste0(zz, collapse = '.'))
    
    s7 <- apply(k[, c('cat1', 'room1')], 1, function(zz) paste0(zz, collapse = '.'))
    s8 <- apply(k[, c('cat0', 'room0')], 1, function(zz) paste0(zz, collapse = '.'))
    
    ids[[i]][[j]]$item_correct <- as.numeric(s1 %in% s2)
    ids[[i]][[j]]$time_correct <- as.numeric(s1 == s2)
    ids[[i]][[j]]$room_correct <- as.numeric(s3 %in% s4)
    ids[[i]][[j]]$cat_correct <- as.numeric(s5 %in% s6)
    ids[[i]][[j]]$cat_room_correct <- as.numeric(s7 %in% s8)
  }
}

ids <- data.frame(do.call(rbind, lapply(ids, function(z) data.frame(do.call(rbind, z)))))
row.names(ids) <- 1:nrow(ids)


### PLOTS
sess <- lapply(1:3, function(z) subset(simple, session == z))
par(mfrow = c(2, 2))
for(i in 1:3){hist(sess[[i]]$correct, main = '', xlab = paste('Session', i))}
idcorr <- sapply(lapply(split(simple, simple$ID), '[[', 'correct'), mean)
plot(sort(idcorr), type = 'b', ylab = 'Average Overall Performance', xlab = '')


# Altogether
dev.off()
ggplot(simple, aes(x = trial, y = correct, color = factor(ID))) +
  geom_line() + geom_point() + theme_bw()

# By Condition
dev.off()
ggplot(simple2, aes(x = trial, y = correct, color = factor(ID))) +
  geom_line() + geom_point() + theme_bw() + facet_wrap(~ condition)

# By Session + Condition
simple2 <- split(simple, simple$session)
simple2$`2`$trial <- rep(1:30, 56)
simple2$`3`$trial <- rep(1:30, 56)
simple2 <- data.frame(do.call(rbind, simple2))
levels(simple2$session) <- c('SESSION 1', 'SESSION 2', 'SESSION 3')

dev.off()
ggplot(simple2, aes(x = trial, y = correct, color = factor(ID))) +
  geom_line() + geom_point() + theme_bw() + facet_wrap(~ session + condition)


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

