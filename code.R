View(data4)
head(data4)
dim(data4)
str(data4)
summary(data4)

names(data4)
names(data4) <- make.names(names(data4), unique = TRUE)
names(data4)

categorical_columns <- c("Gender", "Sexual.Orientation", "race", "Employment.status", 
                         "Marital.status", "No..of.dependent.children", "Sex.worker.", 
                         "Menopausal.status", "Self.report.Learning.disability", 
                         "Self.report.Physical.disability", "Self.report.Mental.Health.Difficulty",
                         "Mental.health.service.involvement", "Self.harm.risk", "Self.harm.history",
                         "Suicide.attempts", "Current.Psychiatric.meds", "Domestic.Violence.history",
                         "Alcohol.use.related.to.incident", "Drug.use.related.to.incident",
                         "Drug.facilitated.sexual.assault..DFSA.", "Previously.sexually.active",
                         "Nature.of.assault.1")
for (col in categorical_columns) {
  data4[[col]] <- as.factor(data4[[col]])
}
str(data4)

library(naniar)
vis_miss(data4)

library(mice)
imp<-mice(data4,seed=1)

completed_data <- complete(imp, 1)
str(completed_data)
summary(completed_data)

####time
hist(completed_data$Age, main="Histogram of Variable", xlab="Variable", breaks=30)
hist(completed_data$Body.mass.index, main="Histogram of Variable", xlab="Variable", breaks=30)
hist(completed_data$LDSQ.score, main="Histogram of Variable", xlab="Variable", breaks=30)

model_age <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Age, data = completed_data)
summary(model_age)###显著

model_bmi <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Body.mass.index, data = completed_data)
summary(model_bmi)

model_LDSQ <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ LDSQ.score, data = completed_data)
summary(model_LDSQ)

anova_model_gender <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Gender, data = completed_data)
summary(anova_model_gender)

anova_model_Sexual.Orientation <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Sexual.Orientation, data = completed_data)
summary(anova_model_Sexual.Orientation)

anova_model_race<- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ race, data = completed_data)
summary(anova_model_race)

anova_model_Employment.status <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Employment.status, data = completed_data)
summary(anova_model_Employment.status)

anova_model_Marital.status <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Marital.status, data = completed_data)
summary(anova_model_Marital.status)

anova_model_No..of.dependent.children <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ No..of.dependent.children, data = completed_data)
summary(anova_model_No..of.dependent.children)###显著

anova_model_Sex.worker. <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Sex.worker., data = completed_data)
summary(anova_model_Sex.worker.)

anova_model_Menopausal.status <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Menopausal.status, data = completed_data)
summary(anova_model_Menopausal.status)

anova_model_Self.report.Learning.disability <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Self.report.Learning.disability, data = completed_data)
summary(anova_model_Self.report.Learning.disability)

anova_model_Self.report.Physical.disability <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Self.report.Physical.disability, data = completed_data)
summary(anova_model_Self.report.Physical.disability)

anova_model_Self.report.Mental.Health.Difficulty <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Self.report.Mental.Health.Difficulty, data = completed_data)
summary(anova_model_Self.report.Mental.Health.Difficulty)

anova_model_Mental.health.service.involvement <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Mental.health.service.involvement, data = completed_data)
summary(anova_model_Mental.health.service.involvement)

anova_model_Self.harm.risk <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Self.harm.risk, data = completed_data)
summary(anova_model_Self.harm.risk)

anova_model_Self.harm.history <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Self.harm.history, data = completed_data)
summary(anova_model_Self.harm.history)

anova_model_Suicide.attempts <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Suicide.attempts, data = completed_data)
summary(anova_model_Suicide.attempts)

anova_model_Current.Psychiatric.meds <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Current.Psychiatric.meds, data = completed_data)
summary(anova_model_Current.Psychiatric.meds)

anova_model_Domestic.Violence.history <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Domestic.Violence.history, data = completed_data)
summary(anova_model_Domestic.Violence.history)###显著

anova_model_Alcohol.use.related.to.incident <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Alcohol.use.related.to.incident, data = completed_data)
summary(anova_model_Alcohol.use.related.to.incident)###显著

anova_model_Drug.use.related.to.incident <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Drug.use.related.to.incident, data = completed_data)
summary(anova_model_Drug.use.related.to.incident)###显著

anova_model_Drug.facilitated.sexual.assault..DFSA. <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Drug.facilitated.sexual.assault..DFSA., data = completed_data)
summary(anova_model_Drug.facilitated.sexual.assault..DFSA.)

anova_model_Previously.sexually.active <- aov(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Previously.sexually.active, data = completed_data)
summary(anova_model_Previously.sexually.active)

###多变量分析
fit <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Age + Gender + Sexual.Orientation + race 
           + Employment.status + Marital.status + No..of.dependent.children + Body.mass.index + LDSQ.score
           + Sex.worker. + Menopausal.status + Self.report.Learning.disability + Self.report.Physical.disability 
           + Self.report.Mental.Health.Difficulty + Mental.health.service.involvement + Self.harm.risk 
           + Self.harm.history + Suicide.attempts + Current.Psychiatric.meds + Domestic.Violence.history 
           + Alcohol.use.related.to.incident + Drug.use.related.to.incident + Drug.facilitated.sexual.assault..DFSA. + Previously.sexually.active,
           data = completed_data)  
stepwise_model <- step(fit, direction="backward")
summary(stepwise_model)

stepwise_model <- step(fit, direction="forward")
summary(stepwise_model)

stepwise_model <- step(fit, direction="both")
summary(stepwise_model)

###相互作用分析
model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Gender * race + Marital.status + Menopausal.status + Self.harm.history +
                                                                         Alcohol.use.related.to.incident,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Gender * Marital.status + race + Menopausal.status + Self.harm.history +
               Alcohol.use.related.to.incident,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Gender * Menopausal.status + Marital.status + race  + Self.harm.history +
               Alcohol.use.related.to.incident,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Gender * Self.harm.history + Marital.status + race + Menopausal.status + 
               Alcohol.use.related.to.incident, 
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Gender * Alcohol.use.related.to.incident + race + Marital.status + Menopausal.status + Self.harm.history ,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ race * Marital.status + Gender + Alcohol.use.related.to.incident  + Menopausal.status + Self.harm.history ,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ race * Menopausal.status + Marital.status + Gender + Alcohol.use.related.to.incident  + Self.harm.history,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ race * Self.harm.history + Menopausal.status + Marital.status + Gender + Alcohol.use.related.to.incident,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ race * Alcohol.use.related.to.incident + Self.harm.history + Menopausal.status + Marital.status + Gender,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Marital.status * Menopausal.status + Alcohol.use.related.to.incident + Self.harm.history +  Gender + race,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Marital.status * Self.harm.history + Menopausal.status + Alcohol.use.related.to.incident  +  Gender + race,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Marital.status * Alcohol.use.related.to.incident + Self.harm.history + Menopausal.status +   Gender + race,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Menopausal.status * Self.harm.history + Marital.status + Alcohol.use.related.to.incident +   Gender + race,
             data = completed_data)
summary(model)

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Menopausal.status * Alcohol.use.related.to.incident + Self.harm.history + Marital.status +   Gender + race,
             data = completed_data)
summary(model)### Menopausal.statusPre:Alcohol.use.related.to.incidentYes>14 units

model <- glm(Time.between.date..time.of.assault.and.attendance.at.SARC ~ Self.harm.history * Alcohol.use.related.to.incident + Menopausal.status  + Marital.status +   Gender + race,
             data = completed_data)### Self.harm.historyRecent/Current:Alcohol.use.related.to.incidentYes>14 units;Self.harm.historyYes - Unclear:Alcohol.use.related.to.incidentYes<14 units 
summary(model)

####nature
library(nnet)
model_age <- multinom(Nature.of.assault.1 ~ Age, data = completed_data)
summary(model_age)
model_summary <- summary(model_age)
z_values <- coef(model_summary)/summary(model_age)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model_bmi <- multinom(Nature.of.assault.1 ~ Body.mass.index, data = completed_data)
summary(model_bmi)
model_summary <- summary(model_bmi)
z_values <- coef(model_summary)/summary(model_bmi)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model_ldsq <- multinom(Nature.of.assault.1 ~ LDSQ.score, data = completed_data)
summary(model_ldsq)
model_summary <- summary(model_ldsq)
z_values <- coef(model_summary)/summary(model_ldsq)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model_gender <- multinom(Nature.of.assault.1 ~ Gender, data = completed_data)
summary(model_gender)
model_summary <- summary(model_gender)
z_values <- coef(model_summary)/summary(model_gender)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Sexual.Orientation, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values###gay

model <- multinom(Nature.of.assault.1 ~ race, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Employment.status, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Marital.status, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ No..of.dependent.children, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Sex.worker., data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Menopausal.status, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Self.report.Learning.disability, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Self.report.Physical.disability, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Self.report.Mental.Health.Difficulty, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Mental.health.service.involvement, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Self.harm.risk, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Self.harm.history, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Suicide.attempts, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Current.Psychiatric.meds, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Domestic.Violence.history, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Alcohol.use.related.to.incident, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Drug.use.related.to.incident, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Drug.facilitated.sexual.assault..DFSA., data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Nature.of.assault.1 ~ Previously.sexually.active, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~ Age + Gender + Sexual.Orientation + race
           + Employment.status + Marital.status + No..of.dependent.children + Body.mass.index + LDSQ.score
           + Sex.worker. + Menopausal.status + Self.report.Learning.disability + Self.report.Physical.disability 
           + Self.report.Mental.Health.Difficulty + Mental.health.service.involvement + Self.harm.risk 
           + Self.harm.history + Suicide.attempts + Current.Psychiatric.meds + Domestic.Violence.history 
           + Alcohol.use.related.to.incident + Drug.use.related.to.incident + Drug.facilitated.sexual.assault..DFSA. + Previously.sexually.active,
           data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~ Gender + Sexual.Orientation + race
                + Employment.status + Marital.status + No..of.dependent.children 
                + Sex.worker.  + Self.report.Physical.disability 
                + Self.report.Mental.Health.Difficulty + Mental.health.service.involvement + Self.harm.risk 
                + Self.harm.history + Suicide.attempts  + Domestic.Violence.history 
                + Alcohol.use.related.to.incident  + Drug.facilitated.sexual.assault..DFSA. ,
                data = completed_data) 
summary(fit)

fit <- multinom(Nature.of.assault.1 ~ Gender * Sexual.Orientation + race
                + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~ Gender * race + Sexual.Orientation 
                + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~ Gender * Sex.worker. + race + Sexual.Orientation 
               + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~ Gender * Alcohol.use.related.to.incident + Sex.worker. + race + Sexual.Orientation,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~  Gender + race * Sexual.Orientation 
                + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~  Gender + race * Sex.worker. + Sexual.Orientation 
                + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~  Gender + race * Alcohol.use.related.to.incident + Sex.worker. + Sexual.Orientation,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~  Gender + race + Sex.worker. * Sexual.Orientation 
                + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~  Gender + race + Sex.worker. * Alcohol.use.related.to.incident + Sexual.Orientation,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Nature.of.assault.1 ~  Gender + race + Sex.worker. + Alcohol.use.related.to.incident * Sexual.Orientation,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

#number of suspects
model_age <- glm(Number.of.suspects ~ Age, data = completed_data)
summary(model_age)

model_bmi <- glm(Number.of.suspects ~ Body.mass.index, data = completed_data)
summary(model_bmi)

model_LDSQ <- glm(Number.of.suspects ~ LDSQ.score, data = completed_data)
summary(model_LDSQ)

anova_model_gender <- aov(Number.of.suspects ~ Gender, data = completed_data)
summary(anova_model_gender)

anova_model_Sexual.Orientation <- aov(Number.of.suspects ~ Sexual.Orientation, data = completed_data)
summary(anova_model_Sexual.Orientation)

anova_model_race <- aov(Number.of.suspects ~ race, data = completed_data)
summary(anova_model_race)

anova_model_Employment.status <- aov(Number.of.suspects ~ Employment.status, data = completed_data)
summary(anova_model_Employment.status)

anova_model_Marital.status <- aov(Number.of.suspects ~ Marital.status, data = completed_data)
summary(anova_model_Marital.status)

anova_model_No..of.dependent.children <- aov(Number.of.suspects ~ No..of.dependent.children, data = completed_data)
summary(anova_model_No..of.dependent.children)

anova_model_Sex.worker. <- aov(Number.of.suspects ~ Sex.worker., data = completed_data)
summary(anova_model_Sex.worker.)

anova_model_Menopausal.status <- aov(Number.of.suspects ~ Menopausal.status, data = completed_data)
summary(anova_model_Menopausal.status)

anova_model_Self.report.Learning.disability <- aov(Number.of.suspects ~ Self.report.Learning.disability, data = completed_data)
summary(anova_model_Self.report.Learning.disability)

anova_model_Self.report.Physical.disability <- aov(Number.of.suspects ~ Self.report.Physical.disability, data = completed_data)
summary(anova_model_Self.report.Physical.disability)

anova_model_Self.report.Mental.Health.Difficulty <- aov(Number.of.suspects ~ Self.report.Mental.Health.Difficulty, data = completed_data)
summary(anova_model_Self.report.Mental.Health.Difficulty)

anova_model_Mental.health.service.involvement <- aov(Number.of.suspects ~ Mental.health.service.involvement, data = completed_data)
summary(anova_model_Mental.health.service.involvement)

anova_model_Self.harm.risk <- aov(Number.of.suspects ~ Self.harm.risk, data = completed_data)
summary(anova_model_Self.harm.risk)

anova_model_Self.harm.history <- aov(Number.of.suspects ~ Self.harm.history, data = completed_data)
summary(anova_model_Self.harm.history)

anova_model_Suicide.attempts <- aov(Number.of.suspects ~ Suicide.attempts, data = completed_data)
summary(anova_model_Suicide.attempts)

anova_model_Current.Psychiatric.meds <- aov(Number.of.suspects ~ Current.Psychiatric.meds, data = completed_data)
summary(anova_model_Current.Psychiatric.meds)

anova_model_Domestic.Violence.history <- aov(Number.of.suspects ~ Domestic.Violence.history, data = completed_data)
summary(anova_model_Domestic.Violence.history)###显著

anova_model_Alcohol.use.related.to.incident <- aov(Number.of.suspects ~ Alcohol.use.related.to.incident, data = completed_data)
summary(anova_model_Alcohol.use.related.to.incident)

anova_model_Drug.use.related.to.incident <- aov(Number.of.suspects ~ Drug.use.related.to.incident, data = completed_data)
summary(anova_model_Drug.use.related.to.incident)

anova_model_Drug.facilitated.sexual.assault..DFSA. <- aov(Number.of.suspects ~ Drug.facilitated.sexual.assault..DFSA., data = completed_data)
summary(anova_model_Drug.facilitated.sexual.assault..DFSA.)

anova_model_Previously.sexually.active <- aov(Number.of.suspects ~ Previously.sexually.active, data = completed_data)
summary(anova_model_Previously.sexually.active)

###多变量分析
fit <- glm(Number.of.suspects ~ Age + Gender + Sexual.Orientation + race 
           + Employment.status + Marital.status + No..of.dependent.children + Body.mass.index + LDSQ.score
           + Sex.worker. + Menopausal.status + Self.report.Learning.disability + Self.report.Physical.disability 
           + Self.report.Mental.Health.Difficulty + Mental.health.service.involvement + Self.harm.risk 
           + Self.harm.history + Suicide.attempts + Current.Psychiatric.meds + Domestic.Violence.history 
           + Alcohol.use.related.to.incident + Drug.use.related.to.incident + Drug.facilitated.sexual.assault..DFSA. + Previously.sexually.active,
           data = completed_data)  
stepwise_model <- step(fit, direction="backward")
summary(stepwise_model)

stepwise_model <- step(fit, direction="forward")
summary(stepwise_model)

stepwise_model <- step(fit, direction="both")
summary(stepwise_model)

fit <- glm(Number.of.suspects ~  Sexual.Orientation * race + Self.report.Physical.disability 
           + Self.harm.risk + Current.Psychiatric.meds + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation * Self.report.Physical.disability + race 
           + Self.harm.risk + Current.Psychiatric.meds + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation * Self.harm.risk + Self.report.Physical.disability + race 
           + Current.Psychiatric.meds + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation * Current.Psychiatric.meds + Self.harm.risk + Self.report.Physical.disability + race 
           + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation * Domestic.Violence.history + Current.Psychiatric.meds 
           + Self.harm.risk + Self.report.Physical.disability + race 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation * Drug.facilitated.sexual.assault..DFSA. 
           + Domestic.Violence.history + Current.Psychiatric.meds 
           + Self.harm.risk + Self.report.Physical.disability + race ,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race * Self.report.Physical.disability 
           + Self.harm.risk + Current.Psychiatric.meds + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race * Self.harm.risk + Self.report.Physical.disability 
           + Current.Psychiatric.meds + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race * Current.Psychiatric.meds + Self.harm.risk + Self.report.Physical.disability 
           + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race * Domestic.Violence.history 
           + Current.Psychiatric.meds + Self.harm.risk + Self.report.Physical.disability 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race * Drug.facilitated.sexual.assault..DFSA. + Domestic.Violence.history 
           + Current.Psychiatric.meds + Self.harm.risk + Self.report.Physical.disability,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race + Self.harm.risk * Self.report.Physical.disability 
           + Current.Psychiatric.meds + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race + Self.harm.risk * Current.Psychiatric.meds + Self.report.Physical.disability 
            + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race + Self.harm.risk * Domestic.Violence.history 
           + Current.Psychiatric.meds + Self.report.Physical.disability 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race + Self.harm.risk * Drug.facilitated.sexual.assault..DFSA. + Domestic.Violence.history 
           + Current.Psychiatric.meds + Self.report.Physical.disability,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race 
           + Self.harm.risk + Self.report.Physical.disability * Current.Psychiatric.meds + Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race 
           + Self.harm.risk + Self.report.Physical.disability * Domestic.Violence.history + Current.Psychiatric.meds  
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race 
           + Self.harm.risk + Self.report.Physical.disability * Drug.facilitated.sexual.assault..DFSA. 
           + Domestic.Violence.history + Current.Psychiatric.meds,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race 
           + Self.harm.risk + Self.report.Physical.disability + Current.Psychiatric.meds * Domestic.Violence.history 
           + Drug.facilitated.sexual.assault..DFSA.,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race 
           + Self.harm.risk + Self.report.Physical.disability + Current.Psychiatric.meds * Drug.facilitated.sexual.assault..DFSA.
           + Domestic.Violence.history,
           data = completed_data)  
summary(fit)

fit <- glm(Number.of.suspects ~  Sexual.Orientation + race 
           + Self.harm.risk + Self.report.Physical.disability + Current.Psychiatric.meds 
           + Drug.facilitated.sexual.assault..DFSA. * Domestic.Violence.history,
           data = completed_data)  
summary(fit)
#Reln..to.suspect.1  
model_age <- multinom(Reln..to.suspect.1 ~ Age, data = completed_data)
summary(model_age)
model_summary <- summary(model_age)
z_values <- coef(model_summary)/summary(model_age)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model_bmi <- multinom(Reln..to.suspect.1 ~ Body.mass.index, data = completed_data)
summary(model_bmi)
model_summary <- summary(model_bmi)
z_values <- coef(model_summary)/summary(model_bmi)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model_ldsq <- multinom(Reln..to.suspect.1 ~ LDSQ.score, data = completed_data)
summary(model_ldsq)
model_summary <- summary(model_ldsq)
z_values <- coef(model_summary)/summary(model_ldsq)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model_gender <- multinom(Reln..to.suspect.1 ~ Gender, data = completed_data)
summary(model_gender)
model_summary <- summary(model_gender)
z_values <- coef(model_summary)/summary(model_gender)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Sexual.Orientation, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ race, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Employment.status, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Marital.status, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ No..of.dependent.children, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Sex.worker., data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Menopausal.status, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Self.report.Learning.disability, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Self.report.Physical.disability, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Self.report.Mental.Health.Difficulty, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Mental.health.service.involvement, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Self.harm.risk, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Self.harm.history, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower_tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Suicide.attempts, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Current.Psychiatric.meds, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Domestic.Violence.history, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Alcohol.use.related.to.incident, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Drug.use.related.to.incident, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Drug.facilitated.sexual.assault..DFSA., data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower_tail = FALSE)
p_values

model <- multinom(Reln..to.suspect.1 ~ Previously.sexually.active, data = completed_data)
summary(model)
model_summary <- summary(model)
z_values <- coef(model_summary)/summary(model)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values


fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation + race
                + Employment.status + Marital.status + No..of.dependent.children + Body.mass.index + LDSQ.score
                + Sex.worker. + Menopausal.status + Self.report.Learning.disability + Self.report.Physical.disability 
                + Self.report.Mental.Health.Difficulty + Mental.health.service.involvement + Self.harm.risk 
                + Self.harm.history + Suicide.attempts + Current.Psychiatric.meds + Domestic.Violence.history 
                + Alcohol.use.related.to.incident + Drug.use.related.to.incident + Drug.facilitated.sexual.assault..DFSA. + Previously.sexually.active,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation + race
                + Employment.status + Marital.status + No..of.dependent.children + Body.mass.index 
                + Sex.worker. + Menopausal.status + Self.report.Learning.disability + Self.report.Physical.disability 
                + Self.report.Mental.Health.Difficulty + Mental.health.service.involvement + Self.harm.risk 
                + Self.harm.history + Suicide.attempts + Current.Psychiatric.meds + Domestic.Violence.history 
                + Alcohol.use.related.to.incident + Drug.use.related.to.incident + Drug.facilitated.sexual.assault..DFSA. + Previously.sexually.active,
                data = completed_data) 

fit <- multinom(Reln..to.suspect.1 ~ Age * Gender + Sexual.Orientation + race
                + Body.mass.index + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age * Sexual.Orientation + Gender  + race
                + Body.mass.index + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age * race + Sexual.Orientation + Gender  
                + Body.mass.index + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age * Body.mass.index + race + Sexual.Orientation + Gender  
                + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age * Sex.worker. + Body.mass.index + race + Sexual.Orientation + Gender  
                + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age * Alcohol.use.related.to.incident + Sex.worker. + Body.mass.index + race + Sexual.Orientation + Gender,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender * Sexual.Orientation + race
                + Body.mass.index + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender * race + Sexual.Orientation 
                + Body.mass.index + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender * Body.mass.index + race + Sexual.Orientation 
                + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender * Sex.worker. + Body.mass.index + race + Sexual.Orientation 
                + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender * Alcohol.use.related.to.incident + Sex.worker. + Body.mass.index + race + Sexual.Orientation ,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation * race
                + Body.mass.index + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation * Body.mass.index + race
                 + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation * Sex.worker.  + Body.mass.index + race
                + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation * Alcohol.use.related.to.incident + Sex.worker.  + Body.mass.index + race,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation + race * Body.mass.index + Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation + race * Sex.worker. + Body.mass.index + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation + race * Alcohol.use.related.to.incident + Sex.worker. +  Body.mass.index,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation + race + Body.mass.index * Sex.worker. + Alcohol.use.related.to.incident,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation + race + Body.mass.index * Alcohol.use.related.to.incident + Sex.worker.,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values

fit <- multinom(Reln..to.suspect.1 ~ Age + Gender + Sexual.Orientation + race + Body.mass.index + Alcohol.use.related.to.incident * Sex.worker.,
                data = completed_data) 
summary(fit)
model_summary <- summary(fit)
z_values <- coef(model_summary)/summary(fit)$standard.errors
p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
p_values
