library(plotly)
library(lubridate)
library(tidyverse)
library(gsheet)
library(lme4) # for linear mixed models
library(MuMIn) # for R-squared values that measures how much variance is explained.
source("utils/visutils.R")

options("digits.secs"=6)
fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_all.rda')


#############
# Summaries #
#############

# St =  Summary of Trials
St <- D %>% ungroup() %>% group_by(GameTitle, Participant,Condition) %>%
  filter(Event == "GameDecision") %>%
  summarize(rejInput = sum(TrialResult == "RejInput"),
            accInput = sum(TrialResult == "AccInput"),
            fabInput = sum(TrialResult == "FabInput"),
            posTrial = sum(TrialResult == "FabInput" | TrialResult == "AccInput"),
            negTrial = sum(TrialResult == "RejInput"),
            otherInput = sum(!TrialResult %in% c("RejInput", "AccInput", "FabInput")),
            GameTitle = unique(GameTitle),
            totalTrials = rejInput+accInput+fabInput,
            accRate = accInput/totalTrials,
            true_feedback_rate = accInput / totalTrials,
            fab_rate = fabInput / totalTrials,
            feedback_rate = (accInput+fabInput) / totalTrials,
            PercNormalized = unique(PercNormalized),
            FrustNormalized = unique(FrustNormalized),
            GameOrder = unique(GameOrder),
            gender = unique(Gender),
            age = unique(Age),
            comment = unique(Comment),
            washHair = unique(WashHair),
            gamePreference = unique(GamePreference),
            )

St <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  filter(Period %in% c("RestPeriod", "OpenPeriod")) %>%
  summarize(time_total = sum(time_delta),
            time_total_min = time_total / 60) %>% right_join(St)

# Blink counts
St <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  summarize(blinks_total = sum(Event == "EyeOpening" & Period == "OpenPeriod")
            ) %>% right_join(St)

# Si = Summary of Input Windows
# Feedback Delay
Si <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition, InputWindowOrderFilled) %>%
  filter(Event == "EyeOpening") %>%
  summarize(last_attempt_stamp = max(Timestamp)) %>% ungroup()

Si <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition, InputWindowOrderWithDecision) %>%
  filter(Event == "GameDecision") %>%
  summarize(feedback_stamp = max(Timestamp)) %>%
  mutate(InputWindowOrderFilled = InputWindowOrderWithDecision) %>% ungroup() %>% right_join(Si)

Si <- Si %>% filter(!InputWindowOrderFilled == -1) %>%
  mutate(feedback_delay = feedback_stamp - last_attempt_stamp,
         feedback_delay = as.numeric(feedback_delay))

fig %>% add_trace(x=~InputWindowOrderFilled, y=~feedback_delay, data=Si, type='scatter')

St <- Si %>% group_by(GameTitle, Participant, Condition) %>%
  summarize(mean_delay = mean(feedback_delay)) %>% right_join(St) %>%
  mutate(mean_delay = ifelse(is.na(mean_delay), 0, mean_delay))
  

St <- St %>% group_by(GameTitle, Participant, Condition) %>%
  summarize(rate_blink = (accInput+fabInput) / blinks_total) %>%
  mutate(rate_blink = ifelse(rate_blink == Inf, 0, rate_blink)) %>% right_join(St)

# FYI: Participant 4 50C30F has no blinks during the game itself (AccInput = 0) during hand,
# Therefore the blink data is NA.

# Sp = Summary of Participants
Sp <- St %>% ungroup() %>% group_by(Participant, GameTitle) %>%
  summarize(
            time_total = mean(time_total),
            mean_delay = mean(mean_delay, na.rm=T),
            mean_blinks = mean(blinks_total),
            mean_perc = mean(PercNormalized),
            mean_frust = mean(FrustNormalized),
            mean_rate_blink = mean(rate_blink)
            )

Sp <- St %>% ungroup() %>% group_by(Participant) %>%
  summarize(
    gender = unique(gender),
    age = unique(age),
    washHair = na.omit(unique(washHair)),
    comment = na.omit(unique(comment)),
    bciExperience = ifelse(grepl("BCIExperience",comment), TRUE, FALSE),
    impairments = ifelse(grepl("MemoryLoss",comment), "MemoryLoss", NA),
    impairments = ifelse(grepl("VisualImpairment",comment), "VisualImpairment", impairments),
    impairments = ifelse(grepl("DifficultBlinkRightEye",comment), "DifficultBlinkRightEye", impairments),
    impairments = ifelse(grepl("HearingAid",comment), "HearingAid", impairments)
  ) %>% right_join(Sp)


#############
# Counts and Aggregates
#############
St %>% select(Participant,Condition,fabInput) %>% 
  mutate(isValid = NA, isValid = ifelse(Condition=="50C0F" & fabInput==0,T,F), 
         isValid = ifelse(Condition=="50C15F" & fabInput==3,T,isValid), 
         isValid=ifelse(Condition=="50C30F"&fabInput==6,T,isValid),
         isValid=ifelse(Condition=="50C50F"&fabInput==10,T,isValid))

St%>% group_by(Condition, GameTitle) %>% summarize(n())

St%>% group_by(Participant, Condition, GameTitle) %>% select(accInput, rejInput, fabInput, totalTrials) %>%
  mutate(accRate = accInput/totalTrials) %>% arrange(Condition) %>% filter(accRate != 0.5 ) %>% View()

St %>% group_by(Participant) %>%
  summarize(gamePreference = na.omit(unique(gamePreference))) %>%
  group_by(gamePreference) %>%
  summarize(n())

#############
# Latex Table with demographic and summary information
#############

Sp_table <- Sp %>% select(GameTitle, gender, age, mean_frust, mean_perc,
                          mean_blinks,mean_rate_blink, mean_delay) %>%
                    mutate(
                      mean_frust = format(round(mean_frust,2), nsmall = 2),
                      mean_perc = format(round(mean_perc,2), nsmall = 2),
                      mean_rate_blink = format(round(mean_rate_blink,2), nsmall = 2),
                      mean_blinks = format(round(mean_blinks,0), nsmall = 0),
                      mean_delay = format(round(mean_delay,2), nsmall = 2),
                      across(everything(), as.character))
Sp_table <- Sp_table %>%
             rename(Gender = gender, Age = age,
                    `Mean Frustration` = mean_frust,
                    `Mean Perc. Control` = mean_perc,
                    `Avg Blink Rate` = mean_rate_blink,
                    `Avg Blinks (count)` = mean_blinks,
                    `Mean Delay (sec)` = mean_delay,)

Sp_table = Sp_table %>% pivot_longer(cols=-c(Participant, GameTitle), names_to = "Feature") %>%
  pivot_wider(names_from = Participant, values_from = value)

paste(colnames(Sp_table), collapse=" & ")
message(paste(Sp_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

Sp_demo <- Sp %>% filter(GameTitle == "kiwi") %>% select(Participant, gender, age, washHair, bciExperience, impairments) %>%
  mutate(age = paste(age), 
         bciExperience = ifelse(bciExperience, "Yes", "-"),
         washHair = ifelse(washHair == "Yes", "Yes", "-"),
         impairments = ifelse(is.na(impairments), "-", impairments),
         impairments = ifelse(impairments == "VisualImpairment", "Visual Impairment", impairments),
         impairments = ifelse(impairments == "MemoryLoss", "Memory Loss", impairments),
         impairments = ifelse(impairments == "DifficultBlinkRightEye", "Eye Blink Difficulty (R)", impairments),
         impairments = ifelse(impairments == "HearingAid", "Hearing Aid", impairments),)
Sp_demo <- Sp_demo %>%
  rename(Gender = gender, Age = age,
         `Able to Wash Hair` = washHair,
         `Notable Impairments` = impairments,
         `BCI Experience` = bciExperience)

paste(colnames(Sp_demo), collapse=" & ")
message(paste(Sp_demo %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))




#############
# Latex Table with stats per condition
#############

St_table <- St %>% ungroup() %>% 
  select(GameTitle, Condition, fab_rate, FrustNormalized, PercNormalized) %>%
  group_by(GameTitle,Condition) %>%
  summarize(mean_frust = mean(FrustNormalized),
            mean_perc = mean(PercNormalized),
            fab_rate = unique(fab_rate)) %>%
  mutate(
    fab_rate = paste(format(round(fab_rate * 100,0), nsmall = 0),"\\%"),
    mean_frust = format(round(mean_frust,2), nsmall = 2),
    mean_perc = format(round(mean_perc,2), nsmall = 2),
    GameTitle = ifelse(GameTitle == "hand", "Hand", "Kiwi"),
    Condition = "50 \\%",
    across(everything(), as.character)) 

St_table <- St_table %>% ungroup() %>%
  rename(`Game` = GameTitle,
         `Control` = Condition,
         `Fab. Input` = fab_rate,
         `Frustation (mean)` = mean_frust,
         `Perc. Control (mean)` = mean_perc)

paste(colnames(St_table), collapse=" & ")
message(paste(St_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

#############
# ICC Scores
#############

D_icc <- St %>% ungroup %>%
  select(Participant, PercNormalized, GameTitle, Condition) %>%
  pivot_wider(names_from = Participant, values_from = PercNormalized) %>%
  ungroup() %>% select(-Condition, -GameTitle)

psych::ICC(D_icc)

D_icc <- St %>% ungroup %>%
  select(Participant, FrustNormalized, GameTitle, Condition) %>%
  pivot_wider(names_from = Participant, values_from = FrustNormalized) %>%
  ungroup() %>% select(-Condition, -GameTitle)

psych::ICC(D_icc)

#############
# Perceived Control, Violin plot by Condition
#############
# 95% conf interval: https://www.r-bloggers.com/2021/04/calculating-confidence-interval-in-r/
Lines <- St %>% ungroup() %>% group_by(Condition) %>%
  summarize(perc_ci = qt(0.95 + (1 - 0.95)/2, df = length(PercNormalized) - 1) * sd(PercNormalized)/sqrt(length(PercNormalized)),
            frust_ci = qt(0.95 + (1 - 0.95)/2, df = length(FrustNormalized) - 1) * sd(FrustNormalized)/sqrt(length(FrustNormalized)),
            perc_mean = mean(PercNormalized), 
            frust_mean = mean(FrustNormalized))
                         
fig1 <- fig %>%
  add_trace(x=factor(St$Condition, levels=c("50C0F", "50C15F","50C30F","50C50F")), y=jitter(St$PercNormalized,amount=.02),
            scalemode='width',points='all', pointpos=0,name='C', jitter=.3,
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.08, color=I('darkgray')) %>%
  add_trace(data=Lines, x=~Condition, y=~perc_mean, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=~perc_ci)) %>%
  #add_trace(data=PercPureCurve, x=~x, y=~y, type='scatter', line=list(dash='dot'), symbol=I('square-x-open'), mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  #add_trace(data=PercLines[["PercFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  layout(showlegend=F, yaxis = list(range=c(0,1.1), title="Perceived Control", violinmode = 'overlay', violingap = 0), xaxis=list(title="Fabrication Rate (%)"))
fig1
orca(fig1, "fig/study2-level-of-control-perceived.pdf", width=350, height=350)

#############
# Frustration, Violin plot by Condition
#############
fig1 <- fig %>%
  add_trace(x=factor(St$Condition, levels=c("50C0F", "50C15F","50C30F","50C50F")), y=jitter(St$FrustNormalized,amount=.02),
            scalemode='width',points='all', pointpos=0,name='C', jitter=.3,
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.08, color=I('darkgray'))  %>%
  add_trace(data=Lines, x=~Condition, y=~frust_mean, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=~frust_ci)) %>%
  #add_trace(data=PercPureCurve, x=~x, y=~y, type='scatter', line=list(dash='dot'), symbol=I('square-x-open'), mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  #add_trace(data=PercFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  layout(showlegend=F, yaxis = list(range=c(0,1.1), title="Frustration", violinmode = 'overlay', violingap = 0), xaxis=list(title="Fabrication Rate (%)"))
fig1
orca(fig1, "fig/study2-level-of-control-frustration.pdf", width=350, height=350)

#############
# Linear Regression Models - Perceived Control
#############

# Perceived Control to Positive Feedback
model.null = lmer(PercNormalized ~ (1|Participant), 
                  data = St, REML=F)
model.feedback = lmer(PercNormalized ~ feedback_rate + (1|Participant),
                      data = St, REML=F)

model.feedbackblinkrate = lmer(PercNormalized ~ feedback_rate + rate_blink + (1|Participant),
                       data = St, REML=F)

model.feedbackgame = lmer(PercNormalized ~ feedback_rate + GameTitle + (1|Participant),
                               data = St, REML=F)

model.feedbackgameorder = lmer(PercNormalized ~ feedback_rate + GameOrder + (1|Participant),
                          data = St, REML=F)

# Perceived Control to Blink Rate
model.blinkrate = lmer(PercNormalized ~ rate_blink + (1|Participant),
                      data = St, REML=F)

# Perceived Control to Blink Delay
model.meandelay = lmer(PercNormalized ~ mean_delay + (1|Participant),
                       data = St, REML=F)

model.feedbackmd = lmer(PercNormalized ~ feedback_rate + mean_delay + (1|Participant),
                               data = St, REML=F)

# Perceived Control to Fabricated Input
model.fabrate = lmer(PercNormalized ~ fab_rate + (1|Participant),
                       data = St, REML=F)

model.feedbackfab = lmer(PercNormalized ~ feedback_rate + fab_rate + (1|Participant),
                        data = St, REML=F)

# Likelihood Ratio Test using ANOVA
anova(model.null, model.feedback)
anova(model.feedback, model.feedbackblinkrate)
anova(model.feedback, model.feedbackmd)
anova(model.feedback, model.feedbackfab)
anova(model.feedback, model.feedbackgameorder)

anova(model.null, model.blinkrate)
anova(model.null, model.meandelay)
anova(model.null, model.fabrate)

# Check variance
r.squaredGLMM(model.feedback, null=model.null)
r.squaredGLMM(model.blinkrate, null=model.null)
r.squaredGLMM(model.fabrate, null=model.null)

#############
# Linear Regression Models - Frustration
#############
# Test whether invalid conditions affect results.
St <- St %>% filter(accRate == 0.5)


# Perceived Control to Positive Feedback
model.null = lmer(FrustNormalized ~ (1|Participant), 
                  data = St, REML=F)
model.feedback = lmer(FrustNormalized ~ feedback_rate + (1|Participant),
                      data = St, REML=F)

model.feedbackblink = lmer(FrustNormalized ~ feedback_rate + rate_blink + (1|Participant),
                      data = St, REML=F)
model.feedbackdelay = lmer(FrustNormalized ~ feedback_rate + mean_delay + (1|Participant),
                           data = St, REML=F)
model.feedbackgame = lmer(FrustNormalized ~ feedback_rate + GameTitle + (1|Participant),
                           data = St, REML=F)

model.blink = lmer(FrustNormalized ~ rate_blink + (1|Participant),
                      data = St, REML=F)

model.fab = lmer(FrustNormalized ~ fab_rate + (1|Participant),
                   data = St, REML=F)

model.delay = lmer(FrustNormalized ~ mean_delay + (1|Participant),
                 data = St, REML=F)

model.game = lmer(FrustNormalized ~ GameTitle + (1|Participant),
                   data = St, REML=F)

model.order = lmer(FrustNormalized ~ GameOrder + (1|Participant),
                  data = St, REML=F)

model.feedbackfab = lmer(FrustNormalized ~ feedback_rate + fab_rate + (1|Participant),
                         data = St, REML=F)

# Likelihood Ratio Test using ANOVA
anova(model.null, model.feedback)
anova(model.feedback, model.feedbackblink) # not significant
anova(model.feedback, model.feedbackdelay) # not significant
anova(model.feedback, model.feedbackgame) # not significant
anova(model.feedback, model.feedbackfab) # not significant

anova(model.null, model.blink)
anova(model.null, model.fab)
anova(model.null, model.delay) # Almost significant (!)
anova(model.null, model.game) # not significant
anova(model.null, model.order) # not significant

# Check variance
r.squaredGLMM(model.feedback, null=model.null)
r.squaredGLMM(model.blink, null=model.null)
r.squaredGLMM(model.fab, null=model.null)
r.squaredGLMM(model.delay, null=model.null)


# Regular lm()
lm(PercNormalized ~ feedback_rate, data=St)
lm(FrustNormalized ~ feedback_rate, data=St)

export <- p_lin(St, "PercNormalized", "feedback_rate")
stroke_frust <- p_lin(St, "FrustNormalized", "feedback_rate")
save(export, file = 'stroke.rda')
save(stroke_frust, file = 'stroke_frust.rda')
