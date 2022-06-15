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
            accRecogRate = accInput / sum(TrialGoal == "AccInput"),
            ConditionOrder = unique(OrderAll),
            true_feedback_rate = accInput / totalTrials,
            fab_rate = fabInput / totalTrials,
            feedback_rate = (accInput+fabInput) / totalTrials,
            PerceivedControlEpisode = unique(PerceivedControlEpisode),
            FrustrationEpisode = unique(FrustrationEpisode),
            PercNormalized = unique(PercNormalized),
            FrustNormalized = unique(FrustNormalized),
            GameOrder = unique(GameOrder),
            gender = unique(Gender),
            age = unique(Age),
            comment = unique(Comment),
            washHair = unique(WashHair),
            gamePreference = unique(GamePreference),
            GameTitle_c = ifelse(GameTitle == "hand", "Stress Ball", "Kiwi")
            )

St <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  filter(Period %in% c("RestPeriod", "OpenPeriod")) %>%
  summarize(time_total = sum(time_delta),
            time_total_min = time_total / 60) %>% right_join(St)

# Blink counts
St <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  summarize(blinks_total = sum(Event == "EyeOpening" & Period == "OpenPeriod")
            ) %>% right_join(St)

# Group by input window. Count the number of attempts in each window.
St <- D %>% ungroup() %>% filter(Period %in% c("OpenPeriod")) %>% group_by(GameTitle, Participant, Condition, InputWindowOrderFilledSoft) %>%
  summarize(blink_recog_window = ifelse(sum(Event %in% c("EyeOpening","EyeClosing") > 0), 1,0), #Whether Blinks happened in the window
            blink_recog_window_count = sum(Event %in% c("EyeOpening","EyeClosing")), #How much Blink happened in the window
            time_window = sum(time_delta)) %>%
  filter(InputWindowOrderFilledSoft > -1) %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  summarize(blink_recog_trial = sum(blink_recog_window > 0),
            blink_recog_window = sum(blink_recog_window),
            time_window = sum(time_window),
            time_window_min = time_window / 60) %>%
  right_join(St)
D %>% ungroup() %>% filter(Period %in% c("OpenPeriod")) %>% group_by(GameTitle, Participant, Condition, InputWindowOrderFilledSoft) %>%
  summarize(blink_recog_window = ifelse(sum(Event %in% c("EyeOpening","EyeClosing") > 0), 1,0), #Whether Blinks happened in the window
            blink_recog_window_count = sum(Event %in% c("EyeOpening","EyeClosing")), #How much Blink happened in the window
            time_window = sum(time_delta)) %>% View()


St <- St %>% ungroup() %>%
  mutate(rate_trial = (blink_recog_trial)/totalTrials)


# Si = Summary of Input Windows
# Feedback Delay
# Fabricated delay
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
Sp <- St %>% ungroup() %>% group_by(Participant, GameTitle_c) %>%
  summarize(
            time_total = mean(time_total),
            mean_delay = mean(mean_delay, na.rm=T),
            mean_blinks = mean(blinks_total),
            mean_perc = mean(PercNormalized),
            mean_frust = mean(FrustNormalized),
            mean_rate_blink = mean(rate_blink),
            mean_rate_trial = mean(rate_trial),
            mean_positive_feedback = mean(feedback_rate),
            mean_blink_recog = mean(accRecogRate)
            )

Sp <- St %>% ungroup() %>% group_by(Participant) %>%
  summarize(
    gender = unique(gender),
    age = unique(age),
    washHair = na.omit(unique(washHair)),
    comment = na.omit(unique(comment)),
    bciexperience = ifelse(grepl("BCIExperience",comment), TRUE, FALSE),
    impairments = ifelse(grepl("MemoryLoss",comment), "MemoryLoss", NA),
    impairments = ifelse(grepl("VisualImpairment",comment), "VisualImpairment", impairments),
    impairments = ifelse(grepl("DifficultBlinkRightEye",comment), "DifficultBlinkRightEye", impairments),
    impairments = ifelse(grepl("HearingAid",comment), "HearingAid", impairments)
  ) %>% right_join(Sp)

save(St, file = 'PAMBCI.rda', compress=TRUE)


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
cri = tibble(lv_perc = c(0.1, 0.35,0.68,0.82,1.1),
             lv_frust = rev(lv_perc),
             lv_rate = c(0.1, 0.40, 0.60,0.80,1.1),
             lv_recog = c(0.7, 0.8, 0.9,0.95,1.0),
             lv_delay = c(1.0, 0.75, 0.5,0.25,0.0),
             colors = c("g0","g1", "g2", "g3", "g4"))



Sp_table <- Sp %>% select(gender, bciexperience, GameTitle_c, Participant, mean_perc, mean_frust,
                          mean_rate_blink, mean_blink_recog, mean_positive_feedback, mean_delay) %>%
                    mutate(
                      bciexperience = ifelse(bciexperience, "Yes", "No"),
                      mean_frust_c = t_color(mean_frust, cri$lv_frust, cri$colors),
                      mean_perc_c = t_color(mean_perc, cri$lv_perc, cri$colors),
                      mean_positive_feedback_c =  t_color(mean_positive_feedback, cri$lv_rate, cri$colors),
                      mean_rate_blink_c =  t_color(mean_rate_blink, cri$lv_rate, cri$colors),
                      mean_blink_recog_c =  t_color(mean_blink_recog, cri$lv_recog, cri$colors),
                      mean_delay_c = t_color(mean_delay, cri$lv_delay, cri$colors),
                      mean_frust = format(round(mean_frust,2), nsmall = 2),
                      mean_perc = format(round(mean_perc,2), nsmall = 2),
                      mean_positive_feedback =  paste0(format(round(mean_positive_feedback * 100,0), nsmall = 0),"\\%"),
                      mean_rate_blink =  paste0(format(round(mean_rate_blink * 100,0), nsmall = 0),"\\%"),
                      mean_blink_recog =  paste0(format(round(mean_blink_recog * 100,0), nsmall = 0),"\\%"),
                      #mean_blinks = format(round(mean_blinks,1), nsmall = 1),
                      #mean_rate_trial = paste0(format(round(mean_rate_trial * 100,0), nsmall = 0),"\\%"),
                      mean_delay = format(round(mean_delay,1), nsmall = 1),
                      mean_perc = paste0("\\cellcolor{", mean_perc_c, "}", mean_perc),
                      mean_frust = paste0("\\cellcolor{", mean_frust_c, "}", mean_frust),
                      mean_positive_feedback = paste0("\\cellcolor{", mean_positive_feedback_c, "}", mean_positive_feedback),
                      mean_rate_blink = paste0("\\cellcolor{", mean_rate_blink_c, "}", mean_rate_blink),
                      mean_blink_recog = paste0("\\cellcolor{", mean_blink_recog_c, "}", mean_blink_recog),
                      mean_delay = paste0("\\cellcolor{", mean_delay_c, "}", mean_delay),
                      mean_perc_c = NULL, mean_frust_c = NULL, mean_positive_feedback_c = NULL, mean_rate_blink_c = NULL,
                      mean_blink_recog_c = NULL,mean_delay_c = NULL,
                      across(everything(), as.character))
Sp_table <- Sp_table %>%
             rename(Gender = gender,
                    `BCI Experience` = bciexperience,
                    `Frustration` = mean_frust,
                    `Perc. Control` = mean_perc,
                    `Blink Recognition` = mean_blink_recog,
                    `Pos. Feedback` = mean_positive_feedback,
                    `Blink Conv. Rate` = mean_rate_blink,
                    #`Blinks (count)` = mean_blinks,
                    `Pos. Feedback Delay (s)` = mean_delay,)

Sp_table = Sp_table %>% pivot_longer(cols=-c(Participant, GameTitle_c), names_to = "Variable") %>%
  pivot_wider(names_from = Participant, values_from = value)
Sp_table = Sp_table %>% select(-GameTitle_c)

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
  select(Condition, fab_rate, FrustNormalized, PercNormalized,GameTitle_c) %>%
  group_by(GameTitle_c,Condition) %>%
  summarize(mean_frust = mean(FrustNormalized),
            mean_perc = mean(PercNormalized),
            fab_rate = unique(fab_rate)) %>%
  mutate(
    fab_rate = paste(format(round(fab_rate * 100,0), nsmall = 0),"\\%"),
    mean_frust = format(round(mean_frust,2), nsmall = 2),
    mean_perc = format(round(mean_perc,2), nsmall = 2),
    Condition = "50 \\%",
    across(everything(), as.character)) 

St_table <- St_table %>% ungroup() %>%
  rename(`Game` = GameTitle_c,
         `Control` = Condition,
         `Fab. Input` = fab_rate,
         `Frust. (mean)` = mean_frust,
         `Perc. Cont. (mean)` = mean_perc) %>% select(`Control`, `Fab. Input`, `Frust. (mean)`, `Perc. Cont. (mean)`)

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
  add_trace(x=factor(St$Condition, levels=c("50C0F", "50C15F","50C30F","50C50F")), y=n_clip(jitter(St$PercNormalized,amount=.02)),
            scalemode='width', points='all', pointpos=0,name='C', jitter=.3,
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.08, color=I('darkgray')) %>%
  add_trace(data=Lines, x=~Condition, y=~perc_mean, type='scatter',mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=~perc_ci)) %>%
  #add_trace(data=PercPureCurve, x=~x, y=~y, type='scatter', line=list(dash='dot'), symbol=I('square-x-open'), mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  #add_trace(data=PercLines[["PercFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  layout(showlegend=F, yaxis = list(range=c(-0.02,1.1), title="Perceived Control", violinmode = 'overlay', violingap = 0, zeroline=F),
         xaxis=list(title="Fabrication Rate (%)", tickmode='array', tickvals=c(0,1,2,3), ticktext=c('+0%', '+15%', '+30%', '+50%')))
fig1
orca(fig1, "fig/study2-level-of-control-perceived.pdf", width=350, height=350)

#############
# Frustration, Violin plot by Condition
#############
fig1 <- fig %>%
  add_trace(x=factor(St$Condition, levels=c("50C0F", "50C15F","50C30F","50C50F")), y=n_clip(jitter(St$FrustNormalized,amount=.02)),
            scalemode='width',points='all', pointpos=0,name='C', jitter=.3,
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.08, color=I('darkgray'))  %>%
  add_trace(data=Lines, x=~Condition, y=~frust_mean, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=~frust_ci)) %>%
  #add_trace(data=PercPureCurve, x=~x, y=~y, type='scatter', line=list(dash='dot'), symbol=I('square-x-open'), mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  #add_trace(data=PercFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  layout(showlegend=F, yaxis = list(range=c(-0.02,1.1), title="Frustration", violinmode = 'overlay', violingap = 0, zeroline=F),
         xaxis=list(zeroline=F, title="Fabrication Rate (%)", tickmode='array', tickvals=c(0-0.02,1,2,3), ticktext=c('+0%', '+15%', '+30%', '+50%')))
fig1
orca(fig1, "fig/study2-level-of-control-frustration.pdf", width=350, height=350)

#############
# Linear Regression Models - Perceived Control
#############
g_lme_table <- function(models, null, nullname) {
  model_lme <- lapply(seq_along(models), function(i) { 
    n=names(models)[[i]]
    a=anova(null, models[[n]]) %>% bind_rows() %>% mutate(model = c(nullname, n))
    return(a) })
  model_r2 <- lapply(seq_along(models), function(i) { 
    n=names(models)[[i]]
    r=r.squaredGLMM(models[[n]], null=null) %>% as.data.frame() %>% mutate(model = n)
    return(r) }) 
  model_r2 <- model_r2 %>% bind_rows()
  model_lme <- model_lme %>% bind_rows()
  table <- model_lme %>% left_join(model_r2)  
  table <- table %>%
    mutate(AIC = format(round(AIC,2), nsmall = 2),
           BIC = format(round(BIC,2), nsmall = 2),
           logLik = format(round(logLik,2), nsmall = 2),
           R2m = format(round(R2m,2), nsmall = 2),
           R2c = format(round(R2c,2), nsmall = 2)
    ) %>%
    arrange(desc(R2m), desc(logLik)) %>%
    rename(`$\\chi^2$` = `Pr(>Chisq)`, `$R^2_m$` = R2m, `$R^2_c$` = R2c, `Fixed Effect` = model, `ML` = logLik)
  
  return(table)
}

perc.null = lmer(PercNormalized ~ (1|Participant), data = St, REML=F)
perc.0 <- list("Condition Order" = lmer(PercNormalized ~ (1|Participant) + (1|ConditionOrder), data = St, REML=F),
               
)

lapply(perc.0, function(x) { anova(perc.null, x) })

perc.null = lmer(PercNormalized ~ (1|Participant), data = St, REML=F)
perc.1 <- list("Blink Rate" = lmer(PercNormalized ~ rate_blink + (1|Participant), data = St, REML=F),# Perceived Control to MI Rate
               "Fab. Rate" = lmer(PercNormalized ~ fab_rate + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Gender" = lmer(PercNormalized ~ gender + (1|Participant), data = St, REML=F), # Perceived Control to Gender
               "Condition" = lmer(PercNormalized ~ Condition + (1|Participant), data = St, REML=F), # Perceived Control to Condition'
               "Delay" = lmer(PercNormalized ~ mean_delay + (1|Participant), data = St, REML=F), # Perceived Control to Condition'
               "Game" = lmer(PercNormalized ~ GameTitle + (1|Participant),  data = St, REML=F), # Perceived Control to Game
               "Pos. Feedback" = lmer(PercNormalized ~ feedback_rate + (1|Participant), data = St, REML=F) # Perceived Control to Feedback
)

lapply(perc.1, function(x) { anova(perc.null, x) })

perc.feedback = lmer(PercNormalized ~ feedback_rate + (1|Participant), data = St, REML=F)
perc.2 <- list("Pos. Feedback + Blink Rate" = lmer(PercNormalized ~ feedback_rate + rate_blink + (1|Participant), data = St, REML=F),# Perceived Control to MI Rate
               "Pos. Feedback + Fab. Rate" = lmer(PercNormalized ~ feedback_rate + fab_rate + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Pos. Feedback + Condition" = lmer(PercNormalized ~ feedback_rate + Condition + (1|Participant), data = St, REML=F) # Perceived Control to Condition
)
lapply(perc.2, function(x) { anova(perc.feedback, x) })

perc_lme_table <- g_lme_table(perc.1, perc.null, "null")  %>% bind_rows(g_lme_table(perc.2, perc.feedback, "feedbacknull")) 
perc_lme_table <- perc_lme_table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Predicted` = "Perceived Control",
         `Random Intercept` = "Participant",
         chi = format(round(`$\\chi^2$`,3), nsmall = 3),
         chi = ifelse(chi == "0.000", "$<$0.001", chi),
         `$\\chi^2$`= chi,
         chi = NULL) %>%
  select(Predicted, `Random Intercept`, `Fixed Effect`, AIC, BIC, ML, `$\\chi^2$`, `$R^2_m$`,`$R^2_c$`)

message(paste(colnames(perc_lme_table), collapse=" & "))
message(paste(perc_lme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))



#############
# Linear Regression Models - Frustration
#############
frust.null = lmer(FrustNormalized ~ (1|Participant), data = St, REML=F)
frust.0 <- list("Condition Order" = lmer(FrustNormalized ~ (1|Participant) + (1|ConditionOrder), data = St, REML=F),
)

lapply(frust.0, function(x) { anova(frust.null, x) })

frust.null = lmer(FrustNormalized ~ (1|Participant), data = St, REML=F)
frust.1 <- list("Blink Rate" = lmer(FrustNormalized ~ rate_blink + (1|Participant), data = St, REML=F),# Perceived Control to MI Rate
               "Fab. Rate" = lmer(FrustNormalized ~ fab_rate + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Gender" = lmer(FrustNormalized ~ gender + (1|Participant), data = St, REML=F), # Perceived Control to Gender
               "Condition" = lmer(FrustNormalized ~ Condition + (1|Participant), data = St, REML=F), # Perceived Control to Condition
               "Delay" = lmer(FrustNormalized ~ mean_delay + (1|Participant), data = St, REML=F), # Perceived Control to Condition'
               "Game" = lmer(FrustNormalized ~ GameTitle + (1|Participant),  data = St, REML=F), # Perceived Control to Game               
               "Pos. Feedback" = lmer(FrustNormalized ~ feedback_rate + (1|Participant), data = St, REML=F) # Perceived Control to Feedback
)

lapply(frust.1, function(x) { anova(frust.null, x) })

boxplot(feedback_rate ~ GameTitle, data = St) #GameTitle predicts frustration

frust.feedback = lmer(FrustNormalized ~ feedback_rate + (1|Participant), data = St, REML=F)
frust.2 <- list("Pos. Feedback + Blink Rate" = lmer(FrustNormalized ~ feedback_rate + rate_blink + (1|Participant), data = St, REML=F),# Perceived Control to MI Rate
               "Pos. Feedback + Fab. Rate" = lmer(FrustNormalized ~ feedback_rate + fab_rate + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Pos. Feedback + Delay" = lmer(FrustNormalized ~ feedback_rate + mean_delay + (1|Participant), data = St, REML=F), # Perceived Control to Fab Rate
               "Pos. Feedback + Condition" = lmer(FrustNormalized ~ feedback_rate + Condition + (1|Participant), data = St, REML=F) # Perceived Control to Condition
)
lapply(frust.2, function(x) { anova(frust.feedback, x) })

frust_lme_table <- g_lme_table(frust.1, frust.null, "null")  %>% bind_rows(g_lme_table(frust.2, frust.feedback, "feedbacknull")) 
frust_lme_table <- frust_lme_table %>% filter(`$\\chi^2$` < 0.05) %>% 
  mutate(`Predicted` = "Frustration",
         `Random Intercept` = "Participant",
         chi = format(round(`$\\chi^2$`,3), nsmall = 3),
         chi = ifelse(chi == "0.000", "$<$0.001", chi),
         `$\\chi^2$`= chi,
         chi = NULL) %>%
  select(Predicted, `Random Intercept`, `Fixed Effect`, AIC, BIC, ML, `$\\chi^2$`, `$R^2_m$`,`$R^2_c$`)

message(paste(colnames(frust_lme_table), collapse=" & "))
message(paste(frust_lme_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))




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

#############
# Frustration vs Perceived Control
#############

fig1 <- fig %>%
  add_trace(name= "0-100%", data = St, x=~n_clip(jitter(PercNormalized, amount=.025)), y=~n_clip(jitter(FrustNormalized, amount=.025)),
            type='scatter',mode='markers', color=I("darkgray"), hoverinfo='text',text=~paste(Participant,Condition, GameTitle)) %>%
  add_trace(name = "0-100%", data=p_lin(St,"FrustNormalized", "PercNormalized"), x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=T, name='0-100%') %>%
  layout(yaxis = list(showgrid=F, zeroline=F, dtick=0.2, range=c(-0.02,1.02), title="Frustration"),
         xaxis = list(showgrid=F, zeroline=F, dtick=0.2, range=c(-0.02,1.02), title="Perceived control"),
         showlegend=F)
fig1
orca(fig1, "fig/study2-perc-control-frustration.pdf", width=350, height=350)

corr<-cor.test(St$FrustrationEpisode,St$PerceivedControlEpisode,method="spearman")
corr$estimate
