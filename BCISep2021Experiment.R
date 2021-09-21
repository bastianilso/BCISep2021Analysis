library(plotly)
library(lubridate)
library(tidyverse)
library(gsheet)

options("digits.secs"=6)
fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_all.rda')


#############
# Summaries #
#############

# Sc = Summary of Conditions
Sc <- D %>% ungroup() %>% group_by(GameTitle, Participant,Condition) %>%
  filter(Event == "GameDecision") %>%
  summarize(rejInput = sum(TrialResult == "RejInput"),
            accInput = sum(TrialResult == "AccInput"),
            fabInput = sum(TrialResult == "FabInput"),
            posTrial = sum(TrialResult == "FabInput" | TrialResult == "AccInput"),
            negTrial = sum(TrialResult == "RejInput"),
            otherInput = sum(!TrialResult %in% c("RejInput", "AccInput", "FabInput")),
            GameTitle = unique(GameTitle),
            totalTrials = rejInput+accInput+fabInput,
            true_feedback_rate = accInput / totalTrials,
            feedback_rate = (accInput+fabInput) / totalTrials,
            PercNormalized = unique(PercNormalized),
            FrustNormalized = unique(FrustNormalized),
            GameOrder = unique(GameOrder),
            gender = unique(Gender),
            age = unique(Age),
            comment = unique(Comment),
            )

Sc <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  filter(Period %in% c("RestPeriod", "OpenPeriod")) %>%
  summarize(time_total = sum(time_delta),
            time_total_min = time_total / 60) %>% right_join(Sc)

# Blink counts
Sc <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  summarize(blinks_total = sum(Event == "EyeOpening" & Period == "OpenPeriod")
            ) %>% right_join(Sc)

# St = Summary of Trials
# Feedback Delay
St <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition, InputWindowOrderFilled) %>%
  filter(Event == "EyeOpening") %>%
  summarize(last_attempt_stamp = max(Timestamp)) %>% ungroup()

St <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition, InputWindowOrderWithDecision) %>%
  filter(Event == "GameDecision") %>%
  summarize(feedback_stamp = max(Timestamp)) %>%
  mutate(InputWindowOrderFilled = InputWindowOrderWithDecision) %>% ungroup() %>% right_join(St)

St <- St %>% filter(!InputWindowOrderFilled == -1) %>%
  mutate(feedback_delay = feedback_stamp - last_attempt_stamp,
         feedback_delay = as.numeric(feedback_delay))

fig %>% add_trace(x=~InputWindowOrderFilled, y=~feedback_delay, data=St, type='scatter')

Sc <- St %>% group_by(GameTitle, Participant, Condition) %>%
  summarize(mean_delay = mean(feedback_delay)) %>% right_join(Sc)

# FYI: Participant 4 50C30F has no blinks during the game itself (AccInput = 0) during hand,
# Therefore the blink data is NA.

# Sp = Summary of Participants
Sp <- Sc %>% ungroup() %>% group_by(Participant, GameTitle) %>%
  summarize(
            gender = unique(gender),
            age = unique(age),
            time_total = mean(time_total),
            mean_delay = mean(mean_delay, na.rm=T),
            mean_blinks = mean(blinks_total),
            mean_perc = mean(PercNormalized),
            mean_frust = mean(FrustNormalized)
            )

#############
# Latex Table with demographic and summary information
#############

Sp_table <- Sp %>% select(GameTitle, gender, age, mean_frust, mean_perc,
                          mean_blinks, mean_delay) %>%
                    mutate(
                      mean_frust = format(round(mean_frust,2), nsmall = 2),
                      mean_perc = format(round(mean_perc,2), nsmall = 2),
                      mean_blinks = format(round(mean_blinks,0), nsmall = 0),
                      mean_delay = format(round(mean_delay,2), nsmall = 2),
                      across(everything(), as.character))
Sp_table <- Sp_table %>%
             rename(Gender = gender, Age = age,
                    `Frustration (mean)` = mean_frust,
                    `Perc. Control (mean)` = mean_perc,
                    `Blinks (avg.)` = mean_blinks,
                    `Delay (mean)` = mean_delay,)

Sp_table = Sp_table %>% pivot_longer(cols=-c(Participant, GameTitle), names_to = "Feature") %>%
  pivot_wider(names_from = Participant, values_from = value)

paste(colnames(Sp_table), collapse=" & ")
message(paste(Sp_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))


#############
# Latex Table with stats per condition
#############

Sc_table <- Sc %>% ungroup() %>% 
  select(GameTitle, Condition, FrustNormalized, PercNormalized) %>%
  group_by(GameTitle,Condition) %>%
  summarize(mean_frust = mean(FrustNormalized),
            mean_perc = mean(PercNormalized)) %>%
  mutate(
    mean_frust = format(round(mean_frust,2), nsmall = 2),
    mean_perc = format(round(mean_perc,2), nsmall = 2),
    GameTitle = ifelse(GameTitle == "hand", "Hand", "Kiwi"),
    across(everything(), as.character))

Sc_table <- Sc_table %>% ungroup() %>%
  rename(`Game` = GameTitle,
         `Frustation (mean)` = mean_frust,
         `Perc. Control (mean)` = mean_perc)

paste(colnames(Sc_table), collapse=" & ")
message(paste(Sc_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))

#############
# Perceived Control, Violin plot by Condition
#############
fig1 <- fig %>%
  add_trace(x=factor(Sc$Condition, levels=c("50C0F", "50C15F","50C30F","50C50F")), y=jitter(Sc$PercNormalized,amount=.02),
            scalemode='width',points='all', pointpos=0,name='C', jitter=.3,
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.08, color=I('darkgray'))  %>%
  #add_trace(data=PercPureCurve, x=~x, y=~y, type='scatter', line=list(dash='dot'), symbol=I('square-x-open'), mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  #add_trace(data=PercFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  layout(showlegend=F, yaxis = list(range=c(0,1.1), title="Perceived Control", violinmode = 'overlay', violingap = 0), xaxis=list(title="Fabrication Rate (%)"))

orca(fig1, "fig/level-of-control-perceived.pdf", width=350, height=350)

#############
# Frustration, Violin plot by Condition
#############
fig1 <- fig %>%
  add_trace(x=factor(Sc$Condition, levels=c("50C0F", "50C15F","50C30F","50C50F")), y=jitter(Sc$FrustNormalized,amount=.02),
            scalemode='width',points='all', pointpos=0,name='C', jitter=.3,
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.08, color=I('darkgray'))  %>%
  #add_trace(data=PercPureCurve, x=~x, y=~y, type='scatter', line=list(dash='dot'), symbol=I('square-x-open'), mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  #add_trace(data=PercFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
  #          error_y= list(array=~perc_error)) %>%
  layout(showlegend=F, yaxis = list(range=c(0,1.1), title="Frustration", violinmode = 'overlay', violingap = 0), xaxis=list(title="Fabrication Rate (%)"))

orca(fig1, "fig/level-of-control-frustration.pdf", width=350, height=350)
