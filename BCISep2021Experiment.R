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

# St = Summary of Trials
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
            PercNormalized = unique(PercNormalized),
            FrustNormalized = unique(FrustNormalized),
            GameOrder = unique(GameOrder),
            gender = unique(Gender),
            comment = unique(Comment),
            )


St <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  filter(Period %in% c("RestPeriod", "OpenPeriod")) %>%
  summarize(time_total = sum(time_delta),
            time_total_min = time_total / 60) %>% right_join(St)


# Sp = Summary of Participants
Sp <- St %>% ungroup() %>% group_by(Participant, GameTitle) %>%
  summarize(
            gender = unique(gender),
            time_total = mean(time_total),
            )

#############
# Latex Table with demographic and summary information
#############

Sp_table <- Sp %>% select(time_total, gender, GameTitle) %>%
                    mutate(
                      time_total = format(round(time_total,0), nsmall = 0),
                      across(everything(), as.character))
Sp_table <- Sp_table %>%
             rename(Gender = gender, `Mean Rate` = mean_rate, `Min. Rate` = min_rate,
                    `Max. Rate` = max_rate, `Mean Pos. Feedback` = mean_feedback, `MI Experience` = mi_experience,
                    `Mean Time (s)` = time_total, `False Positives (pr min)` = fp_rate_minute)

Sp_table = Sp_table %>% pivot_longer(cols=-c(Participant, GameTitle), names_to = "Feature") %>%
  pivot_wider(names_from = Participant, values_from = value)

paste(colnames(Sp_table), collapse=" & ")
message(paste(Sp_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))
