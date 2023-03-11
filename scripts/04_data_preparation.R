# Project: ASE with dynamic displays.
# Participants: 1st, 5th and 9th grade children.
#
# This code generates the data.frame younger_grps_df which splits the
# 1st and 5th grade children into ASE/HTE groups.
# 
# Corrado Caudek
# Last updated: "Sat Mar 16 13:40:58 2019"


# Read data ---------------------------------------------------------------


df <- read_csv(here("data", "input", "raw", "children_data.csv"))

df %>% 
  group_by(grade, gender) %>% 
  summarise(
    n = length(unique(id))
  )


# Compute Caution Score  --------------------------------------------------

# Venom, speed, and caution: effects on performance in a visual search task, 
# Sulikowski (2012).

bysub_rt <- df %>%
  dplyr::filter(correct == 1) %>%
  group_by(id, grade, stim) %>%
  summarise(mrt = mean(rt_tukey, na.rm = TRUE, trim = 0.05))

# Transform from long to wide format for variable <stim>
bysub_wide <- spread(bysub_rt, stim, mrt) 
bysub_wide

bysub_wide$trg_angry <- bysub_wide$`8n1a` 
bysub_wide$trg_happy <- bysub_wide$`8n1h`

bysub_wide$`8n1a` <- NULL 
bysub_wide$`8n1h` <- NULL

# compute caution score (Sulikowski, 2012):
bysub_wide$caution_score_angry <-
  (bysub_wide$alln - bysub_wide$trg_angry) /
  (bysub_wide$alln + bysub_wide$trg_angry)

bysub_wide$caution_score_happy <-
  (bysub_wide$alln - bysub_wide$trg_happy) /
  (bysub_wide$alln + bysub_wide$trg_happy)

# This formula creates a normalised score that is directly proportionate to the 
# relative difference between the mean RT of the target-absent and target-present 
# trials for each condition (Sulikowski, 2012).
# We computed two CSs: one for angry target and one for happy targets.
# Interpretation:
# CS = 0: no advantage for target present over target absent trials
# CS > 0: target present RTs are smaller than target absent trials
# CS < 0: target present RTs are larger than target absent trials

bysub_wide %>%
  group_by(grade) %>%
  summarise(
    cs_a = mean(caution_score_angry, na.rm = TRUE),
    se_a = sqrt(var(caution_score_angry) / n()),
    cs_h = mean(caution_score_happy, na.rm = TRUE),
    se_h = sqrt(var(caution_score_happy) / n())
  )

bysub_long <- gather(
  bysub_wide,
  condition,
  caution_score,
  caution_score_angry:caution_score_happy,
  factor_key = TRUE
) 
# bysub_long

dim(bysub_long)
length(unique(bysub_long$id))

out <- bysub_long %>%
  group_by(grade, condition) %>%
  summarise(y = mean(caution_score, na.rm = TRUE, trim = 0.1),
            se = sqrt(var(caution_score) / n()))

out$grade <- factor(out$grade)

p <- ggplot(out, aes(x=grade, y=y, fill=condition)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=y-se, ymax=y+se), width=0.2,
                position=position_dodge(.9)) +
  scale_fill_OkabeIto() +
  scale_color_OkabeIto() +
  theme(legend.position = "top", legend.title = element_blank())
print(p)
rm(out, p)
# FIGURE 1
# The figure shows that the caution score (CS) takes on larger values 
# for angry rather than happy targets, but only in the 9-th grade group. 
# Moreover, for the other two age groups, the value of the CS is close to 
# zero, indicating the presence of similar RTs for target-absent and 
# target-present conditions. 



# Plot distributions of  CS differences by grade --------------------------


# To better understand the previous results, for each subject, we computed 
# the difference between the CS_angry - CS_happy in order to determine, for
# each subject, wheter there is a preference for the ASE or the HSE.
# If such difference is positive, then this means that the subject's RTs are
# faster for angry than for happy targets. 
# We have examined the distribution of such differences in each age group, 
# which indicate the presence of very large individual differences. 
# If we consider the 9-th grade age group, then the average of such difference 
# is positive, but there are large differences in the individual CD_dif scores.
# For the two other age groups, the distribution of CS differences is centered
# around the zero point.
bysub_wide$cs_dif <- 
  bysub_wide$caution_score_angry - bysub_wide$caution_score_happy

bysub_wide$grade <- factor(bysub_wide$grade)

p <- ggplot(bysub_wide, aes(x=cs_dif, fill=grade)) + 
  geom_density(alpha=0.6) +
  scale_fill_OkabeIto() +
  scale_color_OkabeIto() +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  theme(legend.position="top") 
print(p)
# FIGURE 2

length(bysub_wide$id)
# [1] 257



# Split data depending on cs_dif >= 0 or not ------------------------------


# in this manner I separate the participants in two groups: those who showed 
# the ASE and those who did not.

# if cs_dif >= 0, anger_advantage
bysub_wide$group <- factor(
  ifelse(
    bysub_wide$cs_dif >= 0, "anger_advantage", "anger_disadvantage"
  )
)

df_anger_advantage <- bysub_wide %>% 
  dplyr::filter(group == "anger_advantage")

df_anger_disadvantage <- bysub_wide %>% 
  dplyr::filter(group == "anger_disadvantage")

id_good <- unique(df_anger_advantage$id)
id_bad <- unique(df_anger_disadvantage$id)

# subjects' id for participants who showed the ASE
clean_good_id <- df %>% 
  dplyr::filter(id %in% id_good)

# subjects' id for participants who did not showed the ASE
clean_bad_id <- df %>% 
  dplyr::filter(id %in% id_bad)

# Angry Target Advantage (ATA)
# Happy Target Advantage (HTA)
df$group <- factor(
  ifelse(df$id %in% id_good, "ATA", "HTA")
)

out <- df %>% 
  dplyr::filter(correct == 1) %>% 
  group_by(grade_f, stim, group) %>% 
  summarise(
    y = mean(rt_tukey / 1000, na.rm = TRUE, trim = 0.1),
    se = sqrt(var(rt_tukey / 1000, na.rm = TRUE) / n()),
    n = n()
  )

# FIGURE 3 examine the average RTs in the 8n1a, 8n1h, and alln conditions, 
# for the two groups
p <- ggplot(out, aes(x = grade_f, y = y, fill = stim)) +
  facet_wrap(~ group) +
  geom_bar(
    stat = "identity", color = "black",
    position = position_dodge()
  ) +
  geom_errorbar(aes(ymin = y - se, ymax = y + se),
                width = .2,
                position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(2, 5.3)) +
  labs(
    y = "Average reaction times (s)",
    x = "",
    fill = "Stimuli"
  ) +
  scale_fill_OkabeIto() +
  scale_color_OkabeIto() +
  theme(legend.position = "top", legend.title = element_blank()) 

print(p)
# The figure suggests that, for participants who showed the ASE, RTs tend to
# be lower when the target is angry than in the target-absent condition.
# Interestingly, for participants who did not show the ASE, the RTs in the
# target present condition, when the target is angry, tend to be higher than
# in the target-absent condition.

if(0) ggsave("fig_indiv_diff.pdf", width = 7, height = 3.5)



# Prepare data for brm() analysis -----------------------------------------


grade_1_c <- df %>% 
  dplyr::filter(grade == 1, correct == 1, !is.na(rt_tukey)) %>% 
  mutate(rt = rt_tukey / 1000) %>% 
  select(id, group, where, target, grade, rt) 

grade_5_c <- df %>% 
  dplyr::filter(grade == 5, correct == 1, !is.na(rt_tukey)) %>% 
  mutate(rt = rt_tukey / 1000) %>% 
  select(id, group, where, target, grade, rt) 

grade_9_c <- df %>% 
  dplyr::filter(grade == 9, correct == 1, !is.na(rt_tukey)) %>% 
  mutate(rt = rt_tukey / 1000) %>% 
  select(id, group, where, target, grade, rt) 

# two younger age groups
younger_grps_df <- rbind(grade_1_c, grade_5_c)

rm(id_good, id_bad, p, out, grade_1_c, grade_5_c, grade_9_c, 
   df_anger_advantage, df_anger_disadvantage, df, clean_bad_id,
   clean_good_id, bysub_wide, bysub_rt, bysub_long)
