# load packages
library(tidyverse)
library(criticalpath)
library(igraph)
library(scales)

# create work breakdown structure (WBS) for automated reporting tool project
tibble(
  Activity = c('A','B','C','D','E','F','G','H','I','J','K','L'),
  Description = c(
    'Define Project Objectives and Scope',
    'Assemble Project Team',
    'Gather Requirements from Stakeholders',
    'Data Collection and Integration',
    'Data Cleaning and Preprocessing',
    'Design Report Templates',
    'Develop Data Processing Pipelines',
    'Implement Report Generation Logic',
    'Develop User Interface for Report Access',
    'Integrate Data Pipelines with Report Generation',
    'Conduct User Testing and Feedback',
    'Finalize and Deploy Automated Reporting Tool'),
  Dependencies = c(
    NA,
    'A',
    'A,B',
    'C',
    'D',
    'C',
    'E',
    'F',
    'F',
    'G,H',
    'I,J',
    'K')) -> wbs

# run glimpse() to get a compact, transposed summary of the tibble
glimpse(wbs)

# create network diagram using igraph
# create edge list from Dependencies
wbs %>%
  filter(!is.na(Dependencies)) %>% # remove activities with no predecessors (as necessary)
  separate_rows(Dependencies, sep = ",") %>% # split multiple predecessors into separate rows
  select(from = Dependencies, to = Activity) -> edges # define directed edges (from Dependencies to Activities)
# build graph
g <- graph_from_data_frame(edges, directed = TRUE)
# plot
plot(
  g,
  vertex.size = 30, # node size
  vertex.label.cex = 1.2, # label size
  edge.arrow.size = 0.5, # arrow size
  layout = layout_as_tree, # tree-like presentation
  main = 'Project Network Diagram\nAutomated Reporting Tool Project' # title and subtitle
)

# PERT
# add task durations (in weeks)
wbs %>%
  mutate(
    Optimistic_Estimate = c(2,1,2,4,2,3,3,2,2,3,2,1), # a in PERT
    Most_Likely_Estimate = c(3,2,3,5,3,4,4,3,3,4,3,2), # m in PERT
    Pessimistic_Estimate = c(5,3,5,8,5,6,6,5,5,7,5,3)) -> plan # b in PERT
glimpse(plan)

# add the final estimate using (a + 4m + b) / 6; round up to the nearest quarter
plan %>%
  mutate(
    PERT_Estimate = as.integer(
      round((Optimistic_Estimate + 4 * Most_Likely_Estimate + Pessimistic_Estimate) / 6)
    )
  ) -> updated_plan
glimpse(updated_plan)

# get the critical path - criticalpath package
sch_new() %>% # initialize a new, empty project schedule object
  sch_add_activities( # add activities (tasks) to the schedule
    id = 1:nrow(updated_plan), # assign integer IDs required by criticalpath package
    name = updated_plan$Activity, # use activity labels (A, B, C, ...) as names
    duration = updated_plan$PERT_Estimate # assign task durations (PERT estimates)
  ) %>%
  sch_add_relations( # define dependency relationships between activities
    from = match(edges$from, updated_plan$Activity), # map predecessor labels to integer IDs
    to = match(edges$to, updated_plan$Activity) # map successor labels to integer IDs
  ) %>%
  sch_plan() -> schedule # compute the schedule (ES, EF, LS, LF, float, critical path)
# check results
sch_duration(schedule) # return total project duration
sch_activities(schedule) %>% # return schedule subset on key columns
  select(name, duration, critical, early_start, late_start, early_finish, late_finish, total_float)
sch_critical_activities(schedule) %>% # return critical path tasks subset on key columns
  select(name, duration)
# confirm project duration by summing the durations from critical path tasks
sch_critical_activities(schedule) %>%
  summarize(total_duration = sum(duration))

# estimate the probability of completing the project (estimated to be 29 weeks in duration) within 30 weeks
# step 1 - compute the project variance; measures the uncertainty in project duration
updated_plan %>%
  mutate(
    variance = ((Pessimistic_Estimate - Optimistic_Estimate) / 6)^2
  ) -> updated_plan
# step 2 - sum the variance along the critical path
updated_plan %>%
  filter(Activity %in% sch_critical_activities(schedule)$name) %>%
  summarize(total_variance = sum(variance)) %>%
  pull(total_variance) -> project_variance
print(round(project_variance, 2))
# step 3 - compute standard deviation
project_sd <- sqrt(project_variance)
# step 4 - define a due date and the project mean (project timeline)
due_date <- 30
project_mean <- sch_duration(schedule)
# step 5 - compute z-score
z <- (due_date - project_mean) / project_sd
# step 6 - compute and print the probability of completing the project within the due date
probability <- pnorm(z)
percent(probability, accuracy = 1) # converts 0.7424094 to 74% using the scales package

# project crashing options
# step 1 - add crash inputs
updated_plan %>%
  select(Activity, Most_Likely_Estimate) %>%
  mutate(
    Crash_Time = c(2, 1, 2, 4, 2, 3, 2, 2, 2, 2, 2, 1),
    Most_Likely_Cost = c(5000, 4000, 5000, 9000, 5000, 8000, 8000, 5000, 5000, 7000, 5000, 4000),
    Crash_Cost = c(6000, 5000, 6000, 11000, 6000, 9000, 10000, 7000, 6000, 10000, 7000, 5000)
  ) %>%
  mutate(
    Weekly_Crash_Cost = (Crash_Cost - Most_Likely_Cost) / (Most_Likely_Estimate - Crash_Time)
  ) %>%
  mutate(
    across(-Activity, as.integer) 
    ) -> crash_plan
print(crash_plan)
# step 2 - get the critical path on crash_plan
sch_new() %>%
  sch_add_activities(
    id = 1:nrow(crash_plan),
    name = crash_plan$Activity,
    duration = as.integer(crash_plan$Most_Likely_Estimate)
  ) %>%
  sch_add_relations(
    from = match(edges$from, crash_plan$Activity),
    to = match(edges$to, crash_plan$Activity)
  ) %>%
  sch_plan() -> crash_schedule
# check the results
# still 29 weeks and same critical path 
# nothing has yet changed because the most likely estimates equal the PERT estimates
sch_duration(crash_schedule)  
sch_activities(crash_schedule) %>%
  select(name, duration, critical, early_start, late_start, early_finish, late_finish, total_float)
sch_critical_activities(crash_schedule) %>%
  select(name, duration)
# step 3 - identify the cheapest critical-path activities to crash
crash_plan %>%
  filter(Activity %in% sch_critical_activities(crash_schedule)$name) %>%
  select(Activity, Most_Likely_Estimate, Crash_Time, Weekly_Crash_Cost) %>%
  arrange(Weekly_Crash_Cost) -> critical_crash_candidates # sorts by Weekly_Crash_Cost in ascending order
print(critical_crash_candidates)
# step 4 - identify which activities to crash (A, B, C, E, G, and L; not J, D, or K)
crash_plan %>%
  mutate(
    Crashed_Duration = case_when(
      Activity %in% c("A", "B", "C", "E", "G", "L") ~ Crash_Time,
      TRUE ~ Most_Likely_Estimate
    )
  ) -> crashed_plan
print(crashed_plan)
# step 5 - recompute the schedule after crashing selected activities
sch_new() %>%
  sch_add_activities(
    id = 1:nrow(crashed_plan),
    name = crashed_plan$Activity,
    duration = as.integer(crashed_plan$Crashed_Duration)
  ) %>%
  sch_add_relations(
    from = match(edges$from, crashed_plan$Activity),
    to = match(edges$to, crashed_plan$Activity)
  ) %>%
  sch_plan() -> crashed_schedule
# step 6 - check results
sch_duration(crashed_schedule) # now 22 weeks; down from 29
sch_activities(crashed_schedule) %>%
  select(name, duration, critical, early_start, late_start, early_finish, late_finish, total_float)
sch_critical_activities(crashed_schedule) %>%
  select(name, duration) # confirms that each task duration when added sums to 22 weeks

# estimate the new probability of completing the project within 25 weeks
# assume each task has standard deviation = 20% of its crashed duration
crashed_plan %>%
  mutate(
    variance = (0.20 * Crashed_Duration)^2
  ) -> crashed_plan
# new project mean from crashed schedule
project_mean_crashed <- sch_duration(crashed_schedule)
# new project variance from critical-path activities
project_variance_crashed <- crashed_plan %>%
  filter(Activity %in% sch_critical_activities(crashed_schedule)$name) %>%
  summarise(total_variance = sum(variance)) %>%
  pull(total_variance)
# project standard deviation
project_sd_crashed <- sqrt(project_variance_crashed)
# probability of completing within 30 weeks
due_date <- 25
z_crashed <- (due_date - project_mean_crashed) / project_sd_crashed
probability_crashed <- pnorm(z_crashed)
percent(probability_crashed, accuracy = 1) # 97%

# optional visual 1 - illustrative PERT beta distribution in ggplot2
# define PERT parameters
optimistic_time <- 3
most_likely_time <- 5
pessimistic_time <- 10
# compute alpha and beta
alpha <- 1 + 4 * (most_likely_time - optimistic_time) / (pessimistic_time - optimistic_time)
beta <- 1 + 4 * (pessimistic_time - most_likely_time) / (pessimistic_time - optimistic_time)
# generate x values
x <- seq(optimistic_time, pessimistic_time, length.out = 1000)
# compute beta pdf (scaled to [a, b])
y <- dbeta(
  (x - optimistic_time) / (pessimistic_time - optimistic_time),
  shape1 = alpha,
  shape2 = beta
) / (pessimistic_time - optimistic_time)
# create data frame
df <- data.frame(x = x, y = y)
# plot
ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(color = "Beta Distribution")) +
  geom_area(aes(fill = "Beta Distribution"), alpha = 0.2) +
  geom_vline(aes(xintercept = optimistic_time, color = "Optimistic Time"), linetype = "dashed") +
  geom_vline(aes(xintercept = most_likely_time, color = "Most Likely Time"), linetype = "dashed") +
  geom_vline(aes(xintercept = pessimistic_time, color = "Pessimistic Time"), linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Beta Distribution" = "black",
      "Optimistic Time" = "green",
      "Most Likely Time" = "blue",
      "Pessimistic Time" = "red"
    )
  ) +
  scale_fill_manual(
    values = c("Beta Distribution" = "grey")
  ) +
  labs(
    title = "PERT Beta Distribution",
    x = "Activity Time",
    y = "Probability Density",
    color = "",
    fill = ""
  ) +
  theme_minimal()

# optional visual 2 - normal probability distribution of project completion time (before crashing)
# given values
mu <- 29
sigma <- 1.54
# generate x and y
x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)
df <- data.frame(x = x, y = y)
# probability (for label)
prob <- pnorm(30, mean = mu, sd = sigma)
# plot
ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(color = "Normal Distribution")) +
  # shaded area up to 30
  geom_area(
    data = subset(df, x <= 30),
    aes(fill = "Probability Area"),
    alpha = 0.4
  ) +
  # vertical lines
  geom_vline(aes(xintercept = mu, color = "Mean (29 weeks)"),
             linetype = "dashed") +
  geom_vline(aes(xintercept = 30, color = "Target (30 weeks)"),
             linetype = "dashed") +
  # labels near lines
  annotate("text", x = mu, y = max(y) * 0.65,
           label = "29 weeks", color = "red") +
  annotate("text", x = 30, y = max(y) * 0.5,
           label = "30 weeks", color = "darkgreen") +
  # text box (top-left)
  annotate(
    "label",
    x = min(x),
    y = max(y),
    hjust = 0,
    vjust = 1,
    label = paste0(
      "μ = 29 weeks\n",
      "σ = 1.54 weeks\n",
      "P(X ≤ 30) = ", scales::percent(prob, accuracy = 1)
    ),
    fill = "wheat",
    alpha = 0.6
  ) +
  # manual colors for legend
  scale_color_manual(
    values = c(
      "Normal Distribution" = "black",
      "Mean (29 weeks)" = "red",
      "Target (30 weeks)" = "darkgreen"
    )
  ) +
  scale_fill_manual(
    values = c("Probability Area" = "skyblue")
  ) +
  labs(
    title = "Project Completion Time Probability",
    x = "Weeks",
    y = "Probability Density",
    color = "",
    fill = ""
  ) +
  theme_minimal()









 
