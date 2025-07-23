library(dplyr)
library(readr)
library(stringr)
library(igraph)
library(lubridate)
library(ggplot2)
library(tibble)
library(purrr)

emails <- readRDS("/Users/adamrauh/Downloads/cleaned-tx-2.rds")
#emails <- readRDS("cleaned-tx.rds")

# Preprocess emails
emails_processed <- emails %>%
  mutate(
    sent_week_num = lubridate::week(sent_datetime),
    sender_job_rank = word(sender_class_title, -1),
    sender_job_name = str_remove(sender_class_title, paste0("\\s*", sender_job_rank, "$")),
    receiver_job_rank = word(receiver_class_title, -1),
    receiver_job_name = str_remove(receiver_class_title, paste0("\\s*", receiver_job_rank, "$"))
  ) %>%
  filter(!is.na(sender_employee_id), !is.na(receiver_employee_id))

# Parameters
weeks <- c(1, 2, 11, 12)
email_types <- c("to", "all")

# Function to calculate centrality metrics for a given week and email type
calculate_centrality <- function(week_number, email_type) {
  filtered_emails <- emails_processed %>%
    filter(sent_week_num == week_number) %>%
    {
      if (email_type == "to") filter(., receiver_type == "to") else .
    } %>%
    count(sender_employee_id, receiver_employee_id, name = "weight") %>%
    filter(!is.na(sender_employee_id), !is.na(receiver_employee_id))
  
  if (nrow(filtered_emails) == 0) {
    return(tibble())
  }
  
  g <- graph_from_data_frame(filtered_emails, directed = TRUE)
  
  # Edge weight
  E(g)$weight <- filtered_emails$weight
  
  # Eigenvector centrality
  ec <- eigen_centrality(g, directed = TRUE, weights = E(g)$weight)$vector
  
  # Betweenness (inverse weight = shorter paths through stronger ties)
  bc <- betweenness(g, directed = TRUE, normalized = TRUE, weights = 1 / E(g)$weight)
  
  # Hub and authority scores
  hub_scores <- hub_score(g, weights = E(g)$weight)$vector
  authority_scores <- authority_score(g, weights = E(g)$weight)$vector
  
  # Assemble centrality dataframe
  centrality_df <- tibble(
    employee_id = V(g)$name,
    in_degree = degree(g, mode = "in"),
    out_degree = degree(g, mode = "out"),
    eigen_centrality = ec,
    betweenness_centrality = bc,
    hub_score = hub_scores,
    authority_score = authority_scores,
    week_number = week_number,
    email_type = email_type
  )
  
  return(centrality_df)
}

# Loop over all week/email_type combinations
results <- tidyr::expand_grid(week_number = weeks, email_type = email_types) %>%
  pmap_dfr(~calculate_centrality(..1, ..2))

# Of these measures, in_degree, authority score, and eigenvector centrality are going to give you similar pictures, with in degree being the least sophisticated. Eigenvector and authority give you similar but slightly different stories about importance, but I'm guessing they are very similar. 
# out_degree and hub scores will tell you similar things, but hub scores are Fancier.
# betweenness is the most distinctive -- if you connect two important people, you are high on betweenness. These are lowest priority/interest imo.


employee_info_all <- emails_processed %>%
  # Select sender info
  select(
    employee_id = sender_employee_id,
    salary = sender_salary,
    class_title = sender_class_title,
    job_category = sender_occ_category,
    job_rank = sender_job_rank,
    ethnicity = sender_ethnicity,
    gender = sender_gender,
    hire_date = sender_hire_date
  ) %>%
  bind_rows(
    # Select receiver info
    emails_processed %>%
      select(
        employee_id = receiver_employee_id,
        salary = receiver_salary,
        class_title = receiver_class_title,
        job_category = receiver_occ_category,
        job_rank = receiver_job_rank,
        ethnicity = receiver_ethnicity,
        gender = receiver_gender,
        hire_date = receiver_hire_date
      )
  ) %>%
  distinct(employee_id, .keep_all = TRUE)


results.all <- left_join(results, employee_info_all,
          by = "employee_id")


# Helper function to generate a plot for a given metric (recode weeks, no legend)
plot_avg_centrality <- function(df, metric_col) {
  df %>%
    mutate(week_vis = case_when(
      week_number == 1 ~ 1,
      week_number == 2 ~ 2,
      week_number == 11 ~ 3,
      week_number == 12 ~ 4
    )) %>%
    group_by(job_category, week_vis) %>%
    summarise(avg_score = mean(.data[[metric_col]], na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = week_vis, y = avg_score, color = job_category)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Average", metric_col, "by Job Name and Week"),
      x = "Week Number (Visualized)",
      y = paste("Average", metric_col)
    ) +
    scale_x_continuous(breaks = 1:4, labels = c("1", "2", "11", "12")) +
    theme_minimal() #+
    #theme(legend.position = "none")
}

# Create each plot
plot_eigen <- plot_avg_centrality(results.all, "eigen_centrality")
plot_hub   <- plot_avg_centrality(results.all, "hub_score")
plot_auth  <- plot_avg_centrality(results.all, "authority_score")
plot_in  <- plot_avg_centrality(results.all, "in_degree")
plot_out  <- plot_avg_centrality(results.all, "out_degree")


# Display plots
print(plot_eigen)
print(plot_hub)
print(plot_auth)
print(plot_in)
print(plot_out)
