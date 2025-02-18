
# Prep code (part of Heidi's): Run if you want to plot data  --------

library(pacman)
p_load(hms, tidyverse, readxl, openxlsx)


# TTS Android codebook- this was last updated 20221116
ttscb <- read.xlsx("~/R_projects/tandt_2.0/01_scripts/02.1_check_flags/RadLab_AdultAppCodebook_Masterfile.xlsx", sheet = "AdultAppCodebook")
ttscb2 <- ttscb %>%
  select(app_full_name, appcat_tts)

# using TTS coding scheme since it's slightly more complete

labels <- c("Games/Gaming", "Live Gaming/Streaming", "Kids Apps", "YouTube", "Video Streaming", "Audio Streaming",
            "Photos", "Reading", "News", "Phone Calls", "Chat/Messaging", "Videochat", "Social Media", "Email",
            "Browser", "Tools/Productivity", "Parenting Apps", "Religious Apps", "Health/Fitness/Wellness", 
            "Shopping", "Dating Apps", "Porn/Sex-related Apps", "Gambling", "Background Utilities", "Uncategorized")

appcat_labels <- tibble(appcat_tts=1:25, appcatlabels=labels)

appcat_labels$appcat_tts[25] <-NA


# set colors
myColors <- c("#e6194b", "#ffe119", "#f58231", "#4363d8", "#42d4f4", "#aaffc3","#469990","#800000","#9a6324","#dcbeff","#911eb4","#f032e6",
              "#fabed4", "#bfef45", "#3cb44b", "#ffd8b1", "#000075", "#808000", "#fffac8", "#daa520", "#000000", "#708090", "#2f4f4f", "yellow", "#000000")

# myColors <- c("#42d4f4")
appcat_cols <- tibble(appcat_tts=1:25, appcatlabels=labels, appcatcols=myColors)
appcat_cols$appcat_tts[25] <-NA



# Reading and Filtering Data ------------------------------------------------

library(pacman)
p_load(tidyverse, lubridate)

### Set the directory containing your .csv files
path_to_files <- ""

# Get a list of all .csv files in the directory
csv_files <- list.files(path = path_to_files, pattern = "*.csv", full.names = TRUE)

# Initialize an empty data frame
raw_data <- data.frame()

### Loop through each .csv file and append to the main data frame
for (file in csv_files) {
  # Check if file is empty
  if (file.size(file) == 0) {
    message(paste("Skipping empty file:", file))
    next  # Skip to the next iteration
  }
  
  # Read the .csv file
  temp_data <- read.csv(file)
  
  # Standardize column types (convert all columns to character for simplicity)
  temp_data <- temp_data %>%
    mutate(across(everything(), as.character))
  
  # Append to the main data frame
  raw_data <- bind_rows(raw_data, temp_data)
  
  # Remove temp_data df
  rm(temp_data)
}


### Function: Filter App Names
# TODO Not currently filtering out any apps 
filter_apps <- function(data) {
  excluded_app_package_names <- c(
    # Add excluded app package names here
  )
  
  excluded_application_labels <- c(
    # Add excluded application labels here
  )
  
  # Filter out excluded apps
  data <- data %>% filter(!app_package_name %in% excluded_app_package_names,
                          !application_label %in% excluded_application_labels)
  return(data)
}

### Function: Add Day Number and Day of Week
add_day_number_and_day_of_week <- function(data) {
  data <- data %>%
    # Ensure date is extracted from the start_timestamp
    mutate(date = as.Date(start_timestamp)) %>%
    # Find the first date for each participant
    group_by(participant_id) %>%
    mutate(
      first_date = min(date),
      day_number = as.numeric(difftime(date, first_date, units = "days")) + 1,
      day_of_week = weekdays(date)  # Add day of the week
    ) %>%
    ungroup() %>%
    select(-first_date)
  
  return(data)
}

### Function: Split Usages Across Midnight 
split_usages_across_dates <- function(data) {
  # Filter rows where the start and stop timestamps are on different dates
  usages_split <- data %>%
    filter(as.Date(start_timestamp) != as.Date(stop_timestamp))
  
  # Process each row that spans multiple dates
  if (nrow(usages_split) > 0) {
    split_rows <- usages_split %>%
      rowwise() %>%
      do({
        row <- .
        # First part: start to end of the first day
        first_part <- data.frame(
          participant_id = row$participant_id,
          app_package_name = row$app_package_name,
          start_timestamp = row$start_timestamp,
          stop_timestamp = as.POSIXct(paste(as.Date(row$start_timestamp), "23:59:59.999"), tz = "UTC"),
          duration = as.numeric(difftime(as.POSIXct(paste(as.Date(row$start_timestamp), "23:59:59.999"), tz = "UTC"), row$start_timestamp, units = "secs"))
        )
        # Second part: start of the next day to stop
        second_part <- data.frame(
          participant_id = row$participant_id,
          app_package_name = row$app_package_name,
          start_timestamp = as.POSIXct(paste(as.Date(row$stop_timestamp), "00:00:00.001"), tz = "UTC"),
          stop_timestamp = row$stop_timestamp,
          duration = as.numeric(difftime(row$stop_timestamp, as.POSIXct(paste(as.Date(row$stop_timestamp), "00:00:00.001"), tz = "UTC"), units = "secs"))
        )
        
        # Combine the two parts
        bind_rows(first_part, second_part) %>% 
          mutate(split_flag_midnight = TRUE)
      })
    
    # Combine split rows with the rest of the data
    data <- data %>%
      filter(as.Date(start_timestamp) == as.Date(stop_timestamp)) %>%
      bind_rows(split_rows)
  }
  
  return(data)
}


# Process Data for App Usage Only -------------------------------------------

### Re-code interaction_types to one uniform naming system
relabelled_raw <- raw_data %>%
  
  # Re-code interaction_types to one uniform naming system
  mutate(interaction_type = case_when(
    # interaction_type == "Activity Stopped" ~ "Move to Background",
    interaction_type == "Activity Paused" ~ "Move to Background",
    interaction_type == "Activity Resumed" ~ "Move to Foreground",

    interaction_type == "Unknown importance: 10" ~ "Notification Seen",

    interaction_type == "Unknown importance: 12" ~ "Notification Interruption",


    interaction_type == "Unknown importance: 15" ~ "Screen Interactive",
    interaction_type == "Unknown importance: 16" ~ "Screen Non-interactive",

    interaction_type == "Unknown importance: 17" ~ "Keyguard Shown",
    interaction_type == "Unknown importance: 18" ~ "Keyguard Hidden",

    interaction_type == "Unknown importance: 19" ~ "Foreground Service Start",
    interaction_type == "Unknown importance: 20" ~ "Foreground Service Stop",

    interaction_type == "Unknown importance: 23" ~ "Activity Stopped",

    interaction_type == "Unknown importance: 26" ~ "Device shutdown",
    interaction_type == "Unknown importance: 27" ~ "Device startup",
    
    TRUE ~ interaction_type
  )) %>% 
  arrange(participant_id, event_timestamp)

### Timezone change checks

# tz_flagged_raw <- relabelled_raw %>% 
#   # Add tz_change_flag column based on the first timezone change
#   group_by(participant_id) %>%
#   mutate(
#     tz_change = timezone != lag(timezone, default = timezone[1]),
#     tz_change_cumsum = cumsum(tz_change),
#     tz_change_flag = if_else(tz_change_cumsum > 0, "flag", NA_character_)
#   ) %>%
#   ungroup() %>%
#   select(-tz_change, -tz_change_cumsum) # Remove intermediate columns if not needed

### Remove data after from timezone change

# tz_filtered_raw <- tz_flagged_raw %>%
#   filter(!(tz_change_flag %in% "flag"))

### Remove data after from timezone other than American/NewYork
tz_filtered_raw <- relabelled_raw %>%
  filter(timezone == "America/New_York")
  

### Filter to just app usgae related interaction_types
app_usage_data <- tz_filtered_raw %>% 
  
  filter(interaction_type %in% c("Move to Foreground", "Move to Background")) %>%
  
  filter_apps() %>%
    
  mutate(event_timestamp = as.POSIXct(event_timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) %>%
  arrange(participant_id, app_package_name, event_timestamp) %>%
  group_by(participant_id, app_package_name) %>%
  mutate(
    next_event = lead(interaction_type),
    next_timestamp = lead(event_timestamp)
  ) %>%
  filter(interaction_type == "Move to Foreground" & next_event == "Move to Background") %>%
  mutate(
    start_timestamp = event_timestamp,
    stop_timestamp = next_timestamp,
    duration = as.numeric(difftime(stop_timestamp, start_timestamp, units = "secs"))
  ) %>%
  ungroup() %>%
  filter(duration >= 1) %>%
  split_usages_across_dates() %>%
  add_day_number_and_day_of_week() %>% 
  select(participant_id, app_package_name, application_label, start_timestamp, stop_timestamp,
         duration, date, day_number, day_of_week) %>% 
  arrange(participant_id, start_timestamp)

# Save App Usage Only Data
saveRDS(app_usage_data, ".Rds")

# Process Data for App and Notifications ------------------------------------


notifications_data <- tz_filtered_raw %>%
  filter(interaction_type == "Notification Interruption") %>%
  mutate(
    event_timestamp = as.POSIXct(event_timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),  # Convert to POSIXct
    type = "notification",
    start_timestamp = event_timestamp,
    stop_timestamp = event_timestamp + lubridate::minutes(15),  # Now this works
    duration = 15 * 60
  ) %>%
  filter(duration >= 1)

# notifications_data <- tz_filtered_raw %>%
#   filter(interaction_type %in% c("Move to Foreground", "Move to Background", "Notification Interruption")) %>%
#   filter_apps() %>%
#   mutate(event_timestamp = as.POSIXct(event_timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) %>%
#   arrange(participant_id, app_package_name, event_timestamp) %>%
#   group_by(participant_id, app_package_name) %>%
#   mutate(
#     next_event = lead(interaction_type),
#     next_timestamp = lead(event_timestamp),
#     type = if_else(interaction_type == "Notification Interruption", "notification", "app_usage"),
#     notification_overlap = if_else(
#       interaction_type %in% c("Move to Foreground", "Move to Background") &
#         lag(interaction_type == "Notification Interruption", default = FALSE),
#       TRUE,
#       FALSE
#     )
#   ) %>%
#   mutate(
#     start_timestamp = event_timestamp,
#     stop_timestamp = if_else(
#       type == "notification",
#       event_timestamp + lubridate::minutes(15),
#       next_timestamp
#     ),
#     duration = if_else(
#       type == "notification",
#       15 * 60,
#       as.numeric(difftime(stop_timestamp, start_timestamp, units = "secs"))
#     )
#   ) %>%
#   ungroup() %>%
#   filter(duration >= 1)


# Save Notifications Data
saveRDS(notifications_data, ".Rds")

############################################################################
# Plots ====================================================================

app_usage_data <- readRDS(".Rds")

notifications_data <- readRDS(".Rds")

library(pacman)
p_load(tidyverse, ggplot2, lubridate, readr)

# External datasets already loaded: ttscb2, appcat_labels, appcat_cols
# Using "my_plotting.R"

# Function: Generate Plots -------------------------------------------------
generate_plots <- function(data, output_folder, notifications = NULL) {
  unique_participants <- unique(data$participant_id)
  
  for (participant in unique_participants) {
    # Filter data for the current participant
    dat2 <- data %>%
      filter(participant_id == participant) %>%
      arrange(start_timestamp) %>%
      left_join(ttscb2, by = c("app_package_name" = "app_full_name")) %>%
      left_join(appcat_labels, by = "appcat_tts")
    
    # Filter notifications for the participant, if provided
    notifications_subset <- if (!is.null(notifications)) {
      notifications %>%
        filter(participant_id == participant)
    } else {
      NULL
    }
    
    # Identify app categories present in the data
    whichapps <- dat2 %>%
      group_by(appcat_tts) %>%
      summarise(n = n(), .groups = "drop") %>%
      left_join(appcat_cols, by = "appcat_tts")
    
    # Create a consistent color scale for the app categories
    myCols <- whichapps$appcatcols
    names(myCols) <- whichapps$appcatlabels
    UniqColScale <- scale_color_manual(limits = whichapps$appcatlabels, values = myCols, aesthetics = c("color", "fill"))
    
    # Start building the plot
    fig1 <- ggplot() +
      theme_minimal(base_size = 14) +
      
      # Plot app usage events as bars
      geom_rect(
        data = dat2,
        aes(
          xmin = hms::as_hms(start_timestamp),
          xmax = hms::as_hms(stop_timestamp),
          ymin = as.Date(start_timestamp),
          ymax = as.Date(start_timestamp) + 0.5,
          fill = appcatlabels
        )
      ) +
      
      # Add scales and labels
      UniqColScale +
      labs(fill = "App Categories") +
      scale_x_time(
        breaks = hms::as_hms(c("00:00:00", "08:00:00", "16:00:00", "24:00:00")), 
        limits = hms::as_hms(c("00:00:00", "24:00:00"))
      ) +
      scale_y_date(breaks = "1 day", minor_breaks = NULL) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    
    # Add notifications as vertical lines if provided
    # if (!is.null(notifications_subset) && nrow(notifications_subset) > 0) {
    #   fig1 <- fig1 +
    #     geom_segment(
    #       data = notifications_subset,
    #       aes(
    #         x = hms::as_hms(event_timestamp),
    #         xend = hms::as_hms(event_timestamp),
    #         y = as.Date(event_timestamp),
    #         yend = as.Date(event_timestamp) + 0.5
    #       ),
    #       color = "red",
    #       size = 0.1
    #     )
    # }
    
    if (!is.null(notifications_subset) && nrow(notifications_subset) > 0) {
      fig1 <- fig1 +
        geom_segment(
          data = notifications_subset %>%
            filter(type == "notification"),  # Ensure only notifications are plotted
          aes(
            x = hms::as_hms(event_timestamp),
            xend = hms::as_hms(event_timestamp),
            y = as.Date(event_timestamp),
            yend = as.Date(event_timestamp) + 0.5
          ),
          color = "red",
          size = 0.1
        )
    }
    
    
    # Construct the file path for saving
    outputpath <- paste0(output_folder, participant, "_usage_by_day_prelim_raw.jpeg")
    
    # Save the plot
    ggsave(outputpath, plot = fig1, width = 10, height = 6, units = "in")
  }
}

# Define Output Folders ----------------------------------------------------
output_folder_usage_only <- ""
output_folder_notifications <- ""

# Generate Plots for App Usage Only ----------------------------------------
app_usage_data <- readRDS(".Rds")
generate_plots(app_usage_data, output_folder_usage_only)


# Generate Plots for Notifications Included -------------------------------
notifications_data <- readRDS(".Rds")
generate_plots(app_usage_data, output_folder_notifications, notifications_data)

