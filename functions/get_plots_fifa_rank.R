get_plots_fifa_rank =
  function(data_team,
           teams_in_analysis,
           path_figures,
           last_years_threshold = "2017-01-01") {
    # Max fifa rank -----------------------------------------------------------
    
    max_fifa_rank =
      data |>
      select(home_team_fifa_rank) |>
      max()
    
    # Data filtering ----------------------------------------------------------
    
    data_team_of_interest =
      data_team |>
      filter(team %in% teams_in_analysis)
    
    # plot_fifa_rank_evolution ------------------------------------------------
    
    plot_fifa_rank_evolution =
      data_team_of_interest |>
      ggplot(aes(x = date, y = team_fifa_rank, color = team)) +
      geom_line() +
      ylim(0, max_fifa_rank) +
      ggtitle("Fifa Ranking Evolution") +
      ylab("Fifa Ranking") +
      xlab("Date")
    
    # Restrict to last years --------------------------------------------------
    
    data_team_of_interest_last_years =
      data_team_of_interest |>
      filter(date > lubridate::ymd(last_years_threshold))
    
    # plot_team_metrics -------------------------------------------------------
    
    plot_team_metrics =
      data_team_of_interest_last_years |>
      select_at(vars("date" |
                       "team" |
                       ends_with("_score") & !"team_score")) |>
      pivot_longer(cols = ends_with("_score"),
                   names_to = "metric",
                   values_to = "value") |>
      ggplot(aes(x = date,
                 y = value,
                 color = team)) +
      geom_line() +
      ylim(60, 100) +
      facet_wrap(facets = "metric")
    
    return_list = list(plot_fifa_rank_evolution = plot_fifa_rank_evolution,
                       plot_team_metrics = plot_team_metrics)
    
    return(return_list)
    
  }