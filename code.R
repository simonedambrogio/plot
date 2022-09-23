
library(httr); library(rjson)
library(dplyr); library(ggplot2); 
fetchData <- function(url, token=TOKEN){
  req <- content(GET(
    url,
    add_headers(Authorization = paste("token", TOKEN))
  ), as = "parsed")  
  return(req)
}
getTrialData <- function(trial_list){
  ic  = trial_list[[1]]$coherence[1]
  ec  = trial_list[[1]]$coherence[2]
  iu  = trial_list[[1]]$uncertainty[1]
  eu  = trial_list[[1]]$uncertainty[2]
  id  = trial_list[[1]]$choice[1] # Internal is chosen
  ed  = 1-id
  
  return(data.frame(ic, ec, iu, eu, id, ed))
}
mytheme <- function(palette = "black_and_white", base_size = 14, base_family = "sans",
                    base_fontface = "plain", base_line_size = base_size/20, base_rect_size = base_size/14,
                    axis_text_angle = 0, border = FALSE) {
  
  library(ggprism)
  is_bool <- function(x) {
    is.logical(x) && !is.na(x)
  }
  angle <- axis_text_angle[1]
  if (!angle %in% c(0, 45, 90, 270))
    stop(sprintf("'axis_text_angle' must be one of [%s]", paste(c(0, 45, 90,
                                                                  270), collapse = ", ")), ".\nFor other angles, use the guide_axis() function in ggplot2 instead",
         call. = FALSE)
  if (!palette %in% names(ggprism::ggprism_data$themes)) {
    stop("The palette ", paste(palette), " does not exist.\n         See names(ggprism_data$themes) for valid palette names")
  }
  colours <- tibble::deframe(ggprism::ggprism_data$themes[[palette]])
  if (!is_bool(border)) {
    stop("border must be either: TRUE or FALSE")
  } else {
    if (border) {
      panel.border <- element_rect(fill = NA)
      axis.line <- element_blank()
    } else if (!border) {
      panel.border <- element_blank()
      axis.line <- element_line()
    }
  }
  t <- theme(line = element_line(colour = colours["axisColor"], size = base_line_size,
                                 linetype = 1, lineend = "square"), rect = element_rect(fill = "white", colour = colours["axisColor"],
                                                                                        size = base_rect_size, linetype = 1), text = element_text(family = base_family,
                                                                                                                                                  face = base_fontface, colour = colours["graphTitleColor"], size = base_size,
                                                                                                                                                  lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                                                                                                                                                  debug = FALSE), prism.ticks.length = unit(base_size/50, "pt"), axis.line = axis.line,
             axis.line.x = NULL, axis.line.y = NULL, axis.text = element_text(size = rel(0.95),
                                                                              colour = colours["axisLabelColor"]), axis.text.x = element_text(margin = margin(t = 0.8 *
                                                                                                                                                                base_size/4), angle = axis_text_angle, hjust = ifelse(axis_text_angle %in%
                                                                                                                                                                                                                        c(45, 90, 270), 1, 0.5), vjust = ifelse(axis_text_angle %in% c(0, 90,
                                                                                                                                                                                                                                                                                       270), 0.5, 1)), axis.text.x.top = element_text(margin = margin(b = 0.8 *
                                                                                                                                                                                                                                                                                                                                                        base_size/4), vjust = 0), axis.text.y = element_text(margin = margin(r = 0.5 *
                                                                                                                                                                                                                                                                                                                                                                                                                               base_size/4), hjust = 1), axis.text.y.right = element_text(margin = margin(l = 0.5 *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            base_size/4), hjust = 0), axis.ticks = element_line(), axis.ticks.length = unit(3,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "points"), axis.ticks.length.x = NULL, axis.ticks.length.x.top = NULL,
             axis.ticks.length.x.bottom = NULL, axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL,
             axis.ticks.length.y.right = NULL, axis.title = element_text(colour = colours["axisTitleColor"]),
             axis.title.x = element_text(margin = margin(t = base_size * 0.6), vjust = 1),
             axis.title.x.top = element_text(margin = margin(b = base_size * 0.6), vjust = 0),
             axis.title.y = element_text(angle = 90, margin = margin(r = base_size * 0.6),
                                         vjust = 1), axis.title.y.right = element_text(angle = -90, margin = margin(l = base_size *
                                                                                                                      0.6), vjust = 0), legend.background = element_blank(), legend.spacing = unit(base_size,
                                                                                                                                                                                                   "pt"), legend.spacing.x = NULL, legend.spacing.y = NULL, legend.margin = margin(base_size/2,
                                                                                                                                                                                                                                                                                   base_size/2, base_size/2, base_size/2), legend.key = element_blank(),
             legend.key.size = unit(1.2, "lines"), legend.key.height = NULL, legend.key.width = unit(base_size *
                                                                                                       1.8, "pt"), legend.text = element_text(size = rel(0.8), face = "plain"),
             legend.text.align = NULL, legend.title.align = NULL, legend.position = "right",
             legend.direction = NULL, legend.justification = "center", legend.box = NULL,
             legend.box.margin = margin(0, 0, 0, 0, "cm"), legend.box.background = element_blank(),
             legend.box.spacing = unit(base_size, "pt"), panel.background = element_rect(fill = ifelse(palette ==
                                                                                                         "office", colours["plottingAreaColor"], NA), colour = NA), panel.border = panel.border,
             panel.grid = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(base_size/2,
                                                                                                    "pt"), panel.spacing.x = NULL, panel.spacing.y = NULL, panel.ontop = FALSE,
             strip.background = element_blank(), strip.text = element_text(colour = colours["axisTitleColor"],
                                                                           size = rel(0.8), margin = margin(base_size/2.5, base_size/2.5, base_size/2.5,
                                                                                                            base_size/2.5)), strip.text.x = element_text(margin = margin(b = base_size/3)),
             strip.text.y = element_text(angle = -90, margin = margin(l = base_size/3)),
             strip.text.y.left = element_text(angle = 90), strip.placement = "inside",
             strip.placement.x = NULL, strip.placement.y = NULL, strip.switch.pad.grid = unit(base_size/4,
                                                                                              "pt"), strip.switch.pad.wrap = unit(base_size/4, "pt"), plot.background = element_rect(fill = colours["pageBackgroundColor"],
                                                                                                                                                                                     colour = NA), plot.title = element_text(size = rel(1.2), hjust = 0.5,
                                                                                                                                                                                                                             vjust = 1, margin = margin(b = base_size)), plot.title.position = "panel",
             plot.subtitle = element_text(hjust = 0.5, vjust = 1, margin = margin(b = base_size/2)),
             plot.caption = element_text(size = rel(0.8), hjust = 1, vjust = 1, margin = margin(t = base_size/2)),
             plot.caption.position = "panel", plot.tag = element_text(size = rel(1.2),
                                                                      hjust = 0.5, vjust = 0.5), plot.tag.position = "topleft", plot.margin = margin(base_size/2,
                                                                                                                                                     base_size/2, base_size/2, base_size/2), complete = TRUE)
  ggprism::ggprism_data$themes[["all_null"]] %+replace% t
}

URL = 'https://raw.githubusercontent.com/simonedambrogio/UGM/main/results/metadata.json'
TOKEN = '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';


metadata <- fetchData(URL, TOKEN) %>% rjson::fromJSON()

# Get all Results from all monkeys
monkeys=c('Zack', 'Zap', 'Zeno')
URL_head='https://raw.githubusercontent.com/simonedambrogio/UGM/main/results/'

all_data <- lapply(monkeys, function(monkey){
  all_dates <- metadata[[monkey]]
  purrr::map_dfr(all_dates, function(date){
    URL = paste0(URL_head, monkey, '/', date, '.json')
    all_data_date <- fetchData(URL, TOKEN) %>% rjson::fromJSON()
    purrr::map_dfr(all_data_date, getTrialData) %>% mutate(date=date)
  }) %>% mutate(monkey=monkey)
})


colors=c('zack'='#f2d096', 'zap'='#ed8975', 'zeno'='#8fb9aa')

# Internal Probability
for(monkey in 1:3){
  color=colors[monkey]
  internal_coherence <- all_data[[monkey]] %>% mutate(ic=abs(ic-0.5)) %>% 
      group_by(ic, date) %>% 
      summarise(acc=mean(id)) %>% 
      ggplot(aes(ic, acc)) +
      geom_point(size=4, color='gray') +
      geom_smooth(data=all_data[[monkey]] %>% mutate(ic=abs(ic-0.5)),
                  aes(ic, id), method = 'glm', method.args=list(family='binomial'),
                  se=F, color=color, size=2) +
      scale_x_continuous(limits = c(0,.5), guide = "prism_offset") +
      scale_y_continuous( guide = "prism_offset") +
      ggthemes::theme_wsj() + theme(plot.title = element_text(hjust = 0.5), 
                                    axis.title.x=element_text(size=20)) +
      labs(x='Coherence', title='Proportion of Internal Selection',y='')
  ggsave(plot = internal_coherence, 
         filename = paste(monkeys[monkey],'internal_coherence.svg', sep='/'),
         width = 9, height = 6)
}

# Uncertainty
for(monkey in 1:3){
  color=colors[monkey]
  pl <- all_data[[monkey]] %>%
    group_by(iu, date) %>% 
    summarise(acc=mean(id)) %>% 
    mutate(iu=as.factor(iu)) %>% 
    ggplot(aes(iu, acc)) +
    geom_boxplot(size=2, width = 0.1, color=color) +
    geom_jitter(size=4, width = 0.005, color='gray') +
    scale_x_discrete( guide = "prism_offset" ) +
    scale_y_continuous( guide = "prism_offset" ) +
    ggthemes::theme_wsj() + theme(plot.title = element_text(hjust = 0.5), 
                                  axis.title.x=element_text(size=20)) +
    labs(x='Coherence', title='Proportion of Internal Selection',y='')
  
  ggsave(plot = pl, 
         filename = paste(monkeys[monkey],'internal_uncertainty.svg', sep='/'),
         width = 9, height = 6)
}

# --- External Probability --- #
for(monkey in 1:3){
  color=colors[monkey]
  external_coherence <- all_data[[monkey]] %>%
    group_by(ec, date) %>% 
    summarise(acc=mean(ed)) %>% 
    ggplot(aes(ec, acc)) +
    geom_point(size=4, color='gray') +
    geom_smooth(data=all_data[[monkey]],
                aes(ec, ed), method = 'glm', method.args=list(family='binomial'),
                se=F, color=color, size=2) +
    scale_x_continuous(limits = c(0,1), guide = "prism_offset") +
    scale_y_continuous( guide = "prism_offset") +
    ggthemes::theme_wsj() + theme(plot.title = element_text(hjust = 0.5), 
                                  axis.title.x=element_text(size=20)) +
    labs(x='Coherence', title='Proportion of External Selection',y='')
  ggsave(plot = external_coherence, 
         filename = paste(monkeys[monkey],'external_coherence.svg', sep='/'),
         width = 9, height = 6)
}


# Uncertainty
for(monkey in 1:3){
  color=colors[monkey]
  pl <- all_data[[monkey]] %>%
    group_by(eu, date) %>% 
    summarise(acc=mean(ed)) %>% 
    mutate(eu=as.factor(eu)) %>% 
    ggplot(aes(eu, acc)) +
    geom_boxplot(size=2, width = 0.1, color=color) +
    geom_jitter(size=4, width = 0.005, color='gray') +
    scale_x_discrete( guide = "prism_offset" ) +
    scale_y_continuous( guide = "prism_offset" ) +
    ggthemes::theme_wsj() + theme(plot.title = element_text(hjust = 0.5), 
                                  axis.title.x=element_text(size=20)) +
    labs(x='Coherence', title='Proportion of Internal Selection',y='')
  
  ggsave(plot = pl, 
         filename = paste(monkeys[monkey],'external_uncertainty.svg', sep='/'),
         width = 9, height = 6)
}



