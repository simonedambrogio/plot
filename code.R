
library(httr); library(rjson)
library(dplyr); library(ggplot2); library(ggprism)
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
  internal_coherence <- all_data[[monkey]] %>% mutate(ic=abs(ic-0.5)*2) %>% 
      group_by(ic, date) %>% 
      summarise(acc=mean(id)) %>% 
      ggplot(aes(ic, acc)) +
      geom_point(size=4, color='gray') +
      geom_smooth(data=all_data[[monkey]] %>% mutate(ic=abs(ic-0.5)*2),
                  aes(ic, id), method = 'glm', method.args=list(family='binomial'),
                  se=F, color=color, size=2) +
      scale_x_continuous(limits = c(0,1), guide = "prism_offset") +
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
    labs(x='Uncertainty',y='')
  
  ggsave(plot = pl, 
         filename = paste(monkeys[monkey],'internal_uncertainty.svg', sep='/'),
         width = 9, height = 6)
}

# --- External Probability --- #
for(monkey in 1:3){
  color=colors[monkey]
  external_coherence <- all_data[[monkey]] %>%
    mutate(ec=ec*100) %>% group_by(ec, date) %>% 
    summarise(acc=mean(ed)) %>% 
    ggplot(aes(ec, acc)) +
    geom_point(size=4, color='gray') +
    geom_smooth(data=all_data[[monkey]] %>% mutate(ec=ec*100),
                aes(ec, ed), method = 'glm', method.args=list(family='binomial'),
                se=F, color=color, size=2) +
    scale_x_continuous(limits = c(0,100), guide = "prism_offset") +
    scale_y_continuous( guide = "prism_offset") +
    ggthemes::theme_wsj() + theme(plot.title = element_text(hjust = 0.5), 
                                  axis.title.x=element_text(size=20)) +
    labs(x='Number of Dots', title='Proportion of External Selection',y='')
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
    labs(x='Uncertainty', y='')
  
  ggsave(plot = pl, 
         filename = paste(monkeys[monkey],'external_uncertainty.svg', sep='/'),
         width = 9, height = 6)
}



