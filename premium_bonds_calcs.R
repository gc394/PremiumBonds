
library(tidyverse)

# does higher starting improvement % return
# what do returns look like over time (monte carlo) 
## ASSUMPTIONs
# All money is reinvested until hit 50k then is taken as income - currently just peaks at 50k
# each £25 entry can win multiple prizes (there is replacement)

# load in data - https://www.moneysavingexpert.com/savings/premium-bonds/
pb_df = read.csv(
  # got this from MSE or something
  "data/pb.csv",
  header=FALSE,
  stringsAsFactors=FALSE, 
  fileEncoding="latin1", 
  skip = 1,
  col.names = c('prize', 'number', 'odds_string', 'odds')) %>%
  dplyr::mutate(
    odds = as.numeric(str_trim(gsub(pattern = ',', replacement = '', odds)))
  ) %>%
  dplyr::filter(
    prize != 0
  )

# every £25 is an entry
starting_pot = 5000
months = 36
runs = 1000
month_vec = c()
winnings_vec = c()
pot_vec = c()

# outside of function test

for (m in 1:months){

  # initiate pot
  if (m == 1){
    
    pot = starting_pot
    # print(pot)
  }
  
  print(paste('Month:', m))
  
  month_winnings = map2_dbl(
    .x = pb_df$odds,
    .y = pb_df$prize,
    .f = function(odds, prize){
      # this is assuming you can win multiple on one entry
      # print(paste(pot))
      entries = pot/25
      
      # give 1 or 0 from odds
      win_or_lose = rbinom(1, size = 1, prob=entries/odds)
      
      # calculate how much won      
      winnings = ifelse(win_or_lose == 1, prize, 0)
      
      winnings
      
    }) %>% 
    sum()
  
  # total winnings for the month
  winnings_vec = append(winnings_vec, month_winnings)
  
  pot = pot + month_winnings
  
  pot_vec = append(pot_vec, pot)
  
  month_vec = append(month_vec, m)
  
}

pb_earnings_df = tibble(
  'starting_pot' = rep(c(starting_pot), months),
  'month' = month_vec, 
  'winnings' = winnings_vec,
  'pot' = pot_vec)

# inside function test
premium_bonds_estimator = function(starting_pot, months){
  
  month_vec = c()
  winnings_vec = c()
  pot_vec = c()
  
  for (m in 1:months){
    
    # initiate pot
    if (m == 1){
      
      pot = starting_pot
      
    }
    
    month_winnings = map2_dbl(
      .x = pb_df$odds,
      .y = pb_df$prize,
      
      .f = function(odds, prize){

        # this is assuming you can win multiple on one entry
        entries = pot/25
        
        # give 1 or 0 from odds - each entry
        win_or_lose = rbinom(n = 1, size = entries, prob=1/odds) > 0

        # calculate how much won      
        winnings = ifelse(win_or_lose, prize, 0)
        
        winnings
        
      }) %>% 
      sum()
    
    # add to winnings vector
    winnings_vec = append(winnings_vec, month_winnings)
    
    #maximum in premium bonds in 50k
    pot = ifelse(pot + month_winnings > 50000, 50000, pot + month_winnings)
    
    pot_vec = append(pot_vec, pot)
    
    month_vec = append(month_vec, m)
    
  }
  
  tibble(
    'starting_pot' = rep(c(starting_pot), months),
    'month' = month_vec, 
    'winnings' = winnings_vec,
    'pot' = pot_vec)
  
}

## test run
runs = 10
starting_pot = 50000
months = 5*12

pb_mc_df = replicate(runs, premium_bonds_estimator(starting_pot = starting_pot, months = months), simplify = F) %>%
  map2_df(
    .x = .,
    .y = 1:runs,
    .f = function(x, run){
    
    x %>%
      dplyr::mutate(run = as.factor(run))
    
  })

ggplot(data = pb_mc_df,
       mapping = aes(x = month, y = pot, col = run, group = run)) +
  geom_line(show.legend = FALSE) +
  labs(x = 'Months',
       y = 'Total Pot',
       title = 'Premium Bonds Expected Earnings over five years')

pb_mc_df %>%
  group_by(run) %>%
  summarise(tot_earnings = sum(winnings, na.rm = T),
            avg_earnings_year = tot_earnings/(months/12),
            avg_earnings_month = avg_earnings_year/12)


######### Optimizing
# find optimal spot over five years
runs = 10
month_iterations = c(3,5,10)*12
starting_point_iterations = c(seq(1000, 4000, 2000), seq(5000,50000, 5000))

iterateration_df = expand.grid(month_iterations, starting_point_iterations) %>%
  dplyr::rename(months = Var1, sp = Var2)

### Monte Carlo Simulation
time_series_list = list()
for (i in 1:nrow(iterateration_df)){
  
  sp = iterateration_df[i, 'sp']
  months = iterateration_df[i, 'months']

  time_series_df = replicate(runs, premium_bonds_estimator(starting_pot = sp, months = months), simplify = F) %>%
    map2_df(
      .x = .,
      .y = 1:runs,
      .f = function(x, run){
        
        x %>%
          dplyr::mutate(run = as.factor(run),
                        tot_months = months)
        
      })
  
  time_series_list[[i]] = time_series_df
  
}

ggplot(data = do.call(what = rbind, time_series_list),
       mapping = aes(
         x = month, 
         y = pot, 
         col = run, 
         group = run)) +
  geom_line(show.legend = FALSE) +
  labs(x = 'Months',
       y = 'Total Pot',
       title = 'Premium Bonds Expected Earnings over five years') +
  facet_grid(starting_pot ~ tot_months, )


### Annualized Returns
starting_pot_vec = c()
average_earnings_vec = c()
median_earnings_vec = c()
sd_earnings_vec = c()
for (i in 1:nrow(iterateration_df)){
  
  sp = iterateration_df[i, 'sp']
  months = iterateration_df[i, 'months']
  
  total_earnings = replicate(runs, 
                             premium_bonds_estimator(starting_pot = sp, 
                                                     months = months), 
                             simplify = F) %>%
    # add run id to each df and bind
    map_dbl(
      .x = .,
      .f = function(x){
        
        x %>%
          summarise(total_earnings = sum(winnings)) %>%
          pull('total_earnings')
        
      }) 
  
  starting_pot_vec = append(starting_pot_vec, sp)
  average_earnings_vec = append(average_earnings_vec, mean(total_earnings))
  median_earnings_vec = append(median_earnings_vec, median(total_earnings))
  sd_earnings_vec = append(sd_earnings_vec, sd(total_earnings))
  
  print(paste0(months, ' month run for £', sp,' starting pot complete'))
  
}

# find rough optimal point
# annualise these results
optimal_df = tibble(
  'starting_pot' = starting_pot_vec,
  'months' = iterateration_df[, 'months'],
  'average_earnings' = average_earnings_vec,
  'median_earnings' = median_earnings_vec,
  'sd_earnings' = sd_earnings_vec
  ) %>%
  dplyr::mutate(
    # earnings over period as percentage of starting pot
    avg_total_yield = average_earnings*100/starting_pot,
    med_total_yield = median_earnings*100/starting_pot,
    # this but annualised
    across(.cols = contains('total_yield'), 
           # this voodoo annualises returns into same format
           # as numeric above
           # first find years (months/12) and log of return
           .fns = ~(exp(log(1+(.x/100))/(months/12)) - 1)*100, 
           .names = '{.col}_annualised'),
    across(.cols = where(is.numeric),
           .fns = ~round(.x, 2)),
    across(.cols = c('starting_pot', 'months'),
           .fns = ~as.factor(.x))
  )

ggplot2::ggplot(data = optimal_df,
                mapping = aes(x = starting_pot,
                              y = avg_total_yield_annualised,
                              fill = starting_pot)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~months, nrow = 3)  +
  #scale_fill_discrete_boe() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), 
        axis.text.y = ) +
  labs(
    x = 'Starting Pot (£)',
    y = 'Average Annualized Return (%)',
    title = 'Premium Bonds Annualized Return', 
    subtitle = 'Over different month timeframes using Monte Carlo Simulation'
  ) + 
  guides(fill="none")
  
