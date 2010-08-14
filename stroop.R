library('testthat')
library('ggplot2')

max.blocks <- 10
max.trials <- 25

named.colors <- c('red', 'green', 'blue')

trial.types <- c('Congruous', 'Incongruous')
tasks <- c('Name Color', 'Name Word')

results <- data.frame()

cat('\n\n\n\n\n\n\n')

cat(paste('Welcome to the Stroop Task!', '\n', sep = ''))
cat('\n\n\n\n\n\n\n')

for (block in 1:max.blocks)
{
  task <- sample(tasks, 1)
  
  if (task == 'Name Color')
  {
    cat(colourise('For this block, please type the name of the color of ink used above.\n\n', 'blue'))
  }
  else
  {
    cat(colourise('For this block, please type the word written above.\n\n', 'blue'))
  }
  
  Sys.sleep(5)
  
  for (trial in 1:max.trials)
  {
    color <- sample(named.colors, 1)
    alt.color <- sample(named.colors[named.colors != color], 1)
    
    trial.type <- sample(trial.types, 1)
    
    if (trial.type == 'Congruous')
    {
      cat(paste(colourise(color, color), '\n', sep = ''))
    }
    else
    {
      cat(paste(colourise(color, alt.color), '\n', sep = ''))
    }
    
    start <- Sys.time()
    response <- readline()
    end <- Sys.time()
    rt <- as.numeric(end - start)
    
    cat('\n')
    
    results <- rbind(results, data.frame(Block = block,
                                         Trial = trial,
                                         Task = task,
                                         Type = trial.type,
                                         Color = color,
                                         AltColor = alt.color,
                                         RT = rt))
  }
}

write.table(results,
            file = 'results.csv',
            sep = ',',
            row.names = FALSE,
            col.names = TRUE)

p <- ggplot(results, aes(x = Task, y = RT, fill = Type)) +
      stat_summary(fun.data = 'mean_cl_boot', geom = 'bar', position = 'dodge') +
      stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar', position = 'dodge') +
      opts(title = 'The Stroop Effect') +
      xlab('Task') + 
      ylab('RT (s)')
ggsave(filename = 'stroop_effect.png', plot = p)
