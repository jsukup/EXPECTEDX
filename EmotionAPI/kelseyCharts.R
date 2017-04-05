surprise <- data.frame(Emotion = c('Anger','Contempt','Disgust','Fear','Happiness','Neutral','Sadness','Surprise'), Proportion = c(0.0005987687,0.00748712569,0.000211082282,0.012900766,0.439258963,0.0354554765,9.680452E-05,0.503991))
neutral <- data.frame(Emotion = c('Anger','Contempt','Disgust','Fear','Happiness','Neutral','Sadness','Surprise'), Proportion = c(0.0011186162,0.008029969,0.00108658068,0.0007907312,0.06270089,0.887870848,0.0297426842,0.008659661))
happiness <- data.frame(Emotion = c('Anger','Contempt','Disgust','Fear','Happiness','Neutral','Sadness','Surprise'), Proportion = c(5.61285333E-07,4.83859779E-08,1.3563521E-05,7.73638653E-10,0.999984145,4.606645E-07,5.66495146E-07,6.480125E-07))

kelsey <- merge(happiness, neutral, by = 'Emotion')
kelsey <- merge(kelsey, surprise, by = 'Emotion')
names(kelsey) <- c('emotion','happiness','neutral','surprise')

sur.plot <- ggplot(kelsey, aes(x = 0, y = surprise, fill = emotion)) +
    geom_bar(stat = 'identity', position = 'stack') +
    scale_fill_brewer(palette = 'Set1', 
                      name = 'Emotion') +
    labs(list(x = NULL, 
              y = 'Proportion',
              color = 'Emotion')) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

neu.plot <- ggplot(kelsey, aes(x = 0, y = neutral, fill = emotion)) +
    geom_bar(stat = 'identity', position = 'stack') +
    scale_fill_brewer(palette = 'Set1', 
                      name = 'Emotion') +
    labs(list(x = NULL, 
              y = 'Proportion',
              color = 'Emotion')) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

hap.plot <- ggplot(kelsey, aes(x = 0, y = happiness, fill = emotion)) +
    geom_bar(stat = 'identity', position = 'stack') +
    scale_fill_brewer(palette = 'Set1', 
                      name = 'Emotion') +
    labs(list(x = NULL, 
              y = 'Proportion',
              color = 'Emotion')) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 


