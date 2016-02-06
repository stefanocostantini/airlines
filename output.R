library(ggplot2)

load("lda_model.RData")
load("results.RData")

# Text styling

t <- list(family = "Futura, sans serif", size = 18, color = toRGB("grey50"))

# First do a chart that places topics on a horizontal line based on their score,
# and highlight the position of the topics with wordclouds on this line



# Then do figure one below to show the distribution of the scores - i.e. 
# which are the most frequent scores? We can also highlight our preferred topics
# using this code.

density <- density(scores)
density.plot <- as.data.frame(cbind(density$x,density$y))
colnames(density.plot) <- c("Score","Frequency")
topics.nos <- seq(1,50,1)
topics.scores <- as.data.frame(cbind(topics.nos,scores))
freq <- density.plot[match(round(topics.scores[,2],2),round(density.plot$Score,2)),2]
topics.scores$freq <- freq

ggplot(data=density.plot, aes(x=Score, y=Frequency)) +
  geom_line(size=1.5, color = "#222222") +
  theme(text = element_text(size=14)) +
  scale_x_continuous(name="Scores", breaks=seq(-0.25,0.4,0.05)) +
  scale_y_continuous(name="Frequency", breaks=seq(0,7,0.5)) +
  geom_point(data=topics.scores[14,],aes(x=scores+0.001,y=freq+0.4), size = 5, color = "blue") +
  annotate("text", label="Topic 14", x=-0.07, y=5, size=5)

# Then Use tweets allocations by airline (allocation.dist)
# and do density plot by airline to show which are the topics being discussed the most
# for each airline

# Figure 3 - Show the distributiokn of tweet sentiments by airline

head(d)
s