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
  scale_y_continuous(name="Frequency", breaks=seq(-0.5,7,0.5)) +
  geom_point(data=topics.scores[11,],aes(x=scores,-0.20), size = 5, shape = 17, color = "blue") +
  annotate("text", label="Delay-1 (11)", x=topics.scores[11,2], y=-0.5, size=5) +
  geom_point(data=topics.scores[45,],aes(x=scores,-0.20), size = 5, shape = 17, color = "blue") +
  annotate("text", label="Delay-2 (45)", x=topics.scores[45,2], y=-0.5, size=5) +
  geom_point(data=topics.scores[46,],aes(x=scores,-0.20), size = 5, shape = 17, color = "blue") +
  annotate("text", label="Delay-3 (46)", x=topics.scores[46,2], y=-0.5, size=5) +
  geom_point(data=topics.scores[17,],aes(x=scores,-0.20), size = 5, shape = 17, color = "red") +
  annotate("text", label="Booking (17)", x=topics.scores[17,2], y=-0.5, size=5) +
  geom_point(data=topics.scores[31,],aes(x=scores,-0.20), size = 5, shape = 17, color = "green") +
  annotate("text", label="Cust. service-1 (31)", x=topics.scores[31,2], y=-0.5, size=5) +
  geom_point(data=topics.scores[49,],aes(x=scores,-0.20), size = 5, shape = 17, color = "green") +
  annotate("text", label="Cust. service-2 (49)", x=topics.scores[49,2], y=-0.5, size=5) +
  geom_point(data=topics.scores[14,],aes(x=scores,-0.20), size = 5, shape = 17, color = "purple") +
  annotate("text", label="Luggage-1 (14)", x=topics.scores[14,2], y=-0.5, size=5) +
  geom_point(data=topics.scores[27,],aes(x=scores,-0.20), size = 5, shape = 17, color = "purple") +
  annotate("text", label="Luggage-2 (27)", x=topics.scores[27,2], y=-0.5, size=5) +
  geom_point(data=topics.scores[37,],aes(x=scores,-0.20), size = 5, shape = 17, color = "orange") +
  annotate("text", label="Seating (37)", x=topics.scores[37,2], y=-0.5, size=5) +
  geom_point(data=topics.scores[42,],aes(x=scores,-0.20), size = 5, shape = 17, color = "cyan") +
  annotate("text", label="Check-in (42)", x=topics.scores[42,2], y=-0.5, size=5)


# Then Use tweets allocations by airline (allocation.dist)
# and do density plot by airline to show which are the topics being discussed the most
# for each airline

# Figure 3 - Show the distributiokn of tweet sentiments by airline

head(d)
s