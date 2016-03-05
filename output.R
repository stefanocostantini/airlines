library(ggplot2)
library(dplyr)
library(magrittr)

load("lda_model.RData")
load("results.RData")

# Text styling

t <- list(family = "Futura, sans serif", size = 18, color = toRGB("grey50"))

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

dist <- ggplot(data=density.plot, aes(x=Score, y=Frequency)) +
  geom_line(size=1.5, color = "#222222") +
  theme(text = element_text(size=14)) +
  scale_x_continuous(name="Scores", breaks=seq(-0.25,0.4,0.05)) +
  scale_y_continuous(name="Frequency", breaks=seq(0,7,0.5)) +
  geom_point(data=topics.scores[11,],aes(x=scores,-0.30), size = 5, shape = 17, color = "blue") +
  geom_point(data=topics.scores[45,],aes(x=scores,-0.30), size = 5, shape = 17, color = "blue") +
  geom_point(data=topics.scores[46,],aes(x=scores,-0.30), size = 5, shape = 17, color = "blue") +
  geom_point(data=topics.scores[17,],aes(x=scores,-0.30), size = 5, shape = 17, color = "red") +
  annotate("text", label="17", x=topics.scores[17,2], y=-0.6, size=5) +
  geom_point(data=topics.scores[31,],aes(x=scores,-0.30), size = 5, shape = 17, color = "green") +
  annotate("text", label="31", x=topics.scores[31,2], y=-0.6, size=5) +
  geom_point(data=topics.scores[49,],aes(x=scores,-0.30), size = 5, shape = 17, color = "green") +
  annotate("text", label="49", x=topics.scores[49,2], y=-0.6, size=5) +
  geom_point(data=topics.scores[14,],aes(x=scores,-0.30), size = 5, shape = 17, color = "purple") +
  geom_point(data=topics.scores[27,],aes(x=scores,-0.30), size = 5, shape = 17, color = "purple") +
  geom_point(data=topics.scores[37,],aes(x=scores,-0.30), size = 5, shape = 17, color = "orange") +
  annotate("text", label="37", x=topics.scores[37,2], y=-0.6, size=5) +
  geom_point(data=topics.scores[42,],aes(x=scores,-0.30), size = 5, shape = 17, color = "cyan") +
  geom_point(data=topics.scores[42,],aes(0.20,6.5), size = 5, color = "blue") +
  annotate("text", label="Delay", x=0.22, y=6.5, size=5, hjust=0) +
  geom_point(data=topics.scores[42,],aes(0.20,6), size = 5, color = "red") +
  annotate("text", label="Booking", x=0.22, y=6, size=5, hjust=0) +
  geom_point(data=topics.scores[42,],aes(0.20,5.5), size = 5, color = "green") +
  annotate("text", label="Customer service", x=0.22, y=5.5, size=5, hjust=0) +
  geom_point(data=topics.scores[42,],aes(0.20,5), size = 5, color = "purple") +
  annotate("text", label="Luggage", x=0.22, y=5, size=5, hjust=0) +
  geom_point(data=topics.scores[42,],aes(0.20,4.5), size = 5, color = "orange") +
  annotate("text", label="Seating", x=0.22, y=4.5, size=5, hjust=0) +
  geom_point(data=topics.scores[42,],aes(0.20,4), size = 5, color = "cyan") +
  annotate("text", label="Check-in", x=0.22, y=4, size=5, hjust=0) +
  geom_hline(aes(yintercept=0), linetype = 2, colour = "#333333") +
  geom_vline(aes(xintercept=0), linetype = 2, colour = "#333333")
  
# Do the following figures / illustrations

# 1. Dot clouds of tweets for each airlines, varying x according to their 
# score. One colour per airline. See if we can spot clusters in negative
# or positive regions.

# 2. Dashboard by topic for each airline: what are customers of each airline talking about?

topics.easyjet <- colMeans(allocation.dist[allocation.dist[,51]=="easyjet",-51])
topics.ryanair <- colMeans(allocation.dist[allocation.dist[,51]=="ryanair",-51])
topics.vueling <- colMeans(allocation.dist[allocation.dist[,51]=="vueling",-51])
topics.norwegian <- colMeans(allocation.dist[allocation.dist[,51]=="norwegian",-51])
topics.ba <- colMeans(allocation.dist[allocation.dist[,51]=="ba",-51])

topics.names <- c("17","42","31","49","11","45","46","14","27","37")

# EasyJet
dash.easyjet <- as.data.frame(cbind(topics.names,topics.easyjet[c(17,42,31,49,11,45,46,14,27,37)]))
colnames(dash.easyjet) <- c("Topics","Score")
dash.easyjet$Topics <- as.character(dash.easyjet$Topics)
dash.easyjet$Topics <- factor(dash.easyjet$Topics, levels = unique(dash.easyjet$Topics))

plot.easyjet <- ggplot() + 
    geom_bar(data=dash.easyjet[,], aes(x=Topics, y=Score), stat="Identity", col="grey", fill="grey", width = 0.7) +
    geom_bar(data=dash.easyjet[c(5,6,7),], aes(x=Topics, y=Score), stat="Identity", col="blue", fill="blue", width = 0.7) +
    geom_bar(data=dash.easyjet[1,], aes(x=Topics, y=Score), stat="Identity", col="red", fill="red", width = 0.7) +
    geom_bar(data=dash.easyjet[c(3,4),], aes(x=Topics, y=Score), stat="Identity", col="green", fill="green", width = 0.7) +
    geom_bar(data=dash.easyjet[c(8,9),], aes(x=Topics, y=Score), stat="Identity", col="purple", fill="purple", width = 0.7) +
    geom_bar(data=dash.easyjet[10,], aes(x=Topics, y=Score), stat="Identity", col="orange", fill="orange", width = 0.7) +  
    geom_bar(data=dash.easyjet[2,], aes(x=Topics, y=Score), stat="Identity", col="cyan", fill="cyan", width = 0.7) +  
    scale_y_discrete(name="Topic popularity", breaks=seq(0,0.05))

# Ryanair
dash.ryanair <- as.data.frame(cbind(topics.names,topics.ryanair[c(17,42,31,49,11,45,46,14,27,37)]))
colnames(dash.ryanair) <- c("Topics","Score")
dash.ryanair$Topics <- as.character(dash.ryanair$Topics)
dash.ryanair$Topics <- factor(dash.ryanair$Topics, levels = unique(dash.ryanair$Topics))

plot.ryanair <- ggplot() + 
  geom_bar(data=dash.ryanair[,], aes(x=Topics, y=Score), stat="Identity", col="grey", fill="grey", width = 0.7) +
  geom_bar(data=dash.ryanair[c(5,6,7),], aes(x=Topics, y=Score), stat="Identity", col="blue", fill="blue", width = 0.7) +
  geom_bar(data=dash.ryanair[1,], aes(x=Topics, y=Score), stat="Identity", col="red", fill="red", width = 0.7) +
  geom_bar(data=dash.ryanair[c(3,4),], aes(x=Topics, y=Score), stat="Identity", col="green", fill="green", width = 0.7) +
  geom_bar(data=dash.ryanair[c(8,9),], aes(x=Topics, y=Score), stat="Identity", col="purple", fill="purple", width = 0.7) +
  geom_bar(data=dash.ryanair[10,], aes(x=Topics, y=Score), stat="Identity", col="orange", fill="orange", width = 0.7) +  
  geom_bar(data=dash.ryanair[2,], aes(x=Topics, y=Score), stat="Identity", col="cyan", fill="cyan", width = 0.7) +  
  scale_y_discrete(name="Topic popularity", breaks=seq(0,0.05))

# Vueling

dash.vueling <- as.data.frame(cbind(topics.names,topics.vueling[c(17,42,31,49,11,45,46,14,27,37)]))
colnames(dash.vueling) <- c("Topics","Score")
dash.vueling$Topics <- as.character(dash.vueling$Topics)
dash.vueling$Topics <- factor(dash.vueling$Topics, levels = unique(dash.vueling$Topics))

plot.vueling <- ggplot() + 
  geom_bar(data=dash.vueling[,], aes(x=Topics, y=Score), stat="Identity", col="grey", fill="grey", width = 0.7) +
  geom_bar(data=dash.vueling[c(5,6,7),], aes(x=Topics, y=Score), stat="Identity", col="blue", fill="blue", width = 0.7) +
  geom_bar(data=dash.vueling[1,], aes(x=Topics, y=Score), stat="Identity", col="red", fill="red", width = 0.7) +
  geom_bar(data=dash.vueling[c(3,4),], aes(x=Topics, y=Score), stat="Identity", col="green", fill="green", width = 0.7) +
  geom_bar(data=dash.vueling[c(8,9),], aes(x=Topics, y=Score), stat="Identity", col="purple", fill="purple", width = 0.7) +
  geom_bar(data=dash.vueling[10,], aes(x=Topics, y=Score), stat="Identity", col="orange", fill="orange", width = 0.7) +  
  geom_bar(data=dash.vueling[2,], aes(x=Topics, y=Score), stat="Identity", col="cyan", fill="cyan", width = 0.7) +  
  scale_y_discrete(name="Topic popularity", breaks=seq(0,0.05))

# Norwegian

dash.norwegian <- as.data.frame(cbind(topics.names,topics.norwegian[c(17,42,31,49,11,45,46,14,27,37)]))
colnames(dash.norwegian) <- c("Topics","Score")
dash.norwegian$Topics <- as.character(dash.norwegian$Topics)
dash.norwegian$Topics <- factor(dash.norwegian$Topics, levels = unique(dash.norwegian$Topics))

plot.norwegian <- ggplot() + 
  geom_bar(data=dash.norwegian[,], aes(x=Topics, y=Score), stat="Identity", col="grey", fill="grey", width = 0.7) +
  geom_bar(data=dash.norwegian[c(5,6,7),], aes(x=Topics, y=Score), stat="Identity", col="blue", fill="blue", width = 0.7) +
  geom_bar(data=dash.norwegian[1,], aes(x=Topics, y=Score), stat="Identity", col="red", fill="red", width = 0.7) +
  geom_bar(data=dash.norwegian[c(3,4),], aes(x=Topics, y=Score), stat="Identity", col="green", fill="green", width = 0.7) +
  geom_bar(data=dash.norwegian[c(8,9),], aes(x=Topics, y=Score), stat="Identity", col="purple", fill="purple", width = 0.7) +
  geom_bar(data=dash.norwegian[10,], aes(x=Topics, y=Score), stat="Identity", col="orange", fill="orange", width = 0.7) +  
  geom_bar(data=dash.norwegian[2,], aes(x=Topics, y=Score), stat="Identity", col="cyan", fill="cyan", width = 0.7) +  
  scale_y_discrete(name="Topic popularity", breaks=seq(0,0.05))

# BA

dash.ba <- as.data.frame(cbind(topics.names,topics.ba[c(17,42,31,49,11,45,46,14,27,37)]))
colnames(dash.ba) <- c("Topics","Score")
dash.ba$Topics <- as.character(dash.ba$Topics)
dash.ba$Topics <- factor(dash.ba$Topics, levels = unique(dash.ba$Topics))

plot.ba <- ggplot() + 
  geom_bar(data=dash.ba[,], aes(x=Topics, y=Score), stat="Identity", col="grey", fill="grey", width = 0.7) +
  geom_bar(data=dash.ba[c(5,6,7),], aes(x=Topics, y=Score), stat="Identity", col="blue", fill="blue", width = 0.7) +
  geom_bar(data=dash.ba[1,], aes(x=Topics, y=Score), stat="Identity", col="red", fill="red", width = 0.7) +
  geom_bar(data=dash.ba[c(3,4),], aes(x=Topics, y=Score), stat="Identity", col="green", fill="green", width = 0.7) +
  geom_bar(data=dash.ba[c(8,9),], aes(x=Topics, y=Score), stat="Identity", col="purple", fill="purple", width = 0.7) +
  geom_bar(data=dash.ba[10,], aes(x=Topics, y=Score), stat="Identity", col="orange", fill="orange", width = 0.7) +  
  geom_bar(data=dash.ba[2,], aes(x=Topics, y=Score), stat="Identity", col="cyan", fill="cyan", width = 0.7) +  
  scale_y_discrete(name="Topic popularity", breaks=seq(0,0.05))


# 3. For each topic, the share of each airline. This is the same information
# as above, but this time from the point of view of the topics. Can provide a
# different & interesting visualisation. Use same colours as # 1