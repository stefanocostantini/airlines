Interesting topics:

- Topics 11,45,46: Delay 
- Topics 14, 27: Luggage
- Topic 17: Booking 
- Topic 31,49: Customer service 
- Topic 37: Seating 
- Topic 42: Boarding/check-in

1745x1145

Tweets collected for the month of December 2015.

Now we have:

1. A sentiment score for each topic (vector: scores) 2. A sentiment
assignment distribution for each tweet (51-column data-frame:
allocation.dist) 3. A sentiment scoring for each tweet (two-column data
frame: tweet.scores)

All this data is saved in results.RData

Things to do:

Using (1), talk about the sentiment to each topic and present a
selection with nicer word clouds (which can be done using the data saved
in lda_model.RData, or, alternatively, the topics data frame, copying
the vector of words in another application )




Using(2), find a way to show how an airline places itself with respect
to the 50 topics and perhaps some nice charts with respect to the
selection of topics listed above

Using(3), calculate a sentiment score for each airline.	