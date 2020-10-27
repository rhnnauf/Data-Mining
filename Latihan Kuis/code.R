cuisines <- read.csv("chefmozcuisine.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

places <- read.csv("geoplaces2.csv", stringsAsFactors = FALSE)

ratings <- read.csv("rating_final.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

#join 2 table kalo beda
merge(cuisines, places, by.x = "placeID", by.y = "placeID")

cuisines_places <- merge(cuisines, places, by = "placeID")

rating_places <- merge(ratings, places, by = "placeID")

table(cuisines_places$Rcuisine,cuisines_places$placeID)

#1
cuisine_distribution <- table(cuisines_places$Rcuisine)
cuisine_distribution > 5
cuisine_distribution <- cuisine_distribution[cuisine_distribution > 5]

percentages <- cuisine_distribution / sum(cuisine_distribution) * 100
percentages <- round(percentages, 3)
percentages_label <- paste(names(cuisine_distribution), paste(percentages, "%", sep=""))
# pemisah bisa pake "\n"
pie(cuisine_distribution, main = "Cuisines Distribution", labels = percentages_label, col = rainbow(length(cuisine_distribution)))

#2
restaurant_cuisine_count <- table(cuisines_places$placeID)

#hist(restaurant_cuisine_count, main = "Cuisines Count Frequency based on Restaunrant", xlab = "Cuisines Count", freq = F)

hist(restaurant_cuisine_count, main = "Cuisines Count Frequency based on Restaunrant", xlab = "Cuisines Count")

#3 
rating_places$average_rating <- rowMeans(rating_places[, c("rating", "food_rating", "service_rating")])

rating_places <- rating_places[rating_places$average_rating > 1.2, ]

#rating_places$state %in% c("s.l.p", "S.L.P", "san luis potosi", "San Luis Potosi", "san luis potos", "slp", "SLP")

rating_places[rating_places$state %in% c("s.l.p", "S.L.P", "san luis potosi", "San Luis Potosi", "san luis potos", "slp", "SLP", "s.l.p.", "S.L.P."), ]$state <- 'slp'

rating_places[rating_places$state %in% c("morelos", "Morelos"), ]$state <- 'morelos'

rating_places[rating_places$state %in% c("tamaulipas", "Tamaulipas"), ]$state <- 'tamaulipas'

rating_state_distribution <- table(rating_places$average_rating, rating_places$state)

barplot_color <- c("red", "blue" , "pink")

barplot(rating_state_distribution, beside = T, main = "Average Ratings Distribution\nbased on the State of Restaurant", xlab = "State", col = barplot_color)

legend("top", rownames(rating_state_distribution), fill = barplot_color)

#====================================================================================

rating_places$average_rating <- (rating_places$rating + rating_places$food_rating + rating_places$service_rating) / 3

rating_places <- rating_places[rating_places$average_rating > 1.2 , ]

rating_places$average_rating <- tolower(rating_places$average_rating)

?ifelse

# Frequent Pattern Analysis
# Data Preprocessing

datasets <- cuisines_places

datasets <- datasets[datasets$franchise == "f", ]

datasets <- datasets[datasets$other_services == "none", ]

datasets <- datasets[datasets$country != "?", ]

datasets$Rcuisine <- gsub("_", " ", datasets$Rcuisine)

transformation <- split(datasets$Rcuisine, datasets$placeID)

transformation <- as(transformation, "transactions")

library(arules)

?split

freq <- apriori(transformation,parameter=list(supp=0.008,target="frequent itemset"))

inspect(freq)

rules <- ruleInduction(freq, confidence = 0.8)

inspect(rules)