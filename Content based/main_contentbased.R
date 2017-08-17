cypherToDataFrame = function(x) {
	return(as.data.frame(t(matrix(unlist(x), nrow=length(unlist(x[1]))))))
}

#IMPORT IN Neo4j
if (!require("RNeo4j")) { install.packages("RNeo4j") }

graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="motorsquare")

user_likes = cypherToList(graph, "MATCH (u:users {id_users: '10206938095543821'})-[:LIKE]->(cars) RETURN cars;")
user_likes = cypherToDataFrame(user_likes)
colnames(user_likes) = c('price', 'name', 'make', 'id_cars', 'tags')
rownames(user_likes) = NULL

user_likes$tags = as.character(user_likes$tags)
user_likes$price = as.numeric(as.character(user_likes$price))

#list of tags of cars liked
tags_unique = ''
tags_all = ''

#creating unique tags
for (i in 1:nrow(user_likes)) {
	tmp_tags = user_likes[i,5]
	tmp_tags = unlist(strsplit(tmp_tags, ' '))
	for (j in 1:length(tmp_tags)) {
		tags_all = paste(tags_all, tmp_tags[j])
		if (length(grep(tmp_tags[j], tags_unique)) == 0) {
			tags_unique = paste(tags_unique, tmp_tags[j])
		}
	}
}
tags_unique = sort(unlist(strsplit(trimws(tags_unique), " ")))
tags_unique_txt = do.call(paste, as.list(tags_unique))

#creating table tags_count
library(stringr)
tags_count = data.frame(matrix(ncol=length(tags_unique)))
colnames(tags_count) = tags_unique
for (i in 1:length(tags_unique)) {
	tags_count[1,i] = str_count(tags_all, tags_unique[i])
}
tags_count = sort(tags_count[1,], decreasing=T)

#search first n most counted tags
n = 0
if (length(tags_count) > 9) {
	n = 9
} else {
	n = length(tags_count)
}

tags_most_counted = colnames(tags_count[1, c(1:n)])
tags_most_counted = sort(unlist(strsplit(trimws(tags_most_counted), " ")))
tags_most_counted = do.call(paste, as.list(tags_most_counted))

#search all cars that have ALL same tags of tags_most_counted
query = 'MATCH (c:cars) WHERE ALL (x IN split(c.tags, " ") WHERE x in split({param_name}, " ")) RETURN c'
result_cars = cypherToList(graph, query, list(param_name=tags_unique_txt))
result_cars = cypherToDataFrame(result_cars)
colnames(result_cars) = c('price', 'name', 'make', 'id_cars', 'tags')
result_cars$price = as.numeric(as.character(result_cars$price))

#search for price
price = round(mean(user_likes$price))
price_max = price + 3000
result_cars = subset(result_cars, result_cars[, 1] < price_max)

#remove cars liked by user
require("sqldf")
result_cars = sqldf('SELECT * FROM result_cars EXCEPT SELECT * FROM user_likes')