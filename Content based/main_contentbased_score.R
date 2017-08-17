cypherToDataFrame = function(x) {
	return(as.data.frame(t(matrix(unlist(x), nrow=length(unlist(x[1]))))))
}
getRandomUserID = function() {
	user_id = cypherToList(graph, "MATCH (u:users) WITH u, rand() AS number WHERE toInt(u.likes_count) > 0 RETURN u.id_users ORDER BY number LIMIT 1")
	if (is.null(user_id)) { user_id = getRandomUserID() }
	user_id = user_id[[1]][[1]]
	return(user_id)
}
getGroupOfTag = function(x) {
	x = tags_categories[tags_categories["value_name"] == x]
	return (as.integer(x[1]))
}

if (!require("RNeo4j")) { install.packages("RNeo4j") }
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="motorsquare")

#random_user
#user_id = getRandomUserID()
#user_id = "10208355026414109"
#user_id = "595050960673390"
user_id = "10204601312672919"
user_likes = cypherToList(graph, "MATCH (u:users {id_users: {param_name}})-[:LIKE]->(cars) RETURN cars;", list(param_name=user_id))
#print(user_likes)
user_likes = cypherToDataFrame(user_likes)
colnames(user_likes) = c('price', 'name', 'make', 'id_cars', 'tags')
rownames(user_likes) = NULL
user_likes$tags = as.character(user_likes$tags)
user_likes$price = as.numeric(as.character(user_likes$price))

#View(user_likes);
#Sys.sleep("5000")

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

#search all cars that have ALL same tags of tags_unique
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

#creating table tags_count
library(stringr)
tags_count = data.frame(matrix(ncol=length(tags_unique)))
colnames(tags_count) = tags_unique
for (i in 1:length(tags_unique)) { tags_count[1,i] = str_count(tags_all, tags_unique[i]) }
tags_count = sort(tags_count[1,], decreasing=T)

#calculate score
for (i in 1:nrow(result_cars)) {
	score = 0
	tmp_tags = result_cars[i,"tags"]
	tmp_tags = sort(unlist(strsplit(trimws(tmp_tags), " ")))
	for (j in 1:length(tmp_tags)) { score = score + tags_count[tmp_tags[j]] }
	result_cars[i, "score"] = round(score / sum(tags_count), 2)
}
if (nrow(result_cars) > 20) {
	result_cars = subset(result_cars, result_cars[, "score"] > median(result_cars$score))
}
cat("ID:", user_id, "\n")
cat(nrow(user_likes), "likes.\n")
cat(nrow(result_cars), "macchine consigliate.")

#creating tags_count2 (with unique category of tags)
tags_categories = read.csv("/Users/damianocancemi/Documents/Neo4j/default.graphdb/import/item_attr_enum.csv", na.strings=c("","NA"))
tags_categories = tags_categories[, c(1,3)]
tags_bool = matrix(c(F,F,F))
row.names(tags_bool) = c(4,5,6)
tags_count2 = data.frame(matrix(nrow = 1, ncol = 3))
cont = 1
for (i in 1:ncol(tags_count)) {
	group_i = getGroupOfTag(colnames(tags_count[i]))
	bool_i = tags_bool[rownames(tags_bool) == group_i]
	if (!is.na(group_i) && !bool_i) {
		tags_bool[rownames(tags_bool) == group_i] = TRUE
		colnames(tags_count2)[cont] = colnames(tags_count[i])
		tags_count2[colnames(tags_count2) == colnames(tags_count[i])] = tags_count[i]
		cont = cont + 1
	}
}

#calculate score2
for (i in 1:nrow(result_cars)) {
	score = 0
	tmp_tags = result_cars[i,"tags"]
	tmp_tags = sort(unlist(strsplit(trimws(tmp_tags), " ")))
	for (j in 1:length(tmp_tags)) {
		if (tmp_tags[j] %in% colnames(tags_count2)) { score = score + tags_count2[tmp_tags[j]] }
	}
	result_cars[i, "score2"] = round(score / sum(tags_count2), 2)
}
View(result_cars)