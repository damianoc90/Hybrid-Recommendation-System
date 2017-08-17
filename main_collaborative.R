cypherToDataFrame = function(x) {
	return(as.data.frame(t(matrix(unlist(x), nrow=length(unlist(x[1]))))))
}
getRandomUserID = function() {
	user_id = cypherToList(graph, "MATCH (u:users) WITH u, rand() AS number WHERE toInt(u.likes_count) > 0 RETURN u.id_users ORDER BY number LIMIT 1")
	if (is.null(user_id)) { user_id = getRandomUserID() }
	user_id = user_id[[1]][[1]]
	return(user_id)
}
dataFrameDiff <- function(A, B) { 
	f = function(A, B) { A[!duplicated(rbind(B, A))[nrow(B) + 1:nrow(A)], ] }
	df1 = f(A, B) 
	df2 = f(B, A) 
	rbind(df1, df2) 
}

#system("Rscript /Users/damianocancemi/Documents/Università/Magistrale/Big\\ data/Progetto\\ motorsquare/Collaborative\\ filtering/Create\\ Neo4j.R")

if (!require("RNeo4j")) { install.packages("RNeo4j") }
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="motorsquare")

user_id = getRandomUserID()
#user_id = "10210127410130667"
query = "MATCH (u1:users {id_users: {param_name}})-[s:SIMILARITY]-(u2:users) RETURN u2.id_users, s.similarity AS s ORDER BY s DESC"
similarity = cypherToList(graph, query, list(param_name=user_id))
similarity = cypherToDataFrame(similarity)
similarity = subset(similarity, similarity != 1)
if (nrow(similarity) > 10) { similarity = similarity[1:10,] }
colnames(similarity) = c("id_users", "similarity")

#getting cars
result_cars = as.data.frame(matrix())
for (i in 1:nrow(similarity)) {
	tmp_id = as.character(similarity[i,1])
	
	query = "MATCH (users {id_users: {simil_user}})-[:LIKE]->(c:cars) RETURN c"
	tmp_cars = cypherToList(graph, query, list(simil_user=tmp_id))
	query = "MATCH (u1:users {id_users: {user_id}})-[:LIKE]->(c:cars)<-[:LIKE]-(u2:users {id_users: {simil_user}}) RETURN c"
	common_cars = cypherToList(graph, query, list(user_id=user_id, simil_user=tmp_id))
	
	if (length(common_cars) != 0 && length(tmp_cars) != 0) {
		tmp_cars = cypherToDataFrame(tmp_cars)
		common_cars = cypherToDataFrame(common_cars)
		tmp = dataFrameDiff(common_cars, tmp_cars)
		result_cars = merge(result_cars, tmp, all=T)
	}
}
result_cars = na.omit(result_cars)
colnames(result_cars) = c('price', 'name', 'make', 'id_cars', 'tags')

#search for price
user_likes = cypherToList(graph, "MATCH (u:users {id_users: {param_name}})-[:LIKE]->(cars) RETURN cars.price;", list(param_name=user_id))
user_likes = cypherToDataFrame(user_likes)
colnames(user_likes) = c("price")
user_likes$price = as.numeric(as.character(user_likes$price))

price = round(mean(user_likes$price))
price_max = price + 3000
result_cars = subset(result_cars, price < price_max)

cat("ID:", user_id, "\n")
cat(nrow(user_likes), "likes.\n")
cat(nrow(similarity), "utenti simili.\n")
cat("Range prezzo:", price, "€ e", price_max, "€.\n")
cat(nrow(result_cars), "macchine consigliate.")