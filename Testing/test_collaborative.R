rm(list=ls())
options(warn=-1)

cypherToDataFrame = function(x) {
	return(as.data.frame(t(matrix(unlist(x), nrow=length(unlist(x[1]))))))
}
dataFrameDiff <- function(A, B) { 
	f = function(A, B) { A[!duplicated(rbind(B, A))[nrow(B) + 1:nrow(A)], ] }
	df1 = f(A, B) 
	df2 = f(B, A) 
	rbind(df1, df2) 
}
resetNeo4J = function() {
	cypher(graph, "MATCH(n) DETACH DELETE n")
	cypher(graph, "USING PERIODIC COMMIT LOAD CSV WITH HEADERS FROM 'file:/cars.csv' AS row CREATE (:cars {id_cars: row.id_cars, make: row.make, name: row.name, price: row.price, tags: row.tags});")
	cypher(graph, "USING PERIODIC COMMIT LOAD CSV WITH HEADERS FROM 'file:/users.csv' AS row CREATE (:users {id_users: row.id_users, likes: row.likes, likes_count: row.likes_count});")
	
	#creating index
	cypher(graph, "CREATE INDEX ON :cars(id_cars);")
	cypher(graph, "CREATE INDEX ON :users(id_users);")
	
	#creating relationships
	cypher(graph, "MATCH (u:users), (c:cars) WHERE u.likes CONTAINS c.id_cars CREATE (u)-[:LIKE]->(c)")
	
	#creating relationships beetween users
	query = "
		MATCH (u1:users)-[x:LIKE]->(c:cars)<-[y:LIKE]-(u2:users)
		WITH count(c) AS commonCars, u1, u2
		MERGE (u1)-[s:SIMILARITY]-(u2)
		SET s.similarity = round(100*commonCars*1.0 / (toInt(u1.likes_count) + toInt(u2.likes_count) - commonCars))/100
	"
	cypherToList(graph, query)
}

system("Rscript /Users/damianocancemi/Documents/Università/Magistrale/Big\\ data/Progetto\\ motorsquare/Collaborative\\ filtering/Create\\ Neo4j.R")

require("RNeo4j")
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="motorsquare")

n_chunks = 12

#FETCH ALL USER THAT HAVE MORE THANT 10 likes
#users_list = cypherToList(graph, "MATCH (u:users) WITH u, split(u.likes, ' ') AS list WHERE size(list) > {min} AND size(list) < {max} RETURN u.id_users", list(min=min_like, max=max_like))
users_list = cypherToList(graph, "MATCH (u:users) WITH u, rand() AS number WHERE toInt(u.likes_count) > {param} RETURN u.id_users ORDER BY number LIMIT 10", list(param=n_chunks))
mean = 0
for (user_id in users_list) {
	user_id = user_id[[1]]
		
	user_likes = cypherToList(graph, "MATCH (u:users {id_users: {param_name}})-[:LIKE]->(cars) RETURN cars;", list(param_name=user_id))
	user_likes = cypherToDataFrame(user_likes)
	
	cat(paste0("> User_id: ", user_id, " (likes:", nrow(user_likes), ")"))
	
	colnames(user_likes) = c('price', 'name', 'make', 'id_cars', 'tags')
	rownames(user_likes) = NULL
	user_likes$tags = as.character(user_likes$tags)
	user_likes$id_cars = as.character(user_likes$id_cars)
	user_likes$price = as.numeric(as.character(user_likes$price))
	original_likes = paste(user_likes$id_cars, collapse=" ")
	
	#SPLIT INTO CHUNKS
	nr = nrow(user_likes)
	n_chunksEl = nr/n_chunks
	chunks = split(user_likes, rep(1:ceiling(nr/n_chunksEl), each=n_chunksEl, length.out=nr))
	
	accuracy_score = 0
	for (i in 1:n_chunks) {
		cat("\n", i, "- cross validation")
		validation_set = chunks[i][[1]]
		training_set = dataFrameDiff(user_likes, validation_set)
		
		#update like of user_id
		training_likes = paste(training_set$id_cars, collapse=" ")
		query = "MATCH (u:users {id_users: {param1}}) SET u.likes={param2}, u.likes_count={param3}"
		cypher(graph, query, list(param1=user_id, param2=training_likes, param3=nrow(training_set)))
		
		#RECOMMENDATION SYSTEM
		#creating relationships
		cypher(graph, "MATCH (u:users)-[rel:LIKE]->(c:cars) DELETE rel")
		cypher(graph, "MATCH (u:users)-[rel:SIMILARITY]-(u:users) DELETE rel")
		cypher(graph, "MATCH (u:users), (c:cars) WHERE u.likes CONTAINS c.id_cars CREATE (u)-[:LIKE]->(c)")
		query = "
			MATCH (u1:users)-[x:LIKE]->(c:cars)<-[y:LIKE]-(u2:users)
			WITH count(c) AS commonCars, u1, u2
			MERGE (u1)-[s:SIMILARITY]-(u2)
			SET s.similarity = round(100*commonCars*1.0 / (toInt(u1.likes_count) + toInt(u2.likes_count) - commonCars))/100
		"
		cypherToList(graph, query)
		
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
		price = round(mean(training_set$price))
		price_max = price + 3000
		result_cars = subset(result_cars, result_cars[, 1] < price_max)
		
		#remove cars liked by user
		require("sqldf")
		result_cars = sqldf('SELECT * FROM result_cars EXCEPT SELECT * FROM training_set')
		
		
		#CHECK HIT/MISS
		hit = intersect(result_cars$id_cars, validation_set$id_cars)
		local_accuracy = length(hit) / nrow(validation_set)
		accuracy_score = accuracy_score + local_accuracy
		
		resetNeo4J()
	}
	accuracy_score = accuracy_score / n_chunks
	mean = mean + accuracy_score
	cat("\nAccuracy score for id", user_id, ": ", accuracy_score, "\n")
}

mean = mean/length(users_list)
cat("\nMean accuracy score: ", mean)

#log
line = paste(n_chunks, "\t", mean)
write(line, file="/Users/damianocancemi/Documents/Università/Magistrale/Big\ data/Progetto\ motorsquare/Testing/score_collaborative.txt", append=TRUE)