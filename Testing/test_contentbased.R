cypherToDataFrame = function(x) {
	return(as.data.frame(t(matrix(unlist(x), nrow=length(unlist(x[1]))))))
}
dataFrameDiff <- function(A, B) { 
	f = function(A, B) { A[!duplicated(rbind(B, A))[nrow(B) + 1:nrow(A)], ] }
	df1 = f(A, B) 
	df2 = f(B, A) 
	rbind(df1, df2) 
}

system("Rscript /Users/damianocancemi/Documents/Università/Magistrale/Big\\ data/Progetto\\ motorsquare/Content\\ based/Create\\ Neo4j.R")

require("RNeo4j")
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="motorsquare")

n_chunks = 13

#FETCH ALL USER THAT HAVE MORE THANT 10 likes
#users_list = cypherToList(graph, "MATCH (u:users) WITH u, split(u.likes, ' ') AS list WHERE size(list) > 15 RETURN u.id_users")
users_list = cypherToList(graph, "MATCH (u:users) WITH u, split(u.likes, ' ') AS list, rand() AS number WHERE size(list) > {param} RETURN u.id_users ORDER BY number LIMIT 10", list(param=n_chunks))

mean = 0
for (user_id in users_list) {
	user_id = user_id[[1]]
	user_likes = cypherToList(graph, "MATCH (u:users {id_users: {param_name}})-[:LIKE]->(cars) RETURN cars;", list(param_name=user_id))
	user_likes = cypherToDataFrame(user_likes)
	colnames(user_likes) = c('price', 'name', 'make', 'id_cars', 'tags')
	rownames(user_likes) = NULL
	user_likes$tags = as.character(user_likes$tags)
	user_likes$id_cars = as.character(user_likes$id_cars)
	user_likes$price = as.numeric(as.character(user_likes$price))
	
	#SPLIT INTO CHUNKS
	nr = nrow(user_likes)
	n_chunksEl = nr/n_chunks
	chunks = split(user_likes, rep(1:ceiling(nr/n_chunksEl), each=n_chunksEl, length.out=nr))
	
	accuracy_score = 0
	for (i in 1:n_chunks) {
		validation_set = chunks[i][[1]]
		training_set = dataFrameDiff(user_likes, validation_set)
	
	
		#RECOMMENDATION SYSTEM
		tags_unique = ''
		tags_all = ''
		
		#creating unique tags
		for (i in 1:nrow(training_set)) {
			tmp_tags = training_set[i,5]
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
	}
	accuracy_score = accuracy_score / n_chunks
	mean = mean + accuracy_score
	cat("Accuracy score for id", user_id, ": ", accuracy_score, "\n")
}

mean = mean/length(users_list)
cat("\nMean accuracy score: ", mean)

#log
line = paste(n_chunks, "\t", mean)
write(line, file="/Users/damianocancemi/Documents/Università/Magistrale/Big\ data/Progetto\ motorsquare/Testing/score_contentbased.txt", append=TRUE)