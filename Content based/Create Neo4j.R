options(warn=-1)

checkMF = function(array, tosearch) {
	for (i in 1:length(array)) {
		if (array[i] == tosearch) { return(TRUE) }
	}
	return(FALSE)
}

#manipulate cars
path_cars = "/Users/damianocancemi/Documents/Neo4j/default.graphdb/import/cars_new_models.csv"
cars = read.csv(path_cars, na.strings=c("","NA"))
colnames(cars)[1] = "id_cars"

cars$tags = ""
for (i in 1:nrow(cars)) {
	cars[i,]$tags = apply(cars[i,5:length(cars)],1,paste,collapse=" ")
	cars[i,]$tags = gsub("NA ","",cars[i,]$tags)
	cars[i,]$tags = substr(cars[i,]$tags, 0, nchar(cars[i,]$tags)-1)
	
	#order
	tmp = unlist(strsplit(cars[i,]$tags, " "))
	if (!checkMF(tmp, "M") && !checkMF(tmp, "F")) { tmp[length(tmp) + 1] = "N" }
	tmp = sort(tmp)
	cars[i,]$tags = do.call(paste, as.list(tmp))
}
cars = cars[,-c(6:length(cars)-1)]
write.csv(cars, file="/Users/damianocancemi/Documents/Neo4j/default.graphdb/import/cars.csv", row.names=F)

#manipulate users
path_users = "/Users/damianocancemi/Documents/Neo4j/default.graphdb/import/users_preferences_motorsquare.csv"
users = read.csv(path_users, na.strings=c("","NA"), colClasses=c("character"))
colnames(users)[1] = "id_users"

users$likes = ""
for (i in 1:nrow(users)) {
	users[i,]$likes = apply(users[i, grep("_id$", colnames(users))],1,paste,collapse=" ")
	users[i,]$likes = gsub("NA ","",users[i,]$likes)
	users[i,]$likes = gsub("NA","",users[i,]$likes)
	users[i,]$likes = substr(users[i,]$likes, 0, nchar(users[i,]$likes)-1)
	
	#unique
	list = list(strsplit(users[i,]$likes, " "))[[1]][[1]]
	list = unique(list)
	users[i,]$likes = paste(list, collapse=" ")
}

users = users[,-c(3:length(users)-1)]
colnames(users) = c("id_users", "likes")
write.csv(users, file="/Users/damianocancemi/Documents/Neo4j/default.graphdb/import/users.csv", row.names=F)

#IMPORT IN Neo4j
if (!require("RNeo4j")) { install.packages("RNeo4j") }

graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="motorsquare")
cypher(graph, "MATCH(n) DETACH DELETE n")
cypher(graph, "USING PERIODIC COMMIT LOAD CSV WITH HEADERS FROM 'file:/cars.csv' AS row CREATE (:cars {id_cars: row.id_cars, make: row.make, name: row.name, price: row.price, tags: row.tags});")
cypher(graph, "USING PERIODIC COMMIT LOAD CSV WITH HEADERS FROM 'file:/users.csv' AS row CREATE (:users {id_users: row.id_users, likes: row.likes});")

#creating index
cypher(graph, "CREATE INDEX ON :cars(id_cars);")
cypher(graph, "CREATE INDEX ON :users(id_users);")

#creating relationships
cypher(graph, "MATCH (u:users), (c:cars) WHERE u.likes CONTAINS c.id_cars CREATE (u)-[:LIKE]->(c)")
