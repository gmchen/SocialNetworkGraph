require(Rfacebook)
require(igraph)

# First, we need to set up a Facebook app as described in 
# http://thinktostart.wordpress.com/2013/11/19/analyzing-facebook-with-r/
# Create an authentication token as in the next two lines (with the relevant app id and app secret), and save this R object for later use.
# fb_oauth <- fbOAuth(app_id="", app_secret="")
# save(fb_oauth, file="fb_oauth")

writeLines("Loading authentication token...")
load("fb_oauth")

writeLines("Getting friend data...")

me <- getUsers(token=fb_oauth, users='me', private_info=TRUE)
my_friends <- getFriends(token=fb_oauth, simplify=FALSE)

my_friends_info <- getUsers(c(my_friends$id, me$id), token=fb_oauth, private_info=TRUE)

writeLines("Getting adjacency matrix...")
my_network <- getNetwork(fb_oauth, format="adj.matrix")

# Remove singletons for aesthetic purposes
# (as implemented by http://blog.revolutionanalytics.com/2013/11/how-to-analyze-you-facebook-friends-network-with-r.html)

singletons <- rowSums(my_network) == 0

my_graph <- graph.adjacency(my_network[!singletons, !singletons])

# add self to the graph
my_graph <- add.vertices(my_graph,1)
V(my_graph)$name[length(V(my_graph))] <- me$name
for (i in seq(1,length(V(my_graph)))) {
	my_graph <- add.edges(my_graph, c(i,length(V(my_graph))))
}

# Add graph attributes

for (colname in colnames(my_friends_info)) {
	if(colname=="name") {
		next;
	}
	current_attributes = character()
	for (name in V(my_graph)$name) {
		val <- my_friends_info[my_friends_info$name==name, colnames(my_friends_info)==colname]
		if(is.vector(val)) {
			current_attributes <- append(current_attributes, val)
		}
		else {
			current_attributes <- append(current_attributes, NA)
		}
	}
	my_graph <- set.vertex.attribute(graph=my_graph, name=colname, value=current_attributes)
}

writeLines("\nPlotting...")

# Plot

self_colors <- V(my_graph)$name==me$name
self_colors[self_colors==FALSE] <- "lightgreen"
self_colors[self_colors=="TRUE"] <- "coral1"

my_layout <- layout.auto(my_graph)

# Make a plot with vertex labels and white background
png(filename="network_withlabels.png",width=3600,height=2400)
plot(my_graph, vertex.size=2,edge.arrow.size=0, edge.curved=TRUE,edge.width=0.5,layout=my_layout,vertex.color=self_colors)
dev.off()

# Make a plot with no vertex labels and a black background
png(filename="network_nolabels.png",width=3600,height=2400)
par(bg='black')
plot(my_graph, vertex.size=2,edge.arrow.size=0, edge.curved=TRUE,edge.width=0.5,layout=my_layout,vertex.label=NA,vertex.color=self_colors)
dev.off()

writeLines("Done!")
