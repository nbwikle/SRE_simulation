#This section loads functions required to run the model
source("./r_code/simulation/average_states.R")
source("./r_code/simulation/choro_map.R")
source("./r_code/simulation/environ_similarity.R")
source("./r_code/simulation/loadData.R")
source("./r_code/simulation/many_maps.R")
source("./r_code/simulation/matrix_sampling.R")
source("./r_code/simulation/weight_matrix.R")
source("./r_code/simulation/markov_model.R")

#Example:
#1) Uses loadData("trucks_logs") to load the log shipment by trucks dataset.
loadData("trucks_logs")
test <- createMaps("Michigan", 15, 10, environ = TRUE, r_0 = 0.01, out = "plot")