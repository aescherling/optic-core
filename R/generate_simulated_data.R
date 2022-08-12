# generate_simulated_data.R -----------------------------------------------
#
# Generate simulated data from a given sim_config object.

generate_simulated_data <- function(sim_config, data_dir, data_format='parquet', gen_seed=NULL) {
 
  # set a seed, if provided
  if (!is.null(gen_seed)) set.seed(gen_seed) 
  
  # get the number of simulations
  n_sim <- nrow(sim_config$simulation_params)
  
  # get the number of iterations
  n_iter <- sim_config$iters
  
  # generate a list to fill with the data from each sim
  all_data <- list()
  
  # loop over the simulations
  for (i in 1:n_sim) {
    
    # generate a list to fill with the data from this sim
    sim_data <- list()
    
    # set up the simulation
    single_sim <- sim_config$setup_single_simulation(i)
      
    # loop over the iterations and collect the simulated data
    for (j in 1:n_iter) {
      
      # generate the simulated data and save it in a data frame
      sampled_sim <- single_sim$method_sample(single_sim)
      sampled_data <- sampled_sim$data
      
      # add the simulation and iteration numbers to the data
      sampled_data$simulation <- i
      sampled_data$iteration <- j
      
      # add the simulation parameters to the data
      for (k in 1:length(single_sim$params)) {
        sampled_data[[names(single_sim$params)[k]]] <- single_sim$params[[k]]
      }
      
      sim_data[[j]] <- sampled_data
      
    }
    
    # write the data to a partitioned parquet dataset
    arrow::write_dataset(dataset = bind_rows(sim_data),
                         path = data_dir,
                         format = data_format,
                         partitioning = 'simulation')
  }
  
}


