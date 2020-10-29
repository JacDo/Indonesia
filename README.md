# Modelling ex-ante spatial risks of COVID-19 in Indonesia using network analysis and Bayesian geostatistics
## Goal
We aim to examine the spatial ex-ante risk of COVID-19 in Indonesia, one of the most exposed countries in the South-East Asia region. The goal is to spread the initial risk measured at airport hubs into the entire country. Building a framework of graph statistics of the domestic flight network, a Generalised Additive Model and a geostatistical approach through Integrated Nested Laplace Approximation, we created risk maps displaying the initial exposure of the Indonesian population to COVID-19. Using this methodology, we found that the island of Java and other metropolitan areas on other islands are most at risk initially. 
Our findings provide valuable insights for policy makers as they identify high-risk areas with low statistical uncertainty.
## Software
The analysis was conducted in `R 4.0.2`. Some rasters also used additional functions from ArcMap 10.7.1
## Procedure to replicate results
As the data files are very large, we share them in a separate file: https://www.dropbox.com/sh/ry5hom423kzf6fb/AABbC6RhRjwSk-6PONPu2ll6a?dl=0

In order to replicate the results, use the following order:
1. `coromap.flight.risk` (uses the webscraped data from https://aviationstack.com/): yields the predictions based on the GAM-network-approach
    1.  Use initial risk as obtained by http://rocs.hu-berlin.de/corona/docs/analysis/importrisk/
    1.  Assign it to all routes which contain these airports either as arrival, departure or both as corresponding weights
    1.  Risk: Calculate for each departure airport and the corresponding routes the relative traffic volume and multiply it with the departure weight. If available, add the arrival weight.
    1.  Use log(risk) as dependent variable in GAM
    1.  Potential covariates: Network metrics (Vertex-/Edge-Betweenness, Distance, Transitivity, In-/Out-Strength, Similarity, Connectivity, Eigenvector Centrality) and 
     additional covariates: Geographic Distance between airports, travel volume of routes, relative travel volume of departure airport
    Use GAM on training and testing set
1. `coromap.traveldist.indo`: cleans and prepares rasters as covariates
    1. travel time: Distance to nearest airport, interpolate missing islands
    1. java: Dummy for location on Java/Bali
    1. distance to  Java/Bali: Euclidean distance to the respective islands
    1. traffic density: Obtained from ArcMap
1. `coromap.model_input`: prepares the data for the INLA 
    1. scales the data
    1. fill missing raster values at airport locations
    1. make input data.frame for the analysis
    1. prepare mesh
    1. additional descriptive plots
1. `coromap.inla_analysis`: runs the INLA (ideally run on server)
    1. model selection
    1. run model
1. `coromap.validation`: runs a manual cross-validation (ideally run on server)
    1. run cross-validation to assess fit
    1. basis for a visualization
1. `coromap.inla_results`: conducts diagnostics on the model and provides validation
    1. Diagnostics for INLA
    1. Statistical validation: Plot from previous file
    1. Validation on conceptual level (ODP/PDP vs predicted risk)
Be sure to execute the files in the right order as they build up on each other.
In addition, keep those to files at hand:
1. `impute_function`: Impute missing raster values based on the next closest raster pixel
1. `inla_function`: A simple copy-and-paste of useful discontinued functions (INLA, INLAutils)

