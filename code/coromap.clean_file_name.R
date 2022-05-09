library(here)

current <-c("coromap_dist_java", "coromap_travel_time", "coromap_resultrisk_q0_025",
            "coromap_resultrisk_q_0975",  "coromap_resultrisk_median",
            "coromap_resultrisk_sd", "coromap_resultrisk_lower_q0_025",
            "coromap_resultrisk_lower_q_0975",  "coromap_resultrisk_lower_median",
            "coromap_resultrisk_lower_sd",
            "coromap_resultrisk_upper_q0_025",
            "coromap_resultrisk_upper_q_0975",  "coromap_resultrisk_upper_median",
            "coromap_resultrisk_upper_sd", "excX_1", "excX_2_2",
            "exc_lowerX_1", "exc_lowerX_2_2", "exc_upperX_1", "exc_upperX_2_2")
current<- sapply(current, function(x) paste(here("Spatial", "plots"), "/",
                 x,".pdf", sep=""))
replacement<- c("coromap_distjava", "coromap_travel", 
                 "coromap_quant025", "coromap_quant975", "coromap_median",
                "coromap_sd", "F1_quant025",
                "F1_quant975",  "F1_median",
                "F1_sd","F2_quant025",
                "F2_quant975",  "F2_median",
                "F2_sd",  "coromap_excX_1", "coromap_excX_2_2",
                "F3_excX_1", "F3_excX_2_2", "F4_excX_1", "F4_excX_2_2")
replacement<- sapply(replacement, function(x) paste(here("Spatial", "plots"), "/",
                                                x,".pdf", sep=""))
file.rename(current, replacement)