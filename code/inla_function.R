################################################################################
################### INLA functions #############################################
################################################################################

##### set-up ###################################################################
library(tidyverse)
library(assertthat)
library(latex2exp)
library(scales)
##### autoplot #################################################################
### copy-and-pasted autoplot.inla and inla.show.hyperspec from Github
### as either the package (INLAutils) and function have
### been discontinued

autoplot.inla <- function(object, which = c(1:3, 5), priors = FALSE, CI = FALSE, ...) {
  assert_that(is.numeric(which))

  if (!all(which %in% 1:5)) {
    stop("which should only contain integers in 1:5")
  }

  # Check that the plots requested are possible
  if (length(object$marginals.fixed) == 0 & 1 %in% which) {
    warning("Plot 1 selected in which, but no fixed effects to plot marginals for.")
    warning("Plot 1 will not be plotted.")
    which <- which[which != 1]
  }

  if (length(object$marginals.hyperpar) == 0 & 2 %in% which) {
    warning("Plot 2 selected in which, but no hyperparameters to plot marginals for.")
    warning("Plot 2 will not be plotted.")
    which <- which[which != 2]
  }


  if (5 %in% which & !object$.args$control.predictor$compute) {
    warning("Plot 5 selected but this can only be plotted if `control.predictor = list(compute = TRUE)` is passed to `inla`.
             \nPlot 5 will not be plotted.")
    which <- which[which != 5]
  }

  if (length(object$summary.random) == 0 & 3 %in% which) {
    warning("Plot 3 selected in which, but no random effects to plot marginals for.")
    warning("Plot 3 will not be plotted.")
    which <- which[which != 3]
  }

  if (length(object$summary.random) == 0 & 4 %in% which) {
    warning("Plot 4 selected in which, but no random effects to plot marginals for.")
    warning("Plot 4 will not be plotted.")
    which <- which[which != 4]
  }


  # Create empty list.
  plots <- list()


  # Plot marginals for fixed effects
  if (1 %in% which) {
    plots$fixed.marginals <- plot_fixed_marginals(object, priors, CI)
  }

  # Plot marginals for hyperparameters
  if (2 %in% which) {
    plots$hyper.marginals <- plot_hyper_marginals(object, CI)
  }

  # plot random effects
  if (3 %in% which) {
    plots$random.effects.line <- plot_random_effects(object, type = "line")
  }

  if (4 %in% which) {
    plots$random.effects.boxplots <- plot_random_effects(object, type = "boxplot")
  }

  # plot predicted data
  if (5 %in% which) {
    plots$marginal.fitted <- plot_marginals_fitted(object)
  }

  print(cowplot::plot_grid(plotlist = plots))
  return(invisible(plots))
}


combine_marginals <- function(x, marg = "fixed") {
  index <- case_when(
    marg == "hyper" ~ "marginals.hyperpar",
    marg == "fixed" ~ "marginals.fixed",
    marg == "random" ~ "summary.random"
  )
  marginal <- x[[index]]
  allMarginals <- lapply(
    seq_len(length(marginal)),
    function(p) data.frame(marginal[[p]], var = names(marginal)[p])
  )
  allMarginals <- do.call(rbind, allMarginals)
  if (marg == "fixed") {
    allMarginals <- allMarginals %>% mutate(var = case_when(
      var == "b0" ~ "intercept",
      var == "log(travel)" ~ "ACCESS",
      var == "log(traffic)" ~ "TRAFFIC",
      var == "log(java_dist)" ~ "DIST.JAVA",
      var == "log(population)" ~ "POP",
    ))
  } else if (marg == "hyper") {
    allMarginals <- allMarginals %>%
      mutate(var = case_when(
        var == "Precision for the Gaussian observations" ~
        "tau[epsilon]",
        var == "Range for s" ~ "rho",
        var == "Stdev for s" ~ "sigma[epsilon]"
      ))
  } else {
    allMarginals
      
  }
  return(allMarginals)
}




##### Individual plot functions for INLA objects ###############################
##### plot_random_effects ######################################################
plot_random_effects <- function(x, type = "line", sigma_ratio = NULL, combo = F) {
  assert_that(type %in% c("line", "boxplot"))
  if (any(sapply(x$summary.random, length) == 0)) stop("No random effects to plot")
  if (type == "line") {
    allSummary <- combine_marginals(x, marg = "random")
    allSummary$ID <- as.numeric(factor(allSummary$ID))
    if (is.null(sigma_ratio) & combo) {
      print("Specify PC priors")
    }
    if (combo) {
      pc_prior <- sigma_ratio %>%
        mutate(
          ratio = paste("\\rho_0:", round(ratio, 2), sep = " "),
          sigma = paste("\\sigma_0:", sprintf("%.2f", sigma), sep = " ")
        ) %>%
        unite("PC-Prior", sigma:ratio, sep = " \\,") %>%
        pull()
      pc_prior <- sapply(pc_prior, function(x) paste("$", x, "$", sep = ""))
      allSummary <- map2_dfr(x, pc_prior, ~ data.frame(combine_marginals(.x,
        marg = "random"
      ),
      pc_prior = .y
      ))
    } else {
      allSummary <- combine_marginals(x = x, marg = "random")
      
    }
    allSummary <- allSummary%>%
      mutate(var = case_when(var =="s"~ "spatial random effects"))
    # plot
    p <- ggplot2::ggplot(allSummary, ggplot2::aes_string(x = "ID", y = "mean")) +
      ggplot2::facet_wrap("var", scales = "free", ncol = 1,
                          labeller = label_wrap_gen(width = 30)) +
      ggplot2::xlab("ID") +
      theme_bw() +
      theme(
         strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"), size=17), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),  axis.title = element_text(size = 14),
        legend.text=element_text(size=14),legend.title =element_text(size=16)
      ) +
      xlab("") +
      ylab("")
    if (combo) {
      p <- p + ggplot2::geom_line(aes(color = pc_prior)) +
        ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "X0.025quant", ymax = "X0.975quant"), alpha = 0.3) +
        labs(color = "PC prior specification") +
        scale_color_discrete(labels = unname(TeX(pc_prior)))
    } else {
      p <- p + ggplot2::geom_line() +
        ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "X0.025quant", ymax = "X0.975quant"), alpha = 0.3)
    }
    #   # geom_line(aes(y = X0.025quant), linetype = 2) +
    #   # geom_line(aes(y = X0.975quant), linetype = 2)
  }

  # if (type == "boxplot") {
  #   allMarginals <- list()
  #   for (i in seq_len(length(x$marginals.random))) {
  #     allMarginals[[i]] <- lapply(
  #       seq_len(length(x$marginals.random[[i]])),
  #       function(p) {
  #         data.frame(x$marginals.random[[i]][[p]],
  #           ID = as.character(p),
  #           var = names(x$marginals.random)[i]
  #         )
  #       }
  #     )
  #     allMarginals[[i]] <- do.call(rbind, allMarginals[[i]])
  #   }
  #   combMarginals <- do.call(rbind, allMarginals)
  #
  #
  #   # Plot
  #   p <- ggplot2::ggplot(combMarginals, ggplot2::aes_string(x = "ID", y = "x")) +
  #     ggplot2::facet_wrap("var", scales = "free", ncol = 1, labeller = label_wrap_gen(width = 10)) +
  #     ggplot2::geom_boxplot(outlier.size = 0.0, outlier.colour = "#FFFFFF00") +
  #     theme_bw() +
  #     theme(
  #        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"), size=17),
  #       axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
  #        axis.title = element_text(size = 14),         legend.text=element_text(size=14),legend.title =element_text(size=16)
  #     )
  # }
  return(p)
}


##### plot_fixed_marginals #####################################################
plot_fixed_marginals <- function(x, priors = FALSE, CI = FALSE,
                                 combo = F, sigma_ratio = NULL) {
  # Combine all marginals
  if (is.null(sigma_ratio) & combo) {
    print("Specify PC priors")
  }
  if (combo) {
    pc_prior <- sigma_ratio %>%
      mutate(
        ratio = paste("\\rho_0:", round(ratio, 2), sep = " "),
        sigma = paste("\\sigma_0:", sprintf("%.2f", sigma), sep = " ")
      ) %>%
      unite("PC-Prior", sigma:ratio, sep = " \\,") %>%
      pull()
    pc_prior <- sapply(pc_prior, function(x) paste("$", x, "$", sep = ""))
    allMarginals <- map2_dfr(x, pc_prior, ~ data.frame(combine_marginals(.x),
      pc_prior = .y
    ))
  } else {
    allMarginals <- combine_marginals(x = x)
  }


  # Plot
  p <- ggplot2::ggplot(allMarginals, ggplot2::aes_string("x", "y")) +
    ggplot2::facet_wrap("var", scales = "free", labeller = label_wrap_gen(width = 10)) +
    theme_bw() +
    theme(
      strip.text.x = element_text(margin = margin(
        .1, 0,
        .1, 0, "cm"
      ), size = 17),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),  axis.title = element_text(size = 14),         legend.text=element_text(size=14),legend.title =element_text(size=16)
    ) +
    xlab("") +
    ylab("")
  if (combo) {
    p <- p + ggplot2::geom_line(aes(color = pc_prior)) +
      labs(color = "PC prior specification") +
      scale_color_discrete(labels = unname(TeX(pc_prior)))
  } else {
    p <- p + ggplot2::geom_line()
  }

  if (priors) {
    # empty dataframe for priors
    priorParams <- extractPriors(x)
    evalPriors <- evalPriors(x, allMarginals, priorParams)

    p <- p + ggplot2::geom_line(data = evalPriors, colour = "#2078C0")
  }


  # If CI is just true, use 95% as default
  if (identical(CI, TRUE)) {
    CI <- 0.95
  }
  # Unless CI is false, loop through fixed effects, find quantiles, find y values for those quantiles
  if (CI) {
    CIs <- list()
    for (i in seq_len(length(x$marginals.fixed))) {
      CIs[[i]] <- data.frame(
        x = c(
          INLA::inla.qmarginal((1 - CI) / 2, x$marginals.fixed[[i]]),
          INLA::inla.qmarginal(1 - (1 - CI) / 2, x$marginals.fixed[[i]])
        ),
        var = names(x$marginals.fixed)[i],
        y0 = 0
      )
      CIs[[i]]$y <- INLA::inla.dmarginal(CIs[[i]]$x, x$marginals.fixed[[i]])
    }
    CIs <- do.call(rbind, CIs)

    # Add to plot
    p <- p + ggplot2::geom_segment(data = CIs, ggplot2::aes_string(x = "x", xend = "x", y = "y", yend = "y0"), colour = "darkgrey")
  }

  return(p)
}

##### plot_hyper_marginals #####################################################
plot_hyper_marginals <- function(x, CI = FALSE, combo = F, sigma_ratio = NULL) {
  if (is.null(sigma_ratio) & combo) {
    print("Specify PC priors")
  }
  if (combo) {
    pc_prior <- sigma_ratio %>%
      mutate(
        ratio = paste("\\rho_0:", round(ratio, 2), sep = " "),
        sigma = paste("\\sigma_0:", sprintf("%.2f", sigma), sep = " ")
      ) %>%
      unite("PC-Prior", sigma:ratio, sep = " \\,") %>%
      pull()
    pc_prior <- sapply(pc_prior, function(x) paste("$", x, "$", sep = ""))
    allMarginals <- map2_dfr(x, pc_prior, ~ data.frame(combine_marginals(.x,
      marg = "hyper"
    ),
    pc_prior = .y
    ))
  } else {
    allMarginals <- combine_marginals(x = x, marg = "hyper")
  }

  # Plot

  p <- ggplot2::ggplot(allMarginals, ggplot2::aes_string("x", "y")) +
    ggplot2::facet_wrap("var",
      scales = "free",
      labeller = label_parsed
    ) +
    theme_bw() +
    theme(
       strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"), size=17),
      axis.text.x = element_text(size = 14, angle = 45),
      axis.text.y = element_text(size = 14),  axis.title = element_text(size = 14),         legend.text=element_text(size=14),legend.title =element_text(size=16)
    ) +
    xlab("") +
    ylab("")+xlim(-1,10)
  if (combo) {
    p <- p + ggplot2::geom_line(aes(color = pc_prior)) +
      labs(color = "PC prior specification") +
      scale_color_discrete(labels = unname(TeX(pc_prior)))
  } else {
    p <- p + ggplot2::geom_line()
  }

  # If CI is just true, use 95% as default
  if (identical(CI, TRUE)) {
    CI <- 0.95
  }
  # Unless CI is false, loop through fixed effects, find quantiles, find y values for those quantiles
  if (CI) {
    CIs <- list()
    for (i in seq_len(length(x$marginals.hyper))) {
      CIs[[i]] <- data.frame(
        x = c(
          INLA::inla.qmarginal((1 - CI) / 2, x$marginals.hyper[[i]]),
          INLA::inla.qmarginal(1 - (1 - CI) / 2, x$marginals.hyper[[i]])
        ),
        var = names(x$marginals.hyper)[i],
        y0 = 0
      )
      CIs[[i]]$y <- INLA::inla.dmarginal(CIs[[i]]$x, x$marginals.hyper[[i]])
    }
    CIs <- do.call(rbind, CIs)

    # Add to plot
    p <- p + ggplot2::geom_segment(data = CIs, ggplot2::aes_string(x = "x", xend = "x", y = "y", yend = "y0"), colour = "darkgrey")
  }

  return(p)
}
##### plot_marginals_fitted ####################################################
plot_marginals_fitted <- function(x) {

  # assert_that(type %in% c('line'))
  # if(type == 'line'){
  d1 <- cbind(ID = 1:NROW(x$summary.linear.predictor), x$summary.linear.predictor[, -7], plot = "Linear Predictor")
  d2 <- cbind(ID = 1:NROW(x$summary.fitted.values), x$summary.fitted.values, plot = "Fitted Values")

  d <- rbind(d1, d2)

  p <- ggplot2::ggplot(d, ggplot2::aes_string(x = "ID", y = "mean")) +
    ggplot2::facet_wrap("plot", scales = "free", ncol = 1, 
                        labeller = label_wrap_gen(width = 10)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes_string(
      ymin = "`0.025quant`",
      ymax = "`0.975quant`"
    ), alpha = 0.3) +
    theme_bw() +
    theme(
       strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"), size=17),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),  axis.title = element_text(size = 14),         legend.text=element_text(size=14),legend.title =element_text(size=16)
    )
  # }

  #  if(type == 'boxplot'){

  #    allMarginals = list()
  #    for(i in seq_len(length(x$marginals.fitted.values))){
  #      allMarginals[[i]] <- lapply(seq_len(length(x$marginals.fitted.values[[i]])),
  #                             function(p) data.frame(x$marginals.fitted.values[[i]][[p]],
  #                                                    ID = as.character(p)))
  #      allMarginals[[i]] <- do.call(rbind, allMarginals[[i]])
  #    }
  #    combMarginals <- do.call(rbind, allMarginals)
  #    names(combMarginals) <- c('x', 'ID')

  #    # Plot
  #    p <- ggplot2::ggplot(combMarginals, aes(ID, y = x)) +
  #           geom_boxplot(outlier.size = 0.01)

  #  }
  return(p)
}

##### extractPriors ############################################################

# Get the priors out of the inla object and into a dta.frame
extractPriors <- function(x) {
  priors <- data.frame(var = x$names.fixed, mean = NA, prec = NA)
  row.names(priors) <- x$names.fixed

  priors["(Intercept)", 2:3] <- c(x$.args$control.fixed$mean.intercept, x$.args$control.fixed$prec.intercept)
  if (priors["(Intercept)", 3] == 0) priors["(Intercept)", 3] <- 1e-9


  # find and combine prior means
  if (length(x$.args$control.fixed$mean) == 1) {
    priors$mean[!priors$var == "(Intercept)"] <- x$.args$control.fixed$mean
  } else if (length(x$.args$control.fixed$mean) == length(x$names.fixed) - 1) {
    priors[names(x$.args$control.fixed$mean), "mean"] <- unlist(x$.args$control.fixed$mean)
  } else {
    priors$mean[!priors$var == "(Intercept)"] <- x$.args$control.fixed$mean$default
    # Take mean values that are not defulat
    nondef <- unlist(x$.args$control.fixed$mean)[names(x$.args$control.fixed$mean) != "default"]
    priors[names(nondef), "mean"] <- x$.args$control.fixed$mean[[1]]
  }

  # find and combine prior prec
  if (length(x$.args$control.fixed$prec) == 1) {
    priors$prec[!priors$var == "(Intercept)"] <- x$.args$control.fixed$prec
  } else if (length(x$.args$control.fixed$prec) == length(x$names.fixed) - 1) {
    priors[names(x$.args$control.fixed$prec), "prec"] <- unlist(x$.args$control.fixed$prec)
  } else {
    priors$prec[!priors$var == "(Intercept)"] <- x$.args$control.fixed$prec$default
    # Take mean values that are not defulat
    nondef <- unlist(x$.args$control.fixed$prec)[names(x$.args$control.fixed$prec) != "default"]
    priors[names(nondef), "mean"] <- x$.args$control.fixed$prec[[1]]
  }

  priors <- priors[complete.cases(priors), ]

  return(priors)
}

##### evalPriors ###############################################################
evalPriors <- function(x, allMarginals, priors) {

  # Make x ranges that match the ranges from the inla object
  x_mins <- by(allMarginals, allMarginals$var, function(x) min(x$x))
  x_maxs <- by(allMarginals, allMarginals$var, function(x) max(x$x))

  x_vals <- sapply(seq_along(x_mins), function(x) seq(x_mins[x], x_maxs[x], length.out = 1000))
  x_vals <- as.vector(x_vals)

  priorsEval <- data.frame(
    x = x_vals,
    var = rep(x$names.fixed, each = 1000),
    mean = rep(priors$mean, each = 1000),
    prec = rep(priors$prec, each = 1000)
  )
  priorsEval$sd <- 1 / sqrt(priorsEval$prec)
  priorsEval$sd[is.infinite(priorsEval$sd)] <- 1e100

  priorsEval$y <- with(priorsEval, dnorm(x, mean, sd))
  return(priorsEval)
}

##### inla.show.hyperspec ######################################################
inla.show.hyperspec <- function(result) {
  stopifnot(any(inherits(result, "inla")))
  tfile <- tempfile()
  capture.output(str(result$all.hyper), file = tfile)
  all.hyper <- readLines(tfile)
  unlink(tfile)

  all.hyper <- gsub("\\.\\.", "  ", all.hyper)
  for (r in c(
    "inla\\.read\\.only",
    "attr\\(", "to\\.theta",
    "from\\.theta"
  )) {
    idx <- grep(r, all.hyper)
    if (length(idx) > 0) {
      all.hyper <- all.hyper[-idx]
    }
  }

  cat(all.hyper, sep = "\n")
  return(invisible())
}
