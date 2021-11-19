#' @importFrom rlang .data
draw_sample_efp <- function(rdist,
                            draws = 10,
                            n_eval = 100,
                            prev_eval = 0.5,
                            n_val = NA,
                            prev_val = NA,
                            rdm = TRUE,
                            threshold = c(0.5, 0.5),
                            method_ext = "basic",
                            method_pred = "mbeta_approx"
                            ){
  ## prep
  stopifnot(inherits(rdist, "SIMPle.dist"))
  G <- SIMPle::groups(rdist)
  m <- SIMPle::vars(rdist)
  stopifnot(G == length(threshold))

  ## draw true parameters (sample) and estimates (sample_pred)
  size <- calc_size(G, n_eval, prev_eval, n_val, prev_val, rdm)
  sample <- SIMPle::draw_sample(rdist, draws)
  pred <- SIMPle::draw_sample_pred(rdist,
                                   sample=sample,
                                   size=size,
                                   method_pred = method_pred,
                                   method_ext = method_ext,
                                   regu=1, count=FALSE)

  ## transform samples
  par <- lapply(1:G, function(g){
    sample[[g]]-threshold[g]
  }) %>%
    SIMPle::convert_sample(3, min) %>% {.data[[1]]}
  emp <- lapply(1:G, function(g){
    (pred[[g]]-threshold[g])/sqrt(pred[[g]]*(1-pred[[g]])/size[g])
  }) %>%
    SIMPle::convert_sample(3, min) %>% {.data[[1]]}

  ## calculate expeted final performance (efp)
  lapply(1:draws, function(i){
    sapply(1:m, function(s){
      final_selection(par[i, 1:s], emp[i, 1:s])
    })
  }) %>%
    do.call(rbind, .data) %>%
    return()
}

final_selection <- function(par, emp){
  stopifnot(length(par) == length(emp))
  par[min(which(emp==max(emp)))]
}

calc_size <- function(G, n_eval, prev_eval, n_val, prev_val, rdm=TRUE){
  if(G==1){
    size <- n_eval
  }
  if(G==2){
    size <- numeric(2)
    if(!is.numeric(prev_eval)){
      stopifnot(is.numeric(n_val) & is.numeric(prev_val))
      prev_eval <- stats::rbeta(1, n_val*prev_val, n_val*(1-prev_val))
    }
    if(rdm){
      size[1] <- stats::rbinom(1, n_eval, prev_eval)
    }
    if(!rdm){
      size[1] <- round(prev_eval*n_eval)
    }
    size[2] <- n_eval - size[1]
  }
  return(size)
}

optimize_efp <- function(rdist,
                         n_eval = 100,
                         prev_eval = 0.5,
                         n_val = NA,
                         prev_val = NA,
                         rdm = TRUE,
                         threshold = c(0.5, 0.5),
                         batch_size = 10,
                         max_iter = 50,
                         target_tol = 1e-3,
                         return_simvals=FALSE,
                         method_ext = "basic",
                         method_pred = "mbeta_approx",
                         steady_plot = FALSE,
                         save_plot = FALSE,
                         info_plot = FALSE,
                         ylim_plot = NULL
                         ){
  ## prep:
  m <- SIMPle::vars(rdist)

  ## init:
  run <- TRUE
  act <- 1:m
  simvals <- NULL
  iter = 1
  sel_opt <- 1
  efp_opt <- 0.5
  k <- 1
  time <-  Sys.time()

  ## run iterative simulation:
  while(iter <= max_iter & run){
    efp_old <- efp_opt

    simvals <- rbind(simvals,
                     draw_sample_efp(rdist, draws = batch_size,
                       n_eval = n_eval, prev_eval = prev_eval,
                       n_val = n_val, prev_val = prev_val,
                       rdm = rdm, threshold = threshold,
                       method_ext=method_ext, method_pred = method_pred)
                     )

    efp_est <- colMeans(simvals, na.rm=TRUE)
    efp_opt <- max(efp_est)
    sel_opt <- min(which(efp_est == efp_opt))

    efp_cu <- efp_opt - k*sqrt(stats::var(simvals[, sel_opt], na.rm=TRUE)/sum(!is.na(simvals[, sel_opt])))
    delta <- efp_opt - efp_cu
    act <- which(efp_est >= efp_cu)
    act <- min(act):max(act)
    sel_few <- min(act)
    efp_few <- efp_est[sel_few]

    iter <- iter+1
    run <- abs(delta) > target_tol

    ## plotting:
    if(steady_plot | save_plot){
      opt_efp_plot()
    }
  }

  ## output:
  if(return_simvals){return(simvals)}
  list(
    one = list(sel = 1,
               val = efp_est[1]),
    few = list(sel = sel_few,
               val = efp_few),
    opt = list(sel = sel_opt,
               val = efp_opt),
    max = list(sel = m,
               val = efp_est[m])
  )
}

opt_efp_plot <- function(e = parent.frame()){

  if(e$iter==1){graphics::plot.new()}
  if(e$save_plot){
    grDevices::png(paste0("opt_efp_", e$iter ,".png"), width = 1280, height = 720, res = 108)
  }

  m <- e$m
  cex_text <- 1.25

  if(!is.null(e$ylim_plot)){
    yl <- e$ylim_plot[1]; yu <- e$ylim_plot[2]
  }else{
    yl <- e$efp_est[1]-0.01; yu <- e$efp_opt+0.02
  }

  plot(1:m, e$efp_est, ylim=c(yl, yu),
       ylab=bquote(crayon::bold("EFP(S)")), xlab=bquote(crayon::bold("S")),
       pch=18, cex=1.5, col="darkorange")

  graphics::abline(h=e$efp_opt, col="cyan", lwd=3, lty=5)
  graphics::abline(h=e$efp_cu, col="darkorange", lwd=3, lty=5)

  graphics::lines(1:m, e$efp_est[1:m], lwd=3, col="darkorange")
  graphics::lines(e$act, e$efp_est[e$act], lwd=3, col="cyan")

  graphics::points(1:m, e$efp_est[1:m], pch=18, cex=1.5, col="darkorange")
  graphics::points(e$act, e$efp_est[e$act], pch=18, cex=1.5, col="cyan")

  graphics::points(1, e$efp_est[1], pch=18, cex=2, col="red")
  graphics::points(e$sel_few, e$efp_few, pch=18, cex=2, col="green")
  graphics::points(e$sel_opt, e$efp_opt, pch=18, cex=2, col="magenta")

  yt <- (yu+e$efp_opt)/2
  graphics::text(round(m/6*1), yt, labels=paste0("EFP[1]= ", round(e$efp_est[1], 4)*100, "%"), col="red", cex=cex_text)
  graphics::text(round(m/6*3), yt, labels=paste0("EFP[", e$sel_few, "]= ", round(e$efp_few, 4)*100, "%"), col="green", cex=cex_text)
  graphics::text(round(m/6*5), yt, labels=paste0("EFP[", e$sel_opt, "]= ", round(e$efp_opt, 4)*100, "%"), col="magenta", cex=cex_text)

  if(e$info_plot){
    graphics::text(round(m/6*c(2,3,4,5)), yl+0.005,
                   labels=c(paste0("batch= ", e$iter),
                            paste0("nsim= ", e$iter*e$batch_size),
                            paste0("time= ", round(difftime(Sys.time(), e$time, units="secs"), 0)),
                            paste0("delta= ", round(e$delta, 5))), cex=cex_text)
  }

  if(e$save_plot){
    grDevices::dev.off()
  }
}



