
# custom functions
# mean and sd default remove NAs
Mean <- function(x) mean(x, na.rm=TRUE)
Median <- function(x) median(x, na.rm=TRUE)
Min <- function(x) min(x, na.rm=TRUE)
#Max <- function(x) max(x, na.rm=TRUE)
SD <- function(x) sd(x, na.rm=TRUE)

# Max Function that Returns NA when vector is full of NAs instead of -Inf
Max = function(x) {
  na.pct = sum(is.na(x))/length(x)
  if (na.pct == 1) {
    return(NA) }
  else {
    return(max(x, na.rm=TRUE))
  }
}

# not in set
`%notin%` <- Negate(`%in%`)

# function to skip lines of a source file 
# (redcap r scripts have always a  rm(list=ls()) & graphics.off()) at the beginning
Source <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

# function to get labels from redcap data into a nice table

redcap_labels_table = function(dat){
  lab = get_label(dat)
  data.frame(variable = names(lab), description=lab, row.names = NULL)
}


# function to plot sig bars with symbols
sigbar = function(x1, y1, ymax, x2, y2, txt, offset=0, cex=1.2, style=0) {
  if (style == 1) {
    offset=0.5
    cex=0.75
  }
  segments(x1, y1, x1, ymax, lwd=0.5, lty=1)
  segments(x1, ymax, x2, ymax, lwd=0.5, lty=1)
  segments(x2, ymax, x2, y2, lwd=0.5, lty=1)
  text((x1+x2)/2,ymax, txt, cex=cex, pos=3, offset=offset)
}

# function to plot sig bars with symbols in horizontal barplots
sigbarh = function(x1, y1, xmax, x2, y2, txt, offset=1, cex=1.2, style=0) {
  if (style == 1) {
    offset=0.5
    cex=0.75
  }
  segments(x1, y1, xmax, y1, lwd=0.5, lty=1)
  segments(xmax, y1, xmax, y2, lwd=0.5, lty=1)
  segments(xmax, y2, x2, y2, lwd=0.5, lty=1)
  text(xmax, (y1+y2)/2, txt, cex=cex, pos=4, offset=offset)
}

# function to add Tukey post-hoc to tbl_summary()
# source : https://stackoverflow.com/questions/70129472/gtsummary-columns-for-all-post-hoc-pairwise-comparisons
add_stat_pairwise <- function(data, variable, by, ...) {
  # calculate pairwise p-values
  pw <- pairwise.t.test(data[[variable]], data[[by]], p.adj = "none")
  
  # convert p-values to list
  index <- 0L
  p.value.list <- list()
  for (i in seq_len(nrow(pw$p.value))) {
    for (j in seq_len(nrow(pw$p.value))) {
      index <- index + 1L
      
      p.value.list[[index]] <- 
        c(pw$p.value[i, j]) %>%
        setNames(glue::glue("**{colnames(pw$p.value)[j]} vs. {rownames(pw$p.value)[i]}**"))
    }
  }
  
  # convert list to data frame
  p.value.list %>% 
    unlist() %>%
    purrr::discard(is.na) %>%
    t() %>%
    as.data.frame() %>%
    # formatting/roundign p-values
    dplyr::mutate(dplyr::across(everything(), style_pvalue))
}

# function to add_n() in tbl_summary by group

# usage:
# tbl_summary(by = something) %>% 
# add_stat(fns = everything() ~ add_by_n) %>%
#   modify_header(starts_with("add_n_stat") ~ "**N**") %>%
#   modify_table_body(
#     ~ .x %>%
#       dplyr::relocate(add_n_stat_1, .before = stat_1) %>%
#       dplyr::relocate(add_n_stat_2, .before = stat_2)
#   ) 
  
add_by_n <- function(data, variable, by, tbl, ...) {
  data %>%
    select(all_of(c(variable, by))) %>%
    dplyr::group_by(.data[[by]]) %>%
    dplyr::summarise_all(~sum(!is.na(.))) %>%
    rlang::set_names(c("by", "variable")) %>%
    dplyr::left_join(
      tbl$df_by %>% select(by, by_col),
      by = "by"
    ) %>%
    mutate(
      by_col = paste0("add_n_", by_col),
      variable = style_number(variable)
    ) %>%
    select(-by) %>%
    tidyr::pivot_wider(names_from = by_col, 
                       values_from = variable)
}


# https://stackoverflow.com/questions/51735481/ggplot2-change-axis-limits-for-each-individual-facet-panel
#
#' Scale individual facet y-axes
#' 
#' 
#' VERY hacky method of imposing facet specific y-axis limits on plots made with facet_wrap
#' Briefly, this function alters an internal function within the ggproto object, a function which is called to find any limits imposed on the axes of the plot. 
#' We wrap that function in a function of our own, one which intercepts the return value and modifies it with the axis limits we've specified the parent call 
#' 
#' I MAKE NO CLAIMS TO THE STABILITY OF THIS FUNCTION
#' 
#'
#' @param plot The ggproto object to be modified
#' @param ylims A list of tuples specifying the y-axis limits of the individual facets of the plot. A NULL value in place of a tuple will indicate that the plot should draw that facet as normal (i.e. no axis modification)
#'
#' @return The original plot, with facet y-axes modified as specified
#' @export
#'
#' @examples 
#' Not intended to be added to a ggproto call list. 
#' This is a standalone function which accepts a ggproto object and modifies it directly, e.g.
#' 
#' YES. GOOD: 
#' ======================================
#' plot = ggplot(data, aes(...)) + 
#'   geom_whatever() + 
#'   geom_thing()
#'   
#' scale_individual_facet_y_axes(plot, ylims)
#' ======================================
#' 
#' NO. BAD:
#' ======================================
#' ggplot(data, aes(...)) + 
#'   geom_whatever() + 
#'   geom_thing() + 
#'   scale_individual_facet_y_axes(ylims)
#' ======================================
#' 
scale_inidividual_facet_y_axes = function(plot, ylims) {
  init_scales_orig = plot$facet$init_scales
  
  init_scales_new = function(...) {
    r = init_scales_orig(...)
    # Extract the Y Scale Limits
    y = r$y
    # If this is not the y axis, then return the original values
    if(is.null(y)) return(r)
    # If these are the y axis limits, then we iterate over them, replacing them as specified by our ylims parameter
    for (i in seq(1, length(y))) {
      ylim = ylims[[i]]
      if(!is.null(ylim)) {
        y[[i]]$limits = ylim
      }
    }
    # Now we reattach the modified Y axis limit list to the original return object
    r$y = y
    return(r)
  }
  
  plot$facet$init_scales = init_scales_new
  
  return(plot)
}

# ANOVA in R using summary data
# https://stackoverflow.com/questions/26170002/anova-in-r-using-summary-data
# https://github.com/cran/rpsychi/blob/master/R/ind.oneway.second.R

ind.oneway.second <- function(m, sd, n, unbiased=TRUE, contr=NULL, sig.level=.05, digits=3){
  
  ##if orthogonal
  if(length(n)==1){
    n <- rep(n, length(m))
  }
  
  #if biased standard deviation
  if(unbiased==FALSE){
    sd <- ssd2sd(n,sd)
  }
  
  
  ##(a) anova table
  k  <- length(m)           #number of groups
  Xg  <- sum(n*m)/sum(n)
  
  dfb <- k - 1              #degree of freedom
  dfw   <- sum(n) - k       #degree of freedom
  
  MSb <- sum(n * (m - Xg)^2)/(k-1)  #MS between
  MSw <-  sum((n-1)*sd^2)/dfw       #MS within
  SSb <- dfb * MSb
  SSw <- dfw * MSw
  SSt <- SSb + SSw
  
  f.value <- MSb/MSw                #f value
  
  
  anova.table  <- data.frame(matrix(NA,ncol=4, nrow=3))
  rownames(anova.table) <- c("Between (A)", "Within", "Total")
  colnames(anova.table) <- c("SS", "df", "MS", "F")
  anova.table$SS <- c(SSb, SSw,SSt)
  anova.table$df <- c(dfb, dfw, dfb+dfw)
  anova.table$MS <- c(MSb,MSw,NA)
  anova.table$F  <- c(f.value, NA,NA)
  class(anova.table) <- c("anova", "data.frame")
  anova.table <- round(anova.table, digits)
  
  
  
  ##(b) omnibus effect size eta and omega squared
  
  #eta square
  etasq <- SSb / SSt
  delta.lower <- delta.upper <- numeric(length(etasq))
  delta.lower <- try(FNONCT(f.value, dfb, dfw, prob=1-sig.level/2), silent=TRUE)
  delta.upper <- try(FNONCT(f.value, dfb, dfw, prob=sig.level/2), silent=TRUE)
  if(is.character(delta.lower)){
    delta.lower <- 0
  }
  
  etasq.lower <- delta.lower / (delta.lower + dfb + dfw + 1)
  etasq.upper <- delta.upper / (delta.upper + dfb + dfw + 1)
  
  
  #omega square
  omegasq <- (SSb - dfb * MSw)/(SSt + MSw)
  sosb_L  <- SSt * etasq.lower
  msw_L   <- (SSt - sosb_L)/dfw
  omegasq.lower <- (sosb_L - (dfb*msw_L))/(SSt+msw_L)
  
  sosb_U  <- SSt * etasq.upper
  msw_U   <- (SSt - sosb_U)/dfw
  omegasq.upper <- (sosb_U - (dfb*msw_U))/(SSt+msw_U)
  
  omnibus.es <- round(c(etasq=etasq, etasq.lower=etasq.lower, etasq.upper=etasq.upper),
                      digits)
  
  
  ##(c) raw contrasts
  temp  <- combinations(k,2)
  cont1 <- matrix(0, nrow=nrow(temp),ncol=k)
  cont1.lab <- rep(0,nrow(temp))
  
  #in case did not specify contrasts
  for(i in 1:nrow(temp)){
    cont1[i, temp[i,1]] <- 1
    cont1[i, temp[i,2]] <- -1
    cont1.lab[i] <- paste(temp[i,1],"-",temp[i,2], sep="")
    rownames(cont1) <- cont1.lab
  }
  
  
  #in case specify contrasts
  if(!is.null(contr)){
    if(is.vector(contr)){
      cont1 <- t(as.matrix(contr))
    }else{
      cont1 <- contr
    }
  }
  
  
  #F test for contrasts
  psi <-  colSums(t(cont1)  * as.vector(m))                #raw contrasts
  SSpsi <- (psi^2)/colSums(t(cont1^2) / as.vector(n))
  
  nmat <- matrix(n, nrow=nrow(cont1), ncol=length(n), byrow=TRUE)
  psi.std <- sqrt(MSw * rowSums(cont1 ^ 2/nmat))
  
  psi.lower <- psi + psi.std * qt(sig.level/2, dfw)
  psi.upper <- psi + psi.std * qt(sig.level/2, dfw, lower.tail=FALSE)
  
  raw.contrasts <- round(data.frame(mean.diff=psi, lower=psi.lower, upper=psi.upper, std=psi.std), digits)
  rownames(raw.contrasts) <- rownames(cont1)
  
  
  
  ##(d) standardized contrasts
  gpsi <- psi/sqrt(MSw)       #effect size
  gpsi.std <- sqrt(rowSums(cont1 ^ 2/nmat))
  gpsi.lower <- gpsi + gpsi.std * qt(sig.level/2, dfw)
  gpsi.upper <- gpsi + gpsi.std * qt(sig.level/2, dfw, lower.tail=FALSE)
  standardized.contrasts <- round(data.frame(es=gpsi, lower=gpsi.lower, upper=gpsi.upper, std=gpsi.std), digits)
  rownames(standardized.contrasts) <- rownames(cont1)
  
  
  
  ##(e) statistical power
  c.delta <- c(.10, .25, .4)
  criterion.power <- round(power.f(sig.level=sig.level, u=dfb, n=sum(n)/k,delta=c.delta), digits)
  names(criterion.power) <- c("small", "medium", "large")
  
  
  
  ##(e) output
  output <- list(anova.table=anova.table, omnibus.es=omnibus.es, raw.contrasts=raw.contrasts, standardized.contrasts = standardized.contrasts, power=criterion.power)
  return(output)
}

FNONCT <- function(x,df1,df2,prob, interval=c(0,10000), my.tol=0.000001){
  temp <- function(ncp) pf(x,df1,df2,ncp) - prob
  return(uniroot(temp, interval, tol = my.tol)$root)
}

power.f <- function(u, n, delta, sig.level=.05){
  fc <- qf(p=sig.level,df1=u,df2=(u+1)*(n-1),lower.tail=FALSE)
  lamda <- (delta^2)*(n*(u+1))
  v <- (u+1)*(n-1)
  
  z1b <- (sqrt(2*(u+lamda)-((u+2*lamda)/(u+lamda)))-
            sqrt((2*v-1)*((u*fc)/v)))/
    sqrt(((u*fc)/v)+((u+2*lamda)/(u+lamda)))
  output <- pnorm(z1b)
  return(output)
}
