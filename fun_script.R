## function script ## 
'%ni%' <- Negate('%in%')
between <- dplyr::between
select <- dplyr::select
count <- plyr::count
summarise <- dplyr::summarise
rename <- dplyr::rename
TeX <- latex2exp::TeX

rep.row <- function(x,n){matrix(rep(x,each=n),nrow=n)}
rep.col <- function(x,n){matrix(rep(x,each=n),ncol=n, byrow=TRUE)}

as_numeric <- function(var){
    ## convert factor variable to numeric variable
    as.numeric(levels(var)[var])
}

get_stat <- function(x, q_list=c(0.025, 0.5, 0.975)){
    ## Return summary statistics
    #    @q_list: vector of quantiles to calculate;
    c(summary(x), quantile(x, q_list, na.rm=TRUE))
}

weighted_mean <- function(x, w, ..., na.rm = FALSE){
    if(na.rm){
        df_omit <- na.omit(data.frame(x, w))
        return(weighted.mean(df_omit$x, df_omit$w, ...))
    } 
    weighted.mean(x, w, ...)
}

# addPval(coef_df[,"statistics"], nrow(X)-ncol(X))
addPval <- function(x, df){
    pt(-abs(x), df=df)*2
}

addPval.symbol <- function(x){
    ## Add significance symbols.
    ##      *** (1%), ** (5%), 
    ##      * (10%), no symbol if p > 10%.
    ## @param x: a numeric vector of p-values
    cutpoints <- c(0, 0.01, 0.05, 0.1, 1)
    symbols <- c("***", "**", "*", " ")
    cut(x, breaks=cutpoints, labels=symbols)
}


fill_na_obs <- function(df, regressor_v=NULL, id=c("iso", "year")){
    ## Check if there are missing values, 
    ## and fill the row with NA if existing missing values;
    ## 
    if (!is.null(regressor_v)) {
        # if regressor variable list is pre-decided,
        # we remain only the variable to be used
        select_col <- regressor_v
    } else{
        # if didn't specify regressor_v, use all columns 
        select_col <- colnames(df)
    }
    df[!!rowSums(is.na(df[select_col])), which(colnames(df) %ni% c(id, "ISO_N3", "cntry.name")) ] <- NA   
    df <- df %>% select(id, everything())
    return (df)
}

predict_response_interact <- function(beta_hat){
    ## Create response surface:
    ##    beta1*tmp + beta2*tmp2 + beta3*pre + beta4*pre2
    ##        + beta5*tmp_pre + beta6*tmp2_pre + beta7*pre2_tmp
    ##        + beta8*tmp2_pre2
    plot_df <- tibble(tmp = rep.col(tmp_vec,length(tmp_vec) ) %>% as.vector(),
                      tmp2 = tmp^2,
                      pre = rep.row(pre_vec, length(tmp_vec)) %>% as.vector(),
                      pre2 = pre^2,
                      tmp_pre = tmp*pre,
                      tmp2_pre = tmp2*pre, 
                      pre2_tmp = pre2*tmp,
                      tmp2_pre2 = tmp2*pre2
    )
    z <- as.matrix(plot_df) %*% matrix(beta_hat, ncol=1)
    plot_df <- plot_df %>% mutate(z = as.vector(z) )
    return (plot_df)
}

## format outputs ## ===========================================================
tidy_coeftest <- function(coef_test, nrow=10){
    tible <- broom::tidy(coef_test) %>% head(nrow)
    tible["pval.symbol"] <- addPval.symbol(tible$p.value)
    return (tible)
}

format_reg_tbl <- function(tab){
    ## format regression table to estimates up and SE down
    tab %>% 
        drop_na() %>% 
        gather("key", "value", -term) %>% 
        mutate(term=factor(term, levels=c("tmp", "tmp2", "pre", "pre2",
                                          "tmp_pre", "tmp2_pre", 
                                          "pre2_tmp", "tmp2_pre2", econ_control_var))) %>% 
        arrange(term)
}
    
get_latex_tible <- function(coef_test, nrow=4){
    ## convert regression result to latex table
    tible <- broom::tidy(coef_test) %>% head(nrow)
    tible["pval.symbol"] <- addPval.symbol(tible$p.value)
    xtab <- xtable(tible, digits = 5)
    hline <- c(-1,0,nrow(xtab))
    htype <- c("\\toprule ", "\\midrule ", "\\bottomrule ")
    print(xtab, add.to.row = list(pos = as.list(hline), command = htype),
          hline.after = NULL, include.rownames=FALSE)
}


## plotting functions ## =======================================================
plot_png <- function(p, fn, width=5.5, height=4, ppi=300){
    ## save to .png 
    png(fn, width=width*ppi, height=height*ppi, res=ppi)
    print (p)
    dev.off()
}

library(ggplot2)
base_tmp <- ggplot() +
    xlim(0, 30) 
base_pre <- ggplot() +
    xlim(0, 3)

show_palette <- function(colors) {
    n <- length(colors)
    image(1:n, 1, as.matrix(1:n), col = colors, 
          xlab = "", ylab = "", xaxt = "n", 
          yaxt = "n", bty = "n")
}


plot_impact <- function(impact_df, var, title, pattern=NULL){
    ## Plot maps of historical impacts
    #   @impact_df: data.frame of impacts;
    #   @var: variable to fill the map, i.e., `impacts`;
    #   @title: figure title;
    #   @pattern: string variable, add stripes if `pattern` is specified; 
    world.map <- ne_countries(scale = "medium", returnclass = "sf")
    world.map <- world.map %>% left_join(impact_df, by=c("iso_a3"="ISO_C3"))
    world.map
    
    cold <- colorRampPalette(c("#000033", "#20208F", "#7AACED", "white"))(7) # from -7
    warm <- c("#FFD4D4", "#FFB2B2", "#FF9090", "#FF6D6D", "#FF4B4B", "#FF2A2A")
    myColors <- c(cold, warm)
    
    ## create base plot ##
    p_map <- ggplot(data = world.map %>% filter(continent!="Antarctica") ) +
        geom_sf(aes_string(fill=var), colour='gray50', lwd=0.3 ) +
        scale_fill_stepsn(limits = c(legend_low, legend_high), 
                          breaks = c(legend_low-c, seq(legend_low+step, legend_high-step, step), legend_high+c ), 
                          labels = function(x) {sprintf("%.1f", x*100)},
                          show.limits = TRUE, 
                          right = FALSE, # include right bin, (low, up]
                          colors = myColors,
                          name = TeX(unit)
        ) +
        labs(title = title) 
    
    if (!is.null(pattern)){
        ## if providing `pattern` variable, add stipes ##
        p_map <- p_map + 
            geom_sf_pattern(data=world.map %>% filter(sig=="Nonsig"), aes_string(pattern=pattern, fill=var), 
                            pattern_size = 0.1, 
                            pattern_color="#B6B8B9", # stripes outline
                            pattern_fill="#B6B8B9", # stripes fill
                            color='gray50',
                            # color='red', # country outline color
                            linewidth=0.2, # country outline width
                            show.legend = TRUE
            ) + # significance strip
            scale_pattern_manual(values = c("Nonsig"="stripe", "Sig"="none")) + # specify specific patterns
            guides(pattern = guide_legend(title=element_blank()) )
    }
    
    ## specify general themes ##
    p_map <- p_map +
        coord_sf(datum = NA) +
        theme_minimal() +
        theme(plot.title = element_text(hjust=0.1, size = rel(0.8)),
              legend.title = element_text(hjust=0.85)
              )
    return (p_map)
}


## for GCM projections ## ======================================================
agg_cntry <- function(x, id, w=1) {
    ## Calculate weighted averages grouped by `id`, weighted by `w`
    result <- by(cbind(x,w), id, function(d) weighted_mean(d[,1], d[,2], na.rm=TRUE))
    as.data.frame(cbind(result))
}









