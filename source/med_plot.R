

med_plot <- function(df, model){

 plot_mod <- lavaan::sem(model, data = df)
    
semPlot::semPaths(plot_mod,
             'path',  edge.label.cex = 3, 
             layout = matrix(c(1,0, -1, 0, 1, 0), ncol = 2),
             sizeMan = 10, residuals = F)
}




med_plot_info <- function(df, model){
    
    plot_mod <- suppressWarnings(lavaan::sem(model, data = df))
    
    modx <- summary(plot_mod)
    
    ests <- tibble(label = modx$PE$label,
                   est = modx$PE$est)

    a <- ests %>% 
        filter(label == "a") %>% 
        pull(est) %>% 
        round(2)
    b <- ests %>% 
        filter(label == "b") %>% 
        pull(est)%>% 
        round(2)
    c <- ests %>% 
        filter(label == "c") %>% 
        pull(est)%>% 
        round(2)
    ind <- a*b %>% 
        round(2)
    direc <- (a*b)+ c %>% 
        round(2)
    
    lay <- matrix(c(1,0, -1, 0, 1, 0), ncol = 2) 
    lab = paste("Indirect effect = a * b\n", a, "*", b, "≈", ind)
    lab_2 = paste("Direct effect = a + b + c\n", a, "*", b, "+", c, "≈", direc)

    suppressWarnings(semPlot::semPaths(object = plot_mod, what = 'est', 
                  layout = lay,
                  edge.label.cex = 3, 
                  sizeMan = 10, residuals = F, fade = F) + 
    text(x = c(-.8,.8), y = rep(.9,2), labels = c(lab, lab_2), cex = 1.5) )

return()

}
