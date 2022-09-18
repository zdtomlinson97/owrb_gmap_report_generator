plotly_piper <- function() {
  x_off = 30
  y_off = 30
  
  library(plotly)
  grid1p1 <<- data.frame(x0 = c(20,40,60,80), x1= c(10,20,30,40),y0 = c(0,0,0,0), y1 = c(17.3206,34.6412,51.9618, 69.2824))
  grid1p2 <<- data.frame(x0 = c(20,40,60,80), x1= c(60,70,80,90),y0 = c(0,0,0,0), y1 = c(69.2824, 51.9618,34.6412,17.3206))
  grid1p3 <<- data.frame(x0 = c(10,20,30,40), x1= c(90,80,70,60),y0 = c(17.3206,34.6412,51.9618, 69.2824), y1 = c(17.3206,34.6412,51.9618, 69.2824))
  grid2p1 <<- grid1p1
  grid2p1$x0 <- grid2p1$x0+120+x_off
  grid2p1$x1 <- grid2p1$x1+120+x_off
  grid2p2 <<- grid1p2
  grid2p2$x0 <- grid2p2$x0+120+x_off
  grid2p2$x1 <- grid2p2$x1+120+x_off
  grid2p3 <<- grid1p3
  grid2p3$x0 <- grid2p3$x0+120+x_off
  grid2p3$x1 <- grid2p3$x1+120+x_off
  grid3p1 <<- data.frame(x0=c(100,90, 80, 70)+x_off/2,y0=c(34.6412, 51.9618, 69.2824, 86.603)+y_off, x1=c(150, 140, 130, 120)+x_off/2, y1=c(121.2442,138.5648,155.8854,173.2060)+y_off)
  grid3p2 <<- data.frame(x0=c(70, 80, 90, 100)+x_off/2,y0=c(121.2442,138.5648,155.8854,173.2060)+y_off, x1=c(120, 130, 140, 150)+x_off/2, y1=c(34.6412, 51.9618, 69.2824, 86.603)+y_off)
  
  grid_lines = rbind(grid1p1,grid1p2, grid1p3, grid2p1, grid2p2, grid2p3, grid3p1, grid3p2)
  
  outer_triangles = rbind(
    ## left hand ternary plot
    data.frame(x0=0,y0=0, x1=100, y1=0),
    data.frame(x0=0,y0=0, x1=50, y1=86.603), 
    data.frame(x0=50,y0=86.603, x1=100, y1=0), 
    ## right hand ternary plot
    data.frame(x0=120+x_off,y0=0, x1=220+x_off, y1=0),
    data.frame(x0=120+x_off,y0=0, x1=170+x_off, y1=86.603),
    data.frame(x0=170+x_off,y0=86.603, x1=220+x_off, y1=0),
    ## Upper diamond
    data.frame(x0=110+x_off/2,y0=190.5266+y_off, x1=60+x_off/2, y1=103.9236+y_off),
    data.frame(x0=110+x_off/2,y0=190.5266+y_off, x1=160+x_off/2, y1=103.9236+y_off),
    data.frame(x0=110+x_off/2,y0=17.3206+y_off, x1=160+x_off/2, y1=103.9236+y_off),
    data.frame(x0=110+x_off/2,y0=17.3206+y_off, x1=60+x_off/2, y1=103.9236+y_off)
  )
  
  p = plot_ly(type = "scatter", mode = "markers") %>%
    
    add_segments(data = grid_lines, x = ~x0, y = ~y0, xend = ~x1, yend = ~y1, 
        line = list(color = "grey", width = 0.5),showlegend = F, hoverinfo = 'none') %>% 
    add_segments(data = outer_triangles, x = ~x0, y = ~y0, xend = ~x1, yend = ~y1,
        line = list(color = "black", width = 1),showlegend = F, hoverinfo = 'none') %>% 
  
    add_annotations(x = c(20,40,60,80),y = c(-5,-5,-5,-5), text=c(80, 60, 40, 20), showarrow = F) %>%
    add_annotations(x = c(35,25,15,5)-2.5,y = grid1p2$y1+2.5, text=c(80, 60, 40, 20), showarrow = F) %>%
    add_annotations(x = c(95,85,75,65)+2.5,y = grid1p3$y1+2.5, text=c(80, 60, 40, 20), showarrow = F) %>%
    add_annotations(x = c(155,145,135,125) + x_off - 2.5,y = grid2p2$y1 + 2.5, text=c(20, 40, 60, 80), showarrow = F) %>%
    add_annotations(x = c(215,205,195,185) + x_off + 2.5,y = grid2p3$y1 + 2.5, text=c(20, 40, 60, 80), showarrow = F) %>%
    add_annotations(x = c(140,160,180,200) + x_off,y = c(-5,-5,-5,-5), text=c(20, 40, 60, 80), showarrow = F) %>%
    add_annotations(x = grid3p1$x0-5,y = grid3p1$y0-5, text=c(80, 60, 40, 20), showarrow = F) %>%
    add_annotations(x = grid3p1$x1+5,y = grid3p1$y1+5, text=c(20, 40, 60, 80), showarrow = F) %>%
    add_annotations(x = grid3p2$x0-5,y = grid3p2$y0+5, text=c(20, 40, 60, 80), showarrow = F) %>%
    add_annotations(x = grid3p2$x1+5,y = grid3p2$y1-5, text=c(80, 60, 40, 20), showarrow = F) %>%
    add_annotations(x = 50, y = -15, text = "Ca<sup>+2", showarrow = F) %>%
    add_annotations(x = 10, y = 60, text = "Mg<sup>+2", showarrow = F, textangle = -60) %>%
    add_annotations(x = 75 + x_off/2, y = 65, text = "Na<sup>+</sup>+K<sup>+</sup>", showarrow = F, textangle = 60)%>%
    add_annotations(x = 145 + x_off/2, y = 65, text = "HCO<sub>3</sub><sup>-</sup> + CO<sub>3</sub><sup>-2</sup>", showarrow = F, textangle = -60)%>%
    add_annotations(x = 170 + x_off, y = -15, text = "Cl<sup>-", showarrow = F)%>%
    add_annotations(x = 75 + x_off/2, y = 170+y_off, textangle = -60, showarrow = F, text = 
          "SO<sub>4</sub><sup>-2</sup>+Cl<sup>-")%>%
    add_annotations(x = 145 +x_off/2, y = 165  + y_off, textangle = 60, showarrow = F, text = "Ca<sup>+</sup> + Mg<sup>+") %>%
    add_annotations(x = 205 + x_off, y = 65, text = "SO<sub>4</sub><sup>-2", showarrow = F, textangle = 60) %>%
    layout(xaxis = list(zeroline = FALSE,showline = FALSE,
                        showticklabels = FALSE,showgrid = FALSE, title = ""),
           yaxis = list(scaleanchor = "x",scaleratio = 1,zeroline = FALSE,showline = FALSE,
                        showticklabels = FALSE,showgrid = FALSE, title = ""), uniformtext=list(minsize=18, mode='show'))
  return(p)
}

transform_piper_data <- function(Mg, Ca, Cl,SO4, name=NULL){
  x_off = 30
  y_off = 30
  if(is.null(name)){
    name = rep(1:length(Mg),3)
  } else {
    name = rep(name,3)
  }
  y0 <- Mg * 0.86603
  x0 <- 100*(1-(Ca/100) - (Mg/200))
  y1 <- SO4 * 0.86603
  x1 <-120+(100*Cl/100 + 0.5 * 100*SO4/100)+x_off
  new_point <- function(x0, x1, y0, y1, grad=1.73206){
    b1 <- y0-(grad*(x0))
    b2 <- y1-(-grad*(x1-x_off))
    M <- matrix(c(grad, -grad, -1,-1), ncol=2)
    intercepts <- as.matrix(c(b1,b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x=t_mat[1,1] + x_off/2, y=t_mat[2,1] + y_off)
  }
  np_list <- lapply(1:length(x0), function(i) new_point(x0[i], x1[i], y0[i], y1[i]))
  npoints <- do.call("rbind",np_list)
  data.frame(observation=name,x=c(x0, x1, npoints$x), y=c(y=y0, y1, npoints$y))
}