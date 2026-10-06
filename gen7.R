# =============================================================================
#  Genera las seis figuras didacticas del capitulo 7 (apartados 7.3-7.5).
#  Todas en disposicion VERTICAL, para que quepan sin scroll horizontal.
#  Ejecutar desde la raiz del proyecto; deja los PNG en figuras/.
# =============================================================================
options(bitmapType = "cairo")
suppressMessages({library(ggplot2); library(patchwork)})

# --- utilidad: apilar paneles alineando sus areas de dibujo ------------------
apilar <- function(..., archivo, w, h) {
  gl <- lapply(list(...), ggplotGrob)
  anchos <- do.call(grid::unit.pmax, lapply(gl, function(g) g$widths))
  for (i in seq_along(gl)) gl[[i]]$widths <- anchos
  png(archivo, width = w, height = h, units = "in", res = 150)
  grid::grid.draw(do.call(rbind, c(gl, list(size = "first"))))
  invisible(dev.off())
}

tema9 <- function() theme_bw(base_size = 10) +
  theme(legend.position = "none", axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9.5, colour = "grey30"))

# ============================================================================
# 1. WARD
# ============================================================================
  .pts <- data.frame(
    x = c(0.8,1.2,0.9,1.4, 2.4,2.8,2.2,2.9, 6.2,6.8,6.4,7.0),
    y = c(4.8,5.1,4.2,4.5, 4.2,4.6,3.7,3.9, 1.4,1.8,1.0,1.5),
    g = rep(c("A","B","C"), each = 4))
  .colW <- c(A="steelblue4", B="skyblue3", C="red3")
  .env <- function(d, pad=.45){ h<-chull(d$x,d$y); q<-d[h,]
    cx<-mean(d$x); cy<-mean(d$y); dx<-q$x-cx; dy<-q$y-cy
    r<-sqrt(dx^2+dy^2); r[r==0]<-1e-9
    data.frame(x=cx+dx*(1+pad/r), y=cy+dy*(1+pad/r)) }
  .scw <- function(d) sum((d$x-mean(d$x))^2+(d$y-mean(d$y))^2)
  .sumas <- sapply(list(c("A","B"),c("B","C"),c("A","C")), function(par)
    .scw(.pts[.pts$g%in%par,]) + .scw(.pts[!.pts$g%in%par,]))
  names(.sumas) <- c("A+B","B+C","A+C")
  .panelW <- function(par, tit){
    fus<-.pts[.pts$g%in%par,]; ais<-.pts[!.pts$g%in%par,]
    cf<-data.frame(x=mean(fus$x),y=mean(fus$y)); ca<-data.frame(x=mean(ais$x),y=mean(ais$y))
    ggplot() +
      geom_polygon(data=.env(fus),aes(x,y),fill="grey40",alpha=.10,colour="grey25",linetype="dashed",linewidth=.8)+
      geom_polygon(data=.env(ais),aes(x,y),fill=NA,colour="grey75",linetype="dotted")+
      geom_segment(data=fus,aes(x=x,y=y,xend=cf$x,yend=cf$y),colour="grey50",linewidth=.3)+
      geom_segment(data=ais,aes(x=x,y=y,xend=ca$x,yend=ca$y),colour="grey70",linewidth=.3)+
      geom_point(data=.pts,aes(x,y,colour=g),size=2.4)+
      geom_point(data=ca,aes(x,y),shape=23,size=3.4,fill="grey70",colour="white",stroke=.9)+
      geom_point(data=cf,aes(x,y),shape=23,size=4.2,fill="grey25",colour="white",stroke=1)+
      scale_colour_manual(values=.colW)+
      coord_cartesian(xlim=c(-.6,8.4), ylim=c(-.4,6.4))+
      ggtitle(tit, subtitle=paste0("Suma de cuadrados: ",formatC(.sumas[[paste(par,collapse="+")]],format="f",digits=1)))+
      xlab(NULL)+ylab(NULL)+tema9() }
  apilar(.panelW(c("A","B"),"Opción 1: unir A + B"),
         .panelW(c("B","C"),"Opción 2: unir B + C"),
         .panelW(c("A","C"),"Opción 3: unir A + C"),
         archivo="figuras/ward_opciones.png", w=5.2, h=9.3)
  cat("1 Ward ok | sumas:", round(.sumas,1), "\n")

# ============================================================================
# 2. SILUETA (vertical)
# ============================================================================
  set.seed(12)
  .A <- data.frame(x=rnorm(14,2.2,.50), y=rnorm(14,5.0,.50), g="A")
  .B <- data.frame(x=rnorm(14,6.2,.50), y=rnorm(14,3.2,.50), g="B")
  .esp <- data.frame(x=c(2.0,4.0,5.9), y=c(5.1,4.2,3.5), g="A")
  .P <- rbind(.A,.B,.esp); .idxS <- nrow(.A)+nrow(.B)+1:3
  .ab <- function(i){ mi<-.P$g[i]; d<-sqrt((.P$x-.P$x[i])^2+(.P$y-.P$y[i])^2)
    a<-mean(d[.P$g==mi & seq_len(nrow(.P))!=i]); b<-mean(d[.P$g!=mi])
    c(a=a,b=b,s=(b-a)/max(a,b)) }
  .resS <- t(sapply(.idxS,.ab)); .colS <- c(A="steelblue4",B="red3")
  .panelS <- function(k,tit){ i<-.idxS[k]; mi<-.P$g[i]
    prop<-.P[.P$g==mi & seq_len(nrow(.P))!=i,]; otros<-.P[.P$g!=mi,]
    ggplot()+
      geom_segment(data=prop,aes(x=.P$x[i],y=.P$y[i],xend=x,yend=y),colour="steelblue3",linetype="dashed",linewidth=.25)+
      geom_segment(data=otros,aes(x=.P$x[i],y=.P$y[i],xend=x,yend=y),colour="red2",linetype="dashed",linewidth=.25)+
      geom_point(data=.P[-.idxS,],aes(x,y,colour=g),size=1.9,alpha=.85)+
      geom_point(data=.P[i,],aes(x,y),colour="black",size=3.4)+
      scale_colour_manual(values=.colS)+
      coord_cartesian(xlim=c(.4,8), ylim=c(1.4,6.6))+
      ggtitle(tit, subtitle=sprintf("a = %.2f    b = %.2f    s = %.2f",.resS[k,"a"],.resS[k,"b"],.resS[k,"s"]))+
      xlab(NULL)+ylab(NULL)+tema9() }
  apilar(.panelS(1,"Caso bien agrupado"), .panelS(2,"Caso en la frontera"),
         .panelS(3,"Caso mal clasificado"),
         archivo="figuras/silueta_casos.png", w=5.5, h=9)
  cat("2 Silueta ok | s:", round(.resS[,"s"],2), "\n")

# ============================================================================
# nube comun para k-medias y centroide mas lejano
# ============================================================================
  set.seed(7)
  .nube <- rbind(
    data.frame(x=rnorm(45,2.2,.60), y=rnorm(45,6.2,.60)),
    data.frame(x=rnorm(45,6.8,.60), y=rnorm(45,5.6,.60)),
    data.frame(x=rnorm(45,4.4,.60), y=rnorm(45,2.0,.60)))
  .cK <- c(`1`="steelblue4", `2`="red3", `3`="skyblue3")
  .mkK <- function(p,tit,sub) p + coord_cartesian(xlim=c(.3,8.6),ylim=c(.2,8))+
    ggtitle(tit,subtitle=sub)+xlab(NULL)+ylab(NULL)+tema9()

# ============================================================================
# 3. K-MEDIAS (vertical, CINCO pasos)
# ============================================================================
  .sem <- data.frame(x=c(3.6,4.2,4.8), y=c(4.6,4.2,3.8))
  .asig <- function(X,C){ dm<-as.matrix(dist(rbind(as.matrix(C),as.matrix(X))))[-(1:nrow(C)),1:nrow(C)]
    max.col(-dm,ties.method="first") }
  .rec <- function(X,a,C){ for(j in 1:nrow(C)) if(any(a==j)){C$x[j]<-mean(X$x[a==j]);C$y[j]<-mean(X$y[a==j])};C }
  C<-.sem; .est<-list(); aa<-NULL
  repeat{ a<-.asig(.nube,C)
    .est[[length(.est)+1]]<-list(C=C,a=a,cam=if(is.null(aa)) NA else sum(a!=aa))
    if(!is.null(aa)&&all(a==aa)) break
    aa<-a; C<-.rec(.nube,a,C) }
  .pK <- function(i,tit,sub,asig=TRUE,marcar=TRUE){ e<-.est[[i]]; d<-.nube
    d$g<-factor(e$a,levels=1:3); d$cam<-if(marcar&&i>1) e$a!=.est[[i-1]]$a else FALSE
    p<-ggplot()
    p<-p+ if(asig) geom_point(data=d,aes(x,y,colour=g),size=1.3,alpha=.8)
          else geom_point(data=d,aes(x,y),colour="grey25",size=1.3,alpha=.85)
    if(any(d$cam)) p<-p+geom_point(data=d[d$cam,],aes(x,y),shape=21,size=2.6,fill=NA,colour="black",stroke=.5)
    Cg<-e$C; Cg$g<-factor(seq_len(nrow(Cg)),levels=1:3)
    .mkK(p+geom_point(data=e$C,aes(x,y),shape=21,size=4.6,fill="white",colour="white")+
      geom_point(data=Cg,aes(x,y,colour=g),shape=4,size=3.2,stroke=1.5)+
      scale_colour_manual(values=.cK,drop=FALSE),tit,sub) }
  n_it <- length(.est)
  apilar(
    .pK(1,"1. Semillas iniciales","Las tres, juntas y mal colocadas",asig=FALSE,marcar=FALSE),
    .pK(1,"2. Primera asignación","Cada caso, al centroide más próximo",marcar=FALSE),
    .pK(2,"3. Segunda iteración",paste0("Cambian de grupo ",.est[[2]]$cam," casos")),
    .pK(3,"4. Tercera iteración",paste0("Cambian de grupo ",.est[[3]]$cam," casos")),
    .pK(n_it,"5. Solución convergente","Ningún caso cambia: el algoritmo para"),
    archivo="figuras/kmedias_iteraciones.png", w=5, h=13.5)
  cat("3 k-medias ok | iteraciones:", n_it, "| cambios:", sapply(.est,function(e)e$cam), "\n")

# ============================================================================
# 4. CENTROIDE MAS LEJANO (vertical)
# ============================================================================
  .Pm <- as.matrix(.nube); .Dm <- as.matrix(dist(.Pm)); .el <- 118L
  for(k in 2:3) .el<-c(.el, which.max(apply(.Dm[,.el,drop=FALSE],1,min)))
  .fo <- geom_point(data=.nube,aes(x,y),colour="grey60",size=1.2,alpha=.75)
  .as <- function(idx) geom_point(data=.nube[idx,],aes(x,y),shape=4,size=3.4,stroke=1.5,colour=unname(.cK)[seq_along(idx)])
  .un <- function(a,b) annotate("segment",x=.nube$x[.el[a]],y=.nube$y[.el[a]],xend=.nube$x[.el[b]],yend=.nube$y[.el[b]],colour="grey25",linetype="dashed",linewidth=.5)
  apilar(
    .mkK(ggplot()+.fo+.as(.el[1]),"1. Primer centroide","Se elige un caso al azar"),
    .mkK(ggplot()+.fo+.un(1,2)+.as(.el[1:2]),"2. Segundo centroide","El caso MÁS ALEJADO del primero"),
    .mkK(ggplot()+.fo+.un(3,1)+.un(3,2)+.as(.el),"3. Tercer centroide","Mayor distancia MÍNIMA a los anteriores"),
    .mkK(ggplot()+.fo+.as(.el),"4. Las tres semillas","Una por zona densa"),
    archivo="figuras/centroide_lejano.png", w=5, h=11)
  cat("4 centroide lejano ok\n")

# ============================================================================
# nube comun DBSCAN (arco)
# ============================================================================
  set.seed(5)
  .ang <- seq(pi*.92, pi*.08, length.out=34)
  .Pd <- rbind(
    data.frame(x=4.6+3.1*cos(.ang)+rnorm(34,0,.17), y=2.0+3.1*sin(.ang)+rnorm(34,0,.17)),
    data.frame(x=rnorm(22,4.6,.52), y=rnorm(22,1.6,.52)),
    data.frame(x=c(.8,8.5,4.6,8.6,1,6.9), y=c(1,1.2,3.6,5.6,5.4,6.3)))
  .eps<-.95; .mp<-4; .Dd<-as.matrix(dist(.Pd))
  .vec<-lapply(1:nrow(.Pd), function(i) setdiff(which(.Dd[i,]<=.eps),i))
  .nv<-lengths(.vec); .nuc<-.nv>=.mp
  .tipo<-ifelse(.nuc,"Nucleo",ifelse(sapply(.vec,function(v)any(.nuc[v])),"Borde","Ruido"))
  .ct<-c(Nucleo="steelblue4",Borde="skyblue3",Ruido="red3")
  .cir<-function(i,col,lt="dashed"){ t<-seq(0,2*pi,length.out=160)
    annotate("path",x=.Pd$x[i]+.eps*cos(t),y=.Pd$y[i]+.eps*sin(t),colour=col,linetype=lt,linewidth=.6) }
  .mkD<-function(p,tit,sub) p+coord_equal(xlim=c(.3,9.1),ylim=c(.3,7.6))+ggtitle(tit,subtitle=sub)+xlab(NULL)+ylab(NULL)+tema9()
  .foD<-function() geom_point(data=.Pd,aes(x,y),colour="grey70",size=1.2,alpha=.85)

# ============================================================================
# 5. DBSCAN tipos de punto (vertical)
# ============================================================================
  .des<-function(i,col) list(.cir(i,col),
    geom_point(data=.Pd[.vec[[i]],],aes(x,y),colour=col,size=1.6,alpha=.9),
    geom_point(data=.Pd[i,],aes(x,y),colour=col,size=3),
    annotate("text",x=.Pd$x[i],y=.Pd$y[i]+.eps+.45,label=paste0(c(Nucleo="Núcleo",Borde="Borde",Ruido="Ruido")[.tipo[i]],"\n",.nv[i]," vecinos"),colour=col,size=2.7,fontface="bold",lineheight=.95))
  apilar(
    .mkD(ggplot()+.foD()+.des(37,.ct[["Nucleo"]]),"Punto núcleo",paste0("Tiene ",.nv[37]," vecinos dentro de eps (>= ",.mp,")")),
    .mkD(ggplot()+.foD()+.des(53,.ct[["Borde"]]),"Punto borde",paste0("Solo ",.nv[53],", pero uno de ellos es núcleo")),
    .mkD(ggplot()+.foD()+.des(60,.ct[["Ruido"]]),"Ruido","Ningún vecino dentro de eps"),
    archivo="figuras/dbscan_tipos.png", w=5.5, h=10.5)
  cat("5 DBSCAN tipos ok | tipos:", table(.tipo), "\n")

# ============================================================================
# 6. DBSCAN propagacion (vertical)
# ============================================================================
  .sem2<-1L; .niv<-rep(NA_integer_,nrow(.Pd)); .niv[.sem2]<-0L; fr<-.sem2; k<-0L
  repeat{ k<-k+1L
    nu<-setdiff(unique(unlist(.vec[fr[.nuc[fr]]])),which(!is.na(.niv)))
    if(!length(nu)) break; .niv[nu]<-k; fr<-nu }
  .cortes<-c(0,3,7,max(.niv,na.rm=TRUE))
  .titD<-c("1. Punto núcleo inicial","2. Se añaden sus vecinos","3. La densidad se propaga","4. Clúster cerrado")
  .pan<-lapply(seq_along(.cortes), function(j){
    dentro<-which(!is.na(.niv)&.niv<=.cortes[j])
    nuevos<-which(!is.na(.niv)&.niv<=.cortes[j]&.niv>(if(j==1)-1 else .cortes[j-1]))
    .mkD(ggplot()+.foD()+
      geom_point(data=.Pd[dentro,],aes(x,y),colour="steelblue4",size=1.8)+
      (if(j<4) .cir(.sem2,"steelblue4","dotted") else NULL)+
      geom_point(data=.Pd[nuevos,],aes(x,y),shape=21,size=2.8,fill=NA,colour="black",stroke=.5),
      .titD[j],paste0(length(dentro)," casos en el clúster")) })
  do.call(apilar, c(.pan, list(archivo="figuras/dbscan_propagacion.png", w=5.5, h=13.5)))
  cat("6 DBSCAN propagacion ok | niveles:", k-1, "\n")
