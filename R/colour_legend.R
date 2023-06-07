##' colour_legend function
##'
##' A function to
##'
##' @param palette X
##' @param suffix X
##' @param dir X
##' @return ...
##' @export

colour_legend <- function(palette,suffix="",dir="."){
    oldp <- par()
    l <- length(palette)
    cat("\n\n")
    for(i in 1:l){
        path <- file.path(dir,paste("col",suffix,i,".pdf",sep=""))
        pdf(path)
        par(mar=c(0,0,0,0))
        cat(paste("\\protect\\includegraphics[width=1em]{",path,"} [  ,  ]",sep=""),"\n")
        plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="")
        polygon(c(0,0,1,1),c(0,1,1,0),col=palette[i])
        dev.off()
    }
    cat("\n\n")
    par <- oldp
    dev.off()
}#
