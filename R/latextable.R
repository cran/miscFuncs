##' latextable function
##'
##' A very useful function to create a LaTeX table from a matrix. Rounds numeric entries and also replaces
##' small numbers with standard index form equivalents.
##'
##' To get a backslash to appear, use a double backslash
##'
##' Just copy and paste the results into your LaTeX document.
##'
##' @param x a matrix, can include mixed character and numeric entries 
##' @param dp number of decimal places
##' @param colnames optional column names
##' @param rownames optional row names
##' @param caption optional caption, not normally used
##' @param narep string giving replacement for NA entries in the matrix
##' @param laststr string to write at end, eg note the double backslash!!
##' @return prints the LaTeX table to screen, so it can be copied into reports
##' @export

latextable <- function(x,dp=3,colnames=NULL,rownames=NULL,caption=NULL,narep=" ",laststr=""){
	d <- dim(x)
	write("","")
	write("\\begin{table}","")
	write("    \\centering","")
	if(!is.null(caption)){write(paste("    \\caption{",caption,"}",sep=""),"")}
	cs <- "    \\begin{tabular}{"
	cn <- ""
	times <- d[2]
	if(!is.null(rownames)){times <- times + 1}
	for (i in 1:times){
		cs <- paste(cs,"c",sep="")
		if (i<times){cn <- paste(cn,colnames[i]," & ",sep="")}
	}
	cs <- paste(cs,"}",sep="")
	cn <- paste(cn,colnames[times]," \\\\ \\hline",sep="")
	write(cs,"")
	if(!is.null(colnames)){
		if (!any(length(colnames)==c(d[2],d[2]+1))){stop("Incorrect number of column names")}
		write(paste("        ",cn),"")
	}
	for (i in 1:d[1]){
		if (!is.null(rownames)){
			if (is.na(x[i,1])){
				towrite <- paste("        ",rownames[i]," & ",narep," & ",sep="")
			}
			else if (is.numeric(x[i,1])){
				towrite <- paste("        ",rownames[i]," & ",round(x[i,1],dp)," & ",sep="")
			}
			else{
				towrite <- paste("        ",rownames[i]," & ",x[i,1]," & ",sep="")
			}
		}
		else{
			if (is.na(x[i,1])){
				towrite <- paste("        ",narep," & ",sep="")
			}
			else if (is.numeric(x[i,1])){
				towrite <- paste("        ",round(x[i,1],dp)," & ",sep="")
			}
			else{
				towrite <- paste("        ",x[i,1]," & ",sep="")
			}
		}
		if (d[2] > 2){			
			for (j in 2:(d[2]-1)){
				if (is.na(x[i,j])){
					towrite <- paste(towrite,narep," & ",sep="")
				}
				else if (!is.na(as.numeric(x[i,j]))){
					if (round(as.numeric(x[i,j]),dp)==0){
						if (as.numeric(x[i,j])==0){								
							towrite <- paste(towrite,"0 & ",sep="")
						}
						else{
							ii <- dp + 1
							while (round(as.numeric(x[i,j]),ii)==0){
								ii <- ii + 1
							}
							temptr <- round(as.numeric(x[i,j]),ii+dp) * 10^ii
							towrite <- paste(towrite,paste("$",temptr,"\\","times10^{-",ii,"}$",sep="")," & ",sep="")
						}			
					}
					else{
						towrite <- paste(towrite,paste(round(as.numeric(x[i,j]),dp))," & ",sep="")
					}
				} 
				else{
					towrite <- paste(towrite,x[i,j]," & ",sep="")
				} 
			}
		}	
		if (is.na(x[i,d[2]])){
			towrite <- paste(towrite,narep," \\\\",sep="")
		}
		else if (!is.na(as.numeric(x[i,d[2]]))){
			if (round(as.numeric(x[i,d[2]]),dp)==0){
				if (as.numeric(x[i,d[2]])==0){								
					towrite <- paste(towrite,"0 \\\\ ",sep="")
				}
				else{
					ii <- dp + 1
					while (round(as.numeric(x[i,d[2]]),ii)==0){
						ii <- ii + 1
					}
					temptr <- round(as.numeric(x[i,d[2]]),ii+dp) * 10^ii
					towrite <- paste(towrite,paste("$",temptr,"\\","times10^{-",ii,"}$ \\\\",sep=""),sep="")
				}	
			}
			else{
				towrite <- paste(towrite,paste(round(as.numeric(x[i,d[2]]),dp)," \\\\"),sep="")
			}
		}
		else{
			towrite <- paste(towrite,x[i,d[2]]," \\\\",sep="")
		}
		if (i==d[1] & laststr!=""){
			towrite <- paste(towrite," ", laststr,sep="")
		}
		write(towrite,"")
	}
	write("    \\end{tabular}","")	
	write("\\end{table}","")	
	write("","")
}
