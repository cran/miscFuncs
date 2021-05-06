##' latextable function
##'
##' A very useful function to create a LaTeX table from a matrix. Rounds numeric entries and also replaces
##' small numbers with standard index form equivalents.
##'
##' To get a backslash to appear, use a double backslash
##'
##' Just copy and paste the results into your LaTeX document.
##'
##' @param x a matrix, or object that can be coerced to a matrix. x can include mixed character and numeric entries.
##' @param digits see help file for format
##' @param scientific see help file for format
##' @param colnames optional column names set to NULL (default) to automatically use column names of x. NOTE! if rownames is not NULL present, colnames must include an entry for the rownames i.e. it should be a vector of length the number of columns of x plus 1.
##' @param rownames optional row names set to NULL (default) to automatically use row names of x
##' @param caption optional caption, not normally used
##' @param narep string giving replacement for NA entries in the matrix
##' @param laststr string to write at end, eg note the double backslash!!
##' @param intable output in a table environment?
##' @param manualalign manual align string e.g. 'ccc' or 'l|ccc'
##' @param file connection to write to, default is '' which writes to the console; see ?write for further details
##' @param ... additional arguments passed to format
##' @return prints the LaTeX table to screen, so it can be copied into reports
##' @examples latextable(as.data.frame(matrix(1:4,2,2)))
##' @export

latextable <- function(x,digits=3,scientific=-3,colnames=NULL,rownames=NULL,caption=NULL,narep=" ",laststr="",intable=TRUE,manualalign=NULL,file="",...){

    x <- as.matrix(x)

    trigger <- TRUE # colnames is not null
    if(is.null(colnames)){
        trigger <- FALSE
        colnames <- c("",colnames(x))
    }
    if(is.null(rownames)){
        rownames <- rownames(x)
    }



    form <- function(x,...){
        if(is.character(x)){
            x <- as.numeric(x)
        }
        xtxt <- format(x,digits=digits,scientific=scientific,...)
        if(length(grep("e",xtxt))>0){
            spl <- unlist(strsplit(xtxt,"e"))
            xtxt <- paste(spl[1],"$\\times10^{",as.character(as.numeric(spl[2])),"}$",sep="")
        }
        return(xtxt)
    }

	d <- dim(x)
	write("\n",file)
    if(intable){
    	write("\\begin{table}[htbp]",file,append=TRUE)
    	write("    \\centering",file,append=TRUE)
    }
	if(!is.null(caption)){write(paste("    \\caption{",caption,"}",sep=""),file,append=TRUE)}
	cs <- "    \\begin{tabular}{"

    cn <- ""
	times <- d[2]
    strt <- 1
    en <- times
	if(!is.null(rownames)){
        times <- times + 1
        en <- times
    }
    else{
        strt <- 2
        en <- times + 1
    }

    if(trigger){
        strt <- 1
        en <- d[2]
    }

	for (i in strt:en){
        if(is.null(manualalign)){
    		cs <- paste(cs,"c",sep="")
        }
		if (i<en){
            if(!all(colnames=="")){
                cn <- paste(cn,colnames[i]," & ",sep="")
            }
        }
	}

    if(!is.null(manualalign)){
        cs <- paste(cs,manualalign,"}",sep="")
    }
    else{
        cs <- paste(cs,"}",sep="")
    }




    if(!all(colnames=="")){
    	cn <- paste(cn,colnames[en]," \\\\ \\hline \\hline",sep="")
    }
	write(cs,file,append=TRUE)
	if(!is.null(colnames)){
        if(!all(colnames=="")){
    		if (!any(length(colnames)==c(d[2],d[2]+1))){
                stop("Incorrect number of column names")
            }
    		write(paste("        ",cn),file,append=TRUE)
        }
	}

	for (i in 1:d[1]){
		if (!is.null(rownames)){
			if (is.na(x[i,1])){
				towrite <- paste("        ",rownames[i]," & ",narep," & ",sep="")
			}
			else if (is.numeric(x[i,1])){
				towrite <- paste("        ",rownames[i]," & ",form(x[i,1])," & ",sep="")
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
				towrite <- paste("        ",form(x[i,1])," & ",sep="")
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
					towrite <- paste(towrite,form(x[i,j])," & ",sep="")
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
			towrite <- paste(towrite,form(x[i,d[2]])," \\\\",sep="")
		}
		else{
			towrite <- paste(towrite,x[i,d[2]]," \\\\",sep="")
		}
		if (i==d[1] & laststr!=""){
			towrite <- paste(towrite," ", laststr,sep="")
		}
		write(towrite,file,append=TRUE)
	}
	write("    \\end{tabular}",file,append=TRUE)
    if(intable){
    	write("\\end{table}",file,append=TRUE)
    }
	write("\n",file,append=TRUE)
}
