xlsxAnnot <-
function(annot_all,Path,DF,j.mem = "10g",plots=F){
  options(java.parameters= paste("\"-Xmx",j.mem,"\"",sep=""))
  # create workbook
  wb <- createWorkbook()
  alignment <- Alignment(horizontal="ALIGN_CENTER")
  cell.style <- CellStyle(wb, alignment=alignment)
  font <-  Font(wb, heightInPoints=8)
  # create each sheet for for each mode
	for (i in 1:length(annot_all)){
	  sheet <- createSheet(wb, sheetName = names(annot_all)[i])
	  annot_cur <- annot_all[[i]]
	  # get bin labels present in the sheet
	  bins <- unique(annot_cur[,1])
	  bins[bins==""] <- NA
	  bins <- bins[!(bins=="Bin" | is.na(bins))]
	  # collect row numbers for each bin
	  pos.bin <- sapply(bins,function(x,y){z <- which(y==x); return(z)},y=annot_cur[,1])
	  # add columns and rows for plots if needed 
	  if (plots==T){
	    # add columns
		  annot_cur <- cbind(annot_cur[,1:3],matrix("",nrow=nrow(annot_cur),ncol=14),annot_cur[,4:ncol(annot_cur)])
		  colnames(annot_cur)[4:10] <- "Boxplots"
		  colnames(annot_cur)[11:17] <- "Bin Plots"
		  # calculate where rows need to be added and how many
		  pos.bin.1 <- c(pos.bin,nrow(annot_cur))
		  if (pos.bin.1[length(pos.bin.1)-1]==pos.bin.1[length(pos.bin.1)]){
		    annot_cur <- rbind(annot_cur,matrix("",nrow=2,ncol=ncol(annot_cur)))
		    pos.bin.1 <- c(pos.bin,nrow(annot_cur))
		  }
		  pos.diff <- c()
		  for (x in 1:(length(pos.bin.1)-1)){
		    pos.diff[x] <-  pos.bin.1[x+1] - pos.bin.1[x]
		  }
		  # add rows
		  for (x in 1:length(pos.bin)){
		    if (pos.diff[i] < 15){
		      extra.row <- matrix("",nrow=15-pos.diff[x],ncol=ncol(annot_cur))
		      colnames(extra.row) <- colnames(annot_cur)
		      annot_cur <- rbind(annot_cur[1:(pos.bin.1[x+1]-1),],extra.row,annot_cur[(pos.bin.1[x+1]):nrow(annot_cur),])
		      for (y in 1:length(bins)){
		        pos.bin[y] <- which(annot_cur[,1]==bins[y])
		      }
		      pos.bin <- unlist(pos.bin)
		      pos.bin.1 <- c(pos.bin,nrow(annot_cur))
		    }
		  }
		  # Add in picutres
		  for (x in 1:length(bins)){
		    pos.bin[i] <- which(annot_cur[,1]==bins[x])
		  }
		  pos.bin <- unlist(pos.bin)
		  for (x in 1:length(bins)){
		    if (file.exists(paste(Path,DF,paste(DF,"Boxplots",sep="_"),paste(paste(DF,bins[x],sep="_"),".jpeg",sep=""),sep="/"))){
		      addPicture(paste(Path,DF,paste(DF,"Boxplots",sep="_"),paste(paste(DF,bins[x],sep="_"),".jpeg",sep=""),sep="/"),sheet,startRow=pos.bin[x]+1,startColumn=4,scale=0.4)
		    }
		    if (file.exists(paste(Path,DF,paste(DF,"Bin_Plots",sep="_"),paste(paste(DF,"Peaks",bins[x],sep="_"),".jpeg",sep=""),sep="/"))){
		      addPicture(paste(Path,DF,paste(DF,"Bin_Plots",sep="_"),paste(paste(DF,"Peaks",bins[x],sep="_"),".jpeg",sep=""),sep="/"),sheet,startRow=pos.bin[x]+1,startColumn=11,scale=0.2)
		    }
		  }
	  }
	  print(pos.bin)
	  # set cell alignment, style and add data to workbook
	  cb <- CellBlock(sheet, 1, 1, nrow(annot_cur), ncol(annot_cur))
	  CB.setMatrixData(cb, annot_cur, 1, 1,cellStyle=cell.style)
	  sec.colour <- c(Accurate.m.z="lightblue",Boxplots="lightsalmon",`Bin Plots`="Red",Correlation.Analysis="Blue",Molecular.Formulas="yellow",Putative.Ionisation.Products="bisque1")
	  # calculate rows and columns of borders
	  cols <- colnames(annot_cur)
	  sec.pos <- lapply(names(sec.colour),function(x,y){return(grep(x,y))},y=cols)
	  # add borders
	  border.r <- Border(position="RIGHT")
	  border.b <- Border(position="TOP")
	  for (x in 1:length(pos.bin)){
	    CB.setBorder(cb, border.b, pos.bin[[x]], 1:(ncol(annot_cur)))
	  }
	  for (x in sec.pos){
	      CB.setBorder(cb, border.r, 1:(nrow(annot_cur)), max(x))
	  }
	  # add column names to first row
    annot_cur <- rbind(colnames(annot_cur),annot_cur)
	  # add cell colours
	  for (y in 1:length(sec.colour)){
	    
	    fill <- Fill(foregroundColor = sec.colour[y], backgroundColor=sec.colour[y])
	    fills.1 <- sec.pos[y]
	    for (x in 1:length(fills.1)){
	      CB.setFill(cb, fill, 1, fills.1[x])
	    }	  
	  }
	  # set font size
	  for (x in 1:ncol(annot_cur)){
	    CB.setFont(cb, font,1:(nrow(annot_cur)),x)
	  }
	}
  return(wb)
}