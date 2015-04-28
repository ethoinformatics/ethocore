#!/usr/bin/Rscript

options(warn=-1)

setwd('~/Dropbox/ethocore/')

write.filename <- 'ethocore'

ethocore <- read.delim('ethocore.txt',quote="",stringsAsFactors=FALSE,na.strings=c('','N/A'))

ethocore <- ethocore[ethocore$DECISION %in% c('recommended','proposed','disputed'),]
ethocore <- ethocore[order(ethocore$SORT),]
rownames(ethocore) <- NULL

auxiliary <- c('ResourceRelationship')

classes <- unique(ethocore$CLASS)
classes <- classes[!classes %in% c('','RecordLevel',auxiliary)]

linkify <- function(x,y='md') {
	if (identical(y,'md')) {
		paste0('[',x,'](#',x,')')
	} else if (identical(y,'html')) {
		paste0('<a href="#',x,'">',x,'</a>')
	}
}

write('# Etho Core terms\n',file=paste0(write.filename,'.md'))

write(paste(do.call(c,lapply(c('RecordLevel',classes[!is.na(classes)],auxiliary),function(i) {
	set <- ethocore[ethocore$CLASS %in% i,]
	paste0('## ',if (i %in% 'RecordLevel') 'Record-level Terms' else linkify(i),'\n\n',paste(apply(set,1,function(x) {
		linkify(x['TERM'])
	}),collapse=' | '),'\n')
})),collapse='\n'),file=paste0(write.filename,'.md'),append=TRUE)

write(paste(do.call(c,lapply(c('RecordLevel',classes[!is.na(classes)],auxiliary),function(i) {
	set <- ethocore[ethocore$CLASS %in% i,]
	paste0('<h2>',if (i %in% 'RecordLevel') 'Record-level Terms' else linkify(i,'html'),'</h2>\n\n',paste(apply(set,1,function(x) {
		linkify(x['TERM'],'html')
	}),collapse=' | '),'<br>\n')
})),collapse='\n'),file=paste0(write.filename,'_toc.html'))

# write('# Auxiliary terms\n',file=paste0(write.filename,'.md'),append=TRUE)

# write(paste(do.call(c,lapply(auxiliary,function(i) {
# 	set <- ethocore[ethocore$CLASS %in% i,]
# 	paste0('## ',linkify(i),'\n\n',paste(apply(set,1,function(x) {
# 		linkify(x['TERM'])
# 	}),collapse=' | '),'\n')
# })),collapse='\n'),file=paste0(write.filename,'.md'),append=TRUE)


# CONVERT Table of Contents into HTML
#
# Check if pandoc is installed on Mac
#
#if (Sys.info()['sysname'] %in% 'Darwin' & as.logical(length(system('which pandoc',intern=TRUE)))) {
#	system(paste0('pandoc ethocore.md -o ',write.filename,'.html'))
#}


# WRITE HTML TABLE

ethocore$CLASS <- gsub('CLASS','',ethocore$CLASS)
ethocore$CLASS <- gsub('RecordLevel','all',ethocore$CLASS)

# ethocore$CLASS <- gsub('RecordLevel','A11',ethocore$CLASS)	# a11 to ensure first in alphabetical order
# ethocore <- ethocore[order(ethocore$CLASS %in% auxiliary,ethocore$CLASS,ethocore$TERM),]
# ethocore$CLASS <- gsub('A11','all',ethocore$CLASS)			# replace a11 with all

fields <- c('Identifier','Class','Definition','Comment','Details')
columns <- c('URI','CLASS','DEFINITION','DESCRIPTION','TERM')

## xtable method (the tables are ugly)
# library(xtable)
# dev.null <- apply(ethocore,1,function(x) {
# 	print.xtable(xtable(rbind(c(x[columns][1],''),cbind(paste0(fields,':'),x[columns]))),type='html',file=paste0(write.filename,'.md'),append=TRUE,include.rownames=FALSE,include.colnames=FALSE,html.table.attributes='class="Terms"')
# })

write.row <- function(x) paste0('<tr>',x,'</tr>')
write.col <- function(x,cl,header=FALSE) {
	if (header) paste0('<th colspan="2" id="',x,'">Term Name: ',x,'</th>') else paste0('<td>',x,'</td>')
}

write(paste0('<table id="terms-table" class="terms-dictionary">\n\t<tr style="border:none;"><td></td><td></td></tr>\n\t',paste(write.row(do.call(c,as.list(apply(ethocore,1,function(x) {
	documentation <- x[columns]
	documentation[5] <- paste0('<a href="',paste0('http://ethoinformatics.org/ethocore/',documentation[5]),'">',documentation[5],'</a>')
	documentation[is.na(documentation)] <- ''
	c(write.col(x['TERM'],header=TRUE),apply(cbind(write.col(paste0(fields,':')),write.col(documentation)),1,paste0,collapse=''))
})))),collapse='\n\t'),'\n</table>'),file=paste0(write.filename,'_table.html'))

write(paste0('<table id="terms-table" class="terms-dictionary">\n\t<tr style="border:none;"><td></td><td></td></tr>\n\t',paste(write.row(do.call(c,as.list(apply(ethocore,1,function(x) {
	documentation <- x[columns]
	documentation[5] <- paste0('<a href="',paste0('http://ethoinformatics.org/ethocore/',documentation[5]),'">',documentation[5],'</a>')
	documentation[is.na(documentation)] <- ''
	c(write.col(x['TERM'],header=TRUE),apply(cbind(write.col(paste0(fields,':')),write.col(documentation)),1,paste0,collapse=''))
})))),collapse='\n\t'),'\n</table>'),file=paste0(write.filename,'.md'),append=TRUE)

ethocore.wordpress <- ethocore[,do.call(c,lapply(strsplit(names(ethocore),''),function(x) all(x %in% c(letters,'_'))))]
rownames(ethocore.wordpress) <- NULL

write.csv(ethocore.wordpress,file=paste0(write.filename,'.csv'),row.names=FALSE,na='')


