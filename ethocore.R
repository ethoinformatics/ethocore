#!/usr/bin/Rscript

options(warn=-1)

setwd('~/Desktop')

write.filename <- 'ethocore.md'

ethocore <- read.delim('ethocore.txt',quote="",stringsAsFactors=FALSE,na.strings=c('','N/A'))

ethocore <- ethocore[ethocore$DECISION %in% c('recommended','proposed','disputed'),]
ethocore <- ethocore[order(ethocore$TERM),]

auxiliary <- c('MeasurementOrFact','ResourceRelationship')

classes <- unique(ethocore$CLASS)
classes <- sort(classes[!classes %in% c('CLASS','RecordLevel',auxiliary)])

linkify <- function(x) paste0('[',x,'](#',x,')')

write('# Etho Core terms\n',file=write.filename)

write(paste0('## ','Record-level Terms','\n\n',paste(apply(ethocore[ethocore$CLASS %in% 'RecordLevel',],1,function(x) {
	linkify(x['TERM'])
}),collapse=' | '),'\n'),file=write.filename,append=TRUE)

write(paste(do.call(c,lapply(classes,function(i) {
	set <- ethocore[ethocore$CLASS %in% i,]
	paste0('## ',linkify(i),'\n\n',paste(apply(set,1,function(x) {
		linkify(x['TERM'])
	}),collapse=' | '),'\n')
})),collapse='\n'),file=write.filename,append=TRUE)

write('# Auxiliary terms\n',file=write.filename,append=TRUE)

write(paste(do.call(c,lapply(auxiliary,function(i) {
	set <- ethocore[ethocore$CLASS %in% i,]
	paste0('## ',linkify(i),'\n\n',paste(apply(set,1,function(x) {
		linkify(x['TERM'])
	}),collapse=' | '),'\n')
})),collapse='\n'),file=write.filename,append=TRUE)

ethocore$CLASS <- gsub('CLASS','',ethocore$CLASS)
ethocore$CLASS <- gsub('RecordLevel','A11',ethocore$CLASS)	# a11 to ensure first in alphabetical order
ethocore <- ethocore[order(ethocore$CLASS %in% auxiliary,ethocore$CLASS,ethocore$TERM),]
ethocore$CLASS <- gsub('A11','all',ethocore$CLASS)			# replace a11 with all

fields <- c('Identifier','Class','Project','Definition','Comment','Details')
columns <- c('TERM','CLASS','NAMESPACE','DEFINITION','DESCRIPTION','TERM')

## xtable method (the tables are ugly)
# library(xtable)
# dev.null <- apply(ethocore,1,function(x) {
# 	print.xtable(xtable(rbind(c(x[columns][1],''),cbind(paste0(fields,':'),x[columns]))),type='html',file=write.filename,append=TRUE,include.rownames=FALSE,include.colnames=FALSE,html.table.attributes='class="Terms"')
# })

write.row <- function(x) paste0('<tr>',x,'</tr>')
write.col <- function(x,header=FALSE) {	
	if (header) paste0('<th colspan="2"><a name="',x,'">Term Name: ',x,'</a></th>') else paste0('<td>',x,'</td>')
}

write('<style>.TermsDictionary th { text-align:left; }</style>',file=write.filename,append=TRUE)

write(paste0('<table class="TermsDictionary">\n\t',paste(write.row(do.call(c,as.list(apply(ethocore,1,function(x) {
	documentation <- x[columns]
	documentation[1] <- paste0('http://ethoinformatics.org/ethocore/',documentation[1])
	documentation[6] <- paste(documentation[6],'(placeholder: link to come)')
	documentation[is.na(documentation)] <- ''
	c(write.col(x[columns][1],header=TRUE),apply(cbind(write.col(paste0(fields,':')),write.col(documentation)),1,paste0,collapse=''))
})))),collapse='\n\t'),'\n</table>'),file=write.filename,append=TRUE)

