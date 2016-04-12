#!/usr/bin/env Rscript

options(warn=-1)

setwd('~/github/ethoinformatics/ethocore/')

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

write('# EthoCore terms\n',file=paste0(write.filename,'.md'))

write(paste(do.call(c,lapply(c('RecordLevel',classes[!is.na(classes)],auxiliary),function(i) {
	set <- ethocore[ethocore$CLASS %in% i,]
	paste0('<h2 id="ClassRecordLevel">',if (i %in% 'RecordLevel') 'Record-level Terms' else linkify(i),'\n\n',paste(apply(set,1,function(x) {
		linkify(x['TERM'])
	}),collapse=' | '),'<br>\n')
})),collapse='\n'),file=paste0(write.filename,'.md'),append=TRUE)

write(paste(do.call(c,lapply(c('RecordLevel',classes[!is.na(classes)],auxiliary),function(i) {
	set <- ethocore[ethocore$CLASS %in% i,]
	paste0('<h2 id="Class',i,'">',if (i %in% 'RecordLevel') 'Record-level Terms' else linkify(i,'html'),'</h2>\n\n',paste(apply(set,1,function(x) {
		linkify(x['TERM'],'html')
	}),collapse=' | '),'<br>\n')
})),collapse='\n'),file=paste0(write.filename,'_toc.html'))

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

# Convert into HTML

# Check if pandoc is installed on Mac

if (Sys.info()['sysname'] %in% 'Darwin' & as.logical(length(system('which pandoc',intern=TRUE)))) {
	system(paste0('pandoc ethocore.md -o ',write.filename,'.html'))
}

# Convert into RDF

ethocore.rdf <- ethocore.wordpress

ethocore.rdf$type <- as.character(sapply(ethocore.wordpress$class,function(x) if(is.na(x)) 'http://www.w3.org/2000/01/rdf-schema#Class' else 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'))

ethocore.rdf$definition <- gsub('&','&amp;',ethocore.rdf$definition)
ethocore.rdf$definition <- gsub('"','&quot;',ethocore.rdf$definition)
ethocore.rdf$definition <- gsub('\'','&apos;',ethocore.rdf$definition)
ethocore.rdf$definition <- gsub('<','&lt;',ethocore.rdf$definition)
ethocore.rdf$definition <- gsub('>','&gt;',ethocore.rdf$definition)
ethocore.rdf$description <- gsub('&','&amp;',ethocore.rdf$description)
ethocore.rdf$description <- gsub('"','&quot;',ethocore.rdf$description)
ethocore.rdf$description <- gsub('\'','&apos;',ethocore.rdf$description)
ethocore.rdf$description <- gsub('<','&lt;',ethocore.rdf$description)
ethocore.rdf$description <- gsub('>','&gt;',ethocore.rdf$description)

ethocore.rdf.head <- c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<!--", "  To use the stylesheet to make the RDF more readable in a web browser, uncomment one of the the following stylesheet references:", 
"  <?xml-stylesheet type=\"text/xsl\" href=\"human.xsl\"?>", 
"  <?xml-stylesheet type=\"text/xsl\" href=\"http://rs.tdwg.org/dwc/rdf/human.xsl\"?>", 
"-->", "<!DOCTYPE rdf:RDF [", "    <!ENTITY rdfns 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>", 
"    <!ENTITY rdfsns 'http://www.w3.org/2000/01/rdf-schema#'>", 
"    <!ENTITY dctermsns 'http://purl.org/dc/terms/'>", "    <!ENTITY dctypens 'http://purl.org/dc/dcmitype/'>", 
"    <!ENTITY dwcattributesns 'http://rs.tdwg.org/dwc/terms/attributes/'>", 
"<!--", "    <!ENTITY skosns 'http://www.w3.org/2004/02/skos/core#'>", 
"    <!ENTITY vsns 'http://www.w3.org/2003/06/sw-vocab-status/ns#'>", 
"-->", "]>", "<rdf:RDF", "xmlns:dwcattributes=\"http://rs.tdwg.org/dwc/terms/attributes/\"", 
"xmlns:dcterms=\"http://purl.org/dc/terms/\" ", "xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" ", 
"xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"", "xmlns:dwcterms=\"http://rs.tdwg.org/dwc/terms/\"", 
"xml:base=\"http://ethoinformatics.org/ethocore/\">", "<rdf:Description rdf:about=\"http://ethoinformatics.org/ethocore/\">", 
"<dcterms:title xml:lang=\"en-US\">EthoCore Recommended Terms</dcterms:title>", 
"<rdfs:comment>This document contains a list of EthoCore terms that have the dwcattributes:status equal to \"recommended\".</rdfs:comment>", 
"<dcterms:publisher xml:lang=\"en-US\">Ethoinformatics Core Team</dcterms:publisher>", 
"<dcterms:modified>2015-12-14</dcterms:modified>", "</rdf:Description>", 
"<!-- ", "  Each RDF description uses the following:", "    rdfs:label", 
"    rdfs:comment", "    dcterms:description", "    rdfs:isDefinedBy", 
"    dcterms:issued", "    dcterms:modified", "    rdf:type", 
"    dcterms:hasVersion", "    rdfs:range", "    rdfs:subPropertyOf", 
"    dcterms:replaces", "    dwcattributes:status", "    dwcattributes:decision", 
"    ", "    // potentially useful", "    rdfs:domain", "    skos = http://www.w3.org/2004/02/skos/core#", 
"    skos:example", "    vs = http://www.w3.org/2003/06/sw-vocab-status/ns#", 
"    vs:term_status", "-->", "<!-- Mutable RDF goes here -->"
)

ethocore.rdf.body <- apply(ethocore.rdf,1,function(x) {
	paste0(
		'<rdf:Description rdf:about="',x['identifier'],'"> ',
		'<rdfs:label xml:lang="en-US">',x['label'],'</rdfs:label>',
		'<rdfs:comment xml:lang="en-US">',x['definition'],'</rdfs:comment>',
		if (!is.na(x['description'])) {
			paste0('<dcterms:description xml:lang="en-US">',x['description'],'</dcterms:description>')
		} else { '' },
		'<rdfs:isDefinedBy rdf:resource="http://ethoinformatics.org/ethocore/"/>',
		'<dcterms:issued>',x['date_issued'],'</dcterms:issued>',
		'<dcterms:modified>',x['date_modified'],'</dcterms:modified>',
		'<rdf:type rdf:resource="',x['type'],'"/>',
		'<dcterms:hasVersion rdf:resource="',x['version'],'"/>',
		if (!is.na(x['refines'])) {
			paste0('<dcterms:replaces rdf:resource="',x['refines'],'"/>')
		} else { '' },
		'<dwcattributes:status>recommended</dwcattributes:status>',
		if (!is.na(x['class'])) {
			paste0('<dwcattributes:organizedInClass rdf:resource="',if (x['class'] %in% 'RecordLevel') 'all' else paste0('http://ethoinformatics.org/ethocore/',x['class']),'"/>')
		} else { '' },
		' </rdf:Description>'
	)
})

ethocore.rdf.tail <- c('<!-- end Mutable RDF -->','</rdf:RDF>')

ethocore.rdf <- c(ethocore.rdf.head,'',ethocore.rdf.body,'',ethocore.rdf.tail)

write(ethocore.rdf,file=paste0(write.filename,'.rdf'),sep='\n')
