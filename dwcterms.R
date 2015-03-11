#!/usr/bin/Rscript

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# 
# This script converts the latest list of Darwin Core from RDF to CSV.
# 
# Before beginning, dwcterms.rdf must be downloaded from the TDWG website
# 
# Set the working directory to the location of dwcterms.rdf, or change the path on line 10
#
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

library(XML,quietly=TRUE)

setwd('~/Desktop/')

rdfterms <- xmlRoot(xmlParse('dwcterms.rdf'))

which.terms <- which(names(xmlChildren(rdfterms)) %in% 'Description')

## Don't need the first Description tag
which.terms <- which.terms[2:length(which.terms)]

dwcterms <- data.frame(
	name = gsub('.*?/([A-z]*)$','\\1',as.character(do.call(c,lapply(xmlChildren(rdfterms)[which.terms],function(x) xmlAttrs(x)[['about']])))),
	uri = as.character(do.call(c,lapply(xmlChildren(rdfterms),function(x) xmlGetAttr(x,'rdf:about'))[which.terms])),
	version = as.character(do.call(c,lapply(lapply(xmlChildren(rdfterms),function(x) xmlChildren(x)$hasVersion)[which.terms],function(y) xmlGetAttr(y,'rdf:resource')))),
	label = as.character(do.call(c,lapply(lapply(xmlChildren(rdfterms),function(x) xmlChildren(x)$label)[which.terms],function(y) as(xmlChildren(y)$text,'character')))),
	definition = as.character(do.call(c,lapply(lapply(xmlChildren(rdfterms),function(x) xmlChildren(x)$comment)[which.terms],function(y) as(xmlChildren(y)$text,'character')))),
	description = as.character(do.call(c,lapply(lapply(xmlChildren(rdfterms),function(x) xmlChildren(x)$description)[which.terms],function(y) if (is.null(y)) '' else as(xmlChildren(y)$text,'character')))),
	issued = as.character(do.call(c,lapply(lapply(xmlChildren(rdfterms),function(x) xmlChildren(x)$issued)[which.terms],function(y) if (is.null(y)) '' else as(xmlChildren(y)$text,'character')))),
	modified = as.character(do.call(c,lapply(lapply(xmlChildren(rdfterms),function(x) xmlChildren(x)$modified)[which.terms],function(y) if (is.null(y)) '' else as(xmlChildren(y)$text,'character')))),
	status = as.character(do.call(c,lapply(lapply(xmlChildren(rdfterms),function(x) xmlChildren(x)$status)[which.terms],function(y) if (is.null(y)) '' else as(xmlChildren(y)$text,'character')))),
	abcdEquivalence = as.character(do.call(c,lapply(lapply(xmlChildren(rdfterms),function(x) xmlChildren(x)$abcdEquivalence)[which.terms],function(y) if (is.null(y)) '' else as(xmlChildren(y)$text,'character')))),
	stringsAsFactors=FALSE
)

write.csv(dwcterms,file='dwcterms.csv')
