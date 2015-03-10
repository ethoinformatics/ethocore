# EthoCore quick reference guide

This project uses an R script to convert the latest EthoCore table of terms into a quick reference document similar to the [one published by Darwin Core](http://rs.tdwg.org/dwc/terms/).

For the R script, the working directory on line 5 would need to be updated. Also, a tab-separated version of the first sheet of the latest EthoCore Google Drive document should be saved to the working directory as "ethocore.txt".

The resulting markdown file can be easily converted to html using [pandoc](http://johnmacfarlane.net/pandoc/)

`pandoc ethocore.md -o ethocore.html`
