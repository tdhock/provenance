org.places.RData: org.places.R
	R --no-save < $<
places.RData: places.R places.tsv
	R --no-save < $<

