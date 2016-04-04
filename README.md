Turns entries in one text file into multiple posts.

If in.txt has the following:

    # A
	## A1
	## A2
	# B
	## B1
	# A
	## A3

then running `Postize in.txt C:/Folder` will create files `C:/Folder/a.md` with

    # A1
	# A2
	# A3

and `C:/Folder/b.md` with

	# B1

and each with a header with metadata.

I.e., it turns headers into files, concatenates files that are split up, and promotes headers.I use this for taking 750word entries (e.g., the monthly digest) and breaking them up into posts.
