# Sample .st file for creation of combined Advantzware database with Type II storage areas
#
# Specify a single Primary Recovery Area consisting of one fixed and one
# variable length extent.  They reside in the user-specified database directory
#
b . f 4000000
b . 
#
# The following defines the Schema Area consisting of one fixed and one
# variable length # extent, residing in the user-specified db directory:
#
d "Schema Area":6,64 . f 2000000
d "Schema Area":6,64 .
#
# The following defines two After Image Areas each equal to
# 2+ GB in size (supports After Imaging):
#
a . f 2000000
a . 
#
# The following defines one Application Data Area with multiple fixed
# length extents equal to just under 8 GB in size and 1 variable length extent:
#
d "Data":7,64;64 . f 4000000
d "Data":7,64;64 .
#
# The following defines one Application Data Areas with multiple fixed
# length extents equal to just under 2 GB in size and 1 variable length extent
# to be used for index storage:
#
d "Index":8,32;8 . f 4000000
d "Index":8,32;8 .
#


