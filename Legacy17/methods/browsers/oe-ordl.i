/* oe-ordl.i
"ASI" "oe-ordl"
"i-no" "Item#" ""
"i-name" "Name" ""
"line" "Line#" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"yes" "no" "no"
"no"
*/

&Global-define FLDNAME1 oe-ordl.i-no
&Global-define FORMAT-1 x(15)
&Global-define SORTBY-1 BY oe-ordl.i-no
&Global-define IDXNAME1 
&Global-define DESCRIP1 Item#
&Global-define FLDNAME2 oe-ordl.i-name
&Global-define FORMAT-2 x(30)
&Global-define SORTBY-2 BY oe-ordl.i-name {&SORTBY-1}
&Global-define IDXNAME2 
&Global-define DESCRIP2 Name
&Global-define FLDNAME3 oe-ordl.line
&Global-define DATATYP3 INTEGER
&Global-define FORMAT-3 >>>
&Global-define SORTBY-3 BY oe-ordl.line {&SORTBY-1}
&Global-define IDXNAME3 
&Global-define DESCRIP3 Line#
&Global-define ENHANCE yes
