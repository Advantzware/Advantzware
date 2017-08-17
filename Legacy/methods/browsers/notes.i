/* notes.i
"NOSWEAT" "notes"
"note_date" "Note Date" ""
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
"" "" ""
"" "" ""
"yes" "no" "no"
"no"
*/

&Global-define FLDNAME1 notes.note_date 
&Global-define DATATYP1 DATE
&Global-define FORMAT-1 99/99/9999
&Global-define SORTBY-1 BY {&FLDNAME1} DESCENDING /*BY notes.note_group*/ BY notes.note_code
&Global-define IDXNAME1 
&Global-define DESCRIP1 Note Date
&Global-define FLDNAME2 notes.note_group
&Global-define FORMAT-2 X(20)
&Global-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Global-define IDXNAME2 
&Global-define DESCRIP2 Group
&Global-define FLDNAME3 notes.note_code
&Global-define FORMAT-3 X(2)
&Global-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-1}
&Global-define IDXNAME3 
&Global-define DESCRIP3 Dept/Spec
&Global-define FLDNAME4 
&Global-define SORTBY-4 
&Global-define IDXNAME4 
&Global-define DESCRIP4 
&Global-define FLDNAME5 
&Global-define SORTBY-5 
&Global-define IDXNAME5 
&Global-define DESCRIP5 
&Global-define FLDNAME6 
&Global-define SORTBY-6 
&Global-define IDXNAME6 
&Global-define DESCRIP6 
&Global-define FLDNAME7 
&Global-define SORTBY-7 
&Global-define IDXNAME7 
&Global-define DESCRIP7 
&Global-define FLDNAME8 
&Global-define SORTBY-8 
&Global-define IDXNAME8 
&Global-define DESCRIP8 
&Global-define FLDNAME9 
&Global-define SORTBY-9 
&Global-define IDXNAME9 
&Global-define DESCRIP9 
&Global-define FLDNAME10 
&Global-define SORTBY-10 
&Global-define IDXNAME10 
&Global-define DESCRIP10 
&Global-define FLDNAME11 
&Global-define SORTBY-11 
&Global-define IDXNAME11 
&Global-define DESCRIP11 
&Global-define FLDNAME12 
&Global-define SORTBY-12 
&Global-define IDXNAME12 
&Global-define DESCRIP12 
&Global-define FLDNAME13 
&Global-define SORTBY-13 
&Global-define IDXNAME13 
&Global-define DESCRIP13 
&Global-define ENHANCE YES
