/* itemfg.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"itemfg. " ~
"ASI " ~
"itemfg " ~
"itemfg.company eq cocode " ~
"i-no " ~
"2 " ~
"19 " ~
"90 " ~
"i-no,i-name,i-dscr " ~
"Item No,Name,Desc " ~
"yes " ~
"i-no,i-name,i-dscr " ~
" " ~
"!{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"~{custom/gcompany.i} ~{sys/inc/var.i new shared} ~{fg/fgvars.i} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file itemfg
&Scoped-define where-statement itemfg.company eq cocode
&Scoped-define return-field i-no
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 115
&Scoped-define show-fields itemfg.i-no WIDTH 25 itemfg.i-name itemfg.part-dscr1
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i} 
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared} ~{fg/fgvars.i}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col 30
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 84
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 83
&Scoped-define btn-cancel-col 76
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 itemfg.i-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Item No
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 itemfg.i-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name
&Global-define FORMAT-3 x(30)
&Scoped-define FLDNAME3 itemfg.part-dscr1
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Desc
&SCOPED-DEFINE useMatches YES

{methods/lookup.i}
