/* srchlkup.p - Generated 02/13/1998 -  4:25 pm by Exitt
"srchlkup. " ~
"NOSWEAT " ~
"prgrms " ~
"INDEX(prgrms.prgmname,'.') NE 0 AND prgrms.dir_group = 'windows' AND SEARCH('searches/' + prgrms.prgmname + 'p') NE ? " ~
"prgmname " ~
"4 " ~
"19 " ~
"46 " ~
"prgmname,prgtitle " ~
"Program,Title " ~
"yes " ~
"prgmname,prgtitle " ~
"Search Programs Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db NOSWEAT.
&Scoped-define lookup-file prgrms
&Scoped-define where-statement INDEX(prgrms.prgmname,'.') NE 0 AND prgrms.dir_group = 'windows' AND SEARCH('searches/' + prgrms.prgmname + 'p') NE ?
&Scoped-define return-field prgmname
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields prgrms.prgmname prgrms.prgtitle
&Scoped-define frame-title Search Programs Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(10)
&Scoped-define FLDNAME1 prgrms.prgmname
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Program
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 prgrms.prgtitle
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Title

{methods/lookup.i}
