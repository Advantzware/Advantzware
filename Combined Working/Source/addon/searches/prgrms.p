/* prgrms.p - Generated 09/02/1999 -  4:02 pm by nosweat
"prgrms. " ~
"NOSWEAT " ~
"prgrms " ~
" " ~
"prgmname " ~
"4 " ~
"17 " ~
"46 " ~
"prgmname,prgtitle " ~
"Title,Program " ~
"yes " ~
"prgtitle,prgmname " ~
"prgtitle " ~
"Program Master Search " ~
" ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
" ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"prgrms. " ~
*/

&Scoped-define search-db NOSWEAT.
&Scoped-define search-file prgrms
&Scoped-define where-statement TRUE
&Scoped-define return-field prgmname
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 46
&Scoped-define show-fields prgrms.prgmname prgrms.prgtitle
&Scoped-define frame-title Program Master Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 26
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} 
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname prgrms.
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(30)
&Scoped-define FLDNAME1 prgrms.prgtitle
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Title
&Global-define FORMAT-2 X(10)
&Scoped-define FLDNAME2 prgrms.prgmname
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Program
&Scoped-define WORDFLD prgtitle

{methods/search.i}
