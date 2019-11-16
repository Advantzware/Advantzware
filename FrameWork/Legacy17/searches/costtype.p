/* costtype.p - Generated 01/19/2000 -  8:28 pm by nosweat
"costtype. " ~
"ASI " ~
"costtype " ~
"costtype.company = gcompany AND costtype.loc = gloc " ~
"cost-type " ~
"4 " ~
"17 " ~
"45 " ~
"cost-type,descr " ~
"Cost Type,Description " ~
"yes " ~
"cost-type,descr " ~
"descr " ~
"Material Cost Types Search " ~
"{custom/getcmpny.i} ~{custom/getloc.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} " ~
" " ~
"costtype. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file costtype
&Scoped-define where-statement costtype.company = gcompany AND costtype.loc = gloc
&Scoped-define return-field cost-type
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 45
&Scoped-define show-fields costtype.cost-type costtype.descr
&Scoped-define frame-title Material Cost Types Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 25
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname costtype.
&Scoped-define window-size 23
&Scoped-define window-col 52.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 39
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 32
&Scoped-define btn-cancel-col 21
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 XXX
&Scoped-define FLDNAME1 costtype.cost-type
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Cost Type
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 costtype.descr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD descr

{methods/search.i}
