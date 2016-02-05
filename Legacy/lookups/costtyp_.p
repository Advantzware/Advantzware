/* costtyp_.p - Generated 01/19/2000 -  8:49 pm by nosweat
"costtyp_. " ~
"ASI " ~
"costtype " ~
"costtype.company = gcompany AND costtype.loc = gloc " ~
"descr " ~
"4 " ~
"19 " ~
"45 " ~
"descr,cost-type " ~
"Description,Cost Type " ~
"yes " ~
"descr,cost-type " ~
"Material Cost Types Description Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
"costtype. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file costtype
&Scoped-define where-statement costtype.company = gcompany AND costtype.loc = gloc
&Scoped-define return-field descr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 45
&Scoped-define show-fields costtype.descr costtype.cost-type
&Scoped-define frame-title Material Cost Types Description Lookup
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
&Scoped-define btn-ok-col 36
&Scoped-define btn-cancel-col 25
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 costtype.descr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 X(3)
&Scoped-define FLDNAME2 costtype.cost-type
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Cost Type

{methods/lookup.i}
