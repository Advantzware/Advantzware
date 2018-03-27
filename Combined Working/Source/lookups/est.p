/* est.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"est. " ~
"ASI " ~
"est " ~
"est.company EQ gcompany AND est.loc EQ gloc " ~
"est-no " ~
"4 " ~
"19 " ~
"46 " ~
"est-no,est-date,est-type " ~
"Estimate #,Est Date,Type " ~
"yes " ~
"est-no,est-date,est-type " ~
"Estimate Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file est
&Scoped-define where-statement est.company EQ gcompany AND est.loc EQ gloc
&Scoped-define return-field est-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields est.est-no est.est-date est.est-type
&Scoped-define show-fields-yellow est.est-no LABEL-BGCOLOR 14 est.est-date LABEL-BGCOLOR 14 est.est-type LABEL-BGCOLOR 14
&Scoped-define frame-title Estimate Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 39
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 est.est-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Estimate #
&Global-define DATATYP2 DATE
&Global-define FORMAT-2 99/99/9999
&Scoped-define FLDNAME2 est.est-date
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Est Date
&Global-define DATATYP3 INTEGER
&Global-define FORMAT-3 9
&Scoped-define FLDNAME3 est.est-type
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Type

{methods/lookup.i}
