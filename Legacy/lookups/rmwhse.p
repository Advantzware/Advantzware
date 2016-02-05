/* rmwhse.p - Generated 08/06/2003 -  9:09 am by nosweat
"rmwhse. " ~
"asi " ~
"loc " ~
" " ~
"loc " ~
"4 " ~
"19 " ~
"46 " ~
"loc " ~
"Location " ~
"yes " ~
"loc " ~
"Locations and Bins Lookup " ~
"~{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"~{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"loc. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file loc
&Scoped-define where-statement loc.company eq cocode
&Scoped-define return-field loc
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields loc.loc
&Scoped-define frame-title Locations and Bins Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname loc.
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

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 loc.loc
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Location

{methods/lookup.i}
