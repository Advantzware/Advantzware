/* statecod.p - Generated 01/26/2000 -  1:33 pm by nosweat
"statecod. " ~
"ASI " ~
"statecod " ~
" " ~
"statecod " ~
"4 " ~
"19 " ~
"46 " ~
"statecod,description,fips_code " ~
"State Abbreviation,Description " ~
"yes " ~
"statecod,description " ~
"State Abbreviations Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"statecod. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file statecod
&Scoped-define where-statement TRUE
&Scoped-define return-field statecod
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields statecod.statecod statecod.description statecod.fips_code
&Scoped-define frame-title State Abbreviations Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname statecod.
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

&Global-define FORMAT-1 X(2)
&Scoped-define FLDNAME1 statecod.statecod
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 State Abbreviation
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 statecod.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
