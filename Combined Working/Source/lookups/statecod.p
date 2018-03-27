/* statecod.p - Generated 10/17/2017 -  7:54 pm by NoSweat
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
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
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
&Scoped-define show-fields statecod.statecod statecod.description 
&Scoped-define show-fields-yellow statecod.statecod LABEL-BGCOLOR 14 statecod.description LABEL-BGCOLOR 14
&Scoped-define frame-title State Abbreviations Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname statecod.
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

&Global-define FORMAT-1 X(2)
&Scoped-define FLDNAME1 statecod.statecod
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 State Abbreviation
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 statecod.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
