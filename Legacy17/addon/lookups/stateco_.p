/* stateco_.p - Generated 03/24/1998 - 11:45 pm by Exitt
"stateco_. " ~
"ASI " ~
"statecod " ~
" " ~
"description " ~
"4 " ~
"19 " ~
"46 " ~
"description,statecod " ~
"Description,State Abbreviation " ~
"yes " ~
"description,statecod " ~
"State Name Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"statecod. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file statecod
&Scoped-define where-statement TRUE
&Scoped-define return-field description
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields statecod.description statecod.statecod
&Scoped-define frame-title State Name Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname statecod.
&Scoped-define window-size 24.7
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 39
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 23.6

&Global-define FORMAT-1 X(30)
&Scoped-define FLDNAME1 statecod.description
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 X(2)
&Scoped-define FLDNAME2 statecod.statecod
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 State Abbreviation

{methods/lookup.i}
