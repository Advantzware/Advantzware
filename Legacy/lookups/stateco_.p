/* stateco_.p - Generated 08/06/2003 -  5:55 pm by nosweat
"stateco_. " ~
"NOSWEAT " ~
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
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"statecod. " ~
*/

&Scoped-define lookup-db NOSWEAT.
&Scoped-define lookup-file statecod
&Scoped-define where-statement TRUE
&Scoped-define return-field description
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields statecod.description statecod.statecod
&Scoped-define frame-title State Name Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
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

&Global-define FORMAT-1 X(30)
&Scoped-define FLDNAME1 statecod.description
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 X(2)
&Scoped-define FLDNAME2 statecod.statecod
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 State Abbreviation

{methods/lookup.i}
