/* statecod.p - Generated 06/17/1998 - 12:16 pm by NOSWEAT
"statecod. " ~
"NOSWEAT " ~
"statecod " ~
" " ~
"statecod " ~
"4 " ~
"17 " ~
"46 " ~
"statecod,description,fips_code " ~
"State Abbreviation,Description " ~
"yes " ~
"statecod,description,fips_code " ~
"description " ~
"State Abbreviations Search " ~
" ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
" ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"statecod. " ~
*/

&Scoped-define search-db NOSWEAT.
&Scoped-define search-file statecod
&Scoped-define where-statement TRUE
&Scoped-define return-field statecod
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 46
&Scoped-define show-fields statecod.statecod statecod.description statecod.fips_code
&Scoped-define frame-title State Abbreviations Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 26
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
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(2)
&Scoped-define FLDNAME1 statecod.statecod
&Scoped-define DESCRIP1 State Abbreviation
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 statecod.description
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD statecod.description

{methods/search.i}
