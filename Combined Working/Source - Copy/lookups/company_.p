/* company_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"company_. " ~
"ASI " ~
"company " ~
" " ~
"name " ~
"4 " ~
"19 " ~
"45 " ~
"name,company " ~
"Company Name,Company " ~
"yes " ~
"name,company " ~
"Company Description Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"company. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file company
&Scoped-define where-statement TRUE
&Scoped-define return-field name
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 45
&Scoped-define show-fields company.name company.company
&Scoped-define show-fields-yellow company.name LABEL-BGCOLOR 14 company.company LABEL-BGCOLOR 14
&Scoped-define frame-title Company Description Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname company.
&Scoped-define window-size 24
&Scoped-define window-col 52.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 39
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 38
&Scoped-define btn-cancel-col 31
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 company.name
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Company Name
&Global-define FORMAT-2 x(3)
&Scoped-define FLDNAME2 company.company
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Company

{methods/lookup.i}
