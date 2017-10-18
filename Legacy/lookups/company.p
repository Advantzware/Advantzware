/* company.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"company. " ~
"ASI " ~
"company " ~
" " ~
"company " ~
"4 " ~
"19 " ~
"60 " ~
"company,name,curr-code " ~
"Company,Company Name " ~
"yes " ~
"company,name " ~
"Company Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"company. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file company
&Scoped-define where-statement TRUE
&Scoped-define return-field company
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 60
&Scoped-define show-fields company.company company.name company.curr-code
&Scoped-define show-fields-yellow company.company LABEL-BGCOLOR 14 company.name LABEL-BGCOLOR 14 company.curr-code LABEL-BGCOLOR 14
&Scoped-define frame-title Company Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname company.
&Scoped-define window-size 24
&Scoped-define window-col 45
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 54
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 53
&Scoped-define btn-cancel-col 46
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 company.company
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Company
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 company.name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Company Name

{methods/lookup.i}
