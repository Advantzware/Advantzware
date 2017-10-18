/* employee.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"employee. " ~
"ASI " ~
"employee " ~
"employee.company = gcompany " ~
"employee " ~
"2 " ~
"19 " ~
"50 " ~
"employee,last_name,first_name " ~
"Emp ID,Last Name " ~
"yes " ~
"employee,last_name " ~
"Employees Lookup " ~
"{custom/getcmpny.i}~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i}~{sys/inc/var.i new shared} " ~
" " ~
"employee. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file employee
&Scoped-define where-statement employee.company = gcompany
&Scoped-define return-field employee
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 50
&Scoped-define show-fields employee.employee employee.last_name employee.first_name
&Scoped-define show-fields-yellow employee.employee LABEL-BGCOLOR 14 employee.last_name LABEL-BGCOLOR 14 employee.first_name LABEL-BGCOLOR 14
&Scoped-define frame-title Employees Lookup
&Scoped-define top-include ~{custom/getcmpny.i}~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i}~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname employee.
&Scoped-define window-size 24
&Scoped-define window-col 50
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 44
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 43
&Scoped-define btn-cancel-col 36
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 employee.employee
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Emp ID
&Global-define FORMAT-2 x(15)
&Scoped-define FLDNAME2 employee.last_name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Last Name

{methods/lookup.i}
