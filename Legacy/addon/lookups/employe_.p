/* employe_.p - Generated 07/31/2000 -  4:25 pm by NoSweat
"employe_. " ~
"ASI " ~
"employee " ~
"employee.company = gcompany " ~
"last_name " ~
"2 " ~
"19 " ~
"50 " ~
"last_name,first_name,employee " ~
"Last Name,Emp ID " ~
"yes " ~
"last_name,employee " ~
"Employees Name Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i}" ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"employee. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file employee
&Scoped-define where-statement employee.company = gcompany
&Scoped-define return-field last_name
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 50
&Scoped-define show-fields employee.last_name employee.first_name employee.employee
&Scoped-define frame-title Employees Name Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname employee.
&Scoped-define window-size 23
&Scoped-define window-col 50
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 44
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 41
&Scoped-define btn-cancel-col 30
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 employee.last_name
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Last Name
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 employee.employee
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Emp ID

{methods/lookup.i}
