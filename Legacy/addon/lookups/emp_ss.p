/* emp_ss.p - Generated 03/30/2000 - 10:32 am by nosweat
"emp_ss. " ~
"EMPTRACK " ~
"employee " ~
" " ~
"soc_sec " ~
"2 " ~
"19 " ~
"67 " ~
"soc_sec,last_name,first_name,employee " ~
"Social Security #,Last Name,Emp ID " ~
"yes " ~
"soc_sec,last_name,employee " ~
"Employees Social Security # Lookup " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"employee. " ~
*/

&Scoped-define lookup-db 
&Scoped-define lookup-file employee
&Scoped-define where-statement TRUE
&Scoped-define return-field soc_sec
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 67
&Scoped-define show-fields employee.soc_sec employee.last_name employee.first_name employee.employee
&Scoped-define frame-title Employees Social Security # Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname employee.
&Scoped-define window-size 23
&Scoped-define window-col 41.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 61
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 58
&Scoped-define btn-cancel-col 47
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 XXX-XX-XXXX
&Scoped-define FLDNAME1 employee.soc_sec
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Social Security #
&Global-define FORMAT-2 x(15)
&Scoped-define FLDNAME2 employee.last_name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Last Name
&Global-define FORMAT-3 x(5)
&Scoped-define FLDNAME3 employee.employee
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Emp ID

{methods/lookup.i}
