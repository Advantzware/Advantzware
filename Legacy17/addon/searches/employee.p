/* employee.p - Generated 07/11/2000 -  5:25 pm by NoSweat
"employee. " ~
"EMPTRACK " ~
"employee " ~
"employee.company = gcompany " ~
"employee " ~
"2 " ~
"17 " ~
"67 " ~
"employee,last_name,first_name,soc_sec " ~
"Emp ID,Last Name,Social Security # " ~
"yes " ~
"employee,last_name,soc_sec " ~
"last_name " ~
"Employees Search " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"employee. " ~
*/

&Scoped-define search-db 
&Scoped-define search-file employee
&Scoped-define where-statement employee.company = gcompany
&Scoped-define return-field employee
&Scoped-define font 2
&Scoped-define height-size 17
&Scoped-define width-size 67
&Scoped-define show-fields employee.employee employee.last_name employee.first_name employee.soc_sec
&Scoped-define frame-title Employees Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 47
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} 
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname employee.
&Scoped-define window-size 23
&Scoped-define window-col 41.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 61
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 54
&Scoped-define btn-cancel-col 43
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 employee.employee
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Emp ID
&Global-define FORMAT-2 x(15)
&Scoped-define FLDNAME2 employee.last_name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Last Name
&Global-define FORMAT-3 XXX-XX-XXXX
&Scoped-define FLDNAME3 employee.soc_sec
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Social Security #
&Scoped-define WORDFLD last_name

{methods/search.i}
