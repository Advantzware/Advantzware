/* users.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"users. " ~
"ASI " ~
"users " ~
" " ~
"user_id " ~
"4 " ~
"19 " ~
"44 " ~
"user_id,user_name " ~
"User ID's,User Name " ~
"yes " ~
"user_id,user_name " ~
"User ID Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"users. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file users
&Scoped-define where-statement TRUE
&Scoped-define return-field user_id
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 44
&Scoped-define show-fields users.user_id users.user_name
&Scoped-define show-fields-yellow users.user_id LABEL-BGCOLOR 14 users.user_name LABEL-BGCOLOR 14
&Scoped-define frame-title User ID Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname users.
&Scoped-define window-size 24
&Scoped-define window-col 53
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 38
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 30
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 X(8)
&Scoped-define FLDNAME1 users.user_id
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 User ID's
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 users.user_name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 User Name

{methods/lookup.i}
