/* users.p - Generated 05/02/1998 -  8:42 pm by Exitt
"users. " ~
"NOSWEAT " ~
"users " ~
" " ~
"user_id " ~
"4 " ~
"17 " ~
"46 " ~
"user_id,user_name " ~
"User ID's,User Name " ~
"yes " ~
"user_id,user_name " ~
"user_name " ~
"Users Search " ~
" " ~
" " ~
" " ~
"users. " ~
*/

&Scoped-define search-db NOSWEAT.
&Scoped-define search-file users
&Scoped-define where-statement TRUE
&Scoped-define return-field user_id
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 46
&Scoped-define show-fields users.user_id users.user_name
&Scoped-define frame-title Users Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 26
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname users.
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

&Global-define FORMAT-1 X(8)
&Scoped-define FLDNAME1 users.user_id
&Scoped-define DESCRIP1 User ID's
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 users.user_name
&Scoped-define DESCRIP2 User Name
&Scoped-define WORDFLD users.user_name

{methods/search.i}
