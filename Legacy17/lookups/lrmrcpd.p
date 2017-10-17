/* lrmrcpd.p - Generated 08/05/2002 -  4:53 pm by nosweat
"lrmrcpd. " ~
"ASI " ~
"rm-rctd " ~
" " ~
"job-no " ~
"4 " ~
"19 " ~
"46 " ~
"job-no,job-no2,i-no,i-name " ~
" " ~
"yes " ~
"? " ~
" " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"rm-rctd. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file rm-rctd
&Scoped-define where-statement rm-rctd.company eq cocode
&Scoped-define return-field job-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name
&Scoped-define frame-title 
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname rm-rctd.
&Scoped-define window-size 24
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 23.65


{methods/lookup.i}
