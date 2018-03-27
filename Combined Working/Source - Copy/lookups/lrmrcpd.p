/* lrmrcpd.p - Generated 10/17/2017 -  7:54 pm by NoSweat
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
&Scoped-define where-statement TRUE
&Scoped-define return-field job-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name
&Scoped-define show-fields-yellow rm-rctd.job-no LABEL-BGCOLOR 14 rm-rctd.job-no2 LABEL-BGCOLOR 14 rm-rctd.i-no LABEL-BGCOLOR 14 rm-rctd.i-name LABEL-BGCOLOR 14
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
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 39
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 23.65


{methods/lookup.i}
