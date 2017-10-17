/* job.p - Generated 12/31/2008 - 11:46 am by nosweat
"job. " ~
" " ~
" " ~
" " ~
" " ~
"4 " ~
"19 " ~
"46 " ~
"? " ~
" " ~
"yes " ~
"? " ~
" " ~
" " ~
" " ~
" " ~
" " ~
*/

DEFINE INPUT-OUTPUT PARAMETER m-lookup-var AS CHARACTER.

&Scoped-define lookup-db  asi.
&Scoped-define lookup-file job
&Scoped-define where-statement job.company eq cocode
&Scoped-define return-field job-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields job.job-no
&Scoped-define show-fields-yellow 
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname job-no
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
