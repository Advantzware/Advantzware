/* machchrg.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"machchrg. " ~
"ASI " ~
"machchrg " ~
"machchrg.company = gcompany AND machchrg.machine = s-machine " ~
"charge_code " ~
"2 " ~
"19 " ~
"35 " ~
"charge_code " ~
"Charge Code " ~
"yes " ~
"charge_code " ~
"Machine Assigned Charge Codes Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} ~{methods/defines/machine.i} " ~
" " ~
"machchrg. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file machchrg
&Scoped-define where-statement machchrg.company = gcompany AND machchrg.machine = s-machine
&Scoped-define return-field charge_code
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 35
&Scoped-define show-fields machchrg.charge_code
&Scoped-define show-fields-yellow machchrg.charge_code LABEL-BGCOLOR 14
&Scoped-define frame-title Machine Assigned Charge Codes Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared} ~{methods/defines/machine.i}
&Scoped-define end-include 
&Scoped-define ui-prgmname machchrg.
&Scoped-define window-size 24
&Scoped-define window-col 57.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 29
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 28
&Scoped-define btn-cancel-col 21
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 X(5)
&Scoped-define FLDNAME1 machchrg.charge_code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Charge Code

{methods/lookup.i}
