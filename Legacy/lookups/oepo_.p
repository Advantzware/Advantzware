/* oepo_.p - Generated 11/24/2003 -  3:48 pm by nosweat
"oepo_. " ~
"asi " ~
"oe-rel " ~
"po-no.company=gcompany " ~
"cust-no " ~
"4 " ~
"19 " ~
"46 " ~
"cust-no,po-no,ship-date " ~
"? " ~
"yes " ~
"cust-no,po-no,ship-date " ~
"Order PO# " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"oepo. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file oe-rel
&Scoped-define where-statement oe-rel.company = gcompany
&Scoped-define return-field po-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields oe-rel.cust-no oe-rel.po-no oe-rel.ship-date
&Scoped-define frame-title Order PO#
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname oepo.
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 22.85


{methods/lookup.i}
