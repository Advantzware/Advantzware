/* oepono.p - Generated 11/24/2003 -  6:31 pm by nosweat
"oepono. " ~
"asi " ~
"oe-ordl " ~
"cust-no.company=gcompany " ~
"cust-no " ~
"4 " ~
"19 " ~
"46 " ~
"po-no,ord-no,cust-no " ~
"Cust PO#,Order#,Customer " ~
"yes " ~
"po-no,ord-no,cust-no " ~
"Customer PO# " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file oe-ordl
&Scoped-define where-statement oe-ordl.company eq cocode
&Scoped-define return-field po-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields oe-ordl.po-no oe-ordl.ord-no oe-ordl.cust-no
&Scoped-define frame-title LOOKUP
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname oepono.
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

&Global-define FORMAT-1 x(20)
&Scoped-define FLDNAME1 oe-ordl.po-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Cust PO#
&Global-define DATATYP2 INTEGER
&Global-define FORMAT-2 >>>>>9
&Scoped-define FLDNAME2 oe-ordl.ord-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Order#
&Global-define FORMAT-3 x(8)
&Scoped-define FLDNAME3 oe-ordl.cust-no
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Customer

{methods/lookup.i}
