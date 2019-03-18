/* taskParam.p - Generated 12/12/2018 - 12:32 am by NoSweat
"taskParam. " ~
"ASI " ~
"bUserPrint " ~
"bUserPrint.company EQ gcompany AND bUserPrint.batch-seq GT 0 AND bUserPrint.batch EQ 'Batch' AND bUserPrint.prgmName EQ 'Jasper' " ~
"batch-seq " ~
"4 " ~
"19 " ~
"46 " ~
"batch-seq,program-id " ~
"Task ID,Program ID " ~
"no " ~
"batch-seq,program-id " ~
"Tasks Parameters " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"DEFINE BUFFER bUserPrint FOR user-print. ~{custom/gcompany.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file bUserPrint
&Scoped-define where-statement bUserPrint.company EQ gcompany AND bUserPrint.batch-seq GT 0 AND bUserPrint.batch EQ 'Batch' AND bUserPrint.prgmName EQ 'Jasper'
&Scoped-define return-field batch-seq
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields bUserPrint.batch-seq bUserPrint.program-id
&Scoped-define show-fields-yellow bUserPrint.batch-seq LABEL-BGCOLOR 14 bUserPrint.program-id LABEL-BGCOLOR 14
&Scoped-define frame-title Tasks Parameters
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include DEFINE BUFFER bUserPrint FOR user-print. ~{custom/gcompany.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
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

&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 ->,>>>,>>9
&Scoped-define FLDNAME1 bUserPrint.batch-seq
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Task ID
&Global-define FORMAT-2 X(20)
&Scoped-define FLDNAME2 bUserPrint.program-id
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Program ID

{methods/lookup.i}
