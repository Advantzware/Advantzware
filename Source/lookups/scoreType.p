/* scoreType.p - Generated 07/26/2018 -  3:22 pm by NoSweat
"scoreType. " ~
"ASI " ~
"scoreType " ~
" " ~
"scoreType " ~
"4 " ~
"19 " ~
"77 " ~
"scoreType,description " ~
"Score Type,Description " ~
"yes " ~
"scoreType,description " ~
"Score Types " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"scoreType. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file scoreType
&Scoped-define where-statement TRUE
&Scoped-define return-field scoreType
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 77
&Scoped-define show-fields scoreType.scoreType scoreType.description
&Scoped-define show-fields-yellow scoreType.scoreType LABEL-BGCOLOR 14 scoreType.description LABEL-BGCOLOR 14
&Scoped-define frame-title Score Types
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname scoreType.
&Scoped-define window-size 24
&Scoped-define window-col 36.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 71
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 68
&Scoped-define btn-cancel-col 57
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 scoreType.scoreType
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Score Type
&Global-define FORMAT-2 x(60)
&Scoped-define FLDNAME2 scoreType.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
