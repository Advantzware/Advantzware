/* partnerGrp.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"edPartnerGrp. " ~
"ASI " ~
"edpartnerGrp " ~
"TRUE " ~
"partnerGrp " ~
"4 " ~
"19 " ~
"34 " ~
"partnerGrp,partnerGrpName " ~
"Name,Description " ~
"yes " ~
"partnerGrp,partnerGrpName " ~
"EDI Partner Group Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"edPartnerGrp. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file edPartnerGrp
&Scoped-define where-statement true
&Scoped-define return-field partnerGrp
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 34
&Scoped-define show-fields edPartnerGrp.partnerGrp edPartnerGrp.partnerGrpName
&Scoped-define show-fields-yellow edPartnerGrp.partnerGrp LABEL-BGCOLOR 14 edPartnerGrp.partnerGrpName LABEL-BGCOLOR 14
&Scoped-define frame-title Customer Types Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname edPartnerGrp.
&Scoped-define window-size 24
&Scoped-define window-col 58
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 28
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 27
&Scoped-define btn-cancel-col 20
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 edPartnerGrp.partnerGrp
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Type
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 edPartnerGrp.partnerGrpName
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
