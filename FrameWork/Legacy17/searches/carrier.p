/* carrier.p - Generated 01/13/2000 -  9:40 am by nosweat
"carrier. " ~
"ASI " ~
"carrier " ~
"carrier.company = gcompany " ~
"carrier " ~
"4 " ~
"17 " ~
"51 " ~
"carrier,dscr,loc " ~
"Carrier,Company " ~
"yes " ~
"carrier,company " ~
"dscr " ~
"Common Carriers Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"carrier. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file carrier
&Scoped-define where-statement carrier.company = gcompany
&Scoped-define return-field carrier
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 51
&Scoped-define show-fields carrier.carrier carrier.dscr carrier.loc
&Scoped-define frame-title Common Carriers Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 31
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname carrier.
&Scoped-define window-size 23
&Scoped-define window-col 49.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 45
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 38
&Scoped-define btn-cancel-col 27
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 carrier.carrier
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Carrier
&Global-define FORMAT-2 x(3)
&Scoped-define FLDNAME2 carrier.company
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Company
&Scoped-define WORDFLD dscr

{methods/search.i}
