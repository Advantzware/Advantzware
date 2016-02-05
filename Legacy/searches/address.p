/* address.p - Generated 05/07/1998 - 12:01 am by Exitt
"address. " ~
"NOSWEAT " ~
"address " ~
"address.table_rec_key = s-rec_key " ~
"zipcode " ~
"4 " ~
"17 " ~
"46 " ~
"zipcode,address1 " ~
"Zip/Postal Code,Address Line 1 " ~
"yes " ~
"zipcode,address1 " ~
"address1 " ~
"Address Search " ~
" " ~
"{methods/defines/address.i} " ~
" " ~
" " ~
*/

&Scoped-define search-db NOSWEAT.
&Scoped-define search-file address
&Scoped-define where-statement address.table_rec_key = s-rec_key
&Scoped-define return-field zipcode
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 46
&Scoped-define show-fields address.zipcode address.address1
&Scoped-define frame-title Address Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 26
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{methods/defines/address.i} ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(10)
&Scoped-define FLDNAME1 address.zipcode
&Scoped-define DESCRIP1 Zip/Postal Code
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 address.address1
&Scoped-define DESCRIP2 Address Line 1
&Scoped-define WORDFLD address.address1

{methods/search.i}
