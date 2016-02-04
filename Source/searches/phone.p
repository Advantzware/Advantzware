/* phone.p - Generated 05/05/1998 - 11:44 pm by Exitt
"phone. " ~
"NOSWEAT " ~
"phone " ~
"phone.table_rec_key = s-rec_key " ~
"attention " ~
"4 " ~
"17 " ~
"66 " ~
"attention,phone_ctry_code,phone_city_code,phone,phone_ext " ~
"Attention " ~
"yes " ~
"attention " ~
"attention " ~
"Phone Search " ~
" " ~
"{methods/defines/phone.i} " ~
" " ~
" " ~
*/
/* dgd 05/06/2007 - formatted for readability. */

&Scoped-define search-db          NOSWEAT.
&Scoped-define search-file        phone
&Scoped-define where-statement    phone.table_rec_key = s-rec_key
&Scoped-define return-field       attention
&Scoped-define font               4
&Scoped-define height-size        17
&Scoped-define width-size         66
&Scoped-define show-fields        phone.attention phone.phone_ctry_code phone.phone_city_code phone.phone phone.phone_ext
&Scoped-define frame-title        Phone Search
&Scoped-define search-text-row    18
&Scoped-define word-search-row    19
&Scoped-define btn-search-col     46
&Scoped-define top-include 
&Scoped-define def-include        ~{methods/defines/phone.i}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size        23
&Scoped-define window-col         42
&Scoped-define rect-1-row         20.15
&Scoped-define by-row             20.42
&Scoped-define browse-order-width 60
&Scoped-define browse-order-row   20.42
&Scoped-define btn-row            21.77
&Scoped-define btn-ok-col         53
&Scoped-define btn-cancel-col     42
&Scoped-define auto-find-row      22.85

&Global-define FORMAT-1           X(35)
&Scoped-define FLDNAME1           phone.attention
&Scoped-define DESCRIP1           Attention
&Scoped-define WORDFLD            phone.attention

{sys/inc/var.i new shared}
{methods/search.i}
