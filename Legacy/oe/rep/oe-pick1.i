
{sys/inc/var.i {1} shared}
{sys/form/s-top.f}
    
def buffer xoe-rell for oe-rell.
def buffer xoe-ordl for oe-ordl.

def {1} shared var v-printed    as   log  format "Y/N"          init false.
def {1} shared var v-posted     as   log  format "Y/N"          init false.
def {1} shared var v-s-cust-no  as   char format "x(8)"         init "".
def {1} shared var v-e-cust-no  like v-s-cust-no                init "zzzzzzzz".
DEF {1} SHARED VAR cLocStart    AS   CHAR FORMAT "x(8)"         INIT "".
DEF {1} SHARED VAR cLocEnd      AS   CHAR FORMAT "x(8)"         INIT "zzzzz".
def {1} shared var v-s-rel      as   int  format ">>>"          init 0.
def {1} shared var v-e-rel      like v-s-rel                    init 999.
def {1} shared var v-s-ord      as   int  format ">>>>>>>"      init 0.
def {1} shared var v-e-ord      like v-s-ord                    init 9999999.
def {1} shared var v-s-ter      like shipto.dest-code           init "".
def {1} shared var v-e-ter      like v-s-ter                    init "zzzzz".
def {1} shared var v-fdate      as   date format "99/99/9999"   init today.
def {1} shared var v-tdate      like v-fdate                    init 12/31/9999.
def {1} shared var v-p-bin      as   log                        init yes.
def {1} shared var v-more       as   log                        init no.
def {1} shared var v-zone-s     as   log                        init no.
def {1} shared var v-zone-p     as   log                        init no.

def {1} shared var rel-pack         as   char format "x(15)".
def {1} shared var v-relprint       like sys-ctrl.char-fld.
def {1} shared var v-headers        like sys-ctrl.log-fld.
def {1} shared var v-lines-per-page AS   INT NO-UNDO.
DEF {1} SHARED VAR v-sort-loc-bin AS LOG NO-UNDO.
DEF {1} SHARED VAR lExclCust AS LOG NO-UNDO.
DEF {1} SHARED VAR cCustList AS CHARACTER NO-UNDO.

def var save_id as recid.
def var v-date as date format "99/99/9999" no-undo.
def var v-page-tot as int init 0 no-undo.
def var v-last-page as int init 0 no-undo.
def var v-salesman as char format "x(13)" no-undo.
def var v-name as char format "x(30)" no-undo.
def var v-addr as char format "x(30)" no-undo extent 2.
def var v-c-s-z as char format "x(30)" no-undo.
def var v-carrier as char format "x(20)" no-undo.
def var v-weight as int format ">>>>>9" no-undo.
def var v-pallets as int format ">>>>9" no-undo.
def var v-partial like oe-rell.partial no-undo.
def var v-case as char format "x(20)" no-undo.
def var v-cases like oe-rell.cases no-undo.
def var v-c-cnt as int no-undo.
def var v-set-qty as int no-undo.
def var v-part-qty as int no-undo.
def var v-ans as log init NO NO-UNDO.
def var v-ticket-date as date init today no-undo.
def var v-tot-qty as int no-undo.
def var v-comp-name like cust.NAME NO-UNDO.
def var v-add-line like cust.addr extent 2 NO-UNDO.
def var v-city like cust.city NO-UNDO.
def var v-state like cust.state NO-UNDO.
def var v-zip like cust.zip NO-UNDO.
def var cnt as INT NO-UNDO.
def var locbin as char format "x(5)" extent 10 NO-UNDO.
def var xx as INTE NO-UNDO.
def var correct-po like oe-rel.po-no no-undo.
def var v-po-no like oe-relh.po-no no-undo.
def var v-frt-terms as char format "x(11)"  no-undo.

DEF TEMP-TABLE tt-rell NO-UNDO LIKE oe-rell
    FIELD sort-loc-bin AS CHAR.

DEF BUFFER b-tt-rell FOR tt-rell.               
