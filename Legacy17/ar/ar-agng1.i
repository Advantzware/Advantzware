
def {1} shared var v-s-cust     like cust.cust-no.
def {1} shared var v-e-cust     like cust.cust-no init "zzzzzzzz".
def {1} shared var v-s-sman     like cust.sman.
def {1} shared var v-e-sman     like cust.sman init "zzz".
def {1} shared var v-s-curr     like cust.curr-code no-undo.
def {1} shared var v-e-curr     like cust.curr-code no-undo.
def {1} shared var v-date       as date.
DEF {1} SHARED VAR v-trend-days AS INT FORMAT ">>9" NO-UNDO INIT 0.
def {1} shared var v-days       as int format ">,>>9" extent 3 init [30,60,90].
def {1} shared var det-rpt      as INT format "9" init 1.
def {1} shared var v-sort       as char init "Name" format "x(8)".
def {1} shared var v-sort2      like v-sort init "InvDate".
def {1} shared var v-inc        as log init no format "Y/N".
def {1} shared var v-s-dat      as date format "99/99/9999" init 01/01/0001.
def {1} shared var v-e-dat      like v-s-dat init today.
def {1} shared var v-days-old   as log init no format "Y/N".
def {1} shared var v-prt-add    as log init no format "Y/N".
def {1} shared var v-export     as log init no format "Y/N".
def {1} shared var v-exp-name   as char format "x(40)" initial "ar-aging.csv".
def {1} shared var v-chk-day    as char format "x(13)".
def {1} shared var time_stamp   as ch.
DEF {1} shared VAR v-include-factored AS LOG NO-UNDO.
DEF {1} SHARED VAR v-include-fuel     AS LOG NO-UNDO.
DEF {1} SHARED VAR v-sep-fc     AS LOG NO-UNDO.
DEF {1} SHARED VAR v-print-job  AS LOG NO-UNDO.
DEF {1} SHARED VAR v-print-cust-po AS LOG NO-UNDO.
DEF {1} SHARED VAR v-inactive-custs AS LOG NO-UNDO.
def {1} shared var v-s-terms     AS CHAR NO-UNDO.
def {1} shared var v-e-terms     AS CHAR init "zzzzz" NO-UNDO .
/* def {1} shared var lSelected     AS LOG NO-UNDO. */
def {1} shared var grand-t  as dec extent 6 format "->,>>>,>>>,>>9.99".
def {1} shared var grand-t-pri as dec extent 6 format "->,>>>,>>>,>>9.99".
def {1} shared var grand-t-fc  as dec extent 6 format "->,>>>,>>>,>>9.99".

def {1} shared stream s-temp.

def var str_buffa as char no-undo.
