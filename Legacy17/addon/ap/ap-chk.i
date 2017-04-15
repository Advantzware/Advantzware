
def {1} shared var v-print-mode as   char no-undo init "ALIGN".
def {1} shared var save_id      as   recid.
def {1} shared var wvend-no     like vend.vend-no label "Restart with vend".
def {1} shared var evend-no     like wvend-no label "End with vend".
def {1} shared var wdate        as   date label "Enter Check Date" init today.
def {1} shared var stnum        as   int label "Starting Check Number"
                                     format "999999".
def {1} shared var x-bank       like bank.bank-code label "Enter Bank Code".
def {1} shared var xdate        like wdate.
def {1} shared var laser-chk    as   log no-undo.
def {1} shared var max-per-chk  as   int no-undo.

def var v-sort-name as   log  no-undo.
def var v-print-fmt as   char no-undo.

def var v-left          as   log no-undo.
def var align           as   log format "Y/N" init no.
def var ctot            as   dec format "->>>,>>9.99".
def var cgross          like ctot.
def var cdis            as   dec.
def var fwkdol          as   char.
def var dol             as   char format "x(70)".
def var csz             as   char format "x(30)".
def var add1            like csz.
def var add2            like csz.
def var v-inv-no        like ap-sel.inv-no.
def var v-vend-no       like vend.vend-no.
def var v-vend-name     like vend.name.
def var ll              as   int.
def var cgrossl         as   dec.
def var checks-avail    as   log init no.
def var dollar          as   char extent 2 format "x(70)" no-undo.
def var wordlen         as   int.
DEF VAR lv-zip          LIKE vend.zip NO-UNDO.
DEF VAR lv-country      LIKE vend.country NO-UNDO.
DEF VAR lv-postal       LIKE vend.postal NO-UNDO.


do transaction:
  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "CHKFMT"
      no-error.
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "CHKFMT"
     sys-ctrl.char-fld = "n".
    message "System control record NOT found. " sys-ctrl.descrip
            update sys-ctrl.char-fld.
  end.
  assign
   sys-ctrl.descrip = "Char Value: AP Check Format. " +
                      "Log Value: Sort by Vendor Name?"
   v-print-fmt      = sys-ctrl.char-fld
   v-sort-name      = sys-ctrl.log-fld.
end.

