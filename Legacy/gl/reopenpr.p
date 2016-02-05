/* -------------------------------------------------- gl/reopenpr.p 10/02 JLF */
/* G/L Re-open Period                                                         */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid no-undo.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-racct for account.
def buffer b-cacct for account.

DEF BUFFER b-period FOR period.

def var v-actnum like glhist.actnum no-undo.
def var start-date as date initial 01/01/1901 NO-UNDO.
def var end-date as date initial 01/01/1901 NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.


find period where recid(period) eq v-recid no-lock no-error.
if not avail period then return.

find first company where company.company eq cocode no-lock no-error.

IF period.pnum EQ company.num-per THEN
  MESSAGE "Update Customer & Vendor Totals?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE ll.

find first gl-ctrl where gl-ctrl.company eq cocode no-lock no-error.

find first glhist
    where glhist.company eq cocode
    use-index glhist no-lock no-error.
    
do while avail glhist:
  v-actnum = glhist.actnum.

  for each glhist
      where glhist.company eq cocode
        and glhist.actnum  eq v-actnum
        and glhist.tr-date ge period.pst
        and glhist.tr-date le period.pend
        and glhist.period  eq period.pnum
      transaction:

    find first account
        where account.company eq cocode
          and account.actnum  eq glhist.actnum
        no-error.
    if avail account then do:
      account.cyr[period.pnum] = account.cyr[period.pnum] - glhist.tr-amt.

      if index("RE",account.type) gt 0 then do:
        find first b-racct
            where b-racct.company eq cocode
              and b-racct.actnum  eq gl-ctrl.ret.
                  
        b-racct.cyr[period.pnum] = b-racct.cyr[period.pnum] - glhist.tr-amt.

        find first b-cacct
            where b-cacct.company eq cocode
              and b-cacct.actnum  eq gl-ctrl.contra.

        b-cacct.cyr[period.pnum] = b-cacct.cyr[period.pnum] + glhist.tr-amt.
      end.
      
      create gltrans.
      buffer-copy glhist to gltrans
      assign
       gltrans.trnum = glhist.tr-num.
       
      delete glhist.
    end.
  end.
   
  find first glhist
      where glhist.company eq cocode
        and glhist.actnum  gt v-actnum
      use-index glhist no-lock no-error. 
end.

IF ll THEN DO:
for each cust where cust.company eq cocode transaction:
  assign
   cust.cost[1] = 0
   cust.comm[1] = 0.
       
  for each ar-ledger
      where ar-ledger.company eq cocode
        and ar-ledger.cust-no eq cust.cust-no
        and ar-ledger.tr-date ge period.pst
        and ar-ledger.ref-num begins "INV#"
      no-lock,

      first ar-inv
      where ar-inv.company eq cocode
        and ar-inv.posted  eq yes
        and ar-inv.cust-no eq cust.cust-no
        and ar-inv.inv-no  eq int(substr(ar-ledger.ref-num,6,
                                                   length(ar-ledger.ref-num)))
      use-index posted no-lock:

    assign
     cust.cost[1] = cust.cost[1] +
                    if ar-inv.t-cost eq ? then 0 else ar-inv.t-cost
     cust.comm[1] = cust.comm[1] +
                    if ar-inv.t-comm eq ? then 0 else ar-inv.t-comm.
  end.
END.

IF period.pnum EQ company.num-per THEN DO:
  /* Cust Processing  */
  for each cust where cust.company eq cocode TRANSACTION:
    status default "Please Wait...Updating Customer: " + trim(cust.cust-no).

    {util/reopeny1.i 1 lyytd lyr 6}
    
    {util/reopeny1.i 0 ytd ytd 5}
  END.

  /* Vend Processing  */
  for each vend where vend.company eq cocode TRANSACTION:
    status default "Please Wait...Updating Vendor: " + trim(vend.vend-no).
    
    {util/reopeny2.i 1 lyytd last-year}
    
    {util/reopeny2.i 0 ytd-msf purch[13]}
  end. /* for each vend */
END.

END. /* ll */

/* end ---------------------------------- copr. 2002  advanced software, inc. */
