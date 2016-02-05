def var li as int no-undo.

def buffer b-ap-pay for ap-pay.


for each ap-payl by check-no :
    find first ap-pay
                   where ap-pay.c-no eq ap-payl.c-no
                     and ap-pay.vend-no ne ap-payl.vend-no
                     and ap-pay.bank-code ne ""
                     no-lock no-error.
                     
  if avail ap-pay then display ap-payl.check-no.
  /*
    find bank where bank.bank-code = ap-pay.bank-code and
                         bank.company = ap-pay.company no-error.
    
    find first ap-ledger
        where ap-ledger.company eq ap-pay.company
          and ap-ledger.vend-no eq ap-payl.vend-no
          and ap-ledger.refnum  eq "CHK# " + string(ap-payl.check-no) +
                                   " CD#" + bank.bank-code
        no-error.
         
    if avail ap-ledger and avail bank then do:
      find first period
          where period.company eq ap-ledger.company
            and period.pst     le ap-ledger.tr-date
            and period.pend    ge ap-ledger.tr-date
            no-lock no-error.
            
      FIND LAST b-ap-pay USE-INDEX c-no no-lock NO-ERROR.
      li = (IF AVAIL b-ap-pay THEN b-ap-pay.c-no else 0) + 1.
      
      create b-ap-pay.     
      assign
       b-ap-pay.company    = ap-pay.company
       b-ap-pay.check-act  = bank.actnum
       b-ap-pay.check-amt  = ap-ledger.amt
       b-ap-pay.check-no   = bank.last-chk + 1
       bank.last-chk       = b-ap-pay.check-no
       ap-payl.check-no    = b-ap-pay.check-no
       ap-ledger.refnum    = "CHK# " + string(ap-payl.check-no) +
                                   " CD#" + bank.bank-code
       b-ap-pay.period     = if avail period then period.pnum 
                                             else month(ap-ledger.tr-date)
       b-ap-pay.c-no       = li
       b-ap-pay.vend-no    = ap-payl.vend-no
       b-ap-pay.bank-code  = ap-pay.bank-code
       b-ap-pay.d-no       = ap-payl.d-no
       b-ap-pay.check-date = ap-ledger.ref-date
       b-ap-pay.man-check  = TRUE
       b-ap-pay.posted     = TRUE.
    end. 
  end.*/
end.
