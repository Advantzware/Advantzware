
SESSION:SET-WAIT-STATE ("general").

def var v-days as int no-undo.
def var v-invs as int no-undo.
DEF VAR lv-due LIKE ar-inv.due NO-UNDO.


FOR EACH cust:
  find first sys-ctrl
      where sys-ctrl.company eq cust.company
        and sys-ctrl.name    eq "AGEDAYS"
      no-lock no-error.

  ASSIGN
   cust.num-inv = 0
   cust.avg-pay = 0.

  for each ar-inv
      where ar-inv.company  eq cust.company
        and ar-inv.posted   eq yes
        and ar-inv.cust-no  eq cust.cust-no
        and ar-inv.due      le 0
      use-index posted-due no-lock:

    lv-due = ar-inv.net + ar-inv.tax-amt.

    if NOT AVAIL sys-ctrl OR sys-ctrl.int-fld eq 0 then
    for each ar-cashl
        where ar-cashl.company  EQ ar-inv.company
          AND ar-cashl.cust-no  EQ ar-inv.cust-no
          AND ar-cashl.inv-date EQ ar-inv.inv-date
          AND ar-cashl.inv-no   EQ ar-inv.inv-no
        NO-LOCK,
        FIRST ar-cash
        WHERE ar-cash.c-no   EQ ar-cashl.c-no
          AND ar-cash.posted EQ YES
          AND ar-cash.memo   EQ NO
        NO-LOCK
        BY ar-cash.check-date
        BY ar-cash.c-no
        BY ar-cashl.LINE:

      lv-due = lv-due - ar-cashl.amt-paid.

      IF lv-due LE 0 THEN DO:
        cust.avg-pay = ((cust.num-inv * cust.avg-pay) +
                        (ar-cash.check-date - ar-inv.inv-date)) /
                       (cust.num-inv + 1).
        LEAVE.
      END.
    END.

    cust.num-inv = cust.num-inv + 1.
  END.
  
  if NOT AVAIL sys-ctrl OR sys-ctrl.int-fld eq 0 then.
                     
  else do:
    assign
     v-days = 0
     v-invs = 0.
     
    for each ar-inv
        where ar-inv.company  eq cust.company
          and ar-inv.posted   eq yes
          and ar-inv.cust-no  eq cust.cust-no
          and ar-inv.due      le 0
          and ar-inv.pay-date ge (today - sys-ctrl.int-fld)
        use-index posted-due no-lock:
        
      assign
       v-days = v-days + (ar-inv.pay-date - ar-inv.inv-date)
       v-invs = v-invs + 1.
    end.
    
    cust.avg-pay = v-days / v-invs.
  end.  
  
  if cust.avg-pay lt 1 or cust.avg-pay eq ? then cust.avg-pay = 1.
end.

SESSION:SET-WAIT-STATE ("").
