/* --------------------------------------------------- ap/ap-aged.i 08/98 JLF */
/* Vendor Aging Report Program - A/P Module                                   */
/* -------------------------------------------------------------------------- */

for each ap-inv
    where ap-inv.company   eq vend.company
      and ap-inv.vend-no   eq vend.vend-no
      and ap-inv.posted    eq yes
      and (ap-inv.inv-date le as_of_date or not v-idate)
    use-index ap-inv no-lock,
    
    first ap-ledger
    where ap-ledger.company  eq vend.company
      and ap-ledger.vend-no  eq ap-inv.vend-no
      and ap-ledger.ref-date eq ap-inv.inv-date
      and ap-ledger.refnum   eq ("INV# " + ap-inv.inv-no)
      and (ap-ledger.tr-date le as_of_date or v-idate)
    use-index ap-ledger no-lock
    
    break by ap-inv.vend-no
          by (if v-idate then ap-inv.inv-date else ap-ledger.tr-date)
          by ap-inv.inv-no:
  
  if FIRST(ap-inv.vend-no) then do:
    assign
     first-time = yes
     cust-t[1]  = 0
     cust-t[2]  = 0
     cust-t[3]  = 0
     cust-t[4]  = 0
     cust-t[5]  = 0
     m3 = vend.area-code + vend.phone
     m2 = vend.cont
     ni = 0.
  end.

  assign
   v-amt  = 0
   v-date = if v-idate then ap-inv.inv-date else ap-ledger.tr-date.
   
  for each ap-payl no-lock
      where ap-payl.inv-no   eq ap-inv.inv-no
        and ap-payl.vend-no  eq ap-inv.vend-no
        and ap-payl.posted   eq yes
        and ap-payl.due-date eq ap-inv.due-date
      use-index inv-no:

    find first ap-pay no-lock
        where ap-pay.company eq vend.company
          and ap-pay.c-no eq ap-payl.c-no
        use-index c-no no-error.

    if avail ap-pay THEN
    DO:
       v-check-date = ap-pay.check-date.

       /*check for voided check transaction date*/
      if ap-payl.amt-paid lt 0  and
         ap-payl.memo                eq no and
         ap-inv.net + ap-inv.freight gt 0 THEN
         DO:
            v-refnum = "VOIDED CHECK"
                        + string(ap-pay.check-no, "zzzzzzz9").

            FIND FIRST xap-ledger WHERE
                 xap-ledger.company EQ vend.company AND
                 xap-ledger.vend-no EQ ap-pay.vend-no AND
                 xap-ledger.refnum = v-refnum
                 NO-LOCK NO-ERROR.

            IF AVAIL xap-ledger THEN
            DO:
               v-check-date = xap-ledger.tr-date.
               RELEASE xap-ledger.
            END.
         END.
/*          IF NOT v-idate THEN DO:                                            */
/*              FIND FIRST xap-ledger                                          */
/*                 WHERE xap-ledger.company  eq vend.company                   */
/*                   AND xap-ledger.vend-no  eq ap-pay.vend-no                 */
/*                   AND xap-ledger.ref-date EQ ap-pay.check-date              */
/*                   AND ((xap-ledger.refnum BEGINS "MEMO" AND ap-pay.memo) OR */
/*                     (NOT xap-ledger.refnum BEGINS "MEMO" AND                */
/*                      NOT xap-ledger.refnum BEGINS "INV#"                    */
/*                      AND NOT ap-pay.memo))                                  */
/*              NO-LOCK NO-ERROR.                                              */
/*              IF AVAIL xap-ledger THEN DO:                                   */
/*                  v-check-date = xap-ledger.tr-date.                         */
/*                  RELEASE xap-ledger.                                        */
/*              END.                                                           */
/*                                                                             */
/*          END.                                                               */

       IF v-check-date le as_of_date then do:

       if ap-payl.amt-paid ne 0 then v-amt = v-amt - ap-payl.amt-paid.
       if ap-payl.amt-disc ne 0 then do:
          if not ap-payl.memo then v-amt = v-amt - ap-payl.amt-disc.
          if ap-payl.memo then v-amt = v-amt + ap-payl.amt-disc.
       end.
    end.
    END.

    release ap-pay.
    
  end. /* for each ap-payl */

  assign
   d     = as_of_date - v-date
   v-amt = v-amt + ap-inv.net + ap-inv.freight
   t1    = t1 + v-amt
   ni    = ni + 1.

  /* if days old less then 0 make equal to 0 */
  if d lt 0 then assign d = 0.
  
  if v-amt ne 0 then do:
    if first-time then do:
      FIND FIRST terms WHERE terms.company = vend.company AND
          terms.t-code = vend.terms NO-LOCK NO-ERROR.
      IF AVAIL terms THEN v-terms = terms.dscr.
      display vend.vend-no vend.NAME v-terms
          with no-labels no-box no-attr-space frame vendor1{1}1.
      display m3 SPACE(2) vend.TYPE  with frame vendor2{1} no-labels no-box stream-io width 144.
      first-time = no.

      IF tb_excel AND ll-mult-curr THEN
         PUT STREAM excel UNFORMATTED
             '"' IF FIRST-OF(tt-vend.curr-code) THEN
                  tt-vend.curr-code ELSE ""          '",'.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' vend.vend-no                                         '",'
             '"' vend.NAME                                            '",'
             '"' STRING(m3,"(999) 999-9999")                          '",'
             '"' vend.TYPE                                            '",'
             '"' v-terms                                           '",'.
 
    end.

    ELSE IF tb_excel THEN
    DO:
       IF ll-mult-curr THEN
          PUT STREAM excel UNFORMATTED
             '"' "" '",'.

       PUT STREAM excel UNFORMATTED
           '"' "" '",'
           '"' "" '",'
           '"' "" '",'
           '"' "" '",'
           '"' "" '",'.
    END.

    form header vend.vend-no vend.name
    
        with frame v-top-{1} page-top no-box no-labels no-attr-space stream-io.
    view frame v-top-{1}.
    
    assign
     ag        = 0
     i         = (IF d LE v-days[1] THEN 1 
                  ELSE IF d LE v-days[2] THEN 2
                  ELSE IF d LE v-days[3] THEN 3
                  ELSE IF d LE v-days[4] THEN 4
                  ELSE 5)
     ag[i]     = v-amt
     cust-t[i] = cust-t[i] + ag[i].
    
    if v-dtl then
    DO:
      display ap-inv.inv-no     at 3        format "x(12)"
              space(2)
              v-date FORMAT "99/99/99"
              space(1)
              /*ap-inv.due-date FORMAT "99/99/99"
              space(1)*/
              v-amt
              d                             format "-999"
              ag[1]             to 66       when ag[1] ne 0
              ag[2]             to 82       when ag[2] ne 0
              ag[3]             to 98       when ag[3] ne 0
              ag[4]             to 114      when ag[4] ne 0 
              ag[5]             to 130      when ag[5] ne 0
              
          with frame detail{1} no-labels no-box stream-io width 144.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' ap-inv.inv-no                              '",'
             '"' v-date                                     '",'
             /*'"' ap-inv.due-date                            '",'*/
             '"' STRING(v-amt,"->,>>>,>>>,>>9.99")          '",'
             '"' STRING(d,"-999")                           '",'
             '"' IF ag[1] NE 0 THEN
                    STRING(ag[1],"->>>,>>>,>>9.99") ELSE "" '",'
             '"' IF ag[2] NE 0 THEN
                    STRING(ag[2],"->>>,>>>,>>9.99") ELSE "" '",'
             '"' IF ag[3] NE 0 THEN
                    STRING(ag[3],"->>>,>>>,>>9.99") ELSE "" '",'
             '"' IF ag[4] NE 0 THEN
                    STRING(ag[4],"->>>,>>>,>>9.99") ELSE "" '",'
             '"' IF ag[5] NE 0 THEN
                    STRING(ag[5],"->>>,>>>,>>9.99") ELSE "" '",'
             SKIP.
    END.
  end.  /* if v-amt ne 0  */

  if LAST(ap-inv.vend-no) and t1 ne 0 then do:
    if ni gt 1 then m3 = "".
    if ni eq 1 then m3 = m2.
    
    display space (10) "VENDOR TOTALS" t1 to 42
            cust-t[1] to 66
            cust-t[2] to 82
            cust-t[3] to 98
            cust-t[4] to 114
            cust-t[5] to 130
            skip(1)
            
        with frame vendor3{1} no-labels no-box no-attr-space stream-io width 144.
        
    IF tb_excel THEN
    DO:
       PUT STREAM excel UNFORMATTED SKIP(1).

       IF ll-mult-curr THEN
          PUT STREAM excel UNFORMATTED
             '"' "" '",'.

       PUT STREAM excel UNFORMATTED
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' "VENDOR TOTALS"                     '",'
           '"' STRING(t1,"$->>,>>>,>>9.99")        '",'
           '"' ""                                  '",'
           '"' STRING(cust-t[1],"->>>,>>>,>>9.99") '",'
           '"' STRING(cust-t[2],"->>>,>>>,>>9.99") '",'
           '"' STRING(cust-t[3],"->>>,>>>,>>9.99") '",'
           '"' STRING(cust-t[4],"->>>,>>>,>>9.99") '",'
           '"' STRING(cust-t[5],"->>>,>>>,>>9.99") '",'
           SKIP(1).
    END.

    do i = 1 to 5:
       curr-t[i] = curr-t[i] + cust-t[i].
    end.
    assign
     cust-t = 0
     t2     = t2 + t1
     t1     = 0.
  end.  /* last-of loop */
end.  /* for each ap-inv */

/* end ---------------------------------- copr. 1992  advanced software, inc. */
