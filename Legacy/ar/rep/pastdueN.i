/* ----------------------------------------------- ar/rep/pastdue.i 11/02 JLF */
/* A/R Past Due Receivables Report Program - A/R Module                       */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{sys/inc/ttRptSel.i}
{ar/rep/pastdue1.i}
{sys/ref/CustList.i}
DEF SHARED VAR det-rpt2 AS LOG NO-UNDO.
def var v-cr-db-amt as dec format "->>>,>>>,>>9.99".
def var v-disc-amt  as dec format "->>>,>>>,>>9.99".
def var v-type     as char format "x(2)".
def var v-first-cust as logical.
def var d          as int label "Days".
def var ni         as int.
def var cust-t     as dec extent 6 format "->,>>>,>>>,>>9.99".
def var sman-t     as dec extent 6 format "->,>>>,>>>,>>9.99".
def var onacc      as dec.
def var s          as int.
def var ag         as dec format "->>>,>>>,>>9.99".
def var amt        like ag.
def var paid-amt   like ag.
def var c1         as dec format "->,>>>,>>>,>>9.99".
def var m1         as char format "x(20)".
def var m2         as char format "x(20)".
def var m3         as char format "x(20)".
def var save_id    as recid.
def var unapp like cust-t.
def var first-unapp as log init yes.
def var tmp-var as char format "x(20)".  /* DAR */
def var v-disc-type as char format "x(4)".
def var v-sman as char format "x(21)".
def var v-dec as dec extent 4 no-undo.
def var v-cr-lim like cust.cr-lim no-undo.

DEF SHARED VAR cSelectedList AS cha NO-UNDO.
DEF SHARED VAR str-line AS cha FORM "x(300)" NO-UNDO.
DEF  VAR cDisplay AS cha NO-UNDO.
DEF  VAR cExcelDisplay AS cha NO-UNDO.
DEF  VAR hField AS HANDLE NO-UNDO.
DEF  VAR cTmpField AS CHA NO-UNDO.
DEF  VAR cVarValue AS cha NO-UNDO.
DEF  VAR cExcelVarValue AS cha NO-UNDO.

DEF SHARED VAR ldummy AS LOG NO-UNDO.
DEF SHARED VAR cTextListToSelect AS cha NO-UNDO.
DEF SHARED VAR cFieldListToSelect AS cha NO-UNDO.
DEF SHARED VAR cFieldLength AS cha NO-UNDO.
DEF SHARED VAR cFieldType AS cha NO-UNDO.
DEF SHARED VAR iColumnLength AS INT NO-UNDO.
DEF SHARED BUFFER b-itemfg FOR itemfg .
DEF SHARED VAR cTextListToDefault AS cha NO-UNDO.

/*{sys/form/r-top5L3.f} */
def shared frame r-top.

/*{sys/form/r-top3.f}*/


For each cust
      where cust.company eq cocode
        AND cust.cust-no GE v-s-cust
        AND cust.cust-no LE v-e-cust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
        AND ttCustList.log-fld no-lock) else true)  /*v-s-cust*/
       /* and cust.cust-no le v-e-cust*/
        and cust.sman    ge v-s-sman
        and cust.sman    le v-e-sman
      no-lock
      
      break by {&sort-by}:
      {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}
    find first sman
        where sman.company eq cocode
          and sman.sman    eq cust.sman
        no-lock no-error.
    v-sman = cust.sman + "-" + (if avail sman then sman.sname
                                else "Slsmn not on file").
      
    if first-of({&sort-by})        and
       not first ({&sort-by})      and
       "{&sort-by}" eq "cust.sman" then page.

    v-first-cust = yes.

    for each ar-inv NO-LOCK
        where ar-inv.company     eq cust.company
          and ar-inv.posted      eq yes
          and ar-inv.cust-no     eq cust.cust-no
          and ((ar-inv.inv-date  le v-date - v-days[1] AND ll-date) OR
               (ar-inv.due-date  le v-date - v-days[1] AND NOT ll-date))
          and ar-inv.terms       ne "CASH"
         by ar-inv.{&sort-by2} by ar-inv.inv-no:

      /* task 09200521 include factored fg items*/
  /*    IF NOT v-include-factored THEN DO:
        FIND FIRST ar-invl NO-LOCK
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND CAN-FIND(FIRST reftable
                           WHERE reftable.reftable EQ "FACTORED"
                             AND reftable.company  EQ ar-inv.company
                             AND reftable.loc      EQ ""
                             AND reftable.code     EQ ar-invl.i-no
                             AND reftable.code2    EQ "YES")
            NO-ERROR.
        IF AVAIL ar-invl THEN NEXT.
      END. */

/* Inserted because AR stores gross wrong */
      if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
        amt = ar-inv.net.
      else
        amt = ar-inv.gross.

      assign
       ag     = amt
       d      = v-date - ar-inv.{&date}
       ni     = ni + 1
       v-type = IF ar-inv.terms EQ "FCHG" THEN "FC" ELSE "IN".

      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
          use-index inv-no no-lock,

          each ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-date
          use-index c-no no-lock:

        if ar-cashl.memo then
/*
          if ar-cashl.dscr begins "CREDIT MEMO CREATED FROM OE RETURN" then
*/
          if ar-cashl.amt-disc ne 0 then
            ag = ag - ar-cashl.amt-disc.
          else
          if ar-cashl.amt-paid + ar-cashl.amt-disc gt 0 then
            ag = ag + (ar-cashl.amt-paid + ar-cashl.amt-disc).
          else
            ag = ag + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
        else
          ag = ag + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
      end.

     if ag GT 0 then do:
       if v-first-cust then do:
         assign paid-amt = 0  m3 = ""  ni = 0.
         if cust.area-code ne "" then
            m3 = string(cust.area-code,"(999) ").

         ASSIGN
            m3 = m3 + string(cust.phone,"999-9999")
            v-cr-lim = cust.cr-lim.

         if v-cr-lim gt 9999999.99 then v-cr-lim = 9999999.99.

         find first terms where terms.company = cocode and
                                terms.t-code = cust.terms no-lock no-error.

       /*  if det-rpt2 then 
           display cust.cust-no
                   space(3)
                   cust.name
                   space(3)
                   cust.area-code                           format "(xxx)"
                   cust.phone                               format "xxx-xxxx"
                   "  Fax:"
                   substr(cust.fax,1,3)                     format "(xxx)"
                   substr(cust.fax,4,7)                     format "xxx-xxxx"
                   skip
                   "CL:"
                   trim(string(v-cr-lim,">,>>>,>>>,>>9.99")) format "x(17)"
                   cust.contact                             format "x(20)"
                   space(2)
                   v-sman
                   space(2)
                   terms.dscr when avail terms              format "x(13)"
               with no-labels no-box frame a1 STREAM-IO width 80. 
              
         if v-prt-add then run print-cust-add.   */                 

         v-first-cust = no.
       end.

     /*  if det-rpt2 then
         display d at 4 format "-9999" when v-days-old 
                 space(7) "IN" space(5) ar-inv.inv-no
                 space(2) ar-inv.inv-date FORMAT "99/99/99"
                 amt to 54 ag to 77
                 with frame c no-labels no-box STREAM-IO width 80.  */

       assign
        cust-t[1] = cust-t[1] + ag
        v-dec     = 0
        v-dec[1]  = ag.
       
    /*   if v-export then
         run export-data ("", d, "IN", string(ar-inv.inv-no,">>>>>>>>>>"),
                          ar-inv.inv-date, amt, v-dec[1]). */
       ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".
     
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
             CASE cTmpField: 
                  WHEN "cust"      THEN cVarValue = (cust.cust-no)  .                               
                  WHEN "name"      THEN cVarValue = (cust.name)   .                                 
                  WHEN "cont"      THEN cVarValue = trim(cust.contact) .                                
                  WHEN "rep"       THEN cVarValue = trim(v-sman)        .                               
                  WHEN "trm"       THEN cVarValue = trim(if avail terms then terms.dscr else "")  .     
                  WHEN "add1"      THEN cVarValue = trim(cust.addr[1])  .                               
                  WHEN "add2"      THEN cVarValue = trim(cust.addr[2]) .                                
                  WHEN "cty"       THEN cVarValue = trim(cust.city) .                                   
                  WHEN "stat"      THEN cVarValue = trim(cust.state) .                                  
                  WHEN "zip"       THEN cVarValue = trim(cust.zip)   .                                  
                  WHEN "crdt"      THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99")   .       
                  WHEN "phon"      THEN cVarValue = string(trim(string(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                  WHEN "fax"       THEN cVarValue = string(trim(string(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                  WHEN "chk-mmo"   THEN cVarValue = ""      .                    
                  WHEN "dy-old"    THEN cVarValue = string(d,"->>>>")     .               
                  WHEN "typ"       THEN cVarValue = trim("IN") .                                  
                  WHEN "inv"       THEN cVarValue = string(ar-inv.inv-no,">>>>>>>>>>") .                                   
                  WHEN "inv-dt"    THEN cVarValue = IF ar-inv.inv-date NE ? THEN string(ar-inv.inv-date,"99/99/9999") ELSE "".              
                  WHEN "inv-amt"   THEN cVarValue = string(amt,"->>>>>>>>9.99")  .         
                  WHEN "curr"      THEN cVarValue = string(v-dec[1],"->>>>>>>>9.99")  .         
                                                             
             END CASE.                                       
             
             cExcelVarValue = cVarValue.
             cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
             cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 

     END.
     
     PUT UNFORMATTED cDisplay SKIP.
     IF v-export THEN DO:
          PUT STREAM s-temp UNFORMATTED  
                cExcelDisplay SKIP.
      END.

      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
          use-index inv-no no-lock,

          each ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-date
          use-index c-no no-lock:

        if ar-cashl.memo then

           /* CTS CM/DM signs are reversed *****************************/
           /*if (ar-cashl.amt-paid + ar-cashl.amt-disc) lt 0 then
              assign v-type = "CM"
                     v-cr-db-amt = ar-cashl.amt-paid
                     v-disc-amt = ar-cashl.amt-disc.

           else*/
           if (ar-cashl.amt-paid + ar-cashl.amt-disc) gt 0 then
              assign v-type = "DM"
                     v-cr-db-amt = ar-cashl.amt-paid
                     v-disc-amt = ar-cashl.amt-disc.

           else
              assign v-type = "CM"
                     v-cr-db-amt = ar-cashl.amt-paid
                     v-disc-amt = - (ar-cashl.amt-disc).

        else
          assign v-type = "PY"
                 v-cr-db-amt = (ar-cashl.amt-paid) * -1
                 v-disc-amt = ar-cashl.amt-disc * -1.

        if v-disc-amt ne 0 then do:

          v-disc-type = "DISC".
/*
          if ar-cashl.dscr begins "CREDIT MEMO CREATED FROM OE RETURN" then
*/
          if ar-cashl.memo then
            assign
             v-disc-type = "RETN"
             v-disc-amt  = - v-disc-amt.

          if det-rpt2 then do:
            if v-disc-type eq "DISC" then do:
            /*  display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                      v-type at 16
                      ar-cashl.inv-no at 23
                      ar-cash.check-date at 31 FORMAT "99/99/99"
                      v-cr-db-amt to 54 skip
                  with frame f-1 no-box no-labels STREAM-IO width 80.
                  
              if v-export then
                run export-data (ar-cashl.check-no, 0, v-type,
                                 string(ar-cashl.inv-no,">>>>>>>>>>"),
                                 ar-cash.check-date, v-cr-db-amt, 0). */

              ASSIGN cDisplay = ""
                       cTmpField = ""
                       cVarValue = ""
                       cExcelDisplay = ""
                       cExcelVarValue = "".
                
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                   cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                        CASE cTmpField: 
                             WHEN "cust"      THEN cVarValue = trim(cust.cust-no)  .                               
                             WHEN "name"      THEN cVarValue = trim(cust.name)   .                                 
                             WHEN "cont"      THEN cVarValue = trim(cust.contact) .                                
                             WHEN "rep"       THEN cVarValue = trim(v-sman)        .                               
                             WHEN "trm"       THEN cVarValue = trim(if avail terms then terms.dscr else "")  .     
                             WHEN "add1"      THEN cVarValue = trim(cust.addr[1])  .                               
                             WHEN "add2"      THEN cVarValue = trim(cust.addr[2]) .                                
                             WHEN "cty"       THEN cVarValue = trim(cust.city) .                                   
                             WHEN "stat"      THEN cVarValue = trim(cust.state) .                                  
                             WHEN "zip"       THEN cVarValue = trim(cust.zip)   .                                  
                             WHEN "crdt"      THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99")   .       
                             WHEN "phon"      THEN cVarValue = string(trim(string(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                             WHEN "fax"       THEN cVarValue = string(trim(string(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                             WHEN "chk-mmo"   THEN cVarValue = STRING(ar-cashl.check-no)      .                    
                             WHEN "dy-old"    THEN cVarValue = string(0,"->>>>")     .               
                             WHEN "typ"       THEN cVarValue = trim(v-type) .                                  
                             WHEN "inv"       THEN cVarValue = string(ar-cashl.inv-no,">>>>>>>>>>") .                                   
                             WHEN "inv-dt"    THEN cVarValue = IF ar-cash.check-date NE ? THEN string(ar-cash.check-date,"99/99/9999") ELSE "".              
                             WHEN "inv-amt"   THEN cVarValue = string(v-cr-db-amt,"->>>>>>>>9.99")  .         
                             WHEN "curr"      THEN cVarValue = string(0,"->>>>>>>>9.99")  .         
                                                                        
                        END CASE.                                       
                        
                        cExcelVarValue = cVarValue.
                        cDisplay = cDisplay + cVarValue +
                                   FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
                
                PUT UNFORMATTED cDisplay SKIP.
                IF v-export THEN DO:
                     PUT STREAM s-temp UNFORMATTED  
                           cExcelDisplay SKIP.
                 END.
            end.

          /*  display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                    v-disc-type at 16
                    ar-cashl.inv-no at 23
                    ar-cash.check-date at 31 FORMAT "99/99/99"
                    v-disc-amt to 54
                with frame f-50{&frame} no-box no-labels STREAM-IO width 80.
                
            if v-export then
              run export-data (ar-cashl.check-no, 0, v-disc-type,
                               string(ar-cashl.inv-no,">>>>>>>>>>"),
                               ar-cash.check-date, v-disc-amt, 0). */

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
            
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                         WHEN "cust"      THEN cVarValue = trim(cust.cust-no)  .                               
                         WHEN "name"      THEN cVarValue = trim(cust.name)   .                                 
                         WHEN "cont"      THEN cVarValue = trim(cust.contact) .                                
                         WHEN "rep"       THEN cVarValue = trim(v-sman)        .                               
                         WHEN "trm"       THEN cVarValue = trim(if avail terms then terms.dscr else "")  .     
                         WHEN "add1"      THEN cVarValue = trim(cust.addr[1])  .                               
                         WHEN "add2"      THEN cVarValue = trim(cust.addr[2]) .                                
                         WHEN "cty"       THEN cVarValue = trim(cust.city) .                                   
                         WHEN "stat"      THEN cVarValue = trim(cust.state) .                                  
                         WHEN "zip"       THEN cVarValue = trim(cust.zip)   .                                  
                         WHEN "crdt"      THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99")   .       
                         WHEN "phon"      THEN cVarValue = string(trim(string(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                         WHEN "fax"       THEN cVarValue = string(trim(string(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                         WHEN "chk-mmo"   THEN cVarValue = STRING(ar-cashl.check-no)      .                    
                         WHEN "dy-old"    THEN cVarValue = string(0,"->>>>")     .               
                         WHEN "typ"       THEN cVarValue = trim(v-disc-type) .                                  
                         WHEN "inv"       THEN cVarValue = string(ar-cashl.inv-no,">>>>>>>>>>") .                                   
                         WHEN "inv-dt"    THEN cVarValue = IF ar-cash.check-date NE ? THEN string(ar-cash.check-date,"99/99/9999") ELSE "".              
                         WHEN "inv-amt"   THEN cVarValue = string(v-disc-amt,"->>>>>>>>9.99")  .         
                         WHEN "curr"      THEN cVarValue = string(0,"->>>>>>>>9.99")  .         
                                                                    
                    END CASE.                                       
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            
            PUT UNFORMATTED cDisplay SKIP.
            IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

          end.
        end.

        else
        if det-rpt2 then do:
         /* display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                  v-type at 16
                  ar-cashl.inv-no at 23
                  ar-cash.check-date at 31 FORMAT "99/99/99"
                  v-cr-db-amt to 54
              with frame f-100 no-box no-labels STREAM-IO width 80.
              
          if v-export then
            run export-data (ar-cashl.check-no, 0, v-type,
                             string(ar-cashl.inv-no,">>>>>>>>>>"),
                             ar-cash.check-date, v-cr-db-amt, 0). */
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
            
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                         WHEN "cust"      THEN cVarValue = trim(cust.cust-no)  .                               
                         WHEN "name"      THEN cVarValue = trim(cust.name)   .                                 
                         WHEN "cont"      THEN cVarValue = trim(cust.contact) .                                
                         WHEN "rep"       THEN cVarValue = trim(v-sman)        .                               
                         WHEN "trm"       THEN cVarValue = trim(if avail terms then terms.dscr else "")  .     
                         WHEN "add1"      THEN cVarValue = trim(cust.addr[1])  .                               
                         WHEN "add2"      THEN cVarValue = trim(cust.addr[2]) .                                
                         WHEN "cty"       THEN cVarValue = trim(cust.city) .                                   
                         WHEN "stat"      THEN cVarValue = trim(cust.state) .                                  
                         WHEN "zip"       THEN cVarValue = trim(cust.zip)   .                                  
                         WHEN "crdt"      THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99")   .       
                         WHEN "phon"      THEN cVarValue = string(trim(string(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                         WHEN "fax"       THEN cVarValue = string(trim(string(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                         WHEN "chk-mmo"   THEN cVarValue = STRING(ar-cashl.check-no)      .                    
                         WHEN "dy-old"    THEN cVarValue = string(0,"->>>>")     .               
                         WHEN "typ"       THEN cVarValue = trim(v-type) .                                  
                         WHEN "inv"       THEN cVarValue = string(ar-cashl.inv-no,">>>>>>>>>>") .                                   
                         WHEN "inv-dt"    THEN cVarValue = IF ar-cash.check-date NE ? THEN string(ar-cash.check-date,"99/99/9999") ELSE "".              
                         WHEN "inv-amt"   THEN cVarValue = string(v-cr-db-amt,"->>>>>>>>9.99")  .         
                         WHEN "curr"      THEN cVarValue = string(0,"->>>>>>>>9.99")  .         
                                                                    
                    END CASE.                                       
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            
            PUT UNFORMATTED cDisplay SKIP.
            IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
        end.
      end. /* for each ar-cashl record */
     end.
    end. /* for each ar-inv record */

    assign unapp[1] = 0.

    /* This loop finds all unapplied balances and totals by age */
    if cust-t[1] ne 0 then
    for each ar-cash
        where ar-cash.company     eq cust.company
          and ar-cash.cust-no     eq cust.cust-no
          and (ar-cash.check-date le v-date or
               ar-cash.check-date eq ?)
          and ar-cash.posted      eq yes
        use-index ar-cash no-lock,

        each ar-cashl
        where ar-cashl.c-no       eq ar-cash.c-no
          and ar-cashl.posted     eq yes
        use-index c-no no-lock:

      if ar-cashl.inv-no ne 0 then do:
        find first ar-inv
            where ar-inv.company     eq cust.company
              and ar-inv.inv-no      eq ar-cashl.inv-no
              and ar-inv.inv-date    gt v-date
            use-index inv-no no-lock no-error.
        if not avail ar-inv then next.
      end.

      if ar-cashl.memo then do:

           /* CTS CM/DM signs are reversed *****************************/
         if (ar-cashl.amt-paid + ar-cashl.amt-disc) gt 0 then
            assign v-type = "DM"
                   v-cr-db-amt = ar-cashl.amt-paid
                   v-disc-amt = ar-cashl.amt-disc.

         else
            assign v-type = "CM"
                   v-cr-db-amt = ar-cashl.amt-paid
                   v-disc-amt = ar-cashl.amt-disc.
      end.

      else
        assign v-cr-db-amt = ar-cashl.amt-paid * -1
               v-disc-amt = ar-cashl.amt-disc * -1.

      unapp[1] = unapp[1] + v-cr-db-amt - v-disc-amt.
    end. /* for each ar-cashl record */

    first-unapp = yes.
    /* this loop displays all unapplied balances */
    
    if unapp[1] ne 0 then
    for each ar-cash
        where ar-cash.company     eq cust.company
          and ar-cash.cust-no     eq cust.cust-no
          and (ar-cash.check-date le v-date or
               ar-cash.check-date eq ?)
          and ar-cash.posted      eq yes
        use-index ar-cash no-lock,

        each ar-cashl
        where ar-cashl.c-no       eq ar-cash.c-no
          and ar-cashl.posted     eq yes
        use-index c-no no-lock:

      if ar-cashl.inv-no ne 0 then do:
        find first ar-inv
            where ar-inv.company     eq cust.company
              and ar-inv.inv-no      eq ar-cashl.inv-no
              and ar-inv.inv-date    gt v-date
            use-index inv-no no-lock no-error.
        if not avail ar-inv then next.
      end.

      if v-first-cust then do:
        assign paid-amt = 0  cust-t = 0  m3 = ""  ni = 0.
        if cust.area-code ne "" then
           m3 = string(cust.area-code,"(999) ").

        m3 = m3 + string(cust.phone,"999-9999").

         find first terms where terms.company = cocode and
                                terms.t-code = cust.terms no-lock no-error.

      /*  if det-rpt2 then
           display cust.cust-no
                   cust.name
                   cust.area-code                           format "(xxx)"
                   cust.phone                               format "xxx-xxxx"
                   "Fax:"
                   substr(cust.fax,1,3)                     format "(xxx)"
                   substr(cust.fax,4,7)                     format "xxx-xxxx"
                   skip
                   "CL:"
                   trim(string(v-cr-lim,">,>>>,>>>,>>9.99")) format "x(17)"
                   cust.contact                             format "x(20)"
                   space(2)
                   v-sman
                   space(2)
                   terms.dscr when avail terms              format "x(13)"
               with no-labels no-box frame a2 STREAM-IO width 80.
              
        if v-prt-add then run print-cust-add. */
        
        assign v-first-cust = no.
      end.

      if ar-cashl.memo eq true then do:
         if (ar-cashl.amt-paid + ar-cashl.amt-disc) lt 0 then
            assign v-type = "CM"
                   v-cr-db-amt = ar-cashl.amt-paid
                   v-disc-amt = ar-cashl.amt-disc.
         else
         if (ar-cashl.amt-paid + ar-cashl.amt-disc) gt 0 then
            assign v-type = "DM"
                   v-cr-db-amt = ar-cashl.amt-paid
                   v-disc-amt = ar-cashl.amt-disc.
      end.

      else
        assign v-type = "PY"
               v-cr-db-amt = ar-cashl.amt-paid * -1
               v-disc-amt = ar-cashl.amt-disc * -1.

      if first-unapp then do:

       /*  if det-rpt2 then
           display skip(1)
                   ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                   v-type at 16
                   "ON ACCT" at 23
                   ar-cash.check-date at 31 FORMAT "99/99/99"
                   (v-cr-db-amt + v-disc-amt)
                         format "->>>,>>>,>>9.99" to 54
                   unapp[1] when unapp[1] ne 0 to 77
               with frame ab no-labels no-box STREAM-IO width 80.
               
         if v-export then
           run export-data (ar-cashl.check-no, 0, v-type, "ON ACCT",
                            ar-cash.check-date, v-cr-db-amt + v-disc-amt,
                            unapp[1]).*/
         ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
            
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                         WHEN "cust"      THEN cVarValue = trim(cust.cust-no)  .                               
                         WHEN "name"      THEN cVarValue = trim(cust.name)   .                                 
                         WHEN "cont"      THEN cVarValue = trim(cust.contact) .                                
                         WHEN "rep"       THEN cVarValue = trim(v-sman)        .                               
                         WHEN "trm"       THEN cVarValue = trim(if avail terms then terms.dscr else "")  .     
                         WHEN "add1"      THEN cVarValue = trim(cust.addr[1])  .                               
                         WHEN "add2"      THEN cVarValue = trim(cust.addr[2]) .                                
                         WHEN "cty"       THEN cVarValue = trim(cust.city) .                                   
                         WHEN "stat"      THEN cVarValue = trim(cust.state) .                                  
                         WHEN "zip"       THEN cVarValue = trim(cust.zip)   .                                  
                         WHEN "crdt"      THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99")   .       
                         WHEN "phon"      THEN cVarValue = string(trim(string(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                         WHEN "fax"       THEN cVarValue = string(trim(string(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                         WHEN "chk-mmo"   THEN cVarValue = STRING(ar-cashl.check-no)      .                    
                         WHEN "dy-old"    THEN cVarValue = string(0,"->>>>")     .               
                         WHEN "typ"       THEN cVarValue = trim(v-type) .                                  
                         WHEN "inv"       THEN cVarValue = string("ON ACCT") .                                   
                         WHEN "inv-dt"    THEN cVarValue = IF ar-cash.check-date NE ? THEN string(ar-cash.check-date,"99/99/9999") ELSE "".              
                         WHEN "inv-amt"   THEN cVarValue = string((v-cr-db-amt + v-disc-amt),"->>>>>>>>9.99")  .         
                         WHEN "curr"      THEN cVarValue = string(unapp[1],"->>>>>>>>9.99")  .         
                                                                    
                    END CASE.                                       
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            
            PUT UNFORMATTED cDisplay SKIP.
            IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

         cust-t[1] = cust-t[1] + unapp[1].
      end.

      if first-unapp then first-unapp = no.

      else do:
       /* if det-rpt2 then
          display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                  v-type at 16
                  "ON ACCT" at 23
                  ar-cash.check-date at 31 FORMAT "99/99/99"
                  (v-cr-db-amt + v-disc-amt)
                           format "->>>,>>>,>>9.99" to 54
              with frame f-2 no-box no-labels STREAM-IO width 80.
              
        if v-export then
          run export-data (ar-cashl.check-no, 0, v-type, "ON ACCT",
                           ar-cash.check-date, v-cr-db-amt + v-disc-amt, 0). */
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
            
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                         WHEN "cust"      THEN cVarValue = trim(cust.cust-no)  .                               
                         WHEN "name"      THEN cVarValue = trim(cust.name)   .                                 
                         WHEN "cont"      THEN cVarValue = trim(cust.contact) .                                
                         WHEN "rep"       THEN cVarValue = trim(v-sman)        .                               
                         WHEN "trm"       THEN cVarValue = trim(if avail terms then terms.dscr else "")  .     
                         WHEN "add1"      THEN cVarValue = trim(cust.addr[1])  .                               
                         WHEN "add2"      THEN cVarValue = trim(cust.addr[2]) .                                
                         WHEN "cty"       THEN cVarValue = trim(cust.city) .                                   
                         WHEN "stat"      THEN cVarValue = trim(cust.state) .                                  
                         WHEN "zip"       THEN cVarValue = trim(cust.zip)   .                                  
                         WHEN "crdt"      THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99")   .       
                         WHEN "phon"      THEN cVarValue = string(trim(string(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                         WHEN "fax"       THEN cVarValue = string(trim(string(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                         WHEN "chk-mmo"   THEN cVarValue = STRING(ar-cashl.check-no)      .                    
                         WHEN "dy-old"    THEN cVarValue = string(0,"->>>>")     .               
                         WHEN "typ"       THEN cVarValue = trim(v-type) .                                  
                         WHEN "inv"       THEN cVarValue = string("ON ACCT") .                                   
                         WHEN "inv-dt"    THEN cVarValue = IF ar-cash.check-date NE ? THEN string(ar-cash.check-date,"99/99/9999") ELSE "".              
                         WHEN "inv-amt"   THEN cVarValue = string((v-cr-db-amt + v-disc-amt),"->>>>>>>>9.99")  .         
                         WHEN "curr"      THEN cVarValue = string(0,"->>>>>>>>9.99")  .         
                                                                    
                    END CASE.                                       
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            
            PUT UNFORMATTED cDisplay SKIP.
            IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
      end.
    end. /* for each ar-cashl record */

    c1 = cust-t[1].

    if (not v-first-cust) or c1 ne 0 then do:
     /* if det-rpt2 then do:
        display skip(1) "***** CUSTOMER TOTALS" at 4 c1 to 54 cust-t[1] to 77
                skip(1)
            with frame a3 no-labels no-box no-attr-space STREAM-IO width 80.
        
        if not last-of({&sort-by}) or "{&sort-by}" ne "cust.sman" then
          put skip(1).
      end.
      
      else
        display cust.cust-no space(2) cust.name + "  " + m3 format "x(50)" skip
                c1        to 54
                cust-t[1] to 77
                skip(1)
            with frame a3sum no-labels no-box no-attr-space STREAM-IO width 80. */
        PUT SKIP str-line SKIP.
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
            
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                         WHEN "cust"      THEN cVarValue = "" .
                         WHEN "name"      THEN cVarValue = "" .
                         WHEN "cont"      THEN cVarValue = "" . 
                         WHEN "rep"       THEN cVarValue = "" . 
                         WHEN "trm"       THEN cVarValue = "" . 
                         WHEN "add1"      THEN cVarValue = "" . 
                         WHEN "add2"      THEN cVarValue = "" . 
                         WHEN "cty"       THEN cVarValue = "" . 
                         WHEN "stat"      THEN cVarValue = "" . 
                         WHEN "zip"       THEN cVarValue = "" . 
                         WHEN "crdt"      THEN cVarValue = "" . 
                         WHEN "phon"      THEN cVarValue = "" . 
                         WHEN "fax"       THEN cVarValue = "" . 
                         WHEN "chk-mmo"   THEN cVarValue = "" . 
                         WHEN "dy-old"    THEN cVarValue = "" . 
                         WHEN "typ"       THEN cVarValue = "" . 
                         WHEN "inv"       THEN cVarValue = "" . 
                         WHEN "inv-dt"    THEN cVarValue = "" . 
                         WHEN "inv-amt"   THEN cVarValue = string(c1,"->>>>>>>>9.99")  .         
                         WHEN "curr"      THEN cVarValue = string(cust-t[1],"->>>>>>>>9.99")  .         
                                                                    
                    END CASE.                                       
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            
           PUT UNFORMATTED  "       CUSTOMER TOTALS:" substring(cDisplay,24,300) SKIP.
           IF v-export THEN DO:
               PUT STREAM s-temp UNFORMATTED  
                    "CUSTOMER TOTALS: " + substring(cExcelDisplay,3,300) SKIP.
           END.

            
      ASSIGN
         sman-t[1] = sman-t[1] + cust-t[1]
         cust-t[1] = 0.
    end.
    
    if last-of({&sort-by}) then do:
      c1 = sman-t[1].
          
      if "{&sort-by}" eq "cust.sman" THEN do:
       /* display v-sman                  at 4    format "x(33)"
                "TOTALS: " + v-sman                  @ v-sman
                "***** SALESMAN TOTALS" when det-rpt2 @ v-sman
                c1                      to 54
                sman-t[1]               to 77
                skip(2)
            with frame slsmn no-labels no-box no-attr-space STREAM-IO width 80. */
          PUT SKIP str-line SKIP.
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
            
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                         WHEN "cust"      THEN cVarValue = "" .
                         WHEN "name"      THEN cVarValue = "" .
                         WHEN "cont"      THEN cVarValue = "" . 
                         WHEN "rep"       THEN cVarValue = "" . 
                         WHEN "trm"       THEN cVarValue = "" . 
                         WHEN "add1"      THEN cVarValue = "" . 
                         WHEN "add2"      THEN cVarValue = "" . 
                         WHEN "cty"       THEN cVarValue = "" . 
                         WHEN "stat"      THEN cVarValue = "" . 
                         WHEN "zip"       THEN cVarValue = "" . 
                         WHEN "crdt"      THEN cVarValue = "" . 
                         WHEN "phon"      THEN cVarValue = "" . 
                         WHEN "fax"       THEN cVarValue = "" . 
                         WHEN "chk-mmo"   THEN cVarValue = "" . 
                         WHEN "dy-old"    THEN cVarValue = "" . 
                         WHEN "typ"       THEN cVarValue = "" . 
                         WHEN "inv"       THEN cVarValue = "" . 
                         WHEN "inv-dt"    THEN cVarValue = "" . 
                         WHEN "inv-amt"   THEN cVarValue = string(c1,"->>>>>>>>9.99")  .         
                         WHEN "curr"      THEN cVarValue = string(sman-t[1],"->>>>>>>>9.99")  .         
                                                                    
                    END CASE.                                       
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            
           PUT UNFORMATTED  "       SALESMAN TOTALS:" substring(cDisplay,24,300) SKIP.
           IF v-export THEN DO:
               PUT STREAM s-temp UNFORMATTED  
                    "SALESMAN TOTALS: " + substring(cExcelDisplay,3,300) SKIP.
           END.
      END.

      ASSIGN
         grand-t[1] = grand-t[1] + sman-t[1]
         sman-t[1] = 0.
    end.
    
    m3 = "".
    if ni eq 1 then m3 = m2.
    ASSIGN
       v-cr-db-amt = 0
       v-disc-amt = 0.
  end.  /* for each cust record */
  
  return.
 /* 
  procedure print-cust-add:
    display cust.addr[1]                                                skip
            cust.addr[2]                                                skip
            trim(cust.city) + ", " +
            trim(cust.state) + "  " + trim(cust.zip) format "x(50)"
            
        with no-labels no-box frame cust-detail STREAM-IO width 80.
  end.
  
  procedure export-data:
    def input parameter v-field-01 like ar-cashl.check-no no-undo.
    def input parameter v-field-02 like d                 no-undo.
    def input parameter v-field-03 like v-type            no-undo.
    def input parameter v-field-04 as   char              no-undo.
    def input parameter v-field-05 like ar-inv.inv-date   no-undo.
    def input parameter v-field-06 like amt               no-undo.
    def input parameter v-field-07 like ag                no-undo.
    
    
    /*put stream s-temp unformatted
        trim(cust.cust-no)                                      + "," +
        trim(cust.name)                                         + "," +
        trim(cust.contact)                                      + "," +
        trim(v-sman)                                            + "," +
        trim(if avail terms then terms.dscr else "")            + "," +
        trim(cust.addr[1])                                      + "," +
        trim(cust.addr[2])                                      + "," +
        trim(cust.city)                                         + "," +
        trim(cust.state)                                        + "," +
        trim(cust.zip)                                          + "," +
        trim(string(cust.cr-lim,">>>>>>>>9.99"))                + "," +
        trim(string(cust.area-code,"(xxx)") + " " +
             string(cust.phone,"xxx-xxxx"))                     + "," +
        trim(string(substr(cust.fax,1,3),"(xxx)") + " " +
             string(substr(cust.fax,4,7),"xxx-xxxx"))           + "," +
        trim(v-field-01)                                        + "," +
        trim(string(v-field-02,"->>>>"))                        + "," +
        trim(v-field-03)                                        + "," +
        trim(v-field-04)                                        + "," +
        trim(string(v-field-05,"99/99/9999"))                   + "," +
        trim(string(v-field-06,"->>>>>>>>9.99"))                + "," +
        trim(string(v-field-07,"->>>>>>>>9.99"))
        skip.*/
    EXPORT STREAM s-temp DELIMITER ","
            trim(cust.cust-no)                               
            trim(cust.name)                                  
            trim(cust.contact)                               
            trim(v-sman)                                     
            trim(if avail terms then terms.dscr else "")     
            trim(cust.addr[1])                               
            trim(cust.addr[2])                               
            trim(cust.city)                                  
            trim(cust.state)                                 
            trim(cust.zip)                                   
            trim(string(cust.cr-lim,">>>>>>>>9.99"))         
            trim(string(cust.area-code,"(xxx)") + " " +      
                 string(cust.phone,"xxx-xxxx"))              
            trim(string(substr(cust.fax,1,3),"(xxx)") + " " +
                 string(substr(cust.fax,4,7),"xxx-xxxx"))    
            trim(v-field-01)                                 
            trim(string(v-field-02,"->>>>"))                 
            trim(v-field-03)                                 
            trim(v-field-04)                                 
            trim(string(v-field-05,"99/99/9999"))            
            trim(string(v-field-06,"->>>>>>>>9.99"))         
            trim(string(v-field-07,"->>>>>>>>9.99"))  
       SKIP.

  end.  */
  
/* End ---------------------------------- Copr. 2002  Advanced Software, Inc. */
