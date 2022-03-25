/* ---------------------------------------------- oe/rep/bolhenry.i 12/99 FWK */
/* PRINT Henry BOL                                                            */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.              */
/* -------------------------------------------------------------------------- */
   DEFINE VARIABLE iQuantitySubUnitsPerUnit AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lPartialExist AS LOGICAL NO-UNDO.
   DEFINE VARIABLE dQuantityPerSubUnit AS DECIMAL NO-UNDO.
assign
 v-tot-wt    = 0
 v-tot-cases = 0
 v-tot-palls = 0.

for each report where report.term-id eq v-term-id,

    first oe-boll where recid(oe-boll) eq report.rec-id,

    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,

    first itemfg
    where itemfg.company eq oe-boll.company
      and itemfg.i-no    eq oe-boll.i-no
    no-lock

    break by report.key-01 /* oe-boll.i-no*/
          by report.key-02 /* oe-boll.ord-no*/
          BY oe-boll.line
          BY oe-boll.po-no
          BY oe-boll.job-no
          BY oe-boll.job-no2:

    IF FIRST-OF(report.key-02) THEN DO:
       ASSIGN v-ship-qty = 0
              v-weight   = 0
              v-ord-qty  = 0.
    END.

  ASSIGN
   v-tot-pkgs = v-tot-pkgs + oe-boll.tot-pallets 
   v-pal-cnt  = oe-boll.qty-case.

  FIND FIRST fg-bin
      WHERE fg-bin.company EQ oe-boll.company
        AND fg-bin.i-no    EQ oe-boll.i-no
        AND fg-bin.job-no  EQ oe-boll.job-no
        AND fg-bin.job-no2 EQ oe-boll.job-no2
        AND fg-bin.loc     EQ oe-boll.loc
        AND fg-bin.loc-bin EQ oe-boll.loc-bin
        AND fg-bin.tag     EQ oe-boll.tag
      NO-LOCK NO-ERROR.
  IF AVAIL fg-bin THEN
    v-pal-cnt = v-pal-cnt                                                     *
                (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   *
                (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet).

  v-pal-cnt = oe-boll.qty / v-pal-cnt.

  {sys/inc/roundup.i v-pal-cnt}

  v-tot-palls = v-tot-palls + v-pal-cnt.
  lPartialExist = NO.
  if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.qty-case
                  AND w2.cases EQ oe-boll.cases no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.qty-case
     w2.cases   = oe-boll.cases.
     RUN Inventory_GetQuantityOfUnitsForOEBoll IN hdInventoryProcs (rowid(oe-boll), OUTPUT iPallet).        
     w2.pallets = w2.pallets + iPallet .   
     
     RUN Inventory_GetQuantityOfSubUnitsPerUnitFromBinAndOrder IN hdInventoryProcs (oe-boll.company, oe-boll.i-no, oe-boll.job-no, oe-boll.job-no2, oe-boll.loc, oe-boll.loc-bin, oe-boll.tag, oe-boll.ord-no,
                                                                                    OUTPUT iQuantitySubUnitsPerUnit).
     ASSIGN                                                                                    
      dQuantityPerSubUnit  = oe-boll.qty-case 
      dQuantityPerSubUnit  = MAX(1,dQuantityPerSubUnit)  .
            
      IF oe-boll.qty GT (dQuantityPerSubUnit * iQuantitySubUnitsPerUnit) AND oe-boll.partial GT 0 THEN
      lPartialExist = YES.
      ELSE lPartialExist = NO.
           
      IF lPartialExist THEN
      w2.pallets = w2.pallets - 1.  
      w2.lPartal = lPartialExist.
      w2.partial = oe-boll.partial.      
  end.

  if oe-boll.partial ne 0 AND lPartialExist then do:
    find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.partial
     w2.cases   = w2.cases + 1
     w2.pallets = w2.pallets + 1.
  end.
    
  find first oe-ordl where oe-ordl.company eq cocode
       and oe-ordl.ord-no  eq int(report.key-02)
       and oe-ordl.i-no    eq report.key-01
       no-lock no-error.

  IF LAST(report.key-01) THEN do:
      IF v-printline >= 40 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolhenry2.i}
      END.
  END.
  ELSE
      IF v-printline >= 44 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolhenry2.i}
      END.

          

  v-job-no = "".
  if avail oe-ordl and oe-ordl.job-no ne "" then
     v-job-no = TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-ordl.job-no, oe-ordl.job-no2))).

  ASSIGN v-ship-qty = v-ship-qty + oe-boll.qty
         v-weight   = v-weight + oe-boll.weight
         v-ord-qty = v-ord-qty + oe-ordl.qty.
  
  IF lv-bolfmt-int = 1 THEN DO:  /* show summary per item */
    IF LAST-OF(report.key-02) THEN DO:
      i = 0.
      FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
        i = i + 1.
        IF i eq 1 THEN ASSIGN v-part-dscr = oe-ordl.part-no
                              v-job-po    = oe-boll.po-no.
        ELSE
        if i eq 2 THEN ASSIGN v-part-dscr = oe-ordl.part-dscr1 /*i-name*/
                              v-job-po    = if oe-ordl.job-no eq "" then "" ELSE 
                                            TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-ordl.job-no, oe-ordl.job-no2))).
        ELSE
        if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.
        ELSE
        if i eq 4 then v-part-dscr = oe-ordl.part-dscr2. 
             
        IF v-printline >= 46 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolhenry2.i}
        END.

        IF FIRST(w2.cases * w2.cas-cnt) THEN 
          PUT {1} oe-ordl.part-no
                  v-job-po  AT 17 FORM "x(15)" 
                  oe-boll.i-no AT 33 
                  oe-ordl.i-name FORM "x(22)"
                  w2.pallets       AT 71 FORM "->>>9" " @"
                  (w2.cas-cnt * w2.cases) + (IF NOT w2.lPartal THEN w2.partial ELSE 0)    FORM "->>>>>9"
                  SKIP.
        ELSE do: 
                PUT {1} 
                 oe-ordl.ord-no
                 oe-ordl.part-dscr1 FORM "x(30)" AT 33 
                 w2.pallets AT 71 FORM "->>>9" " @"
                 (w2.cases * w2.cas-cnt) + (IF NOT w2.lPartal THEN w2.partial ELSE 0)  FORM "->>>>>9" SKIP.
                 IF oe-ordl.part-dscr2 NE "" THEN
                 do:                    
                     PUT {1}
                     oe-ordl.part-dscr2 FORM "x(30)" AT 33 SKIP.
                     v-printline = v-printline + 1.
                 END.
                 IF oe-ordl.part-dscr3 NE "" THEN
                 do:
                     PUT {1}
                     oe-ordl.part-dscr3 FORM "x(30)" AT 33 SKIP.
                     v-printline = v-printline + 1.                     
                 END.
        END.         
        v-printline = v-printline + 1.
        
        IF LAST(w2.cases * w2.cas-cnt) THEN DO:
          IF FIRST(w2.cases * w2.cas-cnt) THEN DO:
            PUT {1} 
                oe-ordl.ord-no
                oe-ordl.part-dscr1 FORM "x(30)" AT 33 SKIP.
            IF oe-ordl.part-dscr2 NE "" THEN
            do:
                PUT {1}
                oe-ordl.part-dscr2 FORM "x(30)" AT 33 SKIP.
                v-printline = v-printline + 1.
            END.
            IF oe-ordl.part-dscr3 NE "" THEN
            do:
                PUT {1}    
                oe-ordl.part-dscr3 FORM "x(30)" AT 33 SKIP .
                v-printline = v-printline + 1.
            END.    
            v-printline = v-printline + 1.
          END.

          PUT {1}
              "====================" AT 68 SKIP
              v-tot-pkgs AT 71 FORM "->>>9"  " ="
              v-ship-qty FORM "->>>>>z" SPACE(2)
              oe-boll.p-c SPACE(1)
              v-weight  FORM "->>>,>>9" SKIP.

          ASSIGN
             v-printline = v-printline + 2
             v-tot-pkgs  = 0.

          IF v-print-dept THEN
          DO:
             FOR EACH notes WHERE
                 notes.rec_key EQ oe-ordl.rec_key AND
                 CAN-DO(v-depts,notes.note_code)
                 NO-LOCK
                 BY notes.note_code:
         
                 v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                 {SYS/INC/ROUNDUP.I v-tmp-lines}

                 IF notes.note_text <> "" THEN
                    DO i = 1 TO v-tmp-lines:

                       IF v-printline >= 46 THEN DO:
                          v-printline = 0.
                          PAGE {1}.
                          {oe/rep/bolhenry2.i}
                       END.

                       PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                       v-printline = v-printline + 1.
                    END.
             END.
          END.
        END.
        v-tot-cases = v-tot-cases + w2.cases.
        DELETE w2.
      END.
      PUT {1} SKIP(1).
      v-printline = v-printline + 1.
    END.
  END.
  /* end of summary mods */
  ELSE DO:
     RUN Inventory_GetQuantityOfSubUnitsPerUnitFromBinAndOrder IN hdInventoryProcs (oe-boll.company, oe-boll.i-no, oe-boll.job-no, oe-boll.job-no2, oe-boll.loc, oe-boll.loc-bin, oe-boll.tag, oe-boll.ord-no,
                                                                                    OUTPUT iQuantitySubUnitsPerUnit).
                                                                                          
      lPartialExist =  IF oe-boll.qty - ( MAX(1, iQuantitySubUnitsPerUnit) * ( IF oe-boll.qty-case NE 0 THEN oe-boll.qty-case ELSE 1) )  GT 0 AND oe-boll.partial NE 0 THEN TRUE ELSE FALSE.
      
     DISPLAY  {1}
          oe-ordl.part-no   WHEN AVAIL oe-ordl 
          oe-boll.po-no 
          oe-boll.i-no 
          oe-ordl.i-name  FORM "x(19)"
          (oe-boll.tot-pallets - (IF lPartialExist THEN 1 ELSE 0)) FORM "->>,>>>" "@" SPACE(0)
          (oe-boll.cases * oe-boll.qty-case) + (IF NOT lPartialExist THEN w2.partial ELSE 0) FORM "->>>>>Z" SKIP          
          oe-ordl.part-dscr1 AT 33 FORM "x(25)" SPACE(11)
          v-1    FORM "->>,>>9"  when oe-boll.partial gt 0 AND lPartialExist "@" SPACE(0)
          oe-boll.partial   when oe-boll.partial gt 0 AND lPartialExist FORM "->>>>>z"  SKIP          
     with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
     down {1} with frame bol-mid1.
     IF oe-ordl.part-dscr2 NE "" THEN
     DO:
         PUT {1}
         oe-ordl.part-dscr2 AT 33 FORM "x(25)" SKIP.
         v-printline = v-printline + 1.
     END.
     IF oe-ordl.part-dscr3 NE "" THEN
     DO:
         PUT {1}
         oe-ordl.part-dscr3 AT 33 FORM "x(25)" SKIP.
         v-printline = v-printline + 1.
     END.
             
     DISPLAY 
         "====================" AT 69 SKIP
         v-tot-pkgs AT 69 FORM "->>,>>9"  "=" SPACE(0)
         oe-boll.qty FORM "->>>>>z" SPACE(2)
         oe-boll.p-c SPACE(1)
         oe-boll.weight FORM "->>>,>>9"  SKIP
         with frame bol-mid2 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
     down {1} with frame bol-mid2.  

     v-printline = v-printline + 7.

     IF v-print-dept THEN
     DO:
        FOR EACH notes WHERE
            notes.rec_key EQ oe-ordl.rec_key AND
            CAN-DO(v-depts,notes.note_code)
            NO-LOCK
            BY notes.note_code:

            v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
            {SYS/INC/ROUNDUP.I v-tmp-lines}
            IF notes.note_text <> "" THEN
               DO i = 1 TO v-tmp-lines:
                  IF v-printline >= 46 THEN DO:
                     v-printline = 0.
                     PAGE {1}.
                     {oe/rep/bolhenry2.i}
                  END.

                  PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                  v-printline = v-printline + 1.
               END.
        END.
     END.

     put {1} skip(1).
     ASSIGN
        v-tot-cases = v-tot-cases + v-tot-pkgs
        v-tot-pkgs  = 0.
  END.
  
  if v-print-components then
  for each fg-set
      where fg-set.company eq cocode
        and fg-set.set-no  eq oe-boll.i-no
      no-lock,
      
      first xitemfg
      where xitemfg.company eq cocode
        and xitemfg.i-no    eq fg-set.part-no
      no-lock
      
      break by fg-set.set-no:
      
    {sys/inc/part-qty.i v-part-qty fg-set}
    
    put {1}
        xitemfg.part-no
        fg-set.part-no                  AT 33
        xitemfg.i-name                        FORMAT "x(22)"
        oe-boll.qty * v-part-qty        TO 80 FORMAT "->>>,>>9"
        skip(1).

    v-printline = v-printline + 2.
  end.

  v-tot-wt = v-tot-wt + oe-boll.weight.

  if oe-boll.weight eq 0 then
    v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).
end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
