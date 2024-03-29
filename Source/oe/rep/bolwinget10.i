/* ---------------------------------------------- oe/rep/bolwinget10.i 12/99 FWK */
/* PRINT Empire BOL                                                           */
/* -------------------------------------------------------------------------- */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No). */
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
   v-tot-pkgs = v-tot-pkgs + oe-boll.cases +
                if oe-boll.partial gt 0 then 1 else 0
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

  if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.qty-case
     w2.cases   = w2.cases + oe-boll.cases.
  end.

  if oe-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.partial
     w2.cases   = w2.cases + 1.
  end.

  FIND FIRST oe-ord WHERE oe-ord.company = oe-boll.company
      AND oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
    
  find first oe-ordl where oe-ordl.company eq cocode
       and oe-ordl.ord-no  eq int(report.key-02)
       and oe-ordl.i-no    eq report.key-01
       no-lock no-error.

  ASSIGN v-rel-po-no = "".
  if avail oe-ordl then
      FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company = cocode
        AND oe-rel.ord-no = oe-ordl.ord-no
        AND oe-rel.i-no = oe-ordl.i-no
        AND oe-rel.LINE = oe-ordl.LINE :
        IF oe-rel.po-no NE "" THEN DO:  
            v-rel-po-no = oe-rel.po-no. 
            LEAVE.
        END.
  END.

  IF LAST(report.key-01) THEN do:
      IF v-printline >= 40 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolwinget20.i}
      END.
  END.
  ELSE
      IF v-printline >= 44 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolwinget20.i}
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
                              v-job-po    = IF oe-ordl.job-no EQ "" THEN "" 
                                            ELSE TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-ordl.job-no, oe-ordl.job-no2))).
        ELSE
        if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.
        ELSE
        if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.
             
        IF v-printline >= 46 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolwinget20.i}
        END.

        IF FIRST(w2.cases * w2.cas-cnt) THEN 
          PUT {1} oe-ordl.part-no
                  v-rel-po-no  AT 17 FORM "x(15)" 
                  oe-boll.i-no AT 33 
                  oe-ordl.i-name FORM "x(22)"
                  w2.cases    AT 71 FORM "->>>9" " @"
                  w2.cas-cnt    FORM "->>>>>9"
                  SKIP.
        ELSE PUT {1} 
                 STRING(oe-ordl.ord-no)
                 IF AVAIL oe-ord AND oe-ord.job-no NE "" THEN TRIM(oe-ord.job-no) ELSE "" AT 17 
                 oe-ordl.part-dscr1 FORM "x(30)" AT 33 
                 w2.cases  AT 71 FORM "->>>9" " @"
                 w2.cas-cnt FORM "->>>>>9" SKIP.
        v-printline = v-printline + 1.
        
        IF LAST(w2.cases * w2.cas-cnt) THEN DO:
          IF FIRST(w2.cases * w2.cas-cnt) THEN DO:
            PUT {1} 
                STRING(oe-ordl.ord-no)
                IF AVAIL oe-ord AND oe-ord.job-no NE "" THEN TRIM(oe-ord.job-no) ELSE "" AT 17
                oe-ordl.part-dscr1 FORM "x(30)" AT 33 
                SKIP.
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
                          {oe/rep/bolwinget20.i}
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
     DISPLAY  {1}
          oe-ordl.part-no   WHEN AVAIL oe-ordl 
          v-rel-po-no 
          oe-boll.i-no 
          oe-ordl.i-name  FORM "x(19)"
          oe-boll.cases FORM "->>,>>>" "@" SPACE(0)
          oe-boll.qty-case FORM "->>>>>Z" SKIP
          IF AVAIL oe-ord AND oe-ord.job-no NE "" THEN TRIM(oe-ord.job-no) ELSE "" AT 17
          oe-ordl.part-dscr1 AT 33 FORM "x(25)" SPACE(11)
          v-1    FORM "->>,>>9"  when oe-boll.partial gt 0 "@" SPACE(0)
          oe-boll.partial   when oe-boll.partial gt 0 FORM "->>>>>z"  SKIP
     with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
     down {1} with frame bol-mid1.

     DISPLAY 
         "====================" AT 69 SKIP
         v-tot-pkgs AT 69 FORM "->>,>>9"  "=" SPACE(0)
         oe-boll.qty FORM "->>>>>z" SPACE(2)
         oe-boll.p-c SPACE(1)
         oe-boll.weight FORM "->>>,>>9"  SKIP
         with frame bol-mid2 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
     down {1} with frame bol-mid2.

     v-printline = v-printline + 5.

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
                     {oe/rep/bolwinget20.i}
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
