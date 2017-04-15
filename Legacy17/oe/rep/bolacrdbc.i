/* ---------------------------------------------- oe/rep/bolacrdbc.i 12/99 FWK */
/* PRINT Empire BOL                                                           */
/* -------------------------------------------------------------------------- */

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
          BY report.key-03 /* oe-boll.po-no*/
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
    find first w2 where w2.cas-cnt eq oe-boll.qty-case 
        no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.qty-case
     w2.cases   = w2.cases + oe-boll.cases.
  end.

  if oe-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.partial 
        no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.partial
     w2.cases   = w2.cases + 1.
  end.
    
  find first oe-ordl where oe-ordl.company eq cocode
       and oe-ordl.ord-no  eq INT (report.key-02)
       and oe-ordl.i-no    eq report.key-01
       no-lock no-error.
  
  IF v-printline >= 33 THEN DO:
     v-printline = 0.
     PAGE {1}.
     {oe/rep/bolacrdbc2.i}
  END.

  v-job-no = "".
  if avail oe-ordl and oe-ordl.job-no ne "" then
     v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
                trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")).

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
                              v-job-po    = if oe-ordl.job-no eq "" then "" else
                                (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).
        ELSE
        if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.
        ELSE
        if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.
             
        IF v-printline >= 38 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolacrdbc2.i}
        END.

        IF FIRST(w2.cases * w2.cas-cnt) THEN 
          PUT {1} oe-ordl.part-no 
                  v-job-po  AT 17 FORM "x(15)" 
                  oe-boll.i-no AT 33 
                  oe-ordl.i-name FORM "x(22)"
                  w2.cases    AT 71 FORM "->>>9" " @"
                  w2.cas-cnt    FORM "->>>>>9" SKIP.
         

        ELSE DO:
            PUT {1}
                 oe-boll.ord-no.
            IF NOT v-print-binstags THEN PUT {1} oe-boll.job-no AT 17.
            IF NOT v-print-binstags THEN PUT {1} "-" oe-boll.job-no2.
            PUT {1}
                 oe-ordl.part-dscr1 FORM "x(30)" AT 33 
                 w2.cases  AT 71 FORM "->>>9" " @"
                 w2.cas-cnt FORM "->>>>>9" SKIP.
        END.
        v-printline = v-printline + 1.
        IF LAST(w2.cases * w2.cas-cnt) THEN DO:
          IF FIRST(w2.cases * w2.cas-cnt) THEN DO:
            PUT {1} oe-boll.ord-no.
            IF NOT v-print-binstags THEN PUT {1} oe-boll.job-no AT 17.
            IF NOT v-print-binstags THEN PUT {1} "-" oe-boll.job-no2.
            PUT {1} oe-ordl.part-dscr1 FORM "x(30)" AT 33 SKIP.
            v-printline = v-printline + 1.
          END.
          IF v-print-binstags THEN DO:
            FOR EACH tt-boll-bintag WHERE tt-boll-bintag.i-no = oe-boll.i-no AND
                tt-boll-bintag.po-no = report.key-03:
                
                FIND FIRST loadtag WHERE loadtag.company = cocode
                    AND loadtag.tag-no = tt-boll-bintag.tag
                    AND loadtag.job-no = tt-boll-bintag.job-no
                    AND loadtag.i-no = oe-boll.i-no  NO-LOCK NO-ERROR.

                PUT {1}
                    tt-boll-bintag.job-no AT 17
                    tt-boll-bintag.job-no2
                    tt-boll-bintag.loc-bin AT 33
                    tt-boll-bintag.tag FORMAT "x(24)".
                IF AVAIL loadtag THEN
                    PUT  loadtag.tag-date .
 		        v-printline = v-printline + 1.
		        IF v-printline >= 38 THEN DO:
                    v-printline = 0.
                    PAGE {1}.
                    {oe/rep/bolacrdbc2.i}
                END.
            END.
          END.
         
          PUT {1}
              "====================" AT 68 SKIP
              v-tot-pkgs AT 71 FORM "->>>9"  " ="
              v-ship-qty FORM "->>>>>z" SPACE(2)
              oe-boll.p-c SPACE(1)
              v-weight  FORM "->>>,>>9" SKIP.
          IF lv-bolfmt-int <> 1 THEN PUT "<r-3><UNITS=INCHES><c3><FROM><c23><r+3><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                    string("oe-boll.i-no") + ">" FORM "x(100)" "</B><P10>" SKIP.
          
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

                       IF v-printline >= 38 THEN DO:
                          v-printline = 0.
                          PAGE {1}.
                          {oe/rep/bolacrdbc2.i}
                       END.

                       PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                       v-printline = v-printline + 1.
                    END.
             END.
          END.
        END.
        v-tot-cases = v-tot-cases + w2.cases.
        DELETE w2.
      END. /*each w2*/
      PUT {1} SKIP(1).
      v-printline = v-printline + 1.
    END. /*last of (report.key-03)*/
  END.
  /* end of summary mods */
  ELSE DO:
     DISPLAY  {1}
          oe-ordl.part-no   WHEN AVAIL oe-ordl 
          oe-boll.po-no AT 17
          oe-boll.i-no AT 33 
          oe-ordl.i-name FORM "x(22)"
          oe-boll.cases FORM "->>,>>>" "@" SPACE(0)
          oe-boll.qty-case FORM "->>>>>Z" SKIP 
          oe-boll.ord-no 
          oe-boll.job-no AT 17 SPACE(0) "-" SPACE(0) oe-boll.job-no2 
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
/*          "<r-3><UNITS=INCHES><c3><FROM><c23><r+3><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" + */
/*                     string("oe-boll.i-no") + ">" FORM "x(100)" "</B><P10>" SKIP           */
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
                  IF v-printline >= 38 THEN DO:
                     v-printline = 0.
                     PAGE {1}.
                     {oe/rep/bolacrdbc2.i}
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
