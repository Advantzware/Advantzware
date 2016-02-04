/* ---------------------------------------------- oe/rep/bolaxis.i  */
/* PRINT Capitol City BOL                                             */
/* ------------------------------------------------------------------ */
/*DEF VAR v-jb-nts AS CHAR NO-UNDO.*/

DEF VAR v-inst AS cha FORM "x(77)" EXTENT 6 NO-UNDO.
assign
 v-tot-wt    = 0
 v-tot-cases = 0
 v-tot-palls = 0
 v-tot-sf = 0.


for each report where report.term-id eq v-term-id,

    first oe-boll where recid(oe-boll) eq report.rec-id,

    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,

    first itemfg
    where itemfg.company eq oe-boll.company
      and itemfg.i-no    eq oe-boll.i-no
    no-lock

    break BY report.key-03
          BY oe-boll.i-no
          BY oe-boll.po-no
          BY oe-boll.line
          by report.key-01 /* oe-boll.i-no*/
          by report.key-02 /* oe-boll.ord-no*/
          BY oe-boll.job-no
          BY oe-boll.job-no2:

    IF FIRST-OF(report.key-02) THEN 
       ASSIGN v-ship-qty = 0
              v-weight   = 0 
              v-ord-qty  = 0.

    v-tot-sf = v-tot-sf + (oe-boll.qty * itemfg.t-sqft).

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
  /* Task# 10171112 SAB
     Some orders have duplicate item numbers so linking by order number and
     item number is not unique to find the record we need. */
  find first oe-ordl where oe-ordl.company eq cocode
       and oe-ordl.ord-no  eq int(report.key-02) /* order number */
       AND oe-ordl.job-no  EQ oe-boll.job-no     /* 11291102 */
       AND oe-ordl.job-no2 EQ oe-boll.job-no2    /* added match to sub-number */
       and oe-ordl.i-no    eq report.key-01      /* item number */
       no-lock no-error.
  /* 11291102 */
  IF NOT AVAIL oe-ordl THEN
    find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq int(report.key-02) /* order number */
         and oe-ordl.i-no    eq report.key-01      /* item number */
         no-lock no-error.

  FIND FIRST oe-relh WHERE oe-relh.company = cocode
                       AND oe-relh.r-no = oe-boll.r-no
                     NO-LOCK NO-ERROR.

  IF AVAIL oe-relh THEN
  FIND FIRST oe-rell WHERE oe-rell.company eq cocode
      AND oe-rell.ord-no EQ oe-boll.ord-no
      AND oe-rell.i-no EQ oe-boll.i-no
      AND oe-rell.rel-no EQ oe-relh.release# NO-LOCK NO-ERROR.

  IF v-printline >= 47 THEN DO:
     v-printline = 0.
     PAGE {1}.
     {oe/rep/bolaxis2.i}
        
  END.

  v-job-no = "".
  if avail oe-ordl and oe-ordl.job-no ne "" then
     v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
                trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")).

  ASSIGN v-ship-qty = v-ship-qty + oe-boll.qty
         v-weight   = v-weight + oe-boll.weight.
  IF AVAIL oe-ordl THEN
         v-ord-qty = v-ord-qty + oe-ordl.qty.
  
ASSIGN  j = 1.
  IF lv-bolfmt-int = 1 THEN DO:  /* show summary per item */
    IF LAST-OF(report.key-02) THEN DO:
       i = 0.
       FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
         i = i + 1.
         IF i eq 1 THEN ASSIGN v-part-dscr = (IF avail(oe-ordl) THEN oe-ordl.part-no ELSE "")
                               v-job-po    = (IF AVAIL oe-rell THEN oe-rell.po-no ELSE oe-boll.po-no)
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99")). 
                               /*v-jb-nts    = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99")).*/
            
         ELSE
         if i eq 2 THEN ASSIGN v-part-dscr = (IF AVAIL(oe-ordl) THEN oe-ordl.part-dscr1 ELSE "")
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99")).
         ELSE
         if i eq 3 then ASSIGN v-part-dscr = (IF AVAIL(oe-ordl) THEN oe-ordl.part-dscr1 ELSE "")
                               v-job-var = "".
         ELSE
         if i eq 4 then ASSIGN v-part-dscr = (IF AVAIL(oe-ordl) THEN oe-ordl.part-dscr2 ELSE "")
                               v-job-var = "".
             
         IF v-printline >= 47 THEN DO:
           v-printline = 0.
           PAGE {1}.
           {oe/rep/bolaxis2.i}
              
         END.
      
         IF FIRST(w2.cases * w2.cas-cnt) THEN 
           PUT {1} (IF avail(oe-ordl) THEN oe-ordl.qty ELSE 0)
                   v-job-po  AT 17 FORM "x(15)" 
                   /*oe-ordl.e-num AT 34 */
                   oe-ordl.i-name FORM "x(25)" AT 34
                   w2.cases    AT 68 FORM "->>>9" " @ "
                   w2.cas-cnt  FORM "->>>>>9"
                  SKIP.   
         ELSE PUT {1} 
                 /* (IF AVAIL(oe-ordl) THEN oe-ordl.qty ELSE 0)
                  v-job-po  AT 17 FORM "x(15)"*/
                 (IF avail(oe-ordl) THEN oe-ordl.part-no ELSE "") FORM "x(15)"
                 (IF AVAIL(oe-ordl) THEN oe-ordl.ord-no ELSE 0) AT 17 FORMAT ">>>>>9" 
                  /*oe-boll.i-no  AT 17 FORM "x(15)"*/
                 /* v-job-var AT 34 FORM "x(9)"*/
                  IF avail(oe-ordl) THEN oe-ordl.part-dscr1 ELSE "" FORM "x(25)" AT 34
                  w2.cases  AT 68 FORM "->>>9" " @ "
                  w2.cas-cnt FORM "->>>>>9"
                  SKIP.
         v-printline = v-printline + 1.
         
         IF LAST(w2.cases * w2.cas-cnt) THEN DO:
           IF FIRST(w2.cases * w2.cas-cnt) THEN DO:
             PUT {1}
                 (IF avail(oe-ordl) THEN oe-ordl.part-no ELSE "") FORM "x(15)"
                 (IF AVAIL(oe-ordl) THEN oe-ordl.ord-no ELSE 0) AT 17 FORMAT ">>>>>9" 
                /* oe-boll.i-no  AT 17 FORM "x(15)" */
                /* v-job-var AT 32*/
                 IF AVAIL(oe-ordl) THEN oe-ordl.part-dscr1 ELSE "" FORM "x(25)" AT 34
                 SKIP.
             v-printline = v-printline + 1.
           END.

           IF lv-print-specs THEN DO:
               IF AVAIL itemfg THEN DO:
                   {custom/notespcprt.i itemfg v-inst 6 lv-spec-list}
                    DO i = 1 TO 6:
                       IF v-inst[i] <> "" THEN DO:
               
                          IF v-printline >= 52 THEN DO:
                              v-printline = 0.
                              PAGE {1}.
                              {oe/rep/bolaxis2.i}
                          END.
               
                          PUT "<P12>" v-inst[i] "<P10>" FORMAT "x(77)" SKIP.     
                          v-printline = v-printline + 1.
                       END.
                    END.
               END. /*AVAIL ITEMFG*/
           END. /*LV-PRINT-SPECS*/

           PUT {1}
               "====================" AT 66 SKIP  
               v-tot-pkgs AT 68 FORM "->>>9"  " = "
               v-ship-qty FORM "->>>>>z" SPACE(3)
               oe-boll.p-c SPACE(1)
               v-weight SKIP .
           
           ASSIGN
              v-printline = v-printline + 3
              v-tot-pkgs  = 0.
           
           IF v-print-dept THEN
           DO:
              IF AVAIL oe-ordl THEN
                FIND FIRST job-hdr WHERE
                     job-hdr.company eq cocode
                    AND job-hdr.job-no EQ oe-ordl.job-no 
                    AND job-hdr.job-no2 EQ oe-ordl.job-no2
                    /*AND (trim(job-hdr.job-no) + "-" + string(job-hdr.job-no2,"99")) EQ v-jb-nts*/
                    NO-LOCK NO-ERROR.
            
              IF AVAIL job-hdr THEN
              DO:
                 FIND FIRST job WHERE
                      job.company eq cocode AND
                      job.job     eq job-hdr.job AND
                      job.job-no  eq job-hdr.job-no AND
                      job.job-no2 eq job-hdr.job-no2
                      NO-LOCK NO-ERROR.
               
                 IF AVAIL job THEN
                 DO:
                    FOR EACH notes WHERE
                       notes.rec_key EQ job.rec_key 
                       AND CAN-DO(v-depts,notes.note_code)
                        NO-LOCK
                        BY notes.note_code:
                    
                        v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 70.
                        {SYS/INC/ROUNDUP.I v-tmp-lines}
                       
                        IF notes.note_text <> "" THEN
                           DO i = 1 TO v-tmp-lines:
                          
                              /*IF v-printline >= 47 THEN DO:
                                 v-printline = 0.
                                 PAGE {1}.
                                 {oe/rep/bolaxis2.i}
                                 
                              END.*/ 
                                 
                                 IF j EQ 1  THEN
                                    PUT  "<R-1>".
                                 PUT  substring(NOTES.NOTE_TEXT,(1 + 70 * (i - 1)), 70) FORM "x(70)" SKIP.
                                 v-printline = v-printline + 1.
                                ASSIGN
                                    j = j + 1.
                                
                        END.
                        
                    END.
                    RELEASE job.
                 END.
                 RELEASE job-hdr.
              END.
              
              
           END.
         END.
         v-tot-cases = v-tot-cases + w2.cases.
         DELETE w2.
         
       END.
       PUT "<R-1>____________________________________________________________________________________________"SKIP(1).
       /*PUT {1} SPACE(1). */
       IF v-printline >= 47 THEN DO:
          v-printline = 0.
          PAGE {1}.
          {oe/rep/bolaxis2.i}
       END.
                              
      v-printline = v-printline + 2.
    END.
   
  END.
  /* end of summary mods */
  ELSE DO:
      v-job-po = (IF AVAIL oe-rell THEN oe-rell.po-no ELSE oe-boll.po-no) .
     DISPLAY  {1}
          oe-ordl.qty  WHEN AVAIL oe-ordl
          v-job-po AT 17 FORMAT "x(15)"
          /*oe-ordl.e-num  WHEN AVAIL oe-ordl AT 33*/
          oe-ordl.i-name WHEN AVAIL oe-ordl AT 34 FORM "x(25)"
          oe-boll.cases FORM "->>>>>" AT 68 "@"
          oe-boll.qty-case FORM "->>>>>Z" SKIP
          oe-ordl.part-no   WHEN AVAIL oe-ordl 
          oe-ordl.ord-no WHEN AVAIL oe-ordl AT 17
          /*oe-boll.i-no AT 17 FORM "x(15)"*/
         /* (if oe-boll.job-no eq "" then "" else
           (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99")))  AT 33 */
          oe-ordl.part-dscr1 WHEN AVAIL oe-ordl AT 34 FORM "x(25)"
          v-1    FORM "->>>>9"  when oe-boll.partial gt 0 AT 68  "@"
          oe-boll.partial   when oe-boll.partial gt 0 FORM "->>>>>z"  SKIP
     with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
     down {1} with frame bol-mid1.

     IF lv-print-specs THEN DO:
           IF AVAIL itemfg THEN DO:
               {custom/notespcprt.i itemfg v-inst 6 lv-spec-list}
                DO i = 1 TO 6:
                   IF v-inst[i] <> "" THEN DO:

                      IF v-printline >= 52 THEN DO:
                          v-printline = 0.
                          PAGE {1}.
                          {oe/rep/bolaxis2.i}
                      END.

                      PUT "<P12>" v-inst[i] "<P10>" FORMAT "x(77)" SKIP.     
                      v-printline = v-printline + 1.
                   END.
                END.
           END. /*AVAIL ITEMFG*/
     END. /*LV-PRINT-SPECS*/

     DISPLAY 
         "====================" AT 66 SKIP
         v-tot-pkgs AT 67 FORM "->>,>>9"  "="
         oe-boll.qty FORM "->>>>>z" SPACE(2)
         oe-boll.p-c SPACE(1)
         oe-boll.weight  SKIP
         with frame bol-mid2 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
     down {1} with frame bol-mid2.

     v-printline = v-printline + 6.

     IF v-print-dept AND AVAIL(oe-ordl) THEN
     DO:
        FOR EACH notes WHERE
            notes.rec_key EQ oe-ordl.rec_key AND
            CAN-DO(v-depts,notes.note_code)
            NO-LOCK
            BY notes.note_code:

            v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 70.
            {SYS/INC/ROUNDUP.I v-tmp-lines}
            IF notes.note_text <> "" THEN
               DO i = 1 TO v-tmp-lines:
                  IF v-printline >= 52 THEN DO:
                     v-printline = 0.
                     PAGE {1}.
                     {oe/rep/bolaxis2.i}
                        
                  END.
                    
                  IF j EQ 1 THEN
                     PUT  "<R-1>".
                  PUT  substring(NOTES.NOTE_TEXT,(1 + 70 * (i - 1)), 70) FORM "x(70)" SKIP.
                  v-printline = v-printline + 1.
                  j = j + 1. 
               END.
        END.
     END.
     PUT "<r-1>_____________________________________________________________________________________________"SKIP(1).
     /*PUT {1} SPACE(1).*/
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
        xitemfg.i-name                        FORMAT "x(18)"
        oe-boll.qty * v-part-qty        TO 75 FORMAT "->>>,>>9"
        skip(1).

    v-printline = v-printline + 2.
  end.

  v-tot-wt = v-tot-wt + oe-boll.weight.

  if oe-boll.weight eq 0 then
    v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).
end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
