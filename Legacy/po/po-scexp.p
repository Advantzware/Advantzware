/* ------------------------------------------------ po/po-scexp.p 2011/05 YSK */
/*                                                                            */
/* St. Clair export PO                                                  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-format AS CHAR NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF BUFFER xjob-mat FOR job-mat.
DEF BUFFER xitem    FOR item.

{po/po-print.i}

DEF VAR v-sname LIKE shipto.ship-name.
DEF VAR v-saddr LIKE shipto.ship-addr.
DEF VAR v-scity LIKE shipto.ship-city.
DEF VAR v-sstate LIKE shipto.ship-state.
DEF VAR v-szip LIKE shipto.ship-zip.
DEF VAR v-adder LIKE item.i-no EXTENT 6 NO-UNDO.
DEF VAR xg-flag AS LOG INIT NO NO-UNDO.
DEF VAR v-instr AS CHAR NO-UNDO.
DEF VAR v-ord-qty LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR v-ord-cst LIKE po-ordl.cost NO-UNDO.
DEF VAR v-outfile AS CHAR EXTENT 4 NO-UNDO.
DEF VAR v-mach AS CHAR EXTENT 4 NO-UNDO.
DEF VAR v-line AS CHAR NO-UNDO.
DEF VAR v-char AS CHAR NO-UNDO.
DEF VAR ll-scored AS LOG NO-UNDO.

DEF TEMP-TABLE tt-score NO-UNDO FIELD tt-seq AS INT
                                FIELD tt-scor AS DEC
                                FIELD tt-type AS CHAR
                                INDEX tt-seq tt-seq.


{sys/inc/stclair.i}

FIND FIRST po-ctrl NO-LOCK WHERE po-ctrl.company EQ cocode.

FIND FIRST company NO-LOCK WHERE company.company EQ cocode.

IF STClair-log AND STClair-dir NE "" THEN
print-po-blok:
FOR EACH report NO-LOCK WHERE report.term-id EQ v-term-id,

    FIRST po-ord
    WHERE RECID(po-ord) EQ report.rec-id
      AND CAN-FIND(FIRST po-ordl
                   WHERE po-ordl.company   EQ po-ord.company
                     AND po-ordl.po-no     EQ po-ord.po-no
                     AND po-ordl.item-type EQ YES
                     AND (v-printde-po OR NOT po-ordl.deleted)),

    FIRST vend NO-LOCK
    WHERE vend.company    EQ po-ord.company
      AND vend.vend-no    EQ po-ord.vend-no
      AND (vend.po-export EQ "GP" OR
           (poexport-cha  EQ "GP" AND vend.an-edi-vend))

    BREAK BY po-ord.po-no:

  IF FIRST(po-ord.po-no) THEN DO:
    IF OPSYS EQ "UNIX" AND SUBSTR(STClair-dir,1,1) NE v-slash THEN
      STClair-dir = v-slash + STClair-dir.

    IF SUBSTR(STClair-dir,LENGTH(STClair-dir),1) EQ v-slash THEN
      SUBSTR(STClair-dir,LENGTH(STClair-dir),1) = "".
    
    ASSIGN
     v-outfile[1] = TRIM(STClair-dir) + v-slash + "dataxfer" +
                    v-slash + "in" + v-slash
     v-outfile[2] = v-outfile[1] + STRING(TIME,"99999999")
     v-outfile[3] = "po_" + TRIM(REPLACE(v-format, " ","")) + "STC" +
                    SUBSTR(STRING(YEAR(TODAY),"9999"),3,2) +
                    STRING(MONTH(TODAY),"99") +
                    STRING(DAY(TODAY),"99") +
                    SUBSTR(STRING(TIME,"HH:MM:SS"),1,2) +
                    SUBSTR(STRING(TIME,"HH:MM:SS"),4,2) +
                    SUBSTR(STRING(TIME,"HH:MM:SS"),7,2) + ".xml"
     v-outfile[4] = v-outfile[1] + v-outfile[3]
     v-outfile[4] = replace (v-outfile[4], "'", '').
      FILE-INFO:FILE-NAME = v-outfile[1].
      IF FILE-INFO:FILE-TYPE EQ ?  THEN        
          OS-COMMAND SILENT  "mkdir " + value(v-outfile[1]).
    OUTPUT TO VALUE(v-outfile[4]).

    /* Order Download Specification - BEGIN */
    PUT UNFORMATTED
        "<pkgcustomxmlorders>".
  END.

  IF po-ord.stat EQ "N" THEN po-ord.stat = "O".

  PUT UNFORMATTED
      "<sheetorder>"

      "<canonicalfile>"
      "</canonicalfile>"

      "<purchasedby>"
      po-ord.buyer
      "</purchasedby>"

      "<plantid>"
      vend.name
      "</plantid>"

      "<billto>"
      company.name
      "</billto>".

  v-instr = "".
  FOR EACH notes WHERE notes.rec_key EQ po-ord.rec_key NO-LOCK:
    IF notes.note_text NE "" THEN 
      v-instr = TRIM(v-instr)                         +
                (IF v-instr EQ "" THEN "" ELSE ".**") +
                TRIM(notes.note_text).
  END.

  PUT UNFORMATTED
      "<ordernotes>"
      v-instr
      "</ordernotes>".

  ASSIGN
   v-sname    = company.name
   v-saddr[1] = company.addr[1]
   v-saddr[2] = company.addr[2]
   v-scity    = company.city
   v-sstate   = company.state
   v-szip     = company.zip.
 
  IF po-ord.type EQ "D" THEN
    ASSIGN
     v-sname    = po-ord.ship-name
     v-saddr[1] = po-ord.ship-addr[1]
     v-saddr[2] = po-ord.ship-addr[2]
     v-scity    = po-ord.ship-city
     v-sstate   = po-ord.ship-state
     v-szip     = po-ord.ship-zip.
  
  FOR EACH po-ordl
      WHERE po-ordl.company   EQ po-ord.company
        AND po-ordl.po-no     EQ po-ord.po-no
        AND po-ordl.item-type EQ YES
        AND (v-printde-po OR NOT po-ordl.deleted),
      
      FIRST item NO-LOCK
      WHERE item.company  EQ cocode
        AND item.i-no     EQ po-ordl.i-no
        AND item.mat-type EQ "B"
      
      BY po-ordl.line WITH FRAME po-line:
      
    ASSIGN
     xg-flag = NO
     v-adder = "".
    
    FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
          AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no))) +
                                  TRIM(po-ordl.job-no)
          AND job.job-no2 EQ po-ordl.job-no2
        NO-ERROR.
        
    IF AVAIL job THEN DO:
      FIND FIRST est NO-LOCK
          WHERE est.company EQ job.company
            AND est.est-no  EQ job.est-no
          NO-ERROR.
      
      FOR EACH job-mat NO-LOCK
          WHERE job-mat.company  EQ cocode
            AND job-mat.job      EQ job.job
            AND job-mat.job-no   EQ job.job-no
            AND job-mat.job-no2  EQ job.job-no2
            AND job-mat.i-no     EQ po-ordl.i-no
            AND job-mat.frm      EQ po-ordl.s-num
          USE-INDEX job
          BREAK BY job-mat.blank-no DESC:
        IF last(job-mat.blank-no)            OR
           job-mat.blank-no EQ po-ordl.b-num THEN LEAVE.
      END.

      IF AVAIL job-mat THEN do:
        FIND FIRST ef NO-LOCK
            WHERE ef.e-num   EQ job.e-num
              AND ef.form-no EQ job-mat.frm
            NO-ERROR.
   
        ASSIGN
         xg-flag = AVAIL ef AND (ef.xgrain EQ "S" or ef.xgrain EQ "B")
         i       = 0.
         
        FOR EACH xjob-mat NO-LOCK
            WHERE xjob-mat.company  EQ cocode
              AND xjob-mat.job      EQ job-mat.job
              AND xjob-mat.job-no   EQ job-mat.job-no
              AND xjob-mat.job-no2  EQ job-mat.job-no2
              AND xjob-mat.frm      EQ job-mat.frm
              AND xjob-mat.blank-no EQ job-mat.blank-no
              AND xjob-mat.i-no     ne job-mat.i-no,
              
            FIRST xitem NO-LOCK
            WHERE xitem.company  EQ cocode 
              AND xitem.i-no     EQ xjob-mat.i-no
              AND xitem.mat-type EQ "A":
              
          ASSIGN
           i          = i + 1
           v-adder[i] = xitem.i-no.
             
          IF i GE 6 THEN LEAVE.
        END.
      END.
    END.

    PUT UNFORMATTED
        "<detailline>"

        "<shipto>"
        v-sname
        "</shipto>"

        "<shiptoaddress>"
        v-saddr[1]
        SPACE(1)
        v-saddr[2]
        SPACE(1)
        v-scity
        SPACE(1)
        v-sstate
        SPACE(1)
        v-szip
        "</shiptoaddress>"

        "<customerponbr>"
        po-ord.po-no
        "a"
        po-ordl.line
        "</customerponbr>"

        "<customerpolineseqnbr>"
        po-ordl.job-no "-" + STRING(po-ordl.job-no2,"99") + "-" + STRING(po-ordl.s-num,"99")
        "</customerpolineseqnbr>"

        "<custitemname>"
        po-ordl.i-name
        "</custitemname>"
        
        "<custitemdscr1>"
        po-ordl.dscr[1]
        "</custitemdscr1>"

        "<custitemdscr2>"
        po-ordl.dscr[2]
        "</custitemdscr2>".

    v-ord-qty = po-ordl.ord-qty.
    
    IF po-ordl.pr-qty-uom NE "EA" THEN
      RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-qty, OUTPUT v-ord-qty).
                           
    IF v-ord-qty - TRUNC(v-ord-qty,0) gt 0 THEN
      v-ord-qty = TRUNC(v-ord-qty,0) + 1.

    PUT UNFORMATTED
        "<quantityordered>"
        v-ord-qty
        "</quantityordered>"

        "<uom>EA</uom>"

        "<duedate>"
        STRING(po-ordl.due-date,"99/99/99")
        "</duedate>"

        "<duetime>"
        STRING(61200,"HH:MM")
        "</duetime>"

        "<currentdatetime>"
        STRING(TODAY,"999999")
        SPACE(1)
        STRING(TIME,"HH:MM")
        "</currentdatetime>"

        "<boardcode>"
        po-ordl.i-no
        "</boardcode>"

        "<addoncodes>".

    DO i = 1 TO 6:
      IF v-adder[i] NE "" THEN
        PUT UNFORMATTED
            "<addoncode>"
            v-adder[i]
            "</addoncode>".
    END.

    PUT UNFORMATTED
        "</addoncodes>".

    RUN po/po-ordls.p (RECID(po-ordl)).

    EMPTY TEMP-TABLE tt-score.   
    
    IF po-ordl.scorePanels[i] NE 0 THEN
    DO i = 1 TO 20:
        CREATE tt-score.
        ASSIGN
         tt-seq  = i
         tt-type = SUBSTR(po-ordl.scoreType[i],1)
         tt-scor = TRUNC(po-ordl.scorePanels[i],0) +
                   ((po-ordl.scorePanels[i] - TRUNC(po-ordl.scorePanels[i],0)) * 6.25).
    END.

    i = 0.
    FOR EACH tt-score BREAK BY tt-seq:
      IF NOT FIRST(tt-seq) OR
         NOT LAST(tt-seq)  THEN DO:

        i = i + 1.

        IF NOT FIRST(tt-seq) THEN
          PUT.

        PUT UNFORMATTED
            "<scores>"

            "<scorecode>"
            tt-type
            "</scorecode>"

            "<scoresequence>"
            i - 1
            "</scoresequence>"

            "<score>"
            tt-scor
            "</score>"

            "</scores>".
      END.
    END.

    PUT UNFORMATTED
        "<length>"
        po-ordl.s-len
        "</length>"

        "<width>"
        po-ordl.s-wid
        "</width>".

    v-instr = "".
    FOR EACH notes WHERE notes.rec_key EQ po-ordl.rec_key NO-LOCK:
      IF notes.note_text NE "" THEN 
        v-instr = TRIM(v-instr)                         +
                  (IF v-instr EQ "" THEN "" ELSE ".**") +
                  TRIM(notes.note_text).
    END.

    PUT UNFORMATTED
        "<instructions>"
        v-instr
        "</instructions>"

        "<stackheight>"
        "</stackheight>"

        "<overrunpct>"
        po-ord.over-pct
        "</overrunpct>"

        "<underrunpct>"
        po-ord.under-pct
        "</underrunpct>"

        "</detailline>".
  END. /* FOR EACH po-ordl record */

  PUT UNFORMATTED
      "</sheetorder>".

  po-ord.printed = YES.

  IF LAST(po-ord.po-no)        AND
     SEARCH(v-outfile[4]) NE ? THEN DO:

    /* Order Download Specification - END */
    PUT UNFORMATTED
        "</pkgcustomxmlorders>".

    OUTPUT CLOSE.
    
    RUN po/ftppo.p (v-outfile[4], "STC"). 

    MESSAGE "ST. Clair file:" TRIM(v-outfile[3]) "has been created"
        VIEW-AS ALERT-BOX.
  END.
END. /* FOR EACH po-ord record */

/* end ----------------------------------- Copr. 2006  Advanced Software Inc. */
