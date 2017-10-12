/* -------------------------------------------------- po/po-gpexp.p  WFK 11/15*/
/*                                                                            */
/* Georgia Pacific export PO                                                  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-format AS CHAR NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF BUFFER xjob-mat FOR job-mat.
DEF BUFFER xitem    FOR item.
DEF BUFFER b-ref1   FOR reftable.
DEF BUFFER b-ref2   FOR reftable.

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
DEF STREAM sOut.
DEF VAR cInLn AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-score NO-UNDO 
    FIELD tt-seq  AS INT
    FIELD tt-scor AS DEC
    FIELD tt-type AS CHAR
    INDEX tt-seq tt-seq.
    
DEFINE TEMP-TABLE sheetorder NO-UNDO
    BEFORE-TABLE sheetorderBef
	    FIELD customerpo AS INT
        FIELD canonicalfile AS CHAR
        FIELD purchasedby AS CHAR
        FIELD plantid AS CHAR 
        FIELD billto AS CHAR
        FIELD ordernotes AS CHAR 
        INDEX customerpo IS PRIMARY UNIQUE customerpo
          .

/* Definition for Temp-Table detailline */
DEFINE TEMP-TABLE detailline
    BEFORE-TABLE detaillineBef
        FIELD customerpo AS INT
        FIELD lineNo AS INT
        FIELD shipto AS CHAR
        FIELD shiptoaddress AS CHAR
        FIELD customerponbr AS CHAR
        FIELD customerpolineseqnbr AS CHAR
        FIELD internalprodno AS CHAR
        FIELD custitemname AS CHAR
        FIELD custitemdscr1 AS CHAR
        FIELD custitemdscr2 AS CHAR
        FIELD quantityordered AS DEC
        FIELD uom AS CHAR
        FIELD duedate AS CHAR
        FIELD duetime AS CHAR
        FIELD currentdatetime AS CHAR
        FIELD boardcode AS CHAR
        FIELD addoncodes AS CHAR EXTENT 6
        FIELD scores AS CHAR
        FIELD length AS DEC
        FIELD width AS DEC
        FIELD instructions AS CHAR
        FIELD stackheight AS CHAR
        FIELD overrunpct AS CHAR
        FIELD underrunpct AS CHAR  .

DEF TEMP-TABLE scores
  FIELD customerpo AS INT
  FIELD lineNo AS INT
  FIELD scorecode AS CHAR
  FIELD scoresequence AS INT
  FIELD score AS CHAR
  .
  
DEF TEMP-TABLE addoncodes
  FIELD customerpo AS INT
  FIELD lineNo AS INT
  FIELD addoncode AS CHAR

  .
DEFINE DATASET pkgcustomxmlorders 
    FOR sheetorder, detailline, AddonCodes, scores
    DATA-RELATION custOrd FOR sheetorder, detailline     
    REPOSITION RELATION-FIELDS (customerpo, customerpo) NESTED

    DATA-RELATION forAddons FOR detailline, AddonCodes  
    REPOSITION RELATION-FIELDS (customerpo, customerpo, lineNo, lineNo) NESTED

    DATA-RELATION forscores FOR detailline, scores  
    REPOSITION RELATION-FIELDS (customerpo, customerpo, lineNo, lineNo) NESTED
    .
DEFINE VARIABLE iNextLine AS INTEGER NO-UNDO.
DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.
DEFINE VARIABLE cFieldTypeMapping AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerifySchemaMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEncoding AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted AS LOGICAL NO-UNDO.
DEFINE VARIABLE lWriteSchema AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMinSchema AS LOGICAL NO-UNDO.
DEFINE VARIABLE lWriteBeforeImage AS LOGICAL NO-UNDO.  
DEFINE VARIABLE lOmitInitialValues AS LOGICAL NO-UNDO.

{sys/inc/gp.i}

FIND FIRST po-ctrl NO-LOCK WHERE po-ctrl.company EQ cocode.

FIND FIRST company NO-LOCK WHERE company.company EQ cocode.

IF gp-log AND gp-dir NE "" THEN
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
    IF OPSYS EQ "UNIX" AND SUBSTR(gp-dir,1,1) NE v-slash THEN
      gp-dir = v-slash + gp-dir.

    IF SUBSTR(gp-dir,LENGTH(gp-dir),1) EQ v-slash THEN
      SUBSTR(gp-dir,LENGTH(gp-dir),1) = "".

    /* Output to a temporary file */
    ASSIGN
     iNextLine = 0
     v-outfile[1] = TRIM(gp-dir) + v-slash + "dataxfer" +
                    v-slash + "in" + v-slash
     v-outfile[2] = v-outfile[1] + STRING(TIME,"99999999")
     v-outfile[3] = "po_" + TRIM(REPLACE(v-format, " ","")) + "GP" +
                    SUBSTR(STRING(YEAR(TODAY),"9999"),3,2) +
                    STRING(MONTH(TODAY),"99") +
                    STRING(DAY(TODAY),"99") +
                    SUBSTR(STRING(TIME,"HH:MM:SS"),1,2) +
                    SUBSTR(STRING(TIME,"HH:MM:SS"),4,2) +
                    SUBSTR(STRING(TIME,"HH:MM:SS"),7,2) + ".xml"
     v-outfile[4] = v-outfile[1] + v-outfile[3]
     v-outfile[4] = REPLACE (v-outfile[4], "'", '').
    
    /* OUTPUT TO VALUE(v-outfile[4]). */
  END.

  IF po-ord.stat EQ "N" THEN po-ord.stat = "O".
   CREATE sheetorder .

  ASSIGN
      sheetorder.customerpo = po-ord.po-no
      /* sheetorder.canonicalfile = " " */
      sheetorder.purchasedby = po-ord.buyer
      sheetorder.plantid = vend.name
      sheetorder.billto = company.name.

  v-instr = "".
  FOR EACH notes WHERE notes.rec_key EQ po-ord.rec_key NO-LOCK:
    IF notes.note_text NE "" THEN 
      v-instr = TRIM(v-instr)                         +
                (IF v-instr EQ "" THEN "" ELSE ".**") +
                TRIM(notes.note_text).
  END.

  sheetorder.ordernotes =  v-instr  .

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
        AND (v-printde-po OR NOT po-ordl.deleted) ,
      
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
        IF LAST(job-mat.blank-no)            OR
           job-mat.blank-no EQ po-ordl.b-num THEN LEAVE.
      END.

      IF AVAIL job-mat THEN DO:
        FIND FIRST ef NO-LOCK
            WHERE ef.e-num   EQ job.e-num
              AND ef.form-no EQ job-mat.frm
            NO-ERROR.
   
        ASSIGN
         xg-flag = AVAIL ef AND (ef.xgrain EQ "S" OR ef.xgrain EQ "B")
         i       = 0.
         
        FOR EACH xjob-mat NO-LOCK
            WHERE xjob-mat.company  EQ cocode
              AND xjob-mat.job      EQ job-mat.job
              AND xjob-mat.job-no   EQ job-mat.job-no
              AND xjob-mat.job-no2  EQ job-mat.job-no2
              AND xjob-mat.frm      EQ job-mat.frm
              AND xjob-mat.blank-no EQ job-mat.blank-no
              AND xjob-mat.i-no     NE job-mat.i-no,
              
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

    iNextLine = iNextLine + 1.
    CREATE detailline .
    ASSIGN
        detailline.lineNo     = iNextLine
        detailline.customerpo = po-ord.po-no
        detailline.shipto = v-sname
        detailline.shiptoaddress = v-saddr[1] + " " + v-saddr[2] + " " + v-scity + " " +
                                    v-sstate + " " + v-szip
        detailline.customerponbr = STRING(po-ord.po-no)
        detailline.customerpolineseqnbr = string(po-ordl.line)
        detailline.internalprodno = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99") + "-" + STRING(po-ordl.s-num,"99")
        detailline.custitemname = po-ordl.i-name
        detailline.custitemdscr1 = po-ordl.dscr[1]
        detailline.custitemdscr2 = po-ordl.dscr[2] .

    v-ord-qty = po-ordl.ord-qty.
    
    IF po-ordl.pr-qty-uom NE "EA" THEN
      RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-qty, OUTPUT v-ord-qty).
                           
    IF v-ord-qty - TRUNC(v-ord-qty,0) GT 0 THEN
      v-ord-qty = TRUNC(v-ord-qty,0) + 1.
     ASSIGN
         detailline.quantityordered = v-ord-qty
         detailline.uom = "EA"
         detailline.duedate = STRING(po-ordl.due-date,"99/99/99")
         detailline.duetime = STRING(61200,"HH:MM")
         detailline.currentdatetime = STRING(TODAY,"999999") + " " + STRING(TIME,"HH:MM")
         detailline.boardcode = po-ordl.i-no .

    DO i = 1 TO 6:

      IF v-adder[i] NE "" THEN DO:

        CREATE addoncodes.

        ASSIGN 
          addoncodes.customerPO = detailline.customerPo
          addoncodes.lineNo     = detailline.lineNo
          addoncodes.addoncode  = v-adder[i].
          /* ASSIGN detailline.addoncodes[i] = v-adder[i] . */

      END. /* if v-adder[i] ne "" ... */
          
    END. /* do i = 1 to 6 */

    RUN po/po-ordls.p (RECID(po-ordl)).
    
    {po/po-ordls.i}

    EMPTY TEMP-TABLE tt-score.   
    
    IF AVAIL b-ref1 THEN
    DO i = 1 TO 12:
      IF b-ref1.val[i] NE 0 THEN DO:
        CREATE tt-score.
        ASSIGN
         tt-seq  = i
        tt-type = SUBSTR(b-ref1.dscr,i,1)
        tt-scor = TRUNC(b-ref1.val[i],0) +
                   ((b-ref1.val[i] - TRUNC(b-ref1.val[i],0)) * 6.25).
      END.
    END.

    IF AVAIL b-ref2 THEN
    DO i = 1 TO 8:
      IF b-ref2.val[i] NE 0 THEN DO:
        CREATE tt-score.
        ASSIGN
        tt-seq  = 12 + i
        tt-type = SUBSTR(b-ref2.dscr,i,1)
        tt-scor = TRUNC(b-ref2.val[i],0) +
                   ((b-ref2.val[i] - TRUNC(b-ref2.val[i],0)) * 6.25).
      END.
    END.

      i = 0.
      FOR EACH tt-score BREAK BY tt-seq:
          IF NOT FIRST(tt-seq) OR
              NOT LAST(tt-seq)  THEN 
          DO:

        i = i + 1.
      
          CREATE scores.
          ASSIGN 
          scores.customerpo    = detailline.customerPo
          scores.lineNo        = iNextLine
          scores.scoresequence = i - 1
          scores.score         = STRING(tt-scor)
          scores.scorecode     = tt-type.            
          

      END.
    END.

    ASSIGN detailline.length = po-ordl.s-len
           detailline.width = po-ordl.s-wid .
     
    v-instr = "".
    FOR EACH notes WHERE notes.rec_key EQ po-ordl.rec_key NO-LOCK:
      IF notes.note_text NE "" THEN 
        v-instr = TRIM(v-instr)                         +
                  (IF v-instr EQ "" THEN "" ELSE ".**") +
                  TRIM(notes.note_text).
    END.

    ASSIGN detailline.instructions = v-instr
           detailline.overrunpct = STRING(po-ord.over-pct )
           detailline.underrunpct = STRING(po-ord.under-pct) .

  END. /* FOR EACH po-ordl record */

  /*PUT UNFORMATTED
      "</sheetorder>".*/

  po-ord.printed = YES.
  IF LAST(po-ord.po-no) AND CAN-FIND( FIRST sheetorder)
     /* SEARCH(v-outfile[4]) NE ? */ THEN DO:

      DEFINE VARIABLE returnValue AS LOGICAL NO-UNDO.
      DEFINE VARIABLE hPDS AS HANDLE.
      hPDS = DATASET pkgcustomxmlorders:HANDLE.
      
      ASSIGN
          cTargetType = "FILE"
          cFile = session:TEMP-DIR + "\" + USERID("Nosweat") + STRING(TIME)
          lFormatted = YES
          cEncoding = ?
          cSchemaLocation = ?
          lWriteSchema = NO
          lMinSchema = TRUE
          lWriteBeforeImage = FALSE
          lOmitInitialValues = FALSE.
      
      /*** Before the method call, your application does work with its data. ***/
      
      returnValue = hPDS:WRITE-XML (cTargetType, cFile, lFormatted, cEncoding, 
                                    cSchemaLocation, lWriteSchema, lMinSchema, 
                                    lWriteBeforeImage, lOmitInitialValues).
/*   For Testing....                                                    */
/*       IF returnValue = FALSE THEN DO:                                          */
/*           MESSAGE "WRITE-XML on ProDataSet failed!" VIEW-AS ALERT-BOX.         */
/*           RETURN.                                                              */
/*       END.                                                                     */
/*       ELSE                                                                     */
/*           MESSAGE "Successful WRITE-XML on : " v-outfile[4] VIEW-AS ALERT-BOX. */
      DELETE OBJECT hPDS.

      /* Take out lines from XML that were needed to join the temp-tables */
      /* In the DataSet                                                   */      
      OUTPUT STREAM sOut TO VALUE(v-outfile[4]).
      INPUT FROM VALUE(cFile).

      REPEAT:
      
        cInLn = "".
        IMPORT DELIMITER "`" cInln.

        /* Needed internally, not in file */
        IF INDEX(cInLn, "<customerpo>") GT 0  THEN
          NEXT.
        
        /* Needed internally, not in file */
        IF cInLN BEGINS "<lineNo>" THEN
          NEXT.

        /* Remove empty sections */
        IF cInLn EQ "<addoncodes/>" THEN
          NEXT.
          
        IF cInLn EQ "<scores/>" THEN 
          NEXT.

        PUT STREAM sOut UNFORMATTED cInln SKIP.

      END. /* Repeat */

      INPUT CLOSE.
      OUTPUT STREAM sOut CLOSE.
      
      
      OS-DELETE VALUE(cFile).
      RUN po/ftppo.p (v-outfile[4], "GP").

      MESSAGE "Georgia Pacific file:" TRIM(v-outfile[3]) "has been created"
          VIEW-AS ALERT-BOX.
          
          
  END. /* Last of po # */
END. /* FOR EACH po-ord record */

/* end ----------------------------------- Copr. 2006  Advanced Software Inc. */
