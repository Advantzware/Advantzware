/* -------------------------------------------------- po/po-gpexp.p  WFK 11/15*/
/*                                                                            */
/* Georgia Pacific export PO                                                  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-format AS CHARACTER NO-UNDO.

{-c/inc/var.i SHARED}
{sys/form/s-top.f}

DEFINE BUFFER xjob-mat FOR job-mat. 
DEFINE BUFFER xitem    FOR item.
DEFINE BUFFER b-ref1   FOR reftable.
DEFINE BUFFER b-ref2   FOR reftable.

{po/po-print.i}

DEFINE VARIABLE v-sname LIKE shipto.ship-name.
DEFINE VARIABLE v-saddr LIKE shipto.ship-addr.
DEFINE VARIABLE v-scity LIKE shipto.ship-city.
DEFINE VARIABLE v-sstate LIKE shipto.ship-state.
DEFINE VARIABLE v-szip LIKE shipto.ship-zip.
DEFINE VARIABLE v-adder LIKE item.i-no EXTENT 6 NO-UNDO.
DEFINE VARIABLE xg-flag AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE v-instr AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ord-qty LIKE po-ordl.ord-qty NO-UNDO.
DEFINE VARIABLE v-ord-cst LIKE po-ordl.cost NO-UNDO.
DEFINE VARIABLE v-outfile AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-mach AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-line AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-char AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-scored AS LOG NO-UNDO.
DEFINE STREAM sOut.
DEFINE VARIABLE cInLn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseNew AS LOG NO-UNDO.
DEFINE VARIABLE cShipToChar AS CHAR NO-UNDO.

DEFINE TEMP-TABLE tt-score NO-UNDO 
    FIELD tt-seq  AS INTEGER
    FIELD tt-scor AS DECIMAL
    FIELD tt-type AS CHARACTER
    INDEX tt-seq tt-seq.
    
DEFINE TEMP-TABLE sheetorder NO-UNDO
    BEFORE-TABLE sheetorderBef
    FIELD customerpo AS INTEGER
    FIELD canonicalfile AS CHARACTER
    FIELD purchasedby AS CHARACTER
    FIELD plantid AS CHARACTER 
    FIELD billto AS CHARACTER
/*    FIELD ordernotes AS CHARACTER*/
    INDEX customerpo IS PRIMARY UNIQUE customerpo
    .

/* Definition for Temp-Table detailline */
DEFINE TEMP-TABLE detailline
    BEFORE-TABLE detaillineBef
    FIELD customerpo AS INTEGER
    FIELD lineNo AS INTEGER
    FIELD shipto AS CHARACTER
    FIELD shiptoaddress AS CHARACTER
    FIELD customerponbr AS CHARACTER
    FIELD customerpolineseqnbr AS CHARACTER
    FIELD internalprodno AS CHARACTER
    FIELD custitemname AS CHARACTER
    FIELD custitemdscr1 AS CHARACTER
    FIELD custitemdscr2 AS CHARACTER
    FIELD quantityordered AS DECIMAL
    FIELD uom AS CHARACTER
    FIELD duedate AS CHARACTER
/*    FIELD duetime AS CHARACTER*/
    FIELD currentdatetime AS CHARACTER
    FIELD boardcode AS CHARACTER
    FIELD addoncodes AS CHARACTER EXTENT 6
    FIELD scores AS CHARACTER
    FIELD length AS DECIMAL
    FIELD width AS DECIMAL
    FIELD instructions AS CHARACTER
/*    FIELD stackheight AS CHARACTER*/
    FIELD overrunpcnt AS CHARACTER
    FIELD underrunpcnt AS CHARACTER  .

DEFINE TEMP-TABLE scores
    FIELD customerpo AS INTEGER
    FIELD lineNo AS INTEGER
    FIELD scorecode AS CHARACTER
    FIELD scoresequence AS INTEGER
    FIELD score AS CHARACTER
    .
  
DEFINE TEMP-TABLE addoncodes
    FIELD customerpo AS INTEGER
    FIELD lineNo AS INTEGER
    FIELD addoncode AS CHARACTER
    .
    
DEFINE TEMP-TABLE ttPartnerValue
    FIELD partnervalue AS CHAR.
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

FIND FIRST po-ctrl NO-LOCK WHERE 
    po-ctrl.company EQ cocode
    NO-ERROR.
IF NOT AVAIL po-ctrl THEN DO:
    MESSAGE 
        "PO control record not found for company " + cocode + "."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
        

FIND FIRST company NO-LOCK WHERE 
    company.company EQ cocode
    NO-ERROR.
IF NOT AVAIL po-ctrl THEN DO:
    MESSAGE 
        "Company record not found for company " + cocode + "."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

IF gp-log AND gp-dir NE "" THEN
    print-po-blok:
    FOR EACH report NO-LOCK WHERE 
        report.term-id EQ v-term-id,
        FIRST po-ord 
        WHERE 
            RECID(po-ord) EQ report.rec-id AND 
            CAN-FIND(FIRST po-ordl WHERE 
                po-ordl.company   EQ po-ord.company AND 
                po-ordl.po-no     EQ po-ord.po-no AND 
                po-ordl.item-type EQ YES AND 
                (v-printde-po OR NOT po-ordl.deleted)),
        FIRST vend NO-LOCK WHERE 
            vend.company    EQ po-ord.company AND 
            vend.vend-no    EQ po-ord.vend-no AND 
            (vend.po-export EQ "GP" OR (poexport-cha  EQ "GP" AND vend.an-edi-vend))
        BREAK BY po-ord.po-no:

        FIND FIRST sys-ctrl-shipto NO-LOCK WHERE
            sys-ctrl-shipto.company EQ po-ord.company AND 
            sys-ctrl-shipto.name EQ "GP" AND 
            sys-ctrl-shipto.cust-vend EQ FALSE AND 
            sys-ctrl-shipto.cust-vend-no EQ po-ord.vend-no
            NO-ERROR.
        IF gp-int EQ 1 
        AND AVAIL sys-ctrl-shipto THEN DO:
            CREATE ttPartnerValue.
            ASSIGN
                ttPartnerValue.partnervalue = ENTRY(1,sys-ctrl-shipto.char-fld,"|")
                lUseNew = TRUE
                cShipToChar = sys-ctrl-shipto.char-fld.
        END.
 
         IF FIRST(po-ord.po-no) THEN DO:
            IF OPSYS EQ "UNIX" AND SUBSTR(gp-dir,1,1) NE v-slash THEN
                gp-dir = v-slash + gp-dir.

            IF SUBSTR(gp-dir,LENGTH(gp-dir),1) EQ v-slash THEN
                SUBSTR(gp-dir,LENGTH(gp-dir),1) = "".

            /* Output to a temporary file */
            ASSIGN
                iNextLine = 0
                v-outfile[1] = TRIM(gp-dir) + v-slash + "dataxfer" + v-slash + "in" + v-slash
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
            
            FILE-INFO:FILE-NAME = v-outfile[1].
            IF FILE-INFO:FILE-TYPE EQ ?  THEN        
                OS-COMMAND SILENT  "mkdir " + VALUE(v-outfile[1]). 
        /* OUTPUT TO VALUE(v-outfile[4]). */
        END.

        IF po-ord.stat EQ "N" THEN 
            po-ord.stat = "O".
        CREATE sheetorder .

        ASSIGN
            sheetorder.customerpo = po-ord.po-no
            /* sheetorder.canonicalfile = " " */
            sheetorder.purchasedby = IF lUseNew THEN CAPS(ENTRY(1, cShipToChar,"|")) ELSE po-ord.buyer
            sheetorder.plantid = IF lUseNew THEN ENTRY(2, cShipToChar,"|") ELSE vend.name
            sheetorder.billto = IF lUseNew THEN ENTRY(1, cShipToChar,"|") + " " + 
                                ENTRY(2, cShipToChar,"|") ELSE company.name.

        v-instr = "".
        FOR EACH notes WHERE notes.rec_key EQ po-ord.rec_key NO-LOCK:
            IF notes.note_text NE "" THEN 
                v-instr = TRIM(v-instr)                         +
                    (IF v-instr EQ "" THEN "" ELSE ".**") +
                    TRIM(notes.note_text).
        END.

/*        sheetorder.ordernotes =  v-instr  .*/

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
        
            IF AVAILABLE job THEN 
            DO:
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
                    BREAK BY job-mat.blank-no DESCENDING:
                    IF LAST(job-mat.blank-no)            OR
                        job-mat.blank-no EQ po-ordl.b-num THEN LEAVE.
                END.

                IF AVAILABLE job-mat THEN 
                DO:
                    FIND FIRST ef NO-LOCK
                        WHERE ef.e-num   EQ job.e-num
                        AND ef.form-no EQ job-mat.frm
                        NO-ERROR.
   
                    ASSIGN
                        xg-flag = AVAILABLE ef AND (ef.xgrain EQ "S" OR ef.xgrain EQ "B")
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
                detailline.customerpolineseqnbr = STRING(po-ordl.line)
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
/*                detailline.duetime = STRING(61200,"HH:MM")*/
                detailline.currentdatetime = STRING(TODAY,"999999") + " " + STRING(TIME,"HH:MM")
                detailline.boardcode = po-ordl.i-no .

            DO i = 1 TO 6:

                IF v-adder[i] NE "" THEN 
                DO:

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
    
            IF AVAILABLE b-ref1 THEN
            DO i = 1 TO 12:
                IF b-ref1.val[i] NE 0 THEN 
                DO:
                    CREATE tt-score.
                    ASSIGN
                        tt-seq  = i
                        tt-type = SUBSTR(b-ref1.dscr,i,1)
                        tt-scor = TRUNC(b-ref1.val[i],0) +
                   ((b-ref1.val[i] - TRUNC(b-ref1.val[i],0)) * 6.25).
                END.
            END.

            IF AVAILABLE b-ref2 THEN
            DO i = 1 TO 8:
                IF b-ref2.val[i] NE 0 THEN 
                DO:
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
                        scores.scoresequence = i /* - 1 */
                        scores.score         = STRING(tt-scor)
                        scores.scorecode     = tt-type.            
          

                END.
            END.

            ASSIGN 
                detailline.length = po-ordl.s-len
                detailline.width = po-ordl.s-wid .
     
            v-instr = "".
            FOR EACH notes WHERE notes.rec_key EQ po-ordl.rec_key NO-LOCK:
                IF notes.note_text NE "" THEN 
                    v-instr = TRIM(v-instr)                         +
                        (IF v-instr EQ "" THEN "" ELSE ".**") +
                        TRIM(notes.note_text).
            END.

            ASSIGN 
                detailline.instructions = v-instr
                detailline.overrunpcnt = STRING(po-ord.over-pct )
                detailline.underrunpcnt = STRING(po-ord.under-pct) .

        END. /* FOR EACH po-ordl record */

        /*PUT UNFORMATTED
            "</sheetorder>".*/

        po-ord.printed = YES.
        IF LAST(po-ord.po-no) AND CAN-FIND( FIRST sheetorder)
        /* SEARCH(v-outfile[4]) NE ? */ THEN 
        DO:

            DEFINE VARIABLE returnValue AS LOGICAL NO-UNDO.
            DEFINE VARIABLE hPDS AS HANDLE.
            hPDS = DATASET pkgcustomxmlorders:HANDLE.
      
            ASSIGN
                cTargetType = "FILE"
                cFile = SESSION:TEMP-DIR + "\" + USERID("Nosweat") + STRING(TIME)
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

                IF gp-int EQ 1 
                AND cInLn EQ "<sheetorder>" THEN
                    PUT STREAM sOut UNFORMATTED 
                         "<partnervalue>" +
                         CAPS(ENTRY(1, cShipToChar,"|")) + 
                         "</partnervalue>" + CHR(10).
                         
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
