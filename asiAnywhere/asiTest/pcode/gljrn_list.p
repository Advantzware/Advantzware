
/*------------------------------------------------------------------------
    File        : gljrn_list.p
    Purpose     : General/Ledger
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttGeneralLedgerlist NO-UNDO
    FIELD jrn_no        AS INT          
    FIELD tr_date       AS CHAR
    FIELD period        AS INT
    FIELD t_deb         AS DEC
    FIELD t_crd         AS DEC
    FIELD t_amt         AS DEC
    FIELD reverse       AS CHAR
    FIELD from_revr     AS CHAR
    FIELD cb_freq       AS CHAR
    FIELD reckey        AS CHAR

    FIELD gl_extra      AS CHAR  .

DEFINE DATASET dsGeneralLedgerlist FOR ttGeneralLedgerlist.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmjrn_no       AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmtr_date      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmperiod       AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmt_deb        AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmt_crd        AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmt_amt        AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmreverse      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmfrom_revr    AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmcb_freq      AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHAR NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGeneralLedgerlist .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     FOR EACH ttGeneralLedgerlist:
        DELETE ttGeneralLedgerlist .
    END.

IF prmAction         = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmjrn_no         = ?  THEN ASSIGN prmjrn_no     = 0.
IF prmtr_date        = ?  THEN ASSIGN prmtr_date    = "".
IF prmperiod         = ?  THEN ASSIGN prmperiod     = 0. 
IF prmt_deb          = ?  THEN ASSIGN prmt_deb      = 0. 
IF prmt_crd          = ?  THEN ASSIGN prmt_crd      = 0.
IF prmt_amt          = ?  THEN ASSIGN prmt_amt      = 0.
IF prmreverse        = ?  THEN ASSIGN prmreverse    = "".
IF prmfrom_revr      = ?  THEN ASSIGN prmfrom_revr  = "".
IF prmcb_freq        = ?  THEN ASSIGN prmcb_freq    = "".
IF prmReckey         = ?  THEN ASSIGN prmReckey     = "".




DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.

     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp
    g_user    = prmUser  .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .

  {sys/inc/apsecure.i}


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

DEF TEMP-TABLE tt-gl-jrn NO-UNDO
    FIELD DATE AS DATE
    FIELD reverse AS LOG
    FIELD id AS CHAR
    FIELD j-no AS INT
    FIELD period AS INT.

DEF TEMP-TABLE tt-gl-jrnl NO-UNDO
    FIELD LINE AS INT
    FIELD actnum AS CHAR
    FIELD dscr AS CHAR
    FIELD tr-amt AS DEC
    FIELD id AS CHAR.

DEF VAR ll-recur AS LOG NO-UNDO.
ASSIGN ll-recur = NO.
DEF BUFFER bf-jrn FOR gl-jrn.




IF prmAction = "Search" THEN DO:
    
     FOR EACH gl-jrn WHERE gl-jrn.company = g_company 
         and gl-jrn.journal > 0 
         and gl-jrn.posted eq no 
         and gl-jrn.recur eq no 
         AND (gl-jrn.journal EQ prmjrn_no OR prmjrn_no EQ 0)
         AND (STRING(gl-jrn.tr-date) BEGINS string(prmtr_date) OR string(prmtr_date) EQ "")  NO-LOCK :

        CREATE ttGeneralLedgerlist.
           ASSIGN 
                 ttGeneralLedgerlist.jrn_no        = gl-jrn.journal
                 ttGeneralLedgerlist.tr_date       = string(gl-jrn.tr-date)
                 ttGeneralLedgerlist.period        = gl-jrn.period
                 ttGeneralLedgerlist.t_deb         = gl-jrn.tdeb
                 ttGeneralLedgerlist.t_crd         = gl-jrn.tcred
                 ttGeneralLedgerlist.t_amt         = gl-jrn.tr-amt
                 ttGeneralLedgerlist.reverse       = IF gl-jrn.reverse THEN "R" ELSE ""
                 ttGeneralLedgerlist.from_revr     = IF gl-jrn.from-reverse THEN "PR" ELSE ""
                 ttGeneralLedgerlist.reckey        = gl-jrn.rec_key    .
            
    END. /*FOR EACH gl-jrn  */
END. /*IF prmAction = "Select" THEN DO:*/



IF prmAction = "ValidateAdd" THEN DO:
    DEF VAR choice AS LOG NO-UNDO.

  IF NOT ll-recur THEN
         find first period where period.company  =  g_company  and
                                period.pst  <= date(prmtr_date) and
                                period.pend >= date(prmtr_date)
                                no-lock no-error.
         IF AVAIL period AND period.pstat then do:
            assign prmperiod  = (period.pnum).          
         end.
         else do:
            cError = "Period is not OPEN !".
            RETURN.
         end.
         choice = not (FOCUS:NAME NE "tr-date" AND avail gl-jrn and period.pnum ne gl-jrn.period).
         if not choice then DO:            
            cError = "Reset Journal Entries' period to new Period ?" .
                   
         end.
         if not choice then do:
           ASSIGN
            prmtr_date = STRING(gl-jrn.tr-date)
            prmperiod   = (gl-jrn.period).
           RETURN.
         END.
 
 END.

/********************************************************************/

 IF prmAction = "AddNew" THEN DO:

     CREATE gl-jrn .

     ASSIGN 
         gl-jrn.company      = g_company
         gl-jrn.period       = prmperiod
         gl-jrn.recur        = ll-recur
         gl-jrn.reverse      = NO
         gl-jrn.from-reverse = NO .
        

         ASSIGN prmAction = "View"
                prmReckey = gl-jrn.rec_key .
 END.


IF prmAction = "AddUpdate" THEN DO:
       FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN
        gl-jrn.tr-date         =  date(prmtr_date)  
        gl-jrn.period          =  prmperiod   
        gl-jrn.reverse         =  IF prmreverse = "True" THEN TRUE ELSE FALSE 
        gl-jrn.from-reverse    =  IF prmfrom_revr = "True" THEN TRUE ELSE FALSE   .
                                  
    
    
    ASSIGN prmAction = "View" 
           prmReckey = gl-jrn.rec_key.

END.


/**************Update *************************************************/

IF prmAction = "Update" THEN DO:
  

  IF NOT ll-recur THEN
         find first period where period.company  =  g_company  and
                                period.pst  <= date(prmtr_date) and
                                period.pend >= date(prmtr_date)
                                no-lock no-error.
         IF AVAIL period AND period.pstat then do:
            assign prmperiod  = (period.pnum).          
         end.
         else do:
            cError = "Period is not OPEN !".
            RETURN.
         end.
        /* choice = not (FOCUS:NAME NE "tr-date" AND avail gl-jrn and period.pnum ne gl-jrn.period).
         if not choice then DO:            
            cError = "Reset Journal Entries' period to new Period ?" .
                   
         end.
         if not choice then do:
           ASSIGN
            prmtr_date = STRING(gl-jrn.tr-date)
            prmperiod   = (gl-jrn.period).
           RETURN.
         END.*/
     

END.

IF prmAction = "Update" THEN DO:
     FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

                    
     ASSIGN 
        gl-jrn.tr-date         =  date(prmtr_date)  
        gl-jrn.period          =  prmperiod   
        gl-jrn.reverse         =  IF prmreverse = "True" THEN TRUE ELSE FALSE 
        gl-jrn.from-reverse    =  IF prmfrom_revr = "True" THEN TRUE ELSE FALSE  . 

        
        ASSIGN prmAction = "View" .

END.  

/*********************************delete ******************************/


IF prmAction = "DataDelete"  THEN DO:

    FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
   
    IF AVAIL gl-jrn THEN DELETE gl-jrn .
    FIND LAST gl-jrn WHERE gl-jrn.company = cocode  and gl-jrn.journal > 0 
         and gl-jrn.posted eq no 
         and gl-jrn.recur eq no  NO-LOCK NO-ERROR. 
    IF AVAIL gl-jrn THEN
        ASSIGN
        prmjrn_no = (gl-jrn.journal)
        prmReckey = gl-jrn.rec_key 
        prmAction = "View" .

END.



/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
     FIND FIRST gl-jrn WHERE  gl-jrn.company = cocode AND gl-jrn.rec_key = prmReckey  NO-LOCK NO-ERROR.
        CREATE ttGeneralLedgerlist.
           ASSIGN 
                 ttGeneralLedgerlist.jrn_no        = gl-jrn.journal
                 ttGeneralLedgerlist.tr_date       = string(gl-jrn.tr-date)
                 ttGeneralLedgerlist.period        = gl-jrn.period
                 ttGeneralLedgerlist.t_deb         = gl-jrn.tdeb
                 ttGeneralLedgerlist.t_crd         = gl-jrn.tcred
                 ttGeneralLedgerlist.t_amt         = gl-jrn.tr-amt
                 ttGeneralLedgerlist.reverse       = string(gl-jrn.reverse)
                 ttGeneralLedgerlist.from_revr     = string(gl-jrn.from-reverse)
                 ttGeneralLedgerlist.reckey        = gl-jrn.rec_key    .

 

            


            
END. /*IF prmAction = "View" THEN DO:*/
/*****************************procedure**********************************/

IF prmAction = "impexl" THEN DO:

    DEF VAR lv-answer AS LOG NO-UNDO.
   def var chFile as cha no-undo.
   def var ll-ok as log no-undo.
   DEF VAR chExcelApplication AS COM-HANDLE   NO-UNDO.
   DEF VAR chWorkBook AS COM-HANDLE   NO-UNDO.
   DEF VAR chWorksheet AS COM-HANDLE   NO-UNDO.
   DEF VAR viRowCount AS INT INIT 1 NO-UNDO.
   DEF VAR v-line AS INT INIT 1 NO-UNDO.
   DEF VAR valid-flag AS LOG INIT YES NO-UNDO.
   def var char-hdl as cha no-undo.
   DEF VAR v-rowid AS ROWID NO-UNDO.
   DEF VAR v-id AS CHAR NO-UNDO.
   DEF VAR i-actnum AS INTE NO-UNDO.

   
   
      system-dialog get-file chFile 
                    title "Select File to Import"
                    filters "Excel File (*.xls) " "*.xls"
                    initial-dir "c:\"
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE ll-ok.
     
      IF ll-ok THEN
      DO:
         IF LENGTH(chFile) LT 4 OR
            SUBSTR(chFile,LENGTH(chFile) - 3) NE ".xls" THEN
         DO:
            cError = "Invalid File.  Must Choose Excel (.xls) File."
                .
            LEAVE.
         END.
     
         
     
         EMPTY TEMP-TABLE tt-gl-jrn.
         EMPTY TEMP-TABLE tt-gl-jrnl.

         /* Initialize Excel. */
         CREATE "Excel.Application" chExcelApplication NO-ERROR.
     
         /* Check if Excel got initialized. */
         IF not (valid-handle (chExcelApplication)) THEN
         DO:
            cError = "Unable to Start Excel." .
            RETURN.
         END.
     
         /* Open our Excel File. */  
         chExcelApplication:Visible = FALSE.
         chWorkbook = chExcelApplication:Workbooks:OPEN(chfile) no-error.
     
         /* Do not display Excel error messages. */
         chExcelApplication:DisplayAlerts = false  no-error.
     
         /* Go to the Active Sheet. */
         chWorkbook:WorkSheets(1):Activate no-error.
     
         ASSIGN
            chWorkSheet = chExcelApplication:Sheets:item(1).

         REPEAT:
            IF chWorkSheet:Range("A" + STRING(viRowCount)):VALUE EQ ? OR
               NOT valid-flag THEN LEAVE.

            IF chWorkSheet:Range("B" + STRING(viRowCount)):VALUE = "H" THEN /*Header*/
            DO:
               CREATE tt-gl-jrn.
               tt-gl-jrn.DATE = chWorkSheet:Range("C" + STRING(viRowCount)):value NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
               DO:
                  cError = "Invalid Header Date".
                  valid-flag = NO.
                  LEAVE.
               END.

               RUN check-date-excel(INPUT tt-gl-jrn.DATE, OUTPUT tt-gl-jrn.period) NO-ERROR.

               IF ERROR-STATUS:ERROR THEN
               DO:
                  valid-flag = NO.
                  LEAVE.
               END.

               tt-gl-jrn.reverse = chWorkSheet:Range("D" + STRING(viRowCount)):VALUE NO-ERROR.
           
               IF ERROR-STATUS:ERROR THEN
               DO:
                  cError = "Invalid Reverse Entry Value".
                  valid-flag = NO.
                  LEAVE.
               END.

               IF chWorkSheet:Range("A" + STRING(viRowCount)):VALUE NE ? THEN
                  tt-gl-jrn.id = chWorkSheet:Range("A" + STRING(viRowCount)):VALUE.
               ELSE
               DO:
                   cError = "Invalid Unique Identifier" .
                   valid-flag = NO.
                   LEAVE.
               END.

               RELEASE tt-gl-jrn.
               viRowCount = viRowCount + 1.
            END.
            ELSE IF chWorkSheet:Range("B" + STRING(viRowCount)):VALUE = "T" THEN /*Trailer*/
            DO:
               CREATE tt-gl-jrnl.
               ASSIGN tt-gl-jrnl.actnum = IF chWorkSheet:Range("C" + STRING(viRowCount)):VALUE NE ? THEN
                                             chWorkSheet:Range("C" + STRING(viRowCount)):VALUE
                                          ELSE ""
                      tt-gl-jrnl.dscr   = IF chWorkSheet:Range("D" + STRING(viRowCount)):VALUE NE ? THEN
                                             chWorkSheet:Range("D" + STRING(viRowCount)):VALUE
                                          ELSE ""
                      tt-gl-jrnl.tr-amt = IF chWorkSheet:Range("E" + STRING(viRowCount)):VALUE NE ? THEN
                                             chWorkSheet:Range("E" + STRING(viRowCount)):VALUE
                                          ELSE 0.

              /* to format actnum to integer when there are no dashes */
              IF length(trim(tt-gl-jrnl.actnum)) > 0 AND index(tt-gl-jrnl.actnum,"-") = 0 THEN DO:
                  ASSIGN i-actnum = INTE(tt-gl-jrnl.actnum) NO-ERROR.
                  IF ERROR-STATUS:ERROR = FALSE THEN
                     ASSIGN tt-gl-jrnl.actnum = string(i-actnum).
              END.

               IF NOT CAN-FIND(FIRST account WHERE
                  account.company EQ g_company AND
                  account.type    NE "T" AND
                  account.actnum  EQ tt-gl-jrnl.actnum) THEN
                  DO:
                     cError = "Invalid Account #, in row " + STRING(viRowCount) + " Acct Num: " + tt-gl-jrnl.actnum + "."
                         .
                     valid-flag = NO.
                     LEAVE.
                  END.

               IF chWorkSheet:Range("A" + STRING(viRowCount)):VALUE NE ? THEN
               DO:
                  v-id = chWorkSheet:Range("A" + STRING(viRowCount)):VALUE.

                  IF NOT CAN-FIND(FIRST tt-gl-jrnl WHERE
                     tt-gl-jrnl.id = v-id) THEN
                     v-line = 1.

                  ASSIGN
                     tt-gl-jrnl.id = v-id
                     tt-gl-jrnl.LINE = v-line.
               END.
               ELSE
               DO:
                  cError = "Invalid Unique Identifier " .
                  valid-flag = NO.
                  LEAVE.
               END.

               RELEASE tt-gl-jrnl.
               
               ASSIGN
                  v-line = v-line + 1
                  viRowCount = viRowCount + 1.
            END.
            ELSE
            DO:
               cError = "Invalid Header/Trailer Value" .
               valid-flag = NO.
               LEAVE.
            END.
         END.

         /*Free memory*/
         chWorkbook = chExcelApplication:Workbooks:CLOSE() no-error.
         RELEASE OBJECT chWorkbook NO-ERROR.
         RELEASE OBJECT chWorkSheet NO-ERROR.
         RELEASE OBJECT chExcelApplication NO-ERROR.
     
         IF valid-flag THEN
         DO:
            /*create records*/

            FOR EACH tt-gl-jrn:
            
               CREATE gl-jrn.
               ASSIGN 
                  gl-jrn.reverse = tt-gl-jrn.reverse
                  gl-jrn.tr-date = tt-gl-jrn.DATE
                  gl-jrn.company = g_company
                  gl-jrn.period  = tt-gl-jrn.period
                  gl-jrn.recur   = ll-recur
                  gl-jrn.from-reverse = NO
                  v-rowid = ROWID(gl-jrn)  
                  tt-gl-jrn.j-no = gl-jrn.j-no.
            END.

            FOR EACH tt-gl-jrnl,
                FIRST tt-gl-jrn WHERE
                      tt-gl-jrn.id = tt-gl-jrnl.id,
                FIRST gl-jrn WHERE
                      gl-jrn.j-no EQ tt-gl-jrn.j-no
                      EXCLUSIVE-LOCK:

                CREATE gl-jrnl.
                ASSIGN gl-jrnl.j-no = tt-gl-jrn.j-no
                       gl-jrnl.line = tt-gl-jrnl.LINE
                       gl-jrnl.actnum = tt-gl-jrnl.actnum
                       gl-jrnl.dscr = tt-gl-jrnl.dscr
                       gl-jrnl.tr-amt = tt-gl-jrnl.tr-amt.

                IF gl-jrnl.tr-amt GT 0 THEN
                   gl-jrn.tdeb = gl-jrn.tdeb  + tt-gl-jrnl.tr-amt.
                ELSE
                   gl-jrn.tcred = gl-jrn.tcred + tt-gl-jrnl.tr-amt.

                gl-jrn.tr-amt = gl-jrn.tdeb + gl-jrn.tcred.

                RELEASE gl-jrnl.
            END.

            cError = "Excel File Imported." .

            

           /* IF v-rowid NE ? THEN
               RUN repos-query in WIDGET-HANDLE(char-hdl) (INPUT v-rowid). */
         END.
      END.
  

END.
