/*----------------------------------------------- oe/rep/cocacpi.p */
/* Print Soule COC (Certificate of Compliance)                   */
/*----------------------------------------------------------------*/

{sys/inc/var.i shared}    

DEFINE        VARIABLE viWorkSheetCount   AS INTEGER          NO-UNDO.   
DEFINE SHARED VARIABLE LvOutputSelection  AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE CurActivePrinter   AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE AdobePrinter       AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE vcTemplateFile     AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE        VARIABLE WshNetwork         AS COMPONENT-HANDLE.
DEFINE        VARIABLE chFile             AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE        VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.
DEFINE        VARIABLE CurrDir            AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE CommandString      AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE v-dir              AS CHARACTER        FORMAT "X(80)" NO-UNDO.
DEFINE        VARIABLE iRowCount          AS INTEGER          NO-UNDO.
DEFINE        VARIABLE iQtyOnHand         AS INTEGER          NO-UNDO.

DEFINE BUFFER xoe-bolh FOR oe-bolh.
DEFINE BUFFER xoe-boll FOR oe-boll.
DEFINE BUFFER xitemfg  FOR itemfg.
DEFINE BUFFER xxreport FOR report.
     
{oe/rep/oe-lad.i}
 
DEFINE VARIABLE v-salesman      AS CHARACTER FORMAT "x(26)".
DEFINE VARIABLE v-fob           AS CHARACTER FORMAT "x(12)".
DEFINE VARIABLE v-tot-palls     AS INTEGER   FORMAT "->,>>>,>>9".
DEFINE VARIABLE v-tot-wt        AS DECIMAL   FORMAT "->>,>>>,>>9".
DEFINE VARIABLE v-tot-pkgs      AS INTEGER   FORMAT ">>9". 
DEFINE VARIABLE v-ord-date      LIKE oe-ord.ord-date.
DEFINE VARIABLE v-po-no         LIKE oe-bolh.po-no.
DEFINE VARIABLE v-job-no        AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE v-phone-num     AS CHARACTER FORMAT "x(13)" NO-UNDO. 
DEFINE VARIABLE v-ship-id       LIKE shipto.ship-id.
DEFINE VARIABLE v-ship-name     LIKE shipto.ship-name.
DEFINE VARIABLE v-ship-addr     LIKE shipto.ship-addr.
DEFINE VARIABLE v-ship-city     LIKE shipto.ship-city.
DEFINE VARIABLE v-ship-state    LIKE shipto.ship-state.
DEFINE VARIABLE v-ship-zip      LIKE shipto.ship-zip.
DEFINE VARIABLE v-ship-addr3    AS CHARACTER FORMAT "x(30)".

DEFINE VARIABLE v-shipcontact   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-frt-acct-num  LIKE ar-ctrl.freight NO-UNDO.

DEFINE VARIABLE v-comp-id       LIKE company.company.
DEFINE VARIABLE v-comp-name     LIKE company.name.
DEFINE VARIABLE v-comp-addr     LIKE company.addr.
DEFINE VARIABLE v-comp-city     LIKE company.city.
DEFINE VARIABLE v-comp-state    LIKE company.state.
DEFINE VARIABLE v-comp-zip      LIKE company.zip.
DEFINE VARIABLE v-comp-addr3    AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-cust-addr3    AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-custcontact   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-1             LIKE oe-boll.cases INIT 1 NO-UNDO.

DEFINE VARIABLE v-terms         LIKE oe-ord.terms-d NO-UNDO.
DEFINE VARIABLE v-frt-terms     AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-zone          LIKE carr-mtx.del-zone NO-UNDO.
DEFINE VARIABLE v-lines         AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-job-po        AS CHARACTER NO-UNDO.   
DEFINE VARIABLE v-cases         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-partial       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-total-count   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-weight        AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE v-comp-add1     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-line-total    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-quo-total     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-t-tax         AS DECIMAL   EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bot-lab       AS CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-q-no          LIKE oe-ord.q-no NO-UNDO.
DEFINE VARIABLE lv-display-comp AS LOG       NO-UNDO.  /* display company address */
DEFINE VARIABLE lv-comp-name    AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-email        AS cha       FORM "x(56)" NO-UNDO.
DEFINE VARIABLE lv-comp-color   AS cha       NO-UNDO.
DEFINE VARIABLE lv-other-color  AS cha       INIT "BLACK" NO-UNDO.
DEFINE VARIABLE v-cusx-add1     AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-add2     AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-add3     AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-add4     AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-add5     AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-email    AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-name     AS cha       NO-UNDO.

DEFINE VARIABLE lv-bolfmt-int   AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg.
DEFINE VARIABLE iError      AS INTEGER   NO-UNDO.
DEFINE VARIABLE LvCtr       AS INTEGER   NO-UNDO.  
DEFINE VARIABLE fi_upd-time AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE li-hh       AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-ss       AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-mm       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLineCount  AS INTEGER   NO-UNDO.
DEFINE VARIABLE dTotWeight  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iTotalQty   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotalCase  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotalSumCases   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotalSumQtyCase AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotalSumPartial AS INTEGER   NO-UNDO.

/* Build a Table to keep sequence of pdf files */
DEFINE SHARED TEMP-TABLE tt-filelist NO-UNDO
    FIELD tt-FileCtr  AS INTEGER
    FIELD tt-FileName AS CHARACTER
    INDEX filelist IS PRIMARY 
    TT-FILECTR.
                          
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "BOLFMT" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN 
    ASSIGN lv-display-comp = sys-ctrl.log-fld 
        lv-bolfmt-int   = sys-ctrl.int-fld.
ELSE ASSIGN lv-display-comp = NO
        lv-bolfmt-int   = 0.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

FIND FIRST ar-ctrl NO-LOCK WHERE ar-ctrl.company = cocode NO-ERROR.
IF AVAILABLE ar-ctrl THEN
    ASSIGN v-frt-acct-num = ar-ctrl.freight   .
                                
FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
ASSIGN 
    v-comp-add1  = ""
    v-comp-add2  = ""
    v-comp-add3  = ""
    v-comp-add4  = ""
    v-comp-add5  = ""
    lv-email     = ""
    lv-comp-name = "".
     
IF lv-display-comp THEN 
DO:
    FIND FIRST cust WHERE cust.company = cocode AND
        cust.active = "X" NO-LOCK NO-ERROR.
 
    IF AVAILABLE cust THEN
        ASSIGN v-comp-add1  = cust.addr[1]
            v-comp-add2  = cust.addr[2]
            v-comp-add3  = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-city  = cust.city
            v-comp-state = cust.state
            v-comp-zip   = cust.zip
            v-comp-add4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email     = "Email:  " + cust.email 
            lv-comp-name = cust.NAME   
            v-cusx-add1  = v-comp-add1
            v-cusx-add2  = v-comp-add2
            v-cusx-add3  = v-comp-add3
            v-cusx-add4  = v-comp-add4
            v-cusx-add5  = v-comp-add5
            v-cusx-email = lv-email
            v-cusx-name  = lv-comp-name
            .
END.

FIND FIRST oe-bolh NO-LOCK NO-ERROR.
FIND FIRST carrier NO-LOCK NO-ERROR.
FIND FIRST cust NO-LOCK NO-ERROR.

DEFINE BUFFER bf-eb FOR eb.
FIND FIRST users WHERE
    users.user_id EQ USERID("NOSWEAT")
    NO-LOCK NO-ERROR.

IF AVAILABLE users AND users.user_program[2] NE "" THEN
    v-dir = users.user_program[2] + "\".
ELSE
    v-dir = "c:\tmp\".
       
RUN InitializeExcel.   
RUN MainLoop. 
RUN Cleanup.  

PROCEDURE FillData:

    ASSIGN
        viWorkSheetCount = 1. 
   
    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(viWorkSheetCount):Activate NO-ERROR.   
    
    ASSIGN
        chWorkSheet      = chExcelApplication:Sheets:item(viWorkSheetCount)
        chWorkSheet:name = "Bill of Lading" .
        
    {sa/sa-sls01.i}
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.

    FOR EACH xxreport WHERE xxreport.term-id EQ v-term-id,
        FIRST oe-bolh WHERE RECID(oe-bolh)   EQ xxreport.rec-id,
        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ oe-bolh.cust-no
        NO-LOCK
        BREAK BY oe-bolh.bol-no :
      
    
        IF FIRST-OF(oe-bolh.bol-no) THEN 
        DO:
      
            FIND FIRST carrier
                WHERE carrier.company EQ oe-bolh.company
                AND carrier.carrier EQ oe-bolh.carrier
                NO-LOCK NO-ERROR.

            RUN oe/custxship.p (oe-bolh.company,
                oe-bolh.cust-no,
                oe-bolh.ship-id,
                BUFFER shipto).

            ASSIGN
                v-ship-id      = shipto.ship-id
                v-ship-name    = shipto.ship-name
                v-ship-addr[1] = shipto.ship-addr[1]
                v-ship-addr[2] = shipto.ship-addr[2]
                v-ship-addr3   = shipto.ship-city + ", " +
                         shipto.ship-state + "  " +
                         shipto.ship-zip
                v-ship-city    = shipto.ship-city
                v-ship-state   = shipto.ship-state
                v-ship-zip     = shipto.ship-zip
                v-phone-num    = cust.area-code + cust.phone .
     
            IF shipto.broker THEN 
            DO: 
                ASSIGN
                    v-comp-add1   = cust.addr[1]
                    v-comp-add2   = cust.addr[2]
                    v-comp-add3   = cust.city + ", " +
                        cust.state + "  " +
                         cust.zip
                    v-comp-city   = cust.city
                    v-comp-state  = cust.state
                    v-comp-zip    = cust.zip
                    v-comp-add4   = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
                    v-comp-add5   = "Fax     :  " + string(cust.fax,"(999)999-9999") 
                    lv-email      = "Email:  " + cust.email   
                    lv-comp-name  = cust.NAME 
                    v-custcontact = shipto.contact.
                FIND FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK NO-ERROR.
                IF AVAILABLE oe-boll THEN 
                DO:
                    FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company
                        AND oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
                    IF AVAILABLE oe-ord THEN 
                        ASSIGN lv-comp-name = oe-ord.sold-name
                            v-comp-add1  = oe-ord.sold-addr[1]
                            v-comp-add2  = oe-ord.sold-addr[2]
                            v-comp-add3  = oe-ord.sold-city + ", " +
                                  oe-ord.sold-state + "  " +
                                  oe-ord.sold-zip.  
                END.        
            END.

            ELSE 
                ASSIGN 
                    v-comp-add1  = v-cusx-add1
                    v-comp-add2  = v-cusx-add2    
                    v-comp-add3  = v-cusx-add3    
                    v-comp-add4  = v-cusx-add4                
                    v-comp-add5  = v-cusx-add5
                    lv-email     = v-cusx-email
                    lv-comp-name = v-cusx-name.     
          
            FIND FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK NO-ERROR.
            IF AVAILABLE oe-boll THEN 
            DO:
                FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company
                    AND oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
                IF AVAILABLE oe-ord THEN
                    ASSIGN
                        v-comp-name    = oe-ord.sold-name
                        v-comp-addr[1] = oe-ord.sold-addr[1]
                        v-comp-addr[2] = oe-ord.sold-addr[2]
                        v-comp-addr3   = oe-ord.sold-city + ", " +
                             oe-ord.sold-state + "  " +
                             oe-ord.sold-zip.                                    
            END.
            IF oe-bolh.loc NE "" THEN
            DO:
               FIND FIRST loc NO-LOCK 
                    WHERE loc.company EQ cocode 
                    AND loc.loc EQ oe-bolh.loc NO-ERROR.
               IF AVAIL loc THEN
               DO:
                 FIND FIRST location NO-LOCK
                      WHERE location.locationCode = loc.loc
                      AND location.rec_key = loc.addrRecKey NO-ERROR .
                      
                     ASSIGN
                         v-comp-name    = loc.dscr
                         v-comp-addr[1] = IF AVAIL location THEN location.streetAddr[1] ELSE ""
                         v-comp-addr[2] = IF AVAIL location THEN location.streetAddr[2] ELSE ""
                         v-comp-addr3   = (IF AVAIL location THEN location.subCode3 ELSE "") + ", " +
                                          (IF AVAIL location THEN location.subCode1 ELSE "") + "  " +
                                          (IF AVAIL location THEN location.subCode4 ELSE ""). 
               END.
            END.
       
            IF TRIM(v-comp-addr3) EQ "," THEN v-comp-addr3 = "".
              
            IF v-comp-addr[2] EQ "" THEN
                ASSIGN
                    v-comp-addr[2] = v-comp-addr3
                    v-comp-addr3   = "".
            IF v-ship-addr[2] EQ "" THEN
                ASSIGN
                    v-ship-addr[2] = v-ship-addr3
                    v-ship-addr3   = ""
                    v-ship-city    = ""
                    v-ship-state   = ""
                    v-ship-zip     = ""
                    .

            IF TRIM(v-ship-addr3) EQ "," THEN v-ship-addr3 = "".
            IF TRIM(v-cust-addr3) EQ "," THEN v-cust-addr3 = "".

            ASSIGN
                v-salesman = ""
                v-fob      = ""
                v-terms    = "".


            FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK,
                FIRST oe-ord
                WHERE oe-ord.company EQ oe-boll.company
                AND oe-ord.ord-no  EQ oe-boll.ord-no
                NO-LOCK:

                /* skb - 1/11/07 - Get Ship Contact */
                ASSIGN
                    v-shipcontact = oe-ord.contact
                    v-ord-date    = oe-ord.ord-date.
      
                IF NOT AVAILABLE carrier THEN
                    FIND FIRST carrier WHERE carrier.company = oe-ord.company
                        AND carrier.carrier = oe-ord.carrier NO-LOCK NO-ERROR.
                DO i = 1 TO 3:
                    IF oe-ord.sman[i] NE "" THEN
                        v-salesman = TRIM(v-salesman) + " " + oe-ord.sman[i] + ",".
                END.

                ASSIGN 
                    v-terms     = oe-ord.terms-d
                    v-frt-terms = IF cust.frt-pay EQ "P" THEN "Prepaid"
                           ELSE IF cust.frt-pay EQ "B" THEN "Bill"
                           ELSE IF cust.frt-pay EQ "C" THEN "Collect"
                           ELSE IF cust.frt-pay EQ "T" THEN "Third Party"
                           ELSE ""
                    v-zone      = cust.del-zone.
             
                IF v-terms EQ "" THEN
                DO:
                    FIND FIRST terms WHERE terms.t-code EQ oe-ord.terms NO-LOCK NO-ERROR.
                    IF AVAILABLE terms THEN
                        ASSIGN v-terms = terms.dscr.
                END.
      
                v-salesman = TRIM(v-salesman).
                v-po-no = oe-boll.po-no.
                v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).
                IF v-salesman GT '' THEN
                    IF substr(v-salesman,LENGTH(TRIM(v-salesman)),1) EQ "," THEN
                        substr(v-salesman,LENGTH(TRIM(v-salesman)),1) = "".

                v-fob = IF oe-ord.fob-code BEGINS "ORIG" THEN "Origin" ELSE "Destination".      
                LEAVE.
            END.
        /* end for each oe-boll */    
  
        END. /* first-of(oe-bolh.bol-no) */

  
        FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no:
            CREATE report.
            ASSIGN
                report.term-id  = v-term-id
                report.key-01   = oe-boll.i-no
                report.key-02   = STRING(oe-boll.ord-no,"9999999999")
                report.rec-id   = RECID(oe-boll)
                oe-boll.printed = YES.
        END.

        IF LAST-OF(oe-bolh.bol-no) THEN 
        DO:     
            IF v-comp-addr[2] = "" THEN
                ASSIGN v-comp-addr[2] = v-comp-addr3
                    v-comp-addr3   = "".
            IF v-ship-addr[2] = "" THEN
                ASSIGN v-ship-addr[2] = v-ship-addr3
                    v-ship-addr3   = "".          
            
            FIND FIRST tt-temp-report NO-LOCK NO-ERROR .
            {oe/rep/bolmclean-xl.i}
            {oe/rep/bolmclean2-xl.i}    
     
            IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.
      
            FOR EACH report WHERE report.term-id EQ v-term-id,
                FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK:
                DELETE report.
            END.

        END.  /* last-of*/

        oe-bolh.printed = YES.

    END. /* for each oe-bolh */   
     
    /*chExcelApplication:activeSheet:PageSetup:PrintArea = "$A$1:$N$48".*/
     
    chWorkbook:WorkSheets(1):Activate NO-ERROR.
    
    OS-DELETE VALUE(v-dir + "cofc.xls").     
    OS-DELETE VALUE(v-dir + "asi.pdf").
    OS-DELETE VALUE(v-dir + "cofc.pdf").

    IF LvOutputSelection = "PRINTER" THEN
    DO:
   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,FALSE,).
        chWorkbook:CLOSE(NO) NO-ERROR.
    END.
    ELSE IF LvOutputSelection = "Email" THEN
        DO:
            /*WshNetwork:SetDefaultPrinter(AdobePrinter).*/
            chExcelApplication:ActiveSheet:SaveAs(v-dir + "quote.xls") NO-ERROR. 	   
   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,FALSE,). 
            chWorkbook:CLOSE(NO) NO-ERROR.   
            chExcelApplication:QUIT() NO-ERROR.
            PAUSE 3.
            OS-DELETE VALUE(v-dir + "quote.xls").
            OS-RENAME VALUE(v-dir + "asi.pdf") VALUE(v-dir + "quote.pdf").
            LvCtr = LvCtr + 1.
            CREATE tt-filelist.
            ASSIGN 
                tt-FileCtr  = LvCtr
                tt-FileName = v-dir + "quote.pdf".
        END.

END PROCEDURE. /* FillData*/

PROCEDURE InitializeExcel:

    /* Capture the current active printer. */
    IF LvOutputSelection = "email" THEN
        ASSIGN 
            CurActivePrinter = SESSION:PRINTER-NAME
            AdobePrinter     = "PDFcamp Printer".
  
    RUN sys/ref/getFileFullPathName.p ("Template\bol-mclean.xlt", OUTPUT chFile).
    IF chFile = ? THEN  
        APPLY 'close' TO THIS-PROCEDURE.

    /* Connect to the running Excel session. */
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.

    /* If Excel is running close it. */
    IF VALID-HANDLE (chExcelApplication) THEN
    DO:
        chExcelApplication:Quit()         NO-ERROR.
        RUN CleanUp.
    END.


    /* Network connection checks. */
    CREATE "WScript.Network" WshNetwork NO-ERROR.
    IF NOT(VALID-HANDLE(WshNetwork)) THEN
    DO :
        MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.    
  
    /* Start a new session of Excel. */
    /*if not (valid-handle (chExcelApplication)) THEN*/
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
    /* Check if Excel got initialized. */
    IF NOT (VALID-HANDLE (chExcelApplication)) THEN
    DO :
        MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
  
    /* Make Excel visible. */
    ASSIGN
        chExcelApplication:VISIBLE = IF LvOutputSelection = "Email" OR 
                                     LvOutputSelection = "Printer" THEN  FALSE
                                  ELSE TRUE.      

END PROCEDURE.

PROCEDURE MainLoop:

    /* Open our Excel Template. */  
    ASSIGN 
        chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
    /* Do not display Excel error messages. */
    chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

    /* Disable screen updating so it will go faster */
    chExcelApplication:ScreenUpdating = FALSE.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(1):Activate NO-ERROR.
    chWorkSheet      = chExcelApplication:Sheets:item(1).

    /*Fill in Data*/
    RUN FillData.

    /* enable screen updating */
    chExcelApplication:ScreenUpdating = TRUE.
END PROCEDURE.

PROCEDURE CleanUp:

    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.

    /* Reset the Active Printer to the Original Printer. */
    IF CurActivePrinter <> '' THEN
        WshNetwork:SetDefaultPrinter(CurActivePrinter).

    
    /* Release created objects. */
    RELEASE OBJECT WshNetwork         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.
END PROCEDURE.


