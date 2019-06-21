
/*------------------------------------------------------------------------
    File        : ExceptionsRpt.p
    Purpose     :  Alphabetic Order

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttExceptionsList NO-UNDO
        FIELD vExceptionsFile AS CHAR.
        
    DEF TEMP-TABLE tt-vend-whse-item LIKE vend-whse-item
        FIELD tt-row-id AS ROWID
        FIELD row-id    AS ROWID
        FIELD has-rec   AS LOG INIT NO
        FIELD seq-no    AS INT
        INDEX seq-no seq-no.

    DEF TEMP-TABLE tt-email NO-UNDO
        FIELD cust-part-no LIKE vend-whse-trans.cust-part-no 
        FIELD fg-item-no   LIKE vend-whse-trans.fg-item-no 
        FIELD plant-tot-oh-qty LIKE vend-whse-trans.plant-tot-oh-qty 
        FIELD trans-date       LIKE vend-whse-trans.trans-date 
        FIELD trans-qty        LIKE vend-whse-trans.trans-qty 
        FIELD vendor-dept-code LIKE vend-whse-trans.vendor-dept-code 
        FIELD vendor-code      LIKE vend-whse-trans.vendor-code 
        FIELD vend-plant-code  LIKE vend-whse-trans.vendor-plant-code.
        
    DEFINE DATASET dsExceptionsList FOR ttExceptionsList .

    DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFIBegCustPartNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFIBegFgItemNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFIBegVendCode   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFIBegVendPlantCode   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFIEndCustPartNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFIEndFgItemNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFIEndVendCode   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFIEndVendPlantCode   AS CHARACTER NO-UNDO.
    

   
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsExceptionsList.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser   = ?   THEN ASSIGN prmUser = "".
    IF prmAction = ?  THEN ASSIGN prmAction = "".
    IF prmOut    = ?  THEN ASSIGN prmOut = "No".
    IF prmFIBegCustPartNo = ?  THEN ASSIGN prmFIBegCustPartNo = "".
    IF prmFIBegFgItemNo = ?  THEN ASSIGN prmFIBegFgItemNo = "".
    IF prmFIBegVendCode = ?  THEN ASSIGN prmFIBegVendCode = "".
    IF prmFIBegVendPlantCode = ?  THEN ASSIGN prmFIBegVendPlantCode = "".
    IF prmFIEndCustPartNo = ?  THEN ASSIGN prmFIEndCustPartNo = "".
    IF prmFIEndFgItemNo = ?  THEN ASSIGN prmFIEndFgItemNo = "".
    IF prmFIEndVendCode = ?  THEN ASSIGN prmFIEndVendCode = "".
    IF prmFIEndVendPlantCode = ?  THEN ASSIGN prmFIEndVendPlantCode = "".


    DEFINE VARIABLE FI-beg-cust-part-no AS CHARACTER FORMAT "X(12)" NO-UNDO.
    DEFINE VARIABLE FI-beg-fg-item-no AS CHARACTER FORMAT "X(15)" NO-UNDO.
    DEFINE VARIABLE FI-beg-vend-code AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE FI-beg-vend-plant-code AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE FI-end-cust-part-no AS CHARACTER FORMAT "X(12)":U INITIAL "zzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE FI-end-fg-item-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE FI-end-vend-code AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE FI-end-vend-plant-code AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.

    DEF VAR list-name as cha no-undo.
    DEF VAR init-dir AS CHAR NO-UNDO.
    DEF VAR lv-pdf-file AS cha NO-UNDO.
    DEFINE VAR vPdfFile AS CHAR NO-UNDO.

    DEFINE VAR tmp-dir AS CHAR NO-UNDO.


    /*DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
    DEF NEW SHARED VAR locode AS CHAR NO-UNDO.
    */
    
    {custom/xprint.i}
     
    /*{methods/defines/hndldefs.i}
    {methods/prgsecur.i}

    {custom/gcompany.i}
    {custom/gloc.i}
    {custom/getcmpny.i}
    {custom/getloc.i}
    */

    {sys/inc/VAR.i new shared}

    DEF VAR v-pr-tots AS LOG FORMAT "Y/N" NO-UNDO.
    DEF {1} SHARED VAR v-print-fmt AS CHAR NO-UNDO.
    DEF VAR ls-fax-file AS CHAR NO-UNDO.
    DEF VAR is-xprint-form AS LOG NO-UNDO. 

    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE STREAM excel.

    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 .
    DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" .
    DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" .
    DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" .
    DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 .
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO .
    DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO .
    DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL YES .
    DEF VAR ll-valid AS LOG NO-UNDO.

    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEF VAR prmComp AS CHAR NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(100)":U      NO-UNDO.
    DEFINE VAR custcount AS CHAR NO-UNDO.

    DEF VAR lv-post AS LOG NO-UNDO.
    DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.
    


    ASSIGN
        FI-beg-cust-part-no     = prmFIBegCustPartNo 
        FI-beg-fg-item-no       = prmFIBegFgItemNo
        FI-beg-vend-code        = prmFIBegVendCode
        FI-beg-vend-plant-code  = prmFIBegVendPlantCode
        FI-end-cust-part-no     = prmFIEndCustPartNo
        FI-end-fg-item-no       = prmFIEndFgItemNo
        FI-end-vend-code        = prmFIEndVendCode
        FI-end-vend-plant-code  = prmFIEndVendPlantCode.     


  
/*DEF VAR v-invalid    AS LOG NO-UNDO.
DEF VAR v-download   AS LOG INIT NO NO-UNDO.
DEF VAR v-prior      AS LOG INIT NO NO-UNDO.

DEF BUFFER tmp-per FOR period.

DEF STREAM s-temp.

DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.


DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2. 
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no. 
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no .
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no.


DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(100)":U      NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
*/

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.
    
assign
 cocode = prmComp
 locode = usercomp.loc
 tb_excel  = IF prmOut = "Yes" THEN TRUE ELSE FALSE 
 v-today    = TODAY   . 
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


IF prmAction = "Alpha" THEN DO:
  IF prmOut = "No" THEN DO:
   ASSIGN  
        init-dir    = v-webrootpath
        lv-pdf-file = init-dir + 'EXCEPTIONS' 
        lv-pdf-file = lv-pdf-file + FI-beg-vend-code
        vPdfFile   = 'EXCEPTIONS' + FI-beg-vend-code + '.pdf'.
    
        FOR EACH tt-vend-whse-item.
            DELETE tt-vend-whse-item.
        END.            

        /*run run-report.     
       
        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
           
        CREATE ttExceptionsList.
            ASSIGN ttExceptionsList.vExceptionsFile = vPdfFile.  
            */          
    END.
    

    IF prmOut = "Yes" THEN DO:

        ASSIGN
              init-dir    = v-webrootpath
              v-excel-file = init-dir + "EXCEPTIONS" +
              STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + ".csv".  

               vPdfFile   = "EXCEPTIONS" +
                   STRING(YEAR(v-today),"9999")
                    + STRING(MONTH(v-today),"99")
                    + STRING(DAY(v-today),"99") + ".csv".  
               
                FOR EACH tt-vend-whse-item.
                    DELETE tt-vend-whse-item.
                END.                             

                run run-report.
                MESSAGE "aftrunrpt".
               
               CREATE ttExceptionsList.
               ASSIGN ttExceptionsList.vExceptionsFile = vPdfFile.
    END.    

END.


/******************************************************************************************************/
PROCEDURE run-report :
   {sys/form/r-top3w.f}

DEF VAR v-head      AS CHAR FORMAT "x(280)" EXTENT 4 NO-UNDO.
DEF VAR v-per-rpt   AS LOG  FORMAT "PTD/YTD" INIT YES NO-UNDO.
DEF VAR v-date      AS DATE EXTENT 10 NO-UNDO.

DEF VAR v-length    AS CHAR NO-UNDO.
DEF VAR v-width     AS CHAR NO-UNDO.
DEF VAR v-depth     AS CHAR NO-UNDO.

DEF VAR v-dimensions    AS CHAR FORMAT "X(25)" NO-UNDO.
DEF VAR v-last-prod-dt  AS DATE NO-UNDO.
DEF VAR v-next-prod-dt  AS DATE NO-UNDO.
DEF VAR v-next-prod-qty AS DECI NO-UNDO.
DEF VAR v-caliper       LIKE ef.cal NO-UNDO.
DEF VAR v-last-ship-dt  AS DATE NO-UNDO.
DEF VAR v-no-of-colors  AS INT NO-UNDO.
DEF VAR v-cal           LIKE ef.cal NO-UNDO.
DEF VAR v-tot-inv       AS DECI NO-UNDO.
DEF VAR v-combined-mths AS DECI NO-UNDO.

DEF BUFFER b-itemfg  FOR itemfg.
DEF BUFFER b-eb      FOR eb.
DEF BUFFER b-est     FOR est.
DEF BUFFER b-ef      FOR ef.
DEF BUFFER b-job     FOR job.
DEF BUFFER b-job-hdr FOR job-hdr.
DEF BUFFER b-fg-rcpth FOR fg-rcpth.

FORMAT HEADER
   v-head[1] SKIP
   v-head[2] SKIP
   v-head[3] SKIP
   v-head[4]
WITH FRAME r-top WIDTH 280.

ASSIGN
   str-tit3 = (IF v-per-rpt THEN "P" ELSE "Y") +
              "TD (" + STRING(v-date[1]) + "-" + STRING(v-date[2]) +
              ")"

{sys/inc/ctrtext.i str-tit3 162}

v-head[1] = "".
                                                                                                                                                
ASSIGN                                                                                                                              
   v-head[2] = "         CUSTOMERS    DESCRIPTION/              SUPPLIERS                  TOT                          SUPPLIERS   CUSTOMERS  COMBINED     NEXT   NEXT PROD     LAST     EAU @     LAST      DATE    CASE  PIAP "
   v-head[3] = "PLANT ID PART NO      DIMENSIONS                PART NO         STYLE  CAL INK      PRICE/M        EAU  INVENTORY   INVENTORY    MTHS    PROD DATE  QUANTITY  PROD DATE LAST PROD SHIP DATE OBSOLETE  QTY   SCAN   "
   v-head[4] = FILL("-",208).

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}


/*IF td-show-parm THEN RUN show-param.*/

DISPLAY "" WITH FRAME r-top.

/*
 list-name = init-dir + "tmp" + string(time).
 PUT "<PDF=DIRECT><OLANDSCAPE><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><CPI10.5><P11>" FORM "x(350)". 
*/ 


IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(v-excel-file).
   ASSIGN excelheader = "PLANT ID,CUSTOMERS PART NO,DESCRIPTION,SUPLLIERS PART NO,DIMENSIONS,STYLE,CAL,TOT INK,PRICE/M,EAU,SUPPLIERS INVENTORY,CUSTOMERS INVENTORY,COMBINED MTHS,NEXT PROD DATE,NEXT PROD QTY,LAST PROD DATE,EAU @ LAST PROD,LAST SHIP DATE,DATE OBSOLETE,CASE QTY,PIAP SCAN".
   PUT STREAM excel UNFORMATTED excelheader SKIP.
END.


FOR EACH vend-whse-item WHERE vend-whse-item.vendor-code >= FI-beg-vend-code
                          AND vend-whse-item.vendor-code <= FI-end-vend-code
                          AND vend-whse-item.vendor-plant-code >= FI-beg-vend-plant-code
                          AND vend-whse-item.vendor-plant-code <= FI-end-vend-plant-code
                          AND vend-whse-item.fg-item-no >= FI-beg-fg-item-no
                          AND vend-whse-item.fg-item-no <= FI-end-fg-item-no
                          AND vend-whse-item.cust-part-no >= FI-beg-cust-part-no
                          AND vend-whse-item.cust-part-no <= FI-end-cust-part-no
   BREAK BY vend-whse-item.vendor-code                                            
         BY vend-whse-item.vendor-plant-code:

      CREATE tt-vend-whse-item.
      BUFFER-COPY vend-whse-item TO tt-vend-whse-item
      ASSIGN
         tt-vend-whse-item.row-id     = ROWID(vend-whse-item)
         tt-vend-whse-item.has-rec    = YES
         tt-vend-whse-item.seq-no     = 1.
END.
FOR EACH tt-vend-whse-item
  BREAK BY tt-vend-whse-item.vendor-code                                            
        BY tt-vend-whse-item.vendor-plant-code:
   
   ASSIGN 
      v-dimensions   = ""
      v-last-prod-dt = ?
      v-caliper = 0
      v-next-prod-qty = 0
      v-next-prod-dt = ?
      v-last-ship-dt = ?
      v-no-of-colors = 0
      v-tot-inv = 0
      v-combined-mths = 0.   
   
   FIND FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-item.company
                         AND b-itemfg.i-no    = tt-vend-whse-item.fg-item-no
                         AND b-itemfg.part-no = tt-vend-whse-item.cust-part-no NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE(b-itemfg) THEN 
      FIND FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-item.company
                            AND b-itemfg.i-no    = tt-vend-whse-item.fg-item-no NO-LOCK NO-ERROR.
   
   IF tt-vend-whse-item.cust-part-no = "" THEN
      tt-vend-whse-item.cust-part-no = b-itemfg.part-no.

   RUN sys/inc/dec-frac.p (INPUT l-score[50],
                           INPUT 32,
                           OUTPUT v-length).
   RUN sys/inc/dec-frac.p (INPUT w-score[50],
                           INPUT 32,
                           OUTPUT v-width).
   RUN sys/inc/dec-frac.p (INPUT d-score[50],
                           INPUT 32,
                           OUTPUT v-depth).
   
   ASSIGN
      v-dimensions = TRIM(v-length) + " x " + TRIM(v-width) + " x " + TRIM(v-depth).

   /* start - get last and next production date/quantity */

   FOR EACH b-job-hdr NO-LOCK WHERE b-job-hdr.company = tt-vend-whse-item.company
                                AND b-job-hdr.i-no    = tt-vend-whse-item.fg-item-no
                                AND b-job-hdr.opened  = NO
                          USE-INDEX i-no,
      FIRST b-job NO-LOCK WHERE b-job.company = b-job-hdr.company
                            AND b-job.job     = b-job-hdr.job
                            AND b-job.job-no  = b-job-hdr.job-no
                            AND b-job.job-no2 = b-job-hdr.job-no2
                            AND b-job.due-date <> ?
                       BREAK BY b-job.due-date:
      IF FIRST-OF(b-job.due-date) THEN
         v-last-prod-dt = b-job.due-date.
      
      FIND FIRST b-est WHERE b-est.company = b-job-hdr.company
                         AND b-est.est-no  = b-job-hdr.est-no NO-LOCK NO-ERROR.
      FIND FIRST b-ef WHERE b-ef.company = b-est.company
                        AND b-ef.est-no  = b-est.est-no NO-LOCK NO-ERROR.
      FIND FIRST b-eb WHERE b-eb.company = b-ef.company 
                        AND b-eb.est-no  = b-ef.est-no
                        AND b-eb.form-no = b-ef.form-no NO-LOCK NO-ERROR.  
      
      IF b-job.due-date >= v-last-prod-dt THEN
         ASSIGN
            v-last-prod-dt = b-job.due-date
            v-caliper      = b-ef.cal
            v-no-of-colors = b-eb.i-col.
   END.
    
   FOR EACH b-job-hdr NO-LOCK WHERE b-job-hdr.company = tt-vend-whse-item.company
                                AND b-job-hdr.i-no    = tt-vend-whse-item.fg-item-no
                                AND b-job-hdr.opened  = YES
                          USE-INDEX i-no,
      FIRST b-job NO-LOCK WHERE b-job.company = b-job-hdr.company
                            AND b-job.job     = b-job-hdr.job
                            AND b-job.job-no  = b-job-hdr.job-no
                            AND b-job.job-no2 = b-job-hdr.job-no2
                            AND b-job.due-date <> ?
                       BREAK BY b-job.due-date
                             BY b-job.due-date:
      IF FIRST-OF(b-job.due-date) THEN
         v-next-prod-dt = b-job.due-date.

      
      IF b-job.due-date >= TODAY AND b-job.due-date <= v-next-prod-dt THEN DO:
         ASSIGN
            v-next-prod-dt = b-job.due-date
            v-next-prod-qty = b-job-hdr.qty.
      END.
      ELSE
         ASSIGN
            v-next-prod-dt = ?
            v-next-prod-qty = 0.
   END.
   
   FOR EACH b-fg-rcpth WHERE b-fg-rcpth.company  = tt-vend-whse-item.company 
                         AND b-fg-rcpth.i-no     = tt-vend-whse-item.fg-item-no 
                         AND b-fg-rcpth.rita-code = "S" NO-LOCK
                    BREAK BY b-fg-rcpth.rita-code 
                          BY b-fg-rcpth.trans-date DESC: 
      IF FIRST-OF(b-fg-rcpth.rita-code) THEN DO:
         v-last-ship-dt = b-fg-rcpth.trans-date.
         LEAVE.
      END.
   END.

   /* end - get last date and next production date/quantity */

   v-tot-inv = b-itemfg.q-onh + tt-vend-whse-item.plant-tot-oh-qty.
   IF v-tot-inv > 0 AND tt-vend-whse-item.est-annual-usage > 0 THEN
      v-combined-mths = ROUND((v-tot-inv / tt-vend-whse-item.est-annual-usage) * 12, 2).
    
   DISPLAY
      tt-vend-whse-item.vendor-plant-code FORMAT "X(8)"
      SPACE(1)
      tt-vend-whse-item.cust-part-no      FORMAT "X(12)"
      b-itemfg.i-name                     FORMAT "X(25)"
      tt-vend-whse-item.fg-item-no        FORMAT "X(15)"
      b-itemfg.style                      FORMAT "X(6)"
      b-ef.cal WHEN AVAILABLE b-ef        FORMAT ".999"
      v-no-of-colors                      FORMAT ">9"
      b-itemfg.sell-price                 FORMAT ">,>>>,>>9.99"
      tt-vend-whse-item.est-annual-usage  FORMAT "->,>>>,>>9"
      b-itemfg.q-onh                      FORMAT "->,>>>,>>9"
      tt-vend-whse-item.plant-tot-oh-qty  FORMAT "->,>>>,>>9"
      SPACE(2)
      v-combined-mths                     FORMAT ">>9.99"
      SPACE(6)
      v-next-prod-dt
      v-next-prod-qty                     FORMAT ">,>>>,>>9"
      SPACE(2)
      v-last-prod-dt
      SPACE(13) /* EAU @ last prod */
      v-last-ship-dt
      tt-vend-whse-item.obsolete-date
      b-itemfg.case-count                 FORMAT ">>>9"
      tt-vend-whse-item.piap-scan
      WITH FRAME a NO-BOX NO-LABELS STREAM-IO DOWN  WIDTH 280.

   DISPLAY
      SPACE(22)
      v-dimensions                        FORMAT "X(25)"
      WITH FRAME b NO-BOX NO-LABELS STREAM-IO DOWN  WIDTH 280.

   IF AVAILABLE(b-ef) THEN
      v-cal = b-ef.cal.
   ELSE
      v-cal = 0.

   IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
         '"' tt-vend-whse-item.vendor-plant-code               '",'
         '"' REPLACE(tt-vend-whse-item.cust-part-no,'"', "")   '",'
         '"' REPLACE(b-itemfg.i-name,'"', "")                  '",'
         '"' REPLACE(tt-vend-whse-item.fg-item-no,'"', "")     '",'
         '"' v-dimensions                                      '",'
         '"' b-itemfg.style                                    '",'
         '"' v-cal                                             '",'
         '"' v-no-of-colors                                    '",'
         '"' b-itemfg.sell-price                               '",'
         '"' tt-vend-whse-item.est-annual-usage                '",'
         '"' b-itemfg.q-onh                                    '",'
         '"' tt-vend-whse-item.plant-tot-oh-qty                '",'
         '"' v-combined-mths                                   '",' 
         '"' v-next-prod-dt                                    '",'
         '"' v-next-prod-qty                                   '",'
         '"' v-last-prod-dt                                    '",'
         '"'                                                   '",' /* EAU @ last prod */
         '"' v-last-ship-dt                                    '",'
         '"' tt-vend-whse-item.obsolete-date                   '",'
         '"' b-itemfg.case-count                               '",'
         '"' tt-vend-whse-item.piap-scan                       '",'
         SKIP.
END.
    

END PROCEDURE.





