
/*------------------------------------------------------------------------
    File        : RedFlagRpt.p
    Purpose     :  Alphabetic Order

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttRedFlagList NO-UNDO
        FIELD vRedFlagFile AS CHAR.
        
    DEF TEMP-TABLE tt-vend-whse-item LIKE vend-whse-item
    FIELD tt-row-id            AS ROWID
    FIELD row-id               AS ROWID
    FIELD has-rec              AS LOG INIT NO
    FIELD seq-no               AS INT
    FIELD est-no               LIKE est.est-no
    FIELD board                LIKE ef.board
    FIELD total-inventory      AS DECI
    FIELD weekly-usage         AS DECI
    FIELD style                LIKE itemfg.style
    FIELD sell-price           LIKE itemfg.sell-price
    FIELD q-onh                LIKE itemfg.q-onh
    FIELD need-to-produce      AS DECI
    FIELD producing            AS DECI
    FIELD no-of-ups            AS INT
    FIELD needed-sheets        AS DECI
    FIELD board-cost           AS DECI
    INDEX seq-no seq-no.

    DEF TEMP-TABLE tt-materials
    FIELD fg-item-no        LIKE itemfg.i-no
    FIELD board             LIKE ef.board
    FIELD sheet-width       LIKE ef.gsh-wid
    FIELD sheet-length      LIKE ef.gsh-len
    FIELD need-to-produce   AS DECI
    FIELD no-ups            AS INT
    FIELD colors            LIKE eb.i-coldscr
    FIELD needed-sheets     AS DECI.
        
    DEFINE DATASET dsRedFlagList FOR ttRedFlagList .

    DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vFIBegCustPartNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vFIBegFgItemNo        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vFIBegVendCode     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vFIBegVendPlantCode       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vFIEndCustPartNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vFIEndFgItemNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vFIEndVendCode      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vFIEndVendPlantCode      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vFINumberOfWeeks      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER vTGPrintRqMaterials AS CHARACTER NO-UNDO.
        


    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRedFlagList.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser   = ?   THEN ASSIGN prmUser = "".
    IF prmAction = ?  THEN ASSIGN prmAction = "".
    IF prmOut    = ?  THEN ASSIGN prmOut = "No".
    IF vFIBegCustPartNo = ?  THEN ASSIGN vFIBegCustPartNo = "".
    IF vFIBegFgItemNo = ?  THEN ASSIGN vFIBegFgItemNo = "".
    IF vFIBegVendCode = ?  THEN ASSIGN vFIBegVendCode = "".
    IF vFIBegVendPlantCode = ?  THEN ASSIGN vFIBegVendPlantCode = "".
    IF vFIEndCustPartNo = ?  THEN ASSIGN vFIEndCustPartNo = "".
    IF vFIEndFgItemNo = ?  THEN ASSIGN vFIEndFgItemNo = "".
    IF vFIEndVendCode = ?  THEN ASSIGN vFIEndVendCode = "".
    IF vFIEndVendPlantCode = ?  THEN ASSIGN vFIEndVendPlantCode = "".
    IF vFINumberOfWeeks = ?  THEN ASSIGN vFINumberOfWeeks = 0. 



    DEF VAR list-name AS CHAR NO-UNDO.
    DEF VAR init-dir  AS CHAR NO-UNDO.
    DEF VAR lv-pdf-file AS cha NO-UNDO.
    DEFINE VAR vPdfFile AS CHAR NO-UNDO.

    DEFINE VAR tmp-dir AS CHAR NO-UNDO.

    DEFINE STREAM excel.

    DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
    DEF NEW SHARED VAR locode AS CHAR NO-UNDO.
    
     {custom/xprint.i}
  
DEF VAR v-invalid    AS LOG NO-UNDO.
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
        lv-pdf-file = init-dir + 'REDFLAG' 
        lv-pdf-file = lv-pdf-file + vFIBegVendCode + STRING(TIME)
        vPdfFile   = 'REDFLAG' + vFIBegVendCode + STRING(TIME) + '.pdf'.
    
        FOR EACH tt-vend-whse-item.
            DELETE tt-vend-whse-item.
        END.

        FOR EACH tt-materials.
            DELETE tt-materials.
        END.
        
      /*
        RUN create-tt-vend-whse-item.

        run run-report.
        MESSAGE "runreport" list-name.
       
       RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
        MESSAGE "printpdf".*/
    
        CREATE ttRedFlagList.
            ASSIGN ttRedFlagList.vRedFlagFile = vPdfFile.            
    END.
    

    IF prmOut = "Yes" THEN DO:

        ASSIGN
              init-dir    = v-webrootpath
              v-excel-file = init-dir + "REDFLAG" +
              STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

               vPdfFile   = "REDFLAG" +
                   STRING(YEAR(v-today),"9999")
                    + STRING(MONTH(v-today),"99")
                    + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
               
                FOR EACH tt-vend-whse-item.
                    DELETE tt-vend-whse-item.
                END.

                FOR EACH tt-materials.
                    DELETE tt-materials.
                END.

                RUN create-tt-vend-whse-item.

                run run-report.
               
               CREATE ttRedFlagList.
               ASSIGN ttRedFlagList.vRedFlagFile = vPdfFile.
    END.    

END.


/******************************************************************************************************/
PROCEDURE create-tt-vend-whse-item :
DEF VAR v-total-inventory  AS DECI NO-UNDO.
DEF VAR v-weekly-usage     AS DECI NO-UNDO.
DEF VAR v-number-of-weeks  AS DECI NO-UNDO.
DEF VAR v-weeks-in-inv     AS DECI NO-UNDO.
DEF VAR v-producing        AS DECI NO-UNDO.
DEF VAR v-board-cost       AS DECI NO-UNDO.

DEF BUFFER b-itemfg  FOR itemfg.
DEF BUFFER b-eb      FOR eb.
DEF BUFFER b-est     FOR est.
DEF BUFFER b-ef      FOR ef.
DEF BUFFER b-job-mat FOR job-mat.
DEF BUFFER b-job-hdr FOR job-hdr.

FOR EACH vend-whse-item WHERE vend-whse-item.vendor-code >= vFIBegVendCode
                          AND vend-whse-item.vendor-code <= vFIEndVendCode
                          AND vend-whse-item.vendor-plant-code >= vFIBegVendPlantCode
                          AND vend-whse-item.vendor-plant-code <= vFIEndVendPlantCode
                          AND vend-whse-item.fg-item-no >= vFIBegFgItemNo
                          AND vend-whse-item.fg-item-no <= vFIEndFgItemNo
                          AND vend-whse-item.cust-part-no >= vFIBegCustPartNo
                          AND vend-whse-item.cust-part-no <= vFIEndCustPartNo
                          AND vend-whse-item.obsolete-date = ?
                     BREAK BY vend-whse-item.vendor-code                                            
                           BY vend-whse-item.vendor-plant-code:
   ASSIGN
      v-weekly-usage    = 0
      v-total-inventory = 0
      v-producing       = 0
      v-number-of-weeks = 0
      v-board-cost      = 0.

   FIND FIRST b-itemfg WHERE b-itemfg.company = vend-whse-item.company
                         AND b-itemfg.i-no    = vend-whse-item.fg-item-no
                         AND b-itemfg.part-no = vend-whse-item.cust-part-no NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE(b-itemfg) THEN 
      FIND FIRST b-itemfg WHERE b-itemfg.company = vend-whse-item.company
                            AND b-itemfg.i-no    = vend-whse-item.fg-item-no NO-LOCK NO-ERROR.   

   ASSIGN
      v-total-inventory = b-itemfg.q-onh + vend-whse-item.plant-tot-oh-qty
      v-weekly-usage    = vend-whse-item.est-annual-usage / 52
      v-number-of-weeks = ROUND(v-total-inventory / v-weekly-usage, 2).
   
   IF v-number-of-weeks < vFINumberOfWeeks THEN DO:  
      FIND FIRST b-est WHERE b-est.company      = b-itemfg.company
                         AND TRIM(b-est.est-no) = TRIM(b-itemfg.est-no) NO-LOCK NO-ERROR.
      FIND FIRST b-ef WHERE b-ef.company      = b-itemfg.company
                        AND TRIM(b-ef.est-no) = TRIM(b-itemfg.est-no) NO-LOCK NO-ERROR.
      IF AVAILABLE(b-ef) THEN DO:
         FIND FIRST b-eb WHERE b-eb.company      = b-itemfg.company
                           AND TRIM(b-eb.est-no) = TRIM(b-itemfg.est-no)
                           AND b-eb.form-no      = b-ef.form-no 
                           AND b-eb.part-no      = b-itemfg.part-no NO-LOCK NO-ERROR.

         IF b-ef.cost-msh > 0 THEN DO:
            v-board-cost = b-ef.cost-msh.   
         END.
         ELSE DO:
            FIND FIRST b-job-hdr WHERE b-job-hdr.company    = b-eb.company
                                   AND b-job-hdr.est-no     = b-eb.est-no
                                   AND b-job-hdr.frm        = b-eb.form-no
                                   AND b-job-hdr.blank-no   = b-eb.blank-no
                                   AND b-job-hdr.i-no       = vend-whse-item.fg-item-no NO-LOCK NO-ERROR.

            FIND FIRST b-job-mat WHERE b-job-mat.company  EQ b-eb.company
                                   AND b-job-mat.job      EQ b-job-hdr.job
                                   AND b-job-mat.job-no   EQ b-job-hdr.job-no
                                   AND b-job-mat.job-no2  EQ b-job-hdr.job-no2
                                   AND b-job-mat.frm      EQ b-eb.form-no
                                   AND b-job-mat.blank-no EQ b-eb.blank-no
                                   AND b-job-mat.i-no     EQ b-ef.board
                                   AND b-job-mat.rm-i-no  EQ b-ef.board   
                                    USE-INDEX i-no NO-LOCK NO-ERROR.
            
            v-board-cost = b-job-mat.cost.
         END.

         IF AVAILABLE(b-eb) THEN DO:

            CREATE tt-vend-whse-item.
            BUFFER-COPY vend-whse-item TO tt-vend-whse-item
            ASSIGN
               tt-vend-whse-item.row-id            = ROWID(vend-whse-item)
               tt-vend-whse-item.has-rec           = YES
               tt-vend-whse-item.seq-no            = 1
               tt-vend-whse-item.est-no            = b-itemfg.est-no
               tt-vend-whse-item.total-inventory   = v-total-inventory
               tt-vend-whse-item.weekly-usage      = v-weekly-usage
               tt-vend-whse-item.style             = b-itemfg.style
               tt-vend-whse-item.producing         = v-producing
               tt-vend-whse-item.sell-price        = b-itemfg.sell-price
               tt-vend-whse-item.q-onh             = b-itemfg.q-onh
               tt-vend-whse-item.need-to-produce   = (vFINumberOfWeeks * v-weekly-usage) - v-total-inventory
               tt-vend-whse-item.board             = b-ef.board
               tt-vend-whse-item.caliper           = STRING(b-ef.cal, ".999")
               tt-vend-whse-item.no-of-ups         = b-eb.num-up
               tt-vend-whse-item.needed-sheets     = tt-vend-whse-item.need-to-produce / b-eb.num-up
               tt-vend-whse-item.board-cost        = v-board-cost.
         END.

         IF vTGPrintRqMaterials = "Yes" THEN DO:
            CREATE tt-materials.
            ASSIGN
               tt-materials.fg-item-no      = vend-whse-item.fg-item-no
               tt-materials.board           = b-ef.board
               tt-materials.sheet-width     = b-ef.gsh-wid
               tt-materials.sheet-length    = b-ef.gsh-len
               tt-materials.need-to-produce = (vFINumberOfWeeks * v-weekly-usage) - v-total-inventory               
               tt-materials.no-ups          = b-eb.num-up
               tt-materials.colors          = b-eb.i-coldscr
               tt-materials.needed-sheets   = tt-materials.need-to-produce / b-eb.num-up.          
         END.
      END.
   END.
END. 

END PROCEDURE.


/******************************************************************************************************/
PROCEDURE run-report :
   DEF VAR v-head      AS CHAR FORMAT "x(280)" EXTENT 4 NO-UNDO.
   DEF VAR v-excelheader      AS CHAR NO-UNDO.

   {sys/inc/print1.i}

   {sys/inc/outprint.i VALUE(lines-per-page)}

  {sys/form/r-top3w.f}

   FORMAT HEADER
      v-head[1] SKIP
      v-head[2] SKIP
      v-head[3] SKIP
      v-head[4]
   WITH FRAME f-top WIDTH 285.

   v-head[1] = "".
                                                                                                                                                
   ASSIGN                                                                                                                              
      v-head[2] = "                                  EST ANNUAL    ONHAND      PLANT      PRODUCTION            NEEDED         BOARD"
      v-head[3] = "FG ITEM NO      BOARD       CAL     USAGE      QUANTITY   ONHAND QTY    QUANTITY    #UPS     SHEETS          COST"
      v-head[4] = FILL("-",113).

   DISPLAY "" WITH FRAME r-top.
   DISPLAY "" WITH FRAME f-top.

     MESSAGE "befput1".

   list-name = init-dir + "tmp" + string(time).    
   PUT "<PDF=DIRECT><OLANDSCAPE><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><CPI13.3><p9>" FORM "x(320)". 

   MESSAGE "aftput2" lv-pdf-file.

   IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(v-excel-file).
      ASSIGN v-excelheader = "FG ITEM NO,BOARD,CALIPER,EST ANNUAL USAGE,ONHAND QTY,PLANT ONHAND QTY,PRODUCTION QTY,#UPS,NEEDED SHEETS,BOARD COST".
      PUT STREAM excel UNFORMATTED v-excelheader SKIP.
   END.  
   
   
   FOR EACH tt-vend-whse-item:
      DISPLAY
         tt-vend-whse-item.fg-item-no        FORMAT "X(15)"
         tt-vend-whse-item.board             FORMAT "X(10)"
         tt-vend-whse-item.caliper           FORMAT "X(4)"
         SPACE(3)
         tt-vend-whse-item.est-annual-usage  FORMAT "->,>>>,>>9"
         SPACE(1) 
         tt-vend-whse-item.q-onh             FORMAT "->,>>>,>>9"
         SPACE(3)
         tt-vend-whse-item.plant-tot-oh-qty  FORMAT "->,>>>,>>9"
         SPACE(3)
         tt-vend-whse-item.need-to-produce   FORMAT "->,>>>,>>9"
         SPACE(4)
         tt-vend-whse-item.no-of-ups         FORMAT ">>9"
         tt-vend-whse-item.needed-sheets     FORMAT "->,>>>,>>9"
         tt-vend-whse-item.board-cost        FORMAT "->,>>>,>>9.99"
         WITH FRAME a NO-BOX NO-LABELS STREAM-IO DOWN  WIDTH 280.
      
      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
            '"' REPLACE(tt-vend-whse-item.fg-item-no,'"', "")  '",'
            '"' tt-vend-whse-item.board                        '",'
            '"' tt-vend-whse-item.caliper                      '",'
            '"' tt-vend-whse-item.est-annual-usage             '",'
            '"' tt-vend-whse-item.q-onh                        '",'
            '"' tt-vend-whse-item.plant-tot-oh-qty             '",'
            '"' tt-vend-whse-item.need-to-produce              '",'
            '"' tt-vend-whse-item.no-of-ups                    '",'
            '"' tt-vend-whse-item.needed-sheets                '",'
            '"' tt-vend-whse-item.board-cost                   '",'
            SKIP.
   END.
   
   IF vTGPrintRqMaterials = "Yes" THEN DO:
      HIDE FRAME f-top.
      HIDE FRAME a.
      
      ASSIGN                                                                                                                              
         v-head[2] = "                              SHEET      SHEET  PRODUCTION               NEEDED     COLOR"
         v-head[3] = "FG ITEM NO      BOARD         WIDTH     LENGTH    QUANTITY    #UPS       SHEETS     DESCRIPTION"
         v-head[4] = FILL("-",113).
      
      PAGE.
      
      FORMAT HEADER
         v-head[1] SKIP
         v-head[2] SKIP
         v-head[3] SKIP
         v-head[4]
      WITH FRAME f-top2 WIDTH 285.
   
      DISPLAY "" WITH FRAME f-top2.
     
      IF tb_excel THEN DO:
         ASSIGN v-excelheader = "FG ITEM NO,BOARD,SHEET WIDTH,SHEET LENGTH,PRODUCTION QTY,#UPS,NEEDED SHEETS,COLOR DESCRIPTION".
         PUT STREAM excel UNFORMATTED v-excelheader SKIP.
      END.

      FOR EACH tt-materials:
         DISPLAY
            tt-materials.fg-item-no       FORMAT "X(15)"
            tt-materials.board            FORMAT "X(10)"
            tt-materials.sheet-width
            SPACE(3)
            tt-materials.sheet-length
            SPACE(2)
            tt-materials.need-to-produce  FORMAT "->,>>>,>>9"
            SPACE(5)
            tt-materials.no-ups           FORMAT ">>9"
            SPACE(3)
            tt-materials.needed-sheets    FORMAT "->,>>>,>>9"
            SPACE(4)
            tt-materials.colors
            WITH FRAME b NO-BOX NO-LABELS STREAM-IO DOWN  WIDTH 280.
   
      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
            '"' REPLACE(tt-materials.fg-item-no,'"', "")       '",'
            '"' tt-materials.board                             '",'
            '"' tt-materials.sheet-width                       '",'
            '"' tt-materials.sheet-length                      '",'
            '"' tt-materials.need-to-produce                   '",'
            '"' tt-materials.no-ups                            '",'
            '"' tt-materials.needed-sheets                     '",'
            '"' tt-materials.colors                            '",'
            SKIP.

      END.
   END.

   OUTPUT CLOSE.      

END PROCEDURE.





