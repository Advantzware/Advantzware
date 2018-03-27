&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

{methods/defines/hndldefs.i}
/*    {methods/prgsecur.i}*/
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{src/adm2/widgetprto.i}
DEFINE VARIABLE gchExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE gchWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE gchWorksheet        AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE gchFile             AS CHARACTER        NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiCreateBegin fiCreateEnd Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiCreateBegin fiCreateEnd fiJob 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiCreateBegin AS DATE FORMAT "99/99/99":U 
     LABEL "Calculation Date Begin" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiCreateEnd AS DATE FORMAT "99/99/99":U 
     LABEL "Calculation Date End" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiJob AS CHARACTER FORMAT "X(256)":U 
     LABEL "Processing Job" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fiCreateBegin AT ROW 1.71 COL 22 COLON-ALIGNED WIDGET-ID 2
     fiCreateEnd AT ROW 3.38 COL 22 COLON-ALIGNED WIDGET-ID 4
     fiJob AT ROW 5.52 COL 22 COLON-ALIGNED WIDGET-ID 6
     Btn_OK AT ROW 1.52 COL 49
     Btn_Cancel AT ROW 2.76 COL 49
     SPACE(1.19) SKIP(3.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Job Cost Report"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiJob IN FRAME gDialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Job Cost Report */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
        ASSIGN fiCreateBegin fiCreateEnd.
        RUN ipRunReport.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiCreateBegin fiCreateEnd fiJob 
      WITH FRAME gDialog.
  ENABLE fiCreateBegin fiCreateEnd Btn_OK Btn_Cancel 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
      Purpose:     Super Override
      Parameters:  
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    RUN SUPER.

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        fiCreateBegin:screen-value IN FRAME gDialog = STRING(DATE(MONTH(TODAY),1,YEAR(TODAY)))
        fiCreateEnd:screen-value                    = STRING(TODAY).
    APPLY 'entry' TO fiCreateBegin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRunReport gDialog 
PROCEDURE ipRunReport :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE iRow             AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyInM          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyPerSet       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrdPricePerM    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iEstType         AS INTEGER NO-UNDO.

    
    RUN pInitializeExcel.
    
    iRow = 3.
    FOR EACH costHeader NO-LOCK 
        WHERE costHeader.company EQ gcompany
        AND costHeader.calculationTime GE DATETIME(fiCreateBegin)
        AND costHeader.calculationTime LE DATETIME(fiCreateEnd + 1)
        AND costHeader.jobNo NE ""
        AND costHeader.isItem,
        FIRST job NO-LOCK 
        WHERE job.company EQ costHeader.company
        AND job.job-no EQ costHeader.jobNo
        AND job.job-no2 EQ costHeader.JobNo2,
        FIRST eb NO-LOCK 
        WHERE eb.company EQ costHeader.company
        AND eb.est-no EQ costHeader.estimateNo
        AND eb.form-no EQ costHeader.formNo
        AND eb.blank-no EQ costHeader.blankNo,
        FIRST cust NO-LOCK
        WHERE cust.company EQ eb.company
        AND cust.cust-no EQ eb.cust-no
        :   
                
        FIND FIRST est OF eb NO-LOCK NO-ERROR.
        IF AVAILABLE est THEN iEstType = est.est-type.        
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ costHeader.company
            AND itemfg.i-no EQ costHeader.fgItemID
            NO-ERROR.
        FIND FIRST job-hdr NO-LOCK 
            WHERE job-hdr.company EQ costHeader.company
            AND job-hdr.job-no EQ costHeader.jobNo
            AND job-hdr.job-no2 EQ costHeader.jobNo2
            AND job-hdr.i-no EQ costHeader.fgItemID
            NO-ERROR.                       
        IF AVAILABLE job-hdr THEN 
            FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ job-hdr.company
                AND oe-ordl.ord-no EQ job-hdr.ord-no
                AND oe-ordl.i-no EQ job-hdr.i-no
                NO-ERROR.  
        IF iEstType EQ 6 AND costHeader.formNo NE 0 THEN
            dQtyPerSet = costHeader.quantityPerSet.
        ELSE 
            dQtyPerSet = 1.
            
        dQtyInM = IF AVAILABLE job-hdr THEN job-hdr.qty / 1000 ELSE costHeader.quantityMaster * dQtyPerSet / 1000.
    
        IF AVAILABLE oe-ordl THEN 
        DO:
            RUN sys\ref\convptom.p (oe-ordl.pr-uom, oe-ordl.price, oe-ordl.qty, oe-ordl.cas-cnt, OUTPUT dOrdPricePerM).
        END.
        ELSE 
        DO:
            RUN sys\ref\convptom.p (itemfg.sell-uom, itemfg.sell-price, INT(dQtyInM * 1000), itemfg.case-count, OUTPUT dOrdPricePerM).
        END.
        ASSIGN
            gchWorkSheet:Range("A" + STRING(iRow)):VALUE  = costHeader.jobNo     
            gchWorkSheet:Range("B" + STRING(iRow)):VALUE  = costHeader.jobNo2
            gchWorkSheet:Range("C" + STRING(iRow)):VALUE  = costHeader.fgItemID       
            gchWorkSheet:Range("D" + STRING(iRow)):VALUE  = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""  
            gchWorkSheet:Range("E" + STRING(iRow)):VALUE  = IF AVAILABLE itemfg THEN itemfg.procat ELSE eb.procat   
            gchWorkSheet:Range("F" + STRING(iRow)):VALUE  = IF AVAILABLE itemfg THEN itemfg.part-no ELSE eb.part-no 
            gchWorkSheet:Range("G" + STRING(iRow)):VALUE  = IF AVAILABLE itemfg THEN itemfg.style ELSE eb.style        
            gchWorkSheet:Range("H" + STRING(iRow)):VALUE  = dQtyInM * 1000     
            gchWorkSheet:Range("I" + STRING(iRow)):VALUE  = costHeader.estimateNo
            gchWorkSheet:Range("J" + STRING(iRow)):VALUE  = costHeader.formNo       
            gchWorkSheet:Range("K" + STRING(iRow)):VALUE  = costHeader.blankNo       
            gchWorkSheet:Range("L" + STRING(iRow)):VALUE  = IF AVAILABLE job-hdr THEN job-hdr.ord-no ELSE 0       
            gchWorkSheet:Range("M" + STRING(iRow)):VALUE  = cust.cust-no
            gchWorkSheet:Range("N" + STRING(iRow)):VALUE  = cust.name  
            gchWorkSheet:Range("O" + STRING(iRow)):VALUE  = costHeader.calculationTime  
            gchWorkSheet:Range("P" + STRING(iRow)):VALUE  = costHeader.calculatedBy
            gchWorkSheet:Range("Q" + STRING(iRow)):VALUE  = costHeader.stdCostPrepMaterial / dQtyInM  
            gchWorkSheet:Range("R" + STRING(iRow)):VALUE  = costHeader.stdCostMiscMaterial / dQtyInM 
            gchWorkSheet:Range("S" + STRING(iRow)):VALUE  = costHeader.stdCostPrepLabor / dQtyInM
            gchWorkSheet:Range("T" + STRING(iRow)):VALUE  = costHeader.stdCostMiscLabor / dQtyInM
            gchWorkSheet:Range("U" + STRING(iRow)):VALUE  = costHeader.stdCostDirectMaterial / dQtyInM  
            gchWorkSheet:Range("V" + STRING(iRow)):VALUE  = costHeader.stdCostDirectLabor / dQtyInM 
            gchWorkSheet:Range("W" + STRING(iRow)):VALUE  = costHeader.stdCostVariableOverhead / dQtyInM
            gchWorkSheet:Range("X" + STRING(iRow)):VALUE  = costHeader.stdCostFixedOverhead / dQtyInM
            gchWorkSheet:Range("Y" + STRING(iRow)):VALUE  = costHeader.stdCostTotalFactory / dQtyInM
            gchWorkSheet:Range("Z" + STRING(iRow)):VALUE  = costHeader.stdCostCommission / dQtyInM
            gchWorkSheet:Range("AA" + STRING(iRow)):VALUE  = costHeader.stdCostFreight / dQtyInM
            gchWorkSheet:Range("AB" + STRING(iRow)):VALUE  = costHeader.stdCostFull / dQtyInM
            gchWorkSheet:Range("AC" + STRING(iRow)):VALUE  = costHeader.stdProfitGross / dQtyInM
            gchWorkSheet:Range("AD" + STRING(iRow)):VALUE  = IF costHeader.stdSellPrice GT 0 THEN (costHeader.stdProfitGross / costHeader.stdSellPrice) ELSE 0
            gchWorkSheet:Range("AE" + STRING(iRow)):VALUE  = costHeader.stdProfitNet / dQtyInM 
            gchWorkSheet:Range("AF" + STRING(iRow)):VALUE = IF costHeader.stdSellPrice GT 0 THEN (costHeader.stdProfitNet / costHeader.stdSellPrice) ELSE 0          
            gchWorkSheet:Range("AG" + STRING(iRow)):VALUE = costHeader.stdSellPrice / dQtyInM
            gchWorkSheet:Range("AH" + STRING(iRow)):VALUE = dOrdPricePerM
            gchWorkSheet:Range("AI" + STRING(iRow)):VALUE = IF costHeader.stdSellPrice GT 0 THEN dOrdPricePerM / (costHeader.stdSellPrice / dQtyInM ) ELSE 0
            iRow                                          = iRow + 1
            .
    END.

    RUN pFinishExcel.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFinishExcel gDialog 
PROCEDURE pFinishExcel :
/*------------------------------------------------------------------------------
     Purpose: Gets Excel ready for display and releases Handles
     Notes:
    ------------------------------------------------------------------------------*/
    ASSIGN
        gchExcelApplication:VISIBLE        = TRUE
        gchExcelApplication:ScreenUpdating = TRUE.
    /*    gchWorkBook:WorkSheets(1):Columns("A:AL"):AutoFit.*/
    gchExcelApplication:VISIBLE = TRUE.

    RELEASE OBJECT gchWorkBook         NO-ERROR.
    RELEASE OBJECT gchWorkSheet        NO-ERROR.
    RELEASE OBJECT gchExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitializeExcel gDialog 
PROCEDURE pInitializeExcel :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.

    CREATE "Excel.Application" gchExcelApplication.
    IF SEARCH("template\JobProfitability.xltx") = ? THEN 
    DO:
        FILE-INFO:FILE-NAME = SEARCH("C:\Advantzware\v16\Resources\template\JobProfitability.xltx").
    END.
    ELSE 
    DO:
        FILE-INFO:FILE-NAME = SEARCH("template\JobProfitability.xltx").
    END.
    ASSIGN 
        cFile = SEARCH(FILE-INFO:FULL-PATHNAME).
  
    IF SEARCH (cFile) = ? THEN 
    DO:
        MESSAGE 
            'Spreadsheet File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.

    ASSIGN
        cFile                              = FILE-INFO:FULL-PATHNAME
        gchWorkBook                        = gchExcelApplication:Workbooks:Open(cfile)
        gchExcelApplication:VISIBLE        = FALSE
        gchExcelApplication:DisplayAlerts  = FALSE
        gchExcelApplication:ScreenUpdating = FALSE
        .
        
    gchWorkBook:WorkSheets(1):Activate.
    gchWorkSheet = gchExcelApplication:Sheets:item(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

