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

/* Local Variable Definitions ---                                       */
def temp-table ttSumm
    field due-date as date
    field machine as char
    field tot-hrs as decimal.
def temp-table ttMach
    field machine as char
    field tothrs as decimal.


{src/adm2/widgetprto.i}

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
&Scoped-Define ENABLED-OBJECTS fiDueBegin fiDueEnd Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiDueBegin fiDueEnd fiJob 

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

DEFINE VARIABLE fiDueBegin AS DATE FORMAT "99/99/99":U 
     LABEL "Create Date Begin" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiDueEnd AS DATE FORMAT "99/99/99":U 
     LABEL "Create Date End" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiJob AS CHARACTER FORMAT "X(256)":U 
     LABEL "Processing Job" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fiDueBegin AT ROW 1.71 COL 19 COLON-ALIGNED WIDGET-ID 2
     fiDueEnd AT ROW 3.62 COL 19 COLON-ALIGNED WIDGET-ID 4
     fiJob AT ROW 5.52 COL 19 COLON-ALIGNED WIDGET-ID 6
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
    run ipRunReport.
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
  DISPLAY fiDueBegin fiDueEnd fiJob 
      WITH FRAME gDialog.
  ENABLE fiDueBegin fiDueEnd Btn_OK Btn_Cancel 
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
    assign
        fiDueBegin:screen-value in frame gDialog = string(date(month(today),1,year(today)))
        fiDueEnd:screen-value = string(today).
    apply 'entry' to fiDueBegin.

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
DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chHyper            AS COMPONENT-HANDLE NO-UNDO. 
DEFINE VARIABLE chFile             AS CHARACTER        NO-UNDO.

def var iRow as int no-undo.

CREATE "Excel.Application" chExcelApplication.
FILE-INFO:FILE-NAME = SEARCH("template\PTjobCost.xlt").
ASSIGN 
    chFile = SEARCH(FILE-INFO:FULL-PATHNAME).
  
IF SEARCH (chFile) = ? THEN DO:
    MESSAGE 
        'Spreadsheet File: ' FILE-INFO:FULL-PATHNAME
        'cannot be found. Please verify that the file exists.'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.

ASSIGN
    chFile = FILE-INFO:FULL-PATHNAME
    chWorkbook = chExcelApplication:Workbooks:Open(chfile)
    chExcelApplication:VISIBLE = false
    chExcelApplication:DisplayAlerts = FALSE
    chExcelApplication:ScreenUpdating = false
    iRow = 3.
chWorkbook:WorkSheets(1):Activate.
chWorkSheet = chExcelApplication:Sheets:item(1).
    
FOR EACH job NO-LOCK WHERE 
    job.company = "001" AND 
    job.opened = true and
    job.create-date >= date(fiDueBegin:screen-value in frame gDialog) and
    job.create-date <= date(fiDueEnd:screen-value) 
    use-index due-date: 
    EACH job-hdr NO-LOCK OF job,
    EACH job-mat NO-LOCK OF job:
    
    FIND FIRST est NO-LOCK WHERE
        est.company = job.company and
        est.loc = job.loc and
        est.est-no = job.est-no
        USE-INDEX est-no.
    IF AVAIL est THEN DO:
        FIND FIRST item NO-LOCK WHERE
            item.company = job.company and
            item.i-no = job-mat.rm-i-no
            NO-ERROR.
        IF AVAIL item THEN DO:
            FOR EACH probe WHERE
                probe.company = job.company and
                probe.est-no = job.est-no, 
                
                
                EACH oe-ordl NO-LOCK WHERE 
        FIRST itemfg NO-LOCK OF job-hdr, 
                    oe-ordl.company = job.company AND 
                    oe-ordl.job-no = job.job-no AND 
                    oe-ordl.job-no2 = job.job-no2 use-index job,
                    FIRST oe-ord NO-LOCK of oe-ordl,
                    FIRST cust NO-LOCK OF oe-ord:
    assign
        fiJob:screen-value = string(job.job-no).
    assign
        chWorkSheet:Range("A" + STRING(iRow)):VALUE = job.job-no
        chWorkSheet:Range("B" + STRING(iRow)):VALUE = job.job-no2
        chWorkSheet:Range("C" + STRING(iRow)):VALUE = est.est-no
        chWorkSheet:Range("D" + STRING(iRow)):VALUE = job-hdr.ord-no
        .
    assign
        chWorkSheet:Range("E" + STRING(iRow)):VALUE = if avail oe-ord then oe-ord.cust-no else ""
        chWorkSheet:Range("F" + STRING(iRow)):VALUE = if avail oe-ord then oe-ord.cust-name else ""
        chWorkSheet:Range("G" + STRING(iRow)):VALUE = if avail oe-ord then oe-ord.bill-to else ""
        chWorkSheet:Range("H" + STRING(iRow)):VALUE = if avail oe-ord then oe-ord.sold-no else 0
        chWorkSheet:Range("I" + STRING(iRow)):VALUE = if avail oe-ord then oe-ord.sold-name else ""
        chWorkSheet:Range("J" + STRING(iRow)):VALUE = if avail oe-ord then string(oe-ord.ord-date) else ""
        chWorkSheet:Range("K" + STRING(iRow)):VALUE = if avail oe-ord then string(oe-ord.due-date) else "".
    ASSIGN
        chWorkSheet:Range("L" + STRING(iRow)):VALUE = if avail oe-ord then string(oe-ord.user-id) else ""
        chWorkSheet:Range("M" + STRING(iRow)):VALUE = if avail oe-ord then string(oe-ord.sname) else ""
        chWorkSheet:Range("N" + STRING(iRow)):VALUE = job-hdr.i-no
        chWorkSheet:Range("O" + STRING(iRow)):VALUE = if avail itemfg then itemfg.procat else ""
        chWorkSheet:Range("P" + STRING(iRow)):VALUE = if avail oe-ordl then oe-ordl.part-no else ""
        chWorkSheet:Range("Q" + STRING(iRow)):VALUE = job.create-date
        chWorkSheet:Range("R" + STRING(iRow)):VALUE = job.due-date
        chWorkSheet:Range("S" + STRING(iRow)):VALUE = job.est-no
        chWorkSheet:Range("T" + STRING(iRow)):VALUE = if job.opened then "" else "NO"
        .
    assign    
        chWorkSheet:Range("U" + STRING(iRow)):VALUE = job-hdr.std-mat-cost
        chWorkSheet:Range("V" + STRING(iRow)):VALUE = job-hdr.std-lab-cost
        chWorkSheet:Range("W" + STRING(iRow)):VALUE = job-hdr.std-fix-cost
        chWorkSheet:Range("X" + STRING(iRow)):VALUE = job-hdr.std-var-cost
        chWorkSheet:Range("Y" + STRING(iRow)):VALUE = job-hdr.std-tot-cost
        chWorkSheet:Range("Z" + STRING(iRow)):VALUE = job-hdr.avg-cost.
    assign
        chWorkSheet:Range("AA" + STRING(iRow)):VALUE = job-hdr.qty
        chWorkSheet:Range("AB" + STRING(iRow)):VALUE = est.est-qty
        chWorkSheet:Range("AC" + STRING(iRow)):VALUE = probe.mat-cost
        chWorkSheet:Range("AD" + STRING(iRow)):VALUE = probe.lab-cost
        chWorkSheet:Range("AE" + STRING(iRow)):VALUE = probe.brd-cost
        chWorkSheet:Range("AF" + STRING(iRow)):VALUE = probe.fo-cost
        chWorkSheet:Range("AG" + STRING(iRow)):VALUE = probe.vo-cost
        chWorkSheet:Range("AH" + STRING(iRow)):VALUE = probe.setup.
    assign
        chWorkSheet:Range("AI" + STRING(iRow)):VALUE = probe.comm
        chWorkSheet:Range("AJ" + STRING(iRow)):VALUE = probe.fact-cost
        chWorkSheet:Range("AK" + STRING(iRow)):VALUE = probe.full-cost
        chWorkSheet:Range("AL" + STRING(iRow)):VALUE = probe.sell-price
        chWorkSheet:Range("AM" + STRING(iRow)):VALUE = probe.gross-profit
        chWorkSheet:Range("AN" + STRING(iRow)):VALUE = probe.net-profit
        chWorkSheet:Range("AO" + STRING(iRow)):VALUE = probe.prof-on
        chWorkSheet:Range("AP" + STRING(iRow)):VALUE = item.i-no
        chWorkSheet:Range("AQ" + STRING(iRow)):VALUE = item.s-len
        chWorkSheet:Range("AR" + STRING(iRow)):VALUE = item.s-wid
        
        iRow = iRow + 1.

end.

assign
    chExcelApplication:VISIBLE = true
    chExcelApplication:ScreenUpdating = true.
chWorkbook:WorkSheets(1):Columns("A:AQ"):AutoFit.
chExcelApplication:VISIBLE = TRUE.

RELEASE OBJECT chWorkbook         NO-ERROR.
RELEASE OBJECT chWorkSheet        NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

