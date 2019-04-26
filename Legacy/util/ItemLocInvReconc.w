&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
DEF VAR tONH AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.
DEF VAR tONO AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.
DEF VAR tALL AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.
DEF VAR tAVL AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.
DEF VAR tCLC AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.

DEF VAR tBAK AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.
DEF VAR tCOM AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.
DEF VAR tORD AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.
DEF VAR tPRD AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.
DEF VAR tREL AS DECIMAL FORMAT "->>,>>>,>>9.999" NO-UNDO.

DEF VAR chExcelApplication  AS COM-HANDLE   NO-UNDO.
DEF VAR chWorkbook          AS COM-HANDLE   NO-UNDO.
DEF VAR chWorksheet         AS COM-HANDLE   NO-UNDO.
DEF VAR iLineNo AS INT.
DEF VAR iMaxLines AS INT INITIAL 1.
DEF VAR iCtr AS INT.

DEF TEMP-TABLE ttLocTots
    FIELD company AS CHAR 
    FIELD i-no AS CHAR 
    FIELD loc AS CHAR 
    FIELD ttONH AS DEC 
    FIELD ttONO AS DEC 
    FIELD ttALL AS DEC 
    FIELD ttAVL AS DEC 
    FIELD ttCLC AS DEC 
    FIELD ttBAK AS DEC 
    FIELD ttCOM AS DEC 
    FIELD ttORD AS DEC 
    FIELD ttPRD AS DEC 
    FIELD ttREL AS DEC. 
    
    

def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fromCompany ToCompany FromItem ToItem ~
rsWhatToCheck btn-process btn-cancel eDesc 
&Scoped-Define DISPLAYED-OBJECTS fromCompany ToCompany FromItem ToItem ~
rsWhatToCheck eDesc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process AUTO-GO 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE eDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 74 BY 3.57 NO-UNDO.

DEFINE VARIABLE fromCompany AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Company Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FromItem AS CHARACTER FORMAT "x(32)":U 
     LABEL "From FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE ToCompany AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzz" 
     LABEL "To Company Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ToItem AS CHARACTER FORMAT "x(32)":U INITIAL "zzzzzzzzzzzzzzzzzzzzz" 
     LABEL "To FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE rsWhatToCheck AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All Items", "A",
"Discrepancies Only", "D"
     SIZE 38 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fromCompany AT ROW 2.67 COL 26 COLON-ALIGNED
     ToCompany AT ROW 3.86 COL 26 COLON-ALIGNED
     FromItem AT ROW 5.76 COL 26 COLON-ALIGNED
     ToItem AT ROW 6.95 COL 26 COLON-ALIGNED
     rsWhatToCheck AT ROW 8.62 COL 28 NO-LABEL
     btn-process AT ROW 10.05 COL 28
     btn-cancel AT ROW 10.05 COL 51
     eDesc AT ROW 12.19 COL 9 NO-LABEL
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.48 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 15.48
         DEFAULT-BUTTON btn-process.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "FG Item Whse Inventory Discrepancies"
         HEIGHT             = 15.67
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       eDesc:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* FG Item Whse Inventory Discrepancies */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FG Item Whse Inventory Discrepancies */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  v-process  = yes.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to generate this report?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fromCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fromCompany C-Win
ON LEAVE OF fromCompany IN FRAME FRAME-A /* From Company Code */
OR LEAVE OF fromItem
OR LEAVE OF toCompany
OR LEAVE OF toItem
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  
  ASSIGN 
    eDesc:SCREEN-VALUE IN FRAME frame-a =
    "This utility will generate an Excel spreadsheet comparing FG Item record quantities with the total quantities by Warehouse. " +
    "This report may take a significant amount of time to run, depending on the number of companies and items you choose to reconcile.".

  WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fromCompany ToCompany FromItem ToItem rsWhatToCheck eDesc 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE fromCompany ToCompany FromItem ToItem rsWhatToCheck btn-process 
         btn-cancel eDesc 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCloseExcel C-Win 
PROCEDURE pCloseExcel :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    STATUS DEFAULT "Finishing Data and Opening".
    
    chWorksheet:range("F1:O" + STRING(iLineNo + 1)):HorizontalAlignment = 4.
    chWorksheet:range("A1:E" + STRING(iLineNo + 1)):HorizontalAlignment = 2.
    chWorksheet:range("F2:O" + STRING(iLineNo + 1)):NumberFormat = "#,###,###,##0.00#".
    chWorkSheet:COLUMNS("A:O"):AutoFit.
    chExcelApplication:VISIBLE = TRUE.

    RELEASE OBJECT chWorksheet        NO-ERROR.
    RELEASE OBJECT chWorkBook         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateExcel C-Win 
PROCEDURE pCreateExcel :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
STATUS DEFAULT "Creating Excel Application".

CREATE "Excel.Application" chExcelApplication.

/* Set pages and label rows */                    
ASSIGN 
    iLineNo = 1
    chWorkbook = chExcelApplication:Workbooks:Add()
    chWorksheet = chExcelApplication:Sheets:Item(1)
    chWorksheet:range("A1"):VALUE = "Company"
    chWorksheet:range("B1"):VALUE = "Item No"
    chWorksheet:range("C1"):VALUE = "Name"
    chWorksheet:range("D1"):VALUE = "Whse"
    chWorksheet:range("E1"):VALUE = "Bin"
    chWorksheet:range("F1"):VALUE = "On Hand"
    chWorksheet:range("G1"):VALUE = "On Order"
    chWorksheet:range("H1"):VALUE = "Allocated"
    chWorksheet:range("I1"):VALUE = "Available"
    chWorksheet:range("J1"):VALUE = "Calc.Avail"
    chWorksheet:range("K1"):VALUE = "Backordered"
    chWorksheet:range("L1"):VALUE = "Committed"
    chWorksheet:range("M1"):VALUE = "Ordered"
    chWorksheet:range("N1"):VALUE = "Produced"
    chWorksheet:range("O1"):VALUE = "Released"
    chWorksheet:range("A1" + ":O1"):Font:Bold = TRUE
    chWorksheet:NAME = "ItemLocInvRec"
    chWorksheet:PageSetup:LeftHeader = "Item Location Inventory Reconciliation" + chr(10) + "UserId: " + 
                            entry(1,userid(LDBNAME(1)),"@") + " | DB: " + DBNAME.
    chWorksheet:PageSetup:LeftFooter = "(c) 1985-2019, Advantzware, Inc.".                    
    chWorksheet:PageSetup:RightHeader = "Company Range: " + fromCompany:SCREEN-VALUE IN FRAME frame-a + "-" + toCompany:SCREEN-VALUE + CHR(10) +
                                        "Item Range: " + fromItem:SCREEN-VALUE + "-" + toItem:SCREEN-VALUE.
    chWorksheet:PageSetup:FirstPageNumber = 1.
    chWorksheet:PageSetup:RightFooter = "Print Date: " +
                                            STRING(MONTH(TODAY),"99") + "/" +
                                            STRING(DAY(TODAY),"99") + "/" +
                                            STRING(YEAR(TODAY),"9999") + CHR(10) +
                                        "Page: &P".
    chWorksheet:PageSetup:PrintTitleRows = "A1:O1".
    chWorksheet:PageSetup:PrintGridlines = TRUE.
    chWorksheet:PageSetup:ORIENTATION = 2.
    chWorksheet:PageSetup:Zoom = FALSE.
    chWorksheet:PageSetup:FitToPagesWide = 1.
    chWorksheet:PageSetup:FitToPagesTall = 1000000.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExcelLine C-Win 
PROCEDURE pExcelLine :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER colA AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colB AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colC AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colD AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colE AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colF AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colG AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colH AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colI AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colJ AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colK AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colL AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colM AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colN AS CHAR NO-UNDO.
    DEF INPUT PARAMETER colO AS CHAR NO-UNDO.

    STATUS DEFAULT "Writing Data for Company " + company.company + ", Item: " + itemfg.i-no + ", Whse: " + colD.

    ASSIGN
        iLineNo = iLineNo + 1.
    
    ASSIGN
        chWorksheet:range("A" + STRING(iLineNo)):VALUE = colA
        chWorksheet:range("B" + STRING(iLineNo)):VALUE = colB 
        chWorksheet:range("C" + STRING(iLineNo)):VALUE = colC 
        chWorksheet:range("D" + STRING(iLineNo)):VALUE = colD 
        chWorksheet:range("E" + STRING(iLineNo)):VALUE = colE 
        chWorksheet:range("F" + STRING(iLineNo)):VALUE = colF 
        chWorksheet:range("G" + STRING(iLineNo)):VALUE = colG 
        chWorksheet:range("H" + STRING(iLineNo)):VALUE = colH 
        chWorksheet:range("I" + STRING(iLineNo)):VALUE = colI 
        chWorksheet:range("J" + STRING(iLineNo)):VALUE = colJ 
        chWorksheet:range("K" + STRING(iLineNo)):VALUE = colK 
        chWorksheet:range("L" + STRING(iLineNo)):VALUE = colL 
        chWorksheet:range("M" + STRING(iLineNo)):VALUE = colM 
        chWorksheet:range("N" + STRING(iLineNo)):VALUE = colN 
        chWorksheet:range("O" + STRING(iLineNo)):VALUE = colO 
        .
        
    IF colD EQ "Item Tot" THEN DO:
        IF DECIMAL(colF) NE itemfg.q-onh THEN ASSIGN 
            chWorksheet:range("F" + STRING(iLineNo)):Interior:ColorIndex = 36.
        IF DECIMAL(colG) NE itemfg.q-ono THEN ASSIGN 
            chWorksheet:range("G" + STRING(iLineNo)):Interior:ColorIndex = 36.
        IF DECIMAL(colH) NE itemfg.q-alloc THEN ASSIGN 
            chWorksheet:range("H" + STRING(iLineNo)):Interior:ColorIndex = 36.
        IF DECIMAL(colI) NE itemfg.q-avail THEN ASSIGN 
            chWorksheet:range("I" + STRING(iLineNo)):Interior:ColorIndex = 36.
        IF DECIMAL(colJ) NE (itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc) THEN ASSIGN 
            chWorksheet:range("J" + STRING(iLineNo)):Interior:ColorIndex = 36.
        IF DECIMAL(colK) NE itemfg.q-back THEN ASSIGN 
            chWorksheet:range("K" + STRING(iLineNo)):Interior:ColorIndex = 36.
        IF DECIMAL(colL) NE itemfg.q-comm THEN ASSIGN 
            chWorksheet:range("L" + STRING(iLineNo)):Interior:ColorIndex = 36.
        IF DECIMAL(colM) NE itemfg.q-ord THEN ASSIGN 
            chWorksheet:range("M" + STRING(iLineNo)):Interior:ColorIndex = 36.
        IF DECIMAL(colN) NE itemfg.q-prod THEN ASSIGN 
            chWorksheet:range("N" + STRING(iLineNo)):Interior:ColorIndex = 36.
        IF DECIMAL(colO) NE itemfg.q-rel THEN ASSIGN 
            chWorksheet:range("O" + STRING(iLineNo)):Interior:ColorIndex = 36.
        ASSIGN 
            iLineNo = iLineNo + 1.
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    DEF VAR lDisc AS LOG NO-UNDO.

    RUN pCreateExcel.
        
    FOR EACH company NO-LOCK where
        company.company GE fromCompany and
        company.company LE toCompany:
        
        FOR EACH itemfg NO-LOCK WHERE 
            itemfg.company EQ company.company AND 
            itemfg.i-no GE fromItem AND 
            itemfg.i-no LE toItem:
            
            EMPTY TEMP-TABLE ttLocTots.
            
            ASSIGN 
                tONH = 0
                tONO = 0
                tALL = 0
                tAVL = 0
                tCLC = 0
                tBAK = 0
                tCOM = 0
                tORD = 0
                tPRD = 0
                tREL = 0
                lDisc = FALSE.

            FOR EACH itemfg-loc NO-LOCK WHERE                         
                itemfg-loc.company EQ company.company AND
                itemfg-loc.i-no EQ itemfg.i-no:
                
                CREATE ttLocTots.
                ASSIGN 
                    ttLocTots.loc = itemfg-loc.loc
                    ttONH = itemfg-loc.q-onh
                    ttONO = itemfg-loc.q-ono
                    ttALL = itemfg-loc.q-alloc
                    ttAVL = itemfg-loc.q-avail
                    ttCLC = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
                    ttBAK = itemfg-loc.q-back
                    ttCOM = itemfg-loc.q-comm
                    ttORD = itemfg-loc.q-ord
                    ttPRD = itemfg-loc.q-prod
                    ttREL = itemfg-loc.q-rel
                    tONH = tONH + itemfg-loc.q-onh
                    tONO = tONO + itemfg-loc.q-ono
                    tALL = tALL + itemfg-loc.q-alloc
                    tAVL = tAVL + itemfg-loc.q-avail
                    tCLC = tCLC + itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
                    tBAK = tBAK + itemfg-loc.q-back
                    tCOM = tCOM + itemfg-loc.q-comm
                    tORD = tORD + itemfg-loc.q-ord
                    tPRD = tPRD + itemfg-loc.q-prod
                    tREL = tREL + itemfg-loc.q-rel.
            END.
            
            IF tONH NE itemfg.q-onh
            OR tONO NE itemfg.q-ono
            OR tALL NE itemfg.q-alloc
            OR tAVL NE itemfg.q-avail
            OR tBAK NE itemfg.q-back
            OR tCOM NE itemfg.q-comm
            OR tORD NE itemfg.q-ord
            OR tPRD NE itemfg.q-prod
            OR tREL NE itemfg.q-rel THEN ASSIGN 
                lDisc = TRUE.
            
            IF rsWhatToCheck:SCREEN-VALUE iN FRAME frame-a EQ "A" 
            OR lDisc EQ TRUE THEN DO:
            
                RUN pExcelLine (company.company,
                                itemfg.i-no,
                                itemfg.i-name,
                                "",
                                "",
                                itemfg.q-onh,
                                itemfg.q-ono,
                                itemfg.q-alloc,
                                itemfg.q-avail,
                                itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc,
                                itemfg.q-back,
                                itemfg.q-comm,
                                itemfg.q-ord,
                                itemfg.q-prod,
                                itemfg.q-rel).
                            
                
                FOR EACH ttLocTots:
                    RUN pExcelLine ("",
                                    "",
                                    "",
                                    ttLocTots.loc,
                                    "",
                                    ttLocTots.ttONH,
                                    ttLocTots.ttONO,
                                    ttLocTots.ttALL,
                                    ttLocTots.ttAVL,
                                    ttLocTots.ttCLC,
                                    ttLocTots.ttBAK,
                                    ttLocTots.ttCOM,
                                    ttLocTots.ttORD,
                                    ttLocTots.ttPRD,
                                    ttLocTots.ttREL).
                END. 
            
                RUN pExcelLine ("",
                                "",
                                "",
                                "ITEM TOT",
                                "",
                                tONH,
                                tONO,
                                tALL,
                                tAVL,
                                tCLC,
                                tBAK,
                                tCOM,
                                tORD,
                                tPRD,
                                tREL).
            END.
        END.
    END.        
    
    RUN pCloseExcel.
    
    STATUS DEFAULT "".
    SESSION:SET-WAIT-STATE("").
    APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

