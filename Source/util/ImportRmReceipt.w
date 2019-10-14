&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: util\ImportRmReceipt.w
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

DEFINE TEMP-TABLE tt-module NO-UNDO LIKE module.
DEFINE TEMP-TABLE tt-rm-rctd 
    FIELD company   AS CHARACTER 
    FIELD loc       AS CHARACTER
    FIELD loc-bin   AS CHARACTER
    FIELD tag       AS CHARACTER
    FIELD qty       AS CHARACTER
    FIELD cost      AS CHARACTER 
    FIELD rita-code AS CHARACTER 
    FIELD s-num     AS CHARACTER 
    FIELD b-num     AS CHARACTER 
    FIELD pass      AS CHARACTER 
    FIELD job-no    AS CHARACTER 
    FIELD job-no2   AS CHARACTER 
    FIELD po-no     AS CHARACTER 
    FIELD po-line   AS CHARACTER 
    FIELD i-no      AS CHARACTER 
    FIELD i-name    AS CHARACTER 
    FIELD pur-uom   AS CHARACTER 
    FIELD cost-uom  AS CHARACTER 
    FIELD rct-date  AS CHARACTER 
    FIELD diameter  AS CHARACTER 
    FIELD roll-lf   AS CHARACTER 
    FIELD roll-wt   AS CHARACTER 
    FIELD enteredBy AS CHARACTER 
    .                  
                       

DEFINE STREAM st-input.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 btBrowse fiFileName Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiFileName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btBrowse 
    LABEL "Find File" 
    SIZE 16 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "Cancel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "Import" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE VARIABLE fiFileName AS CHARACTER FORMAT "X(256)":U 
    LABEL "File to Import" 
    VIEW-AS FILL-IN 
    SIZE 57.4 BY 1
    BGCOLOR 8 FGCOLOR 0 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 96.4 BY 9.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    btBrowse AT ROW 4.05 COL 78.6 WIDGET-ID 4
    fiFileName AT ROW 4.14 COL 19.2 COLON-ALIGNED WIDGET-ID 2
    Btn_OK AT ROW 6.71 COL 24
    Btn_Cancel AT ROW 6.71 COL 61
    "This Procedure will import Warehouse Transaction Receipts" VIEW-AS TEXT
    SIZE 70 BY .95 AT ROW 1.48 COL 21
    "from the following file." VIEW-AS TEXT
    SIZE 42 BY .95 AT ROW 2.67 COL 21
    RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96.4 BY 9.48
    FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW wWin ASSIGN
        HIDDEN             = YES
        TITLE              = "Import Warehouse Transaction Receipts"
        HEIGHT             = 9.19
        WIDTH              = 97.4
        MAX-HEIGHT         = 28.81
        MAX-WIDTH          = 146.2
        VIRTUAL-HEIGHT     = 28.81
        VIRTUAL-WIDTH      = 146.2
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = NO
        BGCOLOR            = ?
        FGCOLOR            = ?
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
    THEN wWin:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Import Module Records */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Import Module Records */
    DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBrowse wWin
ON CHOOSE OF btBrowse IN FRAME fMain /* Find File */
    DO:
        APPLY 'Help' TO fiFileName.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel wWin
ON CHOOSE OF Btn_Cancel IN FRAME fMain /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* Import */
    DO:

        RUN processFile.

  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFileName wWin
ON HELP OF fiFileName IN FRAME fMain /* File to Import */
    DO:

        DEFINE VARIABLE ls-filename AS cha NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG NO-UNDO.
   
        SYSTEM-DIALOG GET-FILE ls-filename 
            TITLE "Select File to insert"
            FILTERS "Data Files    (*.csv)" "*.csv",
            "Text files (*.txt)" "*.txt"
            INITIAL-DIR "c:\tmp\"
            MUST-EXIST
            USE-FILENAME
            UPDATE ll-ok.
      
        IF ll-ok THEN SELF:screen-value = ls-filename.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
        THEN DELETE WIDGET wWin.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
    DISPLAY  fiFileName 
        WITH FRAME fMain IN WINDOW wWin.
    ENABLE RECT-1 btBrowse  fiFileName Btn_OK Btn_Cancel 
        WITH FRAME fMain IN WINDOW wWin.
    {&OPEN-BROWSERS-IN-QUERY-fMain}
    VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
    /*------------------------------------------------------------------------------
      Purpose:  Window-specific override of this procedure which destroys 
                its contents and itself.
        Notes:  
    ------------------------------------------------------------------------------*/

    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
    /*------------------------------------------------------------------------------
      Purpose:     Super Override
      Parameters:  
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
  

    RUN SUPER.

/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processFile wWin 
PROCEDURE processFile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cErrorList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRNo       AS INTEGER   NO-UNDO.
    EMPTY TEMP-TABLE tt-rm-rctd.

    cFile = fiFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT STREAM st-input FROM VALUE(cFile).
        REPEAT:
            CREATE tt-rm-rctd.
            IMPORT STREAM st-input DELIMITER ","
                tt-rm-rctd.company 
                tt-rm-rctd.loc
                tt-rm-rctd.loc-bin
                tt-rm-rctd.tag 
                tt-rm-rctd.qty 
                tt-rm-rctd.cost 
                tt-rm-rctd.rita-code
                tt-rm-rctd.s-num 
                tt-rm-rctd.b-num 
                tt-rm-rctd.pass 
                tt-rm-rctd.job-no 
                tt-rm-rctd.job-no2 
                tt-rm-rctd.po-no 
                tt-rm-rctd.po-line 
                tt-rm-rctd.i-no 
                tt-rm-rctd.i-name 
                tt-rm-rctd.pur-uom
                tt-rm-rctd.cost-uom
                tt-rm-rctd.rct-date 
                tt-rm-rctd.diameter
                tt-rm-rctd.roll-lf
                tt-rm-rctd.roll-wt 
                tt-rm-rctd.enteredBy  .
        
            i = i + 1.
            IF i EQ 1 THEN
                DELETE tt-rm-rctd .

            IF AVAILABLE tt-rm-rctd AND length(tt-rm-rctd.company) LE 2  THEN
                ASSIGN tt-rm-rctd.company = FILL("0",3 - length(TRIM(tt-rm-rctd.company))) + trim(tt-rm-rctd.company).
       
        END.
        OUTPUT STREAM st-input CLOSE.
    END.
      
      
    FOR EACH tt-rm-rctd:
        cErrorList = "".

        FIND FIRST ITEM 
            WHERE ITEM.company EQ tt-rm-rctd.company 
            AND item.i-no EQ tt-rm-rctd.i-no
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ITEM THEN 
        DO:
            cErrorList = cErrorList + tt-rm-rctd.i-no + ":Invalid Item Number. ".
            MESSAGE tt-rm-rctd.i-no + ": Invalid Item Number. Please import valid Item number ... " VIEW-AS ALERT-BOX INFORMATION.
            RETURN NO-APPLY .
        END.

        IF tt-rm-rctd.rita-code NE "R" THEN 
        DO:
            MESSAGE "Please enter receipt only(Rita-code = 'R') .."
                VIEW-AS ALERT-BOX ERROR .
            RETURN NO-APPLY .
        END.

        IF NOT CAN-FIND(FIRST job WHERE job.company = tt-rm-rctd.company 
            AND job.job-no = tt-rm-rctd.job-no)
            AND tt-rm-rctd.job-no GT "" THEN 
        DO:
            MESSAGE tt-rm-rctd.job-no + ": Invalid Job Number. Please import a valid Job#... " VIEW-AS ALERT-BOX INFORMATION.
            RETURN NO-APPLY .
        END.

        IF NOT CAN-FIND(FIRST po-ord WHERE po-ord.company = tt-rm-rctd.company 
            AND po-ord.po-no = integer(tt-rm-rctd.po-no))
            AND  integer(tt-rm-rctd.po-no) NE 0 THEN 
        DO:
            MESSAGE STRING(tt-rm-rctd.po-no) + ": Invalid Po Number. Please import a valid PO#... " VIEW-AS ALERT-BOX INFORMATION.
            RETURN NO-APPLY .
        END.

        FIND FIRST loc WHERE loc.company EQ tt-rm-rctd.company 
            AND loc.loc EQ tt-rm-rctd.loc
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE loc AND tt-rm-rctd.loc GT "" THEN 
        DO: 
            MESSAGE tt-rm-rctd.loc + ": Invalid Warehouse Number. Please import a valid Warehouse... " VIEW-AS ALERT-BOX INFORMATION.
            RETURN NO-APPLY .
      
        END.
    
      
        IF NOT CAN-FIND(FIRST rm-bin WHERE rm-bin.company = tt-rm-rctd.company  
            AND rm-bin.loc = tt-rm-rctd.loc 
            AND rm-bin.loc-bin = tt-rm-rctd.loc-bin)
            AND tt-rm-rctd.loc-bin GT "" THEN 
        DO:
            MESSAGE tt-rm-rctd.loc-bin + ": Invalid Warehouse Bin Number. Please import valid Bin ... " VIEW-AS ALERT-BOX INFORMATION.
            RETURN NO-APPLY .
        END.
    END.

    i = 0 .
    FOR EACH tt-rm-rctd.

        RUN sys/ref/asiseq.p (INPUT tt-rm-rctd.company, INPUT "rm_rcpt_seq", OUTPUT iRNo) NO-ERROR.
      
        CREATE rm-rctd.
        ASSIGN 
            rm-rctd.r-no = iRNo .

        ASSIGN
            rm-rctd.company   = tt-rm-rctd.company      
            rm-rctd.loc       = tt-rm-rctd.loc       
            rm-rctd.loc-bin   = tt-rm-rctd.loc-bin   
            rm-rctd.tag       = tt-rm-rctd.tag       
            rm-rctd.qty       = DECIMAL(tt-rm-rctd.qty)       
            rm-rctd.cost      = DECIMAL(tt-rm-rctd.cost)      
            rm-rctd.rita-code = tt-rm-rctd.rita-code 
            rm-rctd.s-num     = INTEGER(tt-rm-rctd.s-num)     
            rm-rctd.b-num     = INTEGER(tt-rm-rctd.b-num )    
            rm-rctd.pass      = INTEGER(tt-rm-rctd.pass)
            rm-rctd.job-no    = tt-rm-rctd.job-no    
            rm-rctd.job-no2   = INTEGER(tt-rm-rctd.job-no2)
            rm-rctd.po-no     = (tt-rm-rctd.po-no)     
            rm-rctd.po-line   = INTEGER(tt-rm-rctd.po-line)   
            rm-rctd.i-no      = tt-rm-rctd.i-no      
            rm-rctd.i-name    = tt-rm-rctd.i-name    
            rm-rctd.pur-uom   = tt-rm-rctd.pur-uom   
            rm-rctd.cost-uom  = tt-rm-rctd.cost-uom  
            rm-rctd.rct-date  = DATE(tt-rm-rctd.rct-date)  
            rm-rctd.diameter  = DECIMAL(tt-rm-rctd.diameter)
            rm-rctd.roll-lf   = DECIMAL(tt-rm-rctd.roll-lf)
            rm-rctd.roll-wt   = DECIMAL(tt-rm-rctd.roll-wt)
            rm-rctd.enteredBy = tt-rm-rctd.enteredBy .

        IF tt-rm-rctd.tag NE "" THEN
            ASSIGN rm-rctd.tag = STRING(CAPS(rm-rctd.i-no),"x(15)") + string(tt-rm-rctd.tag,"99999")

                i           = i + 1.

        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company = rm-rctd.company
            AND loadtag.item-type = YES
            AND loadtag.tag-no = tt-rm-rctd.tag  NO-ERROR.
      
        IF NOT AVAILABLE loadtag AND tt-rm-rctd.tag NE "" THEN 
        DO:
            CREATE loadtag.
            ASSIGN 
                loadtag.company   = tt-rm-rctd.company
                loadtag.tag-no    = rm-rctd.tag
                loadtag.item-type = YES /*item*/
                loadtag.po-no     = INTEGER(tt-rm-rctd.po-no) 
                loadtag.LINE      = INTEGER(tt-rm-rctd.po-line )
                loadtag.job-no    = tt-rm-rctd.job-no
                loadtag.job-no2   = INTEGER(tt-rm-rctd.job-no2)
                loadtag.i-no      = CAPS(tt-rm-rctd.i-no)
                loadtag.i-name    = tt-rm-rctd.i-name
                loadtag.qty       = rm-rctd.qty
                loadtag.loc       = tt-rm-rctd.loc
                loadtag.loc-bin   = tt-rm-rctd.loc-bin  .
      
        END.

    END.

    MESSAGE "Records: " + STRING(i) " Created Successfully." 
        VIEW-AS ALERT-BOX INFORMATION .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
