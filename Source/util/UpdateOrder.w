&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/UpdateOrder.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:  Sewa Singh

  Created: 10  Nov, 2021

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
DEFINE VARIABLE cLocation     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cocode        AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
     
RUN spGetSessionParam (
    INPUT "Company", 
    OUTPUT cocode
    ).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btSimulatePurge btExit RECT-15 RECT-16 ~
fiBeginingCustomer fiEndingCustomer fiBeginingOrder fiEndingOrder ~
begin_rel-date end_rel-date btStartProcess tbRelease fiCurrentStatus ~
fiChangeStatus fiUpdateRelDate fiDirectory tbOpenFile tbReleaseDate ~
btnCalendar-1
&Scoped-Define DISPLAYED-OBJECTS fiBeginingCustomer fiEndingCustomer ~
fiBeginingOrder fiEndingOrder begin_rel-date end_rel-date tbRelease ~
fiCurrentStatus fiChangeStatus fiUpdateRelDate fiDirectory tbOpenFile ~
tbReleaseDate

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pfSetDirectory C-Win 
FUNCTION pfSetDirectory RETURNS LOGICAL PRIVATE
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".
     
DEFINE BUTTON btExit 
    IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Button 1" 
    SIZE 7.2 BY 1.71.

DEFINE BUTTON btSimulatePurge 
    IMAGE-UP FILE "Graphics/32x32/simulate.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Simulate" 
    SIZE 19 BY 1.14
    BGCOLOR 14 .

DEFINE BUTTON btStartProcess 
    IMAGE-UP FILE "Graphics/32x32/execute.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Start Process" 
    SIZE 16 BY 1.14.

DEFINE VARIABLE begin_rel-date     AS DATE      FORMAT "99/99/9999":U INITIAL TODAY 
    LABEL "Beginning Release Date" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_rel-date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Release Date" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeginingCustomer AS CHARACTER FORMAT "X(10)":U 
    LABEL "Begining Customer" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiBeginingOrder    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Begining Order" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiChangeStatus     AS CHARACTER FORMAT "X(19)":U 
    LABEL "Change Status" 
    VIEW-AS COMBO-BOX INNER-LINES 13 
    LIST-ITEM-PAIRS "","",
    "A- Approved", "A",
    "C- Closed", "C",
    "D- Deleted","D",
    "F- Fill", "F",
    "H- Hold", "H",  
    "I- Invoiced", "I",
    "N- New","N",
    "O- Original Invoice", "O",
    "P- Partial", "P",
    "R- Release", "R",
    "S- Ship Only", "S",
    "U- Updated" , "U"                     
    DROP-DOWN-LIST 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiCurrentStatus    AS CHARACTER FORMAT "X(19)":U 
    LABEL "Current Status" 
    VIEW-AS COMBO-BOX INNER-LINES 13 
    LIST-ITEM-PAIRS "","",
    "A- Approved", "A",
    "C- Closed", "C",
    "D- Deleted","D",
    "F- Fill", "F",
    "H- Hold", "H",  
    "I- Invoiced", "I",
    "N- New","N",
    "O- Original Invoice", "O",
    "P- Partial", "P",
    "R- Release", "R",
    "S- Ship Only", "S",
    "U- Updated" , "U"                       
    DROP-DOWN-LIST 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiDirectory        AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN NATIVE 
    SIZE 64.8 BY 1
    FONT 22 NO-UNDO.

DEFINE VARIABLE fiEndingCustomer   AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Customer" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiEndingOrder      AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 
    LABEL "Ending Order" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiUpdateRelDate    AS DATE      FORMAT "99/99/9999":U INITIAL ? 
    LABEL "Release Date" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE RECTANGLE RECT-15
    EDGE-PIXELS 2 GRAPHIC-EDGE    
    SIZE 96 BY 1.91
    BGCOLOR 21 FGCOLOR 21 .

DEFINE RECTANGLE RECT-16
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 93 BY 12.33.

DEFINE VARIABLE tbOpenFile    AS LOGICAL INITIAL NO 
    LABEL "Open File" 
    VIEW-AS TOGGLE-BOX
    SIZE 14.4 BY .81
    FONT 22 NO-UNDO.

DEFINE VARIABLE tbRelease     AS LOGICAL INITIAL NO 
    LABEL "Release All Items" 
    VIEW-AS TOGGLE-BOX
    SIZE 23.6 BY .81
    FONT 22 NO-UNDO.
 
DEFINE VARIABLE tbReleaseDate AS LOGICAL INITIAL NO 
    LABEL "Update Release Date" 
    VIEW-AS TOGGLE-BOX
    SIZE 27.6 BY .81
    FONT 22 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    btSimulatePurge AT ROW 15.81 COL 22.4 WIDGET-ID 10
    btExit AT ROW 1.14 COL 88.8 WIDGET-ID 24
    fiBeginingCustomer AT ROW 3.52 COL 28.4 COLON-ALIGNED WIDGET-ID 2
    fiEndingCustomer AT ROW 3.52 COL 71.4 COLON-ALIGNED WIDGET-ID 4
    fiBeginingOrder AT ROW 5.05 COL 28.4 COLON-ALIGNED WIDGET-ID 6
    fiEndingOrder AT ROW 5.05 COL 71.4 COLON-ALIGNED WIDGET-ID 8
    begin_rel-date AT ROW 6.48 COL 28.4 COLON-ALIGNED WIDGET-ID 28
    end_rel-date AT ROW 6.48 COL 71.4 COLON-ALIGNED HELP
    "Enter Ending Due Date" WIDGET-ID 14
    btStartProcess AT ROW 15.81 COL 49.6 WIDGET-ID 12             
    fiCurrentStatus AT ROW 8.00  COL 28.4 COLON-ALIGNED WIDGET-ID 32
    tbRelease AT ROW 9.52 COL 30.4 WIDGET-ID 30
    fiChangeStatus AT ROW 9.52 COL 71.4 COLON-ALIGNED WIDGET-ID 34
    tbReleaseDate  AT ROW 10.95 COL 30.4       
    fiUpdateRelDate AT ROW 10.95 COL 71.4 COLON-ALIGNED WIDGET-ID 36
    btnCalendar-1 AT ROW 10.95 COL 88.2
    fiDirectory AT ROW 13.86 COL 3 COLON-ALIGNED NO-LABELS WIDGET-ID 18
    tbOpenFile AT ROW 13.95 COL 70.4 WIDGET-ID 26
    "Records to view  will be stored in directory:" VIEW-AS TEXT
    SIZE 50.6 BY .62 AT ROW 12.95 COL 11.4 WIDGET-ID 16
    FONT 22
    RECT-15 AT ROW 1 COL 1 WIDGET-ID 20
    RECT-16 AT ROW 2.95 COL 3 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 16.38
    BGCOLOR 15  WIDGET-ID 100.


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
        TITLE              = "Update Order"
        HEIGHT             = 16.43
        WIDTH              = 97.2
        MAX-HEIGHT         = 33.57
        MAX-WIDTH          = 273.2
        VIRTUAL-HEIGHT     = 33.57
        VIRTUAL-WIDTH      = 273.2
        MAX-BUTTON         = NO
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
    begin_rel-date:PRIVATE-DATA IN FRAME DEFAULT-FRAME = "parm".

ASSIGN 
    end_rel-date:PRIVATE-DATA IN FRAME DEFAULT-FRAME = "parm".

ASSIGN 
    fiDirectory:READ-ONLY IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Recalculate Open Order Price */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Recalculate Open Order Price */
    DO:
        /* IF VALID-HANDLE(hdOutputProcs) THEN 
             DELETE PROCEDURE hdOutputProcs.    */
          
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON HELP OF FRAME DEFAULT-FRAME
    DO:
        DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.
    
        CASE FOCUS:NAME :
            WHEN "fiBeginingCustomer" OR 
            WHEN "fiEndingCustomer"  THEN 
                DO:
                    RUN system/openLookup.p (
                        INPUT  cocode, 
                        INPUT  "",  /* Lookup ID */
                        INPUT  23,  /* Subject ID */
                        INPUT  "",  /* User ID */
                        INPUT  0,   /* Param Value ID */
                        OUTPUT cFieldsValue, 
                        OUTPUT cFoundValue, 
                        OUTPUT recFoundRecID
                        ).   
                    IF cFoundValue <> "" THEN 
                        ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
                END.
            WHEN "fiBeginingOrder" OR 
            WHEN "fiEndingOrder"  THEN    
                DO:
                    RUN system/openLookup.p (
                        INPUT  cocode, 
                        INPUT  "",  /* Lookup ID */
                        INPUT  27,  /* Subject ID */
                        INPUT  "",  /* User ID */
                        INPUT  0,   /* Param Value ID */
                        OUTPUT cFieldsValue, 
                        OUTPUT cFoundValue, 
                        OUTPUT recFoundRecID
                        ).   
                    IF cFoundValue <> "" THEN 
                        ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
                END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rel-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rel-date C-Win
ON LEAVE OF begin_rel-date IN FRAME DEFAULT-FRAME /* Beginning Release Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Button 1 */
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulatePurge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulatePurge C-Win
ON CHOOSE OF btSimulatePurge IN FRAME DEFAULT-FRAME /* Simulate */
    DO:
        RUN pRunProcess(
            INPUT NO
            ).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btStartProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btStartProcess C-Win
ON CHOOSE OF btStartProcess IN FRAME DEFAULT-FRAME /* Start Process */
    DO:
        RUN pRunProcess(
            INPUT YES
            ).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rel-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rel-date C-Win
ON LEAVE OF end_rel-date IN FRAME DEFAULT-FRAME /* Ending Release Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tbReleaseDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbReleaseDate C-Win
ON VALUE-CHANGED OF tbReleaseDate IN FRAME DEFAULT-FRAME /* Ending Release Date */
    DO:
        ASSIGN {&self-name}.
        IF tbReleaseDate EQ YES THEN
            ASSIGN
                fiUpdateRelDate:HIDDEN IN FRAME DEFAULT-FRAME = NO
                btnCalendar-1:HIDDEN IN FRAME DEFAULT-FRAME   = NO. 
        ELSE 
            ASSIGN
                fiUpdateRelDate:HIDDEN IN FRAME DEFAULT-FRAME = YES
                btnCalendar-1:HIDDEN IN FRAME DEFAULT-FRAME   = YES.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fiUpdateRelDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUpdateRelDate C-Win
ON HELP OF fiUpdateRelDate IN FRAME DEFAULT-FRAME /* Release Date */
    DO:    
        {methods/calendar.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME DEFAULT-FRAME
    DO:
        {methods/btnCalendar.i fiUpdateRelDate}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
    {sys/inc/f3helpw.i}     
    RUN enable_UI.
    pfSetDirectory().
    fiUpdateRelDate:HIDDEN IN FRAME DEFAULT-FRAME = YES.
    btnCalendar-1:HIDDEN IN FRAME DEFAULT-FRAME = YES.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
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
    DISPLAY fiBeginingCustomer fiEndingCustomer fiBeginingOrder fiEndingOrder 
        begin_rel-date end_rel-date tbRelease fiCurrentStatus fiChangeStatus 
        fiUpdateRelDate fiDirectory tbOpenFile tbReleaseDate
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    ENABLE btSimulatePurge btExit RECT-15 RECT-16 fiBeginingCustomer 
        fiEndingCustomer fiBeginingOrder fiEndingOrder begin_rel-date 
        end_rel-date btStartProcess tbRelease fiCurrentStatus fiChangeStatus 
        fiUpdateRelDate fiDirectory tbOpenFile tbReleaseDate btnCalendar-1
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplExpire AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    
        
    MESSAGE "Do you want to start the process with the selected parameters?"
        VIEW-AS ALERT-BOX QUESTION
        BUTTON YES-NO
        UPDATE lResponse AS LOGICAL.
    
    IF NOT lResponse THEN 
        RETURN.
    
    SESSION:SET-WAIT-STATE("General"). 
    STATUS DEFAULT "Processing...".  
    
    IF NOT iplExpire THEN    
        RUN FileSys_CreateDirectory(
            INPUT  cLocation,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ). 
    DO WITH FRAME {&FRAME-NAME}:
        RUN util/UpdateOrderProc.p( 
            INPUT cocode,
            INPUT fiBeginingCustomer:SCREEN-VALUE, 
            INPUT fiEndingCustomer:SCREEN-VALUE, 
            INPUT DATE(begin_rel-date:SCREEN-VALUE), 
            INPUT DATE(end_rel-date:SCREEN-VALUE), 
            INPUT INTEGER(fiBeginingOrder:SCREEN-VALUE), 
            INPUT INTEGER(fiEndingOrder:SCREEN-VALUE),
            INPUT LOGICAL(tbReleaseDate:SCREEN-VALUE),
            INPUT DATE(fiUpdateRelDate:SCREEN-VALUE),
            INPUT LOGICAL(tbRelease:SCREEN-VALUE),
            INPUT fiCurrentStatus:SCREEN-VALUE,
            INPUT fiChangeStatus:SCREEN-VALUE,
            INPUT iplExpire, 
            INPUT cLocation + "\OrderChanges.csv").                 
    END.    
    
    IF NOT iplExpire THEN 
    DO:         
        MESSAGE "Simulation Completed." SKIP
            "Check " + TRIM(cLocation) + " directory for the CSV file."
            VIEW-AS ALERT-BOX INFORMATION.   
        IF tbOpenFile:CHECKED THEN 
            RUN OS_RunFile(
                INPUT cLocation + "\OrderChanges.csv",
                OUTPUT lSuccess,
                OUTPUT cMessage).        
    END.   
    ELSE 
        MESSAGE "Process Completed"
            VIEW-AS ALERT-BOX INFORMATION.  
    
    pfSetDirectory().  
         
    SESSION:SET-WAIT-STATE("").   
    STATUS DEFAULT "".                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pfSetDirectory C-Win 
FUNCTION pfSetDirectory RETURNS LOGICAL PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN FileSys_GetTempDirectory(
        OUTPUT cLocation
        ).
    cLocation = cLocation + "\OrderChanges-" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")+ STRING(DAY(TODAY),"99") + "-" + STRING(TIME,"99999").  
    DO WITH FRAME {&FRAME-NAME}:
        fiDirectory:SCREEN-VALUE = cLocation.
    END.   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

