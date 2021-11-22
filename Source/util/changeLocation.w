&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\changeLocation.w

------------------------------------------------------------------------*/
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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.

DEFINE TEMP-TABLE tt_loc    
    FIELD existLoc AS CHARACTER
    FIELD newLoc AS CHARACTER
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_loc end_loc tb_import ~
fi_file btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_loc end_loc tb_import fi_file 

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

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Old Location#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc AS CHARACTER FORMAT "x(5)":U 
     LABEL "New Location#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\Location.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 9.52.

DEFINE VARIABLE tb_import AS LOGICAL INITIAL no 
     LABEL "Import CSV Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_loc AT ROW 7.14 COL 37 COLON-ALIGNED
     end_loc AT ROW 9.19 COL 37 COLON-ALIGNED
     tb_import AT ROW 11.19 COL 24.4 WIDGET-ID 2
     fi_file AT ROW 12.76 COL 22 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 4
     btn-process AT ROW 15.76 COL 21
     btn-cancel AT ROW 15.76 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
     "" VIEW-AS TEXT
          SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 1 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.95 COL 1
          BGCOLOR 11 
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 2.91 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 3.76 COL 1
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Update Location"
         HEIGHT             = 17.71
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
   FRAME-NAME                                                           */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Customer Number */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Customer Number */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc C-Win
ON LEAVE OF begin_loc IN FRAME FRAME-A /* Old Customer# */
DO:
  assign {&self-name}.

  {&self-name}:screen-value = caps({&self-name}).
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
  DEFINE VARIABLE lProcessItem AS LOGICAL NO-UNDO. 
  IF NOT tb_import THEN
  DO:    
      if not can-find(first loc
                      where loc.company eq cocode
                        and loc.loc eq begin_loc) then do:
        message "You must enter a valid Location number" view-as alert-box error.
        apply "entry" to begin_loc.
        return no-apply.
      end.

      IF begin_loc EQ end_loc THEN
      DO:
        MESSAGE "Old and New Location #s are the same.  Cannot Process."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        apply "entry" to end_loc.
        return no-apply.
      END.

      if can-find(first loc
                  where loc.company eq cocode
                    and loc.loc eq end_loc) then do:
        v-process = no.

        message "The new Location# already exists, merge old Location# into new Location#?"
                view-as alert-box question button yes-no update v-process.

        if not v-process then return no-apply.
      end.

      v-process  = no.

      message "Are you sure you want change Location" trim(caps(begin_loc))
              "to" trim(caps(end_loc)) + "?"       
              view-as alert-box question button yes-no update v-process.

      if v-process then run run-process(NO).
  END.
  ELSE DO:
        EMPTY TEMP-TABLE tt_loc.
        INPUT FROM VALUE(fi_file).
        REPEAT:
            CREATE tt_loc.
            IMPORT DELIMITER "," tt_loc.
        END.
        INPUT CLOSE.  
        
        FOR EACH tt_loc:
         IF tt_loc.newLoc EQ "" THEN DELETE tt_loc. 
        END.
        
        FOR EACH tt_loc NO-LOCK:
                            
              if not can-find(first loc
                      where loc.company eq cocode
                        and loc.loc eq tt_loc.existLoc) then do:
                message "Location '" + tt_loc.existLoc + "' is not valid in csv file.."   view-as alert-box error.
                apply "entry" to fi_file.
                return no-apply.
              end.

              IF tt_loc.existLoc EQ tt_loc.newLoc THEN
              DO:
                MESSAGE "Old and New Location '" + tt_loc.existLoc + "' are the same.  Cannot Process."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                apply "entry" to fi_file.
                return no-apply.
              END.   
            
              lProcessItem = YES.          
           
        END.  /* for each tt_cust*/
        
        IF lProcessItem THEN
        DO:
           v-process  = no. 
              message "Are you sure you want change location"       
                      view-as alert-box question button yes-no update v-process.
            
        END.
        IF v-process THEN
        DO:
         FOR EACH tt_loc NO-LOCK :            
            begin_loc = tt_loc.existLoc .
            end_loc = tt_loc.newLoc .
            
            run run-process(YES).         
         END.
            
         begin_loc = "".
         end_loc = "".
         MESSAGE trim(c-win:title) + " Process Complete..." view-as alert-box.
         EMPTY TEMP-TABLE tt_loc.
         apply "close" to this-procedure.
        END.
        
        
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc C-Win
ON LEAVE OF end_loc IN FRAME FRAME-A /* New Customer# */
DO:
  assign {&self-name}.

  {&self-name}:screen-value = caps({&self-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Font */
DO:
     DEF VAR okClicked AS LOGICAL NO-UNDO.
     SYSTEM-DIALOG GET-FILE fi_file 
                TITLE 'Select Image File to insert'
                FILTERS 'CSV Files    (*.csv)' '*.csv'
                INITIAL-DIR fi_file
                MUST-EXIST USE-FILENAME UPDATE okClicked.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_import
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_import C-Win
ON VALUE-CHANGED OF tb_import IN FRAME FRAME-A /* Import CSV Files */
DO:
     assign {&self-name}.
     IF logical(tb_import:SCREEN-VALUE) EQ YES THEN
     DO:
         end_loc:SENSITIVE = NO.
         begin_loc:SENSITIVE = NO.
         fi_file:SENSITIVE = YES.
     END.
     ELSE 
     ASSIGN
       end_loc:SENSITIVE = YES
       begin_loc:SENSITIVE = YES
       fi_file:SENSITIVE = NO.
     
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
    IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  RUN enable_UI.
  fi_file:SENSITIVE = NO.
  apply "entry" to begin_loc.
  {methods/nowait.i}
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
  DISPLAY begin_loc end_loc tb_import fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_loc end_loc tb_import fi_file btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------ util/updcust#.p 05/01 JLF */
/*  Update Customer with a new cust-no                                        */
/* -------------------------------------------------------------------------- */
DEFINE INPUT PARAMETER ipclRunMulti AS LOGICAL NO-UNDO.   
def buffer b-cust for cust.
def buffer b-ship for shipto.
def buffer b-sold for soldto.
DEF BUFFER b-sold2 FOR soldto.
DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.

def var cOldLoc      AS CHARACTER NO-UNDO.
def VAR cNewLoc  AS CHARACTER NO-UNDO.
def var v-char      as   char.
DEF VAR old-rec-key AS CHAR NO-UNDO.

DEF BUFFER bf-cust FOR cust.  
DEF BUFFER bf-cust-new FOR cust. 

assign
 cOldLoc     = begin_loc
 cNewLoc     = caps(end_loc).

session:set-wait-state("General").

RUN  util/ChangeLocationProc.p(INPUT cocode, INPUT cOldLoc, INPUT cNewLoc).

session:set-wait-state("").
IF ipclRunMulti EQ NO THEN
DO:
    message trim(c-win:title) + " Process Complete..." view-as alert-box.

    apply "close" to this-procedure.
END.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

