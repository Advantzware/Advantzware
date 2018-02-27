&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/glinvdif.w

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
{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

assign
 cocode = gcompany.

/* Variables */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

DEFINE STREAM excel.
excelheader  = "FG Item#,Vendor PO#,Job No,Job No2,TR Date,TR Code,Cust#,Warehouse,Bin,Quantity,Tag#" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS scr-date-from scr-date-to ~
btnOk BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS scr-date-from scr-date-to fi_count fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnOk AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE scr-date-from AS DATE FORMAT "99/99/9999":U 
     LABEL "Beginning TR Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE scr-date-to AS DATE FORMAT "99/99/9999":U 
     LABEL "Ending TR Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_count AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:\tmp\r-hisitm.csv" 
     LABEL "File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .



/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     scr-date-from AT ROW 3.57 COL 21.8 COLON-ALIGNED
     scr-date-to AT ROW 3.57 COL 58.2 COLON-ALIGNED
     fi_count AT ROW 7.00 COL 30.6 COLON-ALIGNED NO-LABEL
      "Status:" VIEW-AS TEXT
          SIZE 6.8 BY .62 AT ROW 7.20 COL 25 WIDGET-ID 10
      fi_file AT ROW 8.91 COL 25 COLON-ALIGNED
     btnOk AT ROW 10 COL 39.2
     BtnCancel AT ROW 10 COL 61.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98 BY 12.62.


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
         TITLE              = "Find history transactions of type 'S' without Bol# "
         HEIGHT             = 12.62
         WIDTH              = 98
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 105.8
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 105.8
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       BtnCancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       btnOk:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Find history transactions of type 'S' without Bol#  */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Find history transactions of type 'S' without Bol#  */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* OK */
DO:
   DEFINE  VARIABLE iCount AS INTEGER NO-UNDO.
   
   OUTPUT STREAM excel TO VALUE(fi_file).

   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.

   DO WITH FRAME {&FRAME-NAME}:

     ASSIGN {&displayed-objects}.
  
      SESSION:SET-WAIT-STATE ("general").
  FOR EACH itemfg NO-LOCK
      WHERE itemfg.company EQ cocode :
      
     FOR EACH fg-rcpth OF itemfg 
         WHERE fg-rcpth.rita-code EQ "S"
           AND fg-rcpth.trans-date GE scr-date-from 
           AND fg-rcpth.trans-date LE scr-date-to NO-LOCK, 
         EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no 
         AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-LOCK :

        FIND FIRST oe-bolh NO-LOCK
          WHERE oe-bolh.company EQ cocode
            AND oe-bolh.b-no    EQ fg-rcpth.b-no NO-ERROR .
        
        IF AVAIL oe-bolh THEN NEXT .

         PUT STREAM excel UNFORMATTED
             SKIP
             '"' fg-rcpth.i-no  '",'
             '"' fg-rcpth.po-no  '",'
             '"' fg-rcpth.job-no  '",'
             '"'  fg-rcpth.job-no2  '",'
             '"' fg-rcpth.trans-date  '",'
             '"' fg-rcpth.rita-code  '",'
             '"' fg-rdtlh.cust-no  '",'
             '"' fg-rdtlh.loc  '",'
             '"' fg-rdtlh.loc-bin  '",'
             '"' fg-rdtlh.qty  '",'
             '"' fg-rdtlh.tag  '",'.

        iCount = iCount + 1 .

     END.
  END.
     fi_count:SCREEN-VALUE     = string(iCount) + " Records..." .

      OUTPUT STREAM excel CLOSE.
  
  /*    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).*/

      SESSION:SET-WAIT-STATE ("").

     
   END.

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

   RUN enable_UI.

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
  DISPLAY scr-date-from scr-date-to fi_count fi_file
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE scr-date-from scr-date-to btnOk BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

