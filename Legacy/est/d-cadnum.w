&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&if defined(UIB_IS_Running) &then
  def var  op-artiocad# as cha no-undo.
&else
 def output param  op-artiocad# as cha no-undo.
&endif

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 vCadNum Ed_text BUTTON-2 Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS vCadNum Ed_text 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.17
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.17
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-2 
     LABEL "Get Design and Dimensions" 
     SIZE 40 BY 2.39.

DEFINE VARIABLE Ed_text AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 53 BY 20.94
     FONT 0 NO-UNDO.

DEFINE VARIABLE vCadNum AS CHARACTER FORMAT "X(256)":U 
     LABEL "Artios CAD#" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "adeicon/blank":U
     STRETCH-TO-FIT
     SIZE 139 BY 27.61.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     vCadNum AT ROW 1.72 COL 25 COLON-ALIGNED WIDGET-ID 2
     Ed_text AT ROW 2.89 COL 142 NO-LABEL WIDGET-ID 10
     BUTTON-2 AT ROW 31.72 COL 80 WIDGET-ID 6
     Btn_OK AT ROW 32.44 COL 18
     Btn_Cancel AT ROW 32.67 COL 137
     IMAGE-1 AT ROW 2.89 COL 2 WIDGET-ID 8
     SPACE(57.79) SKIP(4.25)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Artios CAD#"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 1.72
       COLUMN          = 120
       HEIGHT          = 2.17
       WIDTH           = 9
       WIDGET-ID       = 12
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {B642F377-218D-11D2-AFBC-0060976A10C1} type: CadX */
      CtrlFrame:MOVE-AFTER(vCadNum:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Artios CAD# */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    op-artiocad# = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    assign {&DISPLAYED-OBJECTS}.
    op-artiocad# = vCadNum.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 Dialog-Frame
ON CHOOSE OF BUTTON-2 IN FRAME Dialog-Frame /* Get Design and Dimensions */
DO:
 
   
   DEF VAR resultx AS INT NO-UNDO.
   def var vFilename as cha no-undo.
   def var ll-ok as log no-undo.
   def var vFilenameJPG as cha no-undo.
   
   
system-dialog get-file vFilename
      title "Select Cad image file to open"
      filters "ARD Files   (*.ARD)" "*.ARD"
      must-exist
      use-filename
      update ll-ok.
      
 if not ll-ok then return.
 
    /* resultx = chCtrlFrame:CadX:OpenDesign ("c:\esko\artios\artioscad7.40en\instlib\gm_lock1.ard",1).
    */
    resultx = chCtrlFrame:CadX:OpenDesign (vFilename,0).

    if resultx = 0 then do:
       vFilenameJPG = "c:\tmp\" + chCtrlFrame:CadX:ReturnTextCode4Param("#W$") + ".jpg".
       resultx = chCtrlFrame:CadX:SaveAsBitmap(1,vFilenameJPG, 600, 600, 20, , , , 100).
       image-1:load-image(vFilenameJPG).
    end.
   
    /*
     MESSAGE "open design: " vFilename resultx SKIP
         "Design: " chCtrlFrame:CadX:ReturnTextCode4Param("#W$")  "  JPG: " vFilenamejpg SKIP
         "X: " chCtrlFrame:CadX:ReturnNumericCode4Param("#SHTSIZEX") SKIP
         "Y: " chCtrlFrame:CadX:ReturnNumericCode4Param("#SHTSIZEY") SKIP
         "L: " chCtrlFrame:CadX:ReturnNumericCode4Param("L") SKIP
         "W: " chCtrlFrame:CadX:ReturnNumericCode4Param("W") SKIP
         "D: " chCtrlFrame:CadX:ReturnNumericCode4Param("D") SKIP
         "Side: " chCtrlFrame:CadX:ReturnTextCode4Param("#SIDE$")  SKIP
         "Blank Width: " chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEX") SKIP
         "Blank Height: " chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEY")    SKIP
         "Grain: " chCtrlFrame:CadX:ReturnTextCode4Param("#GRAIN$")        SKIP
         "Board: " chCtrlFrame:CadX:ReturnTextCode4Param("BRD$")          SKIP
         VIEW-AS ALERT-BOX.
    */     
       ed_text:screen-value = 
              "open design: " + vFilename + chr(13) +
              "Design: " + chCtrlFrame:CadX:ReturnTextCode4Param("#W$") + chr(13) +
              "JPG: " + vFilenamejpg + chr(13) +
              "Standard(Style) Name: " + chCtrlFrame:CadX:ReturnTextCode4Param("#CFN$") + chr(13) +
              "Sheet Size X: " + chCtrlFrame:CadX:ReturnNumericCode4Param("#SHTSIZEX") + chr(13) +
              "Sheet Size Y: " + chCtrlFrame:CadX:ReturnNumericCode4Param("#SHTSIZEY") + chr(13) +
              "L: " + chCtrlFrame:CadX:ReturnNumericCode4Param("L") + chr(13) +
              "W: " + chCtrlFrame:CadX:ReturnNumericCode4Param("W") + chr(13) +
              "D: " + chCtrlFrame:CadX:ReturnNumericCode4Param("D") + chr(13) +
              "Side: " + chCtrlFrame:CadX:ReturnTextCode4Param("#SIDE$")  + chr(13) +
              "Blank Width: " + chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEX") + chr(13) +
              "Blank Height: " + chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEY") + chr(13) +
              "Grain: " + chCtrlFrame:CadX:ReturnTextCode4Param("#GRAIN$") + chr(13) +
              "Board: " + chCtrlFrame:CadX:ReturnTextCode4Param("BRD$")  + chr(13) +
              "#UP/Sheet : " + chCtrlFrame:CadX:ReturnTextCode4Param("#NUP") + chr(13) +
              "#UP/Design: " + chCtrlFrame:CadX:ReturnTextCode4Param("#NUPD")  + chr(13) +
              "Item#: " + chCtrlFrame:CadX:ReturnTextCode4Param("#ITEM$")  + chr(13) /* +
              "Total Sheet Cost: " + chCtrlFrame:CadX:CostSht(1)  + chr(13)*/
              
               +
              "Cust Name: " + chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(CUST,NAME$)") +
              chr(13) +
              "Sales First Name: " + chCtrlFrame:CadX:ReturnTextCode4Param("DBGET(SLSPN,FNAME$)") +
              chr(13)

              .
         
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vCadNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vCadNum Dialog-Frame
ON HELP OF vCadNum IN FRAME Dialog-Frame /* Artios CAD# */
DO:
   def var vfilename as cha no-undo.
   def var ll-ok as log no-undo.
   def var resultx as int no-undo.
   def var vFilenameJPG as cha no-undo.
   
   system-dialog get-file vFilename 
                 title "Select Artios CAD Image File to insert"
                 filters "ARD Files   (*.ARD)" "*.ARD"
                 initial-dir "c:\artios\asi\"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.
      
    IF ll-ok THEN do:
       self:screen-value = vfilename.
       
       resultx = chCtrlFrame:CadX:OpenDesign (vFilename,0).

       if resultx = 0 then do:
         vFilenameJPG = "c:\tmp\" + chCtrlFrame:CadX:ReturnTextCode4Param("#W$") + ".jpg".
         resultx = chCtrlFrame:CadX:SaveAsBitmap(1,vFilenameJPG, 600, 600, 20, , , , 100).
         image-1:load-image(vFilenameJPG).
       end.
   
    /*
     MESSAGE "open design: " vFilename resultx SKIP
         "Design: " chCtrlFrame:CadX:ReturnTextCode4Param("#W$")  "  JPG: " vFilenamejpg SKIP
         "X: " chCtrlFrame:CadX:ReturnNumericCode4Param("#SHTSIZEX") SKIP
         "Y: " chCtrlFrame:CadX:ReturnNumericCode4Param("#SHTSIZEY") SKIP
         "L: " chCtrlFrame:CadX:ReturnNumericCode4Param("L") SKIP
         "W: " chCtrlFrame:CadX:ReturnNumericCode4Param("W") SKIP
         "D: " chCtrlFrame:CadX:ReturnNumericCode4Param("D") SKIP
         "Side: " chCtrlFrame:CadX:ReturnTextCode4Param("#SIDE$")  SKIP
         "Blank Width: " chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEX") SKIP
         "Blank Height: " chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEY")    SKIP
         "Grain: " chCtrlFrame:CadX:ReturnTextCode4Param("#GRAIN$")        SKIP
         "Board: " chCtrlFrame:CadX:ReturnTextCode4Param("BRD$")          SKIP
         VIEW-AS ALERT-BOX.
    */     
       ed_text:screen-value = 
              "open design: " + vFilename + chr(13) +
              "Design: " + chCtrlFrame:CadX:ReturnTextCode4Param("#W$") + chr(13) +
              "JPG: " + vFilenamejpg + chr(13) +
              "Standard(Style) Name: " + chCtrlFrame:CadX:ReturnTextCode4Param("#CFN$") + chr(13) +
              "Sheet Size X: " + chCtrlFrame:CadX:ReturnNumericCode4Param("#SHTSIZEX") + chr(13) +
              "Sheet Size Y: " + chCtrlFrame:CadX:ReturnNumericCode4Param("#SHTSIZEY") + chr(13) +
              "L: " + chCtrlFrame:CadX:ReturnNumericCode4Param("L") + chr(13) +
              "W: " + chCtrlFrame:CadX:ReturnNumericCode4Param("W") + chr(13) +
              "D: " + chCtrlFrame:CadX:ReturnNumericCode4Param("D") + chr(13) +
              "Side: " + chCtrlFrame:CadX:ReturnTextCode4Param("#SIDE$")  + chr(13) +
              "Blank Width: " + chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEX") + chr(13) +
              "Blank Height: " + chCtrlFrame:CadX:ReturnNumericCode4Param("#MANSIZEY") + chr(13) +
              "Grain: " + chCtrlFrame:CadX:ReturnTextCode4Param("#GRAIN$") + chr(13) +
              "Board: " + chCtrlFrame:CadX:ReturnTextCode4Param("BRD$")  + chr(13) +
              "#UP/Sheet : " + chCtrlFrame:CadX:ReturnTextCode4Param("#NUP") + chr(13) +
              "#UP/Design: " + chCtrlFrame:CadX:ReturnTextCode4Param("#NUPD")  + chr(13) +
              "Item#: " + chCtrlFrame:CadX:ReturnTextCode4Param("#ITEM$")  + chr(13) /* +
              "Total Sheet Cost: " + chCtrlFrame:CadX:CostSht(1)  + chr(13)*/
              
              .


    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "d-cadnum.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "d-cadnum.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY vCadNum Ed_text 
      WITH FRAME Dialog-Frame.
  ENABLE IMAGE-1 vCadNum Ed_text BUTTON-2 Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

