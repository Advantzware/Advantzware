&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input param ip-recid as recid no-undo.
def input param ip-est-no as cha no-undo.
def input param ip-type as cha no-undo.
def output param op-flag as cha no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-43 lv-prompt lv-flag-1 lv-flag-2 ~
lv-flag-3 lv-flag-4 lv-flag-5 lv-flag-6 lv-flag-10 lv-flag-7 lv-flag-8 ~
lv-flag-9 Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS lv-prompt lv-flag-1 lv-flag-2 lv-flag-3 ~
lv-flag-4 lv-flag-5 lv-flag-6 lv-flag-10 lv-flag-7 lv-flag-8 lv-flag-9 ~
lv-text-1 lv-text-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE lv-text-1 AS CHARACTER FORMAT "X(256)":U INITIAL "System control record is not on file for Finished Goods update from order." 
      VIEW-AS TEXT 
     SIZE 84 BY .62 NO-UNDO.

DEFINE VARIABLE lv-text-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Please set the detaults below." 
      VIEW-AS TEXT 
     SIZE 43 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 17.62.

DEFINE VARIABLE lv-flag-1 AS LOGICAL INITIAL no 
     LABEL "Sell     Price" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-flag-10 AS LOGICAL INITIAL no 
     LABEL "Description 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-flag-2 AS LOGICAL INITIAL no 
     LABEL "UOM" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-flag-3 AS LOGICAL INITIAL no 
     LABEL "Count" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-flag-4 AS LOGICAL INITIAL no 
     LABEL "Name" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-flag-5 AS LOGICAL INITIAL no 
     LABEL "Description 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-flag-6 AS LOGICAL INITIAL no 
     LABEL "Description 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-flag-7 AS LOGICAL INITIAL no 
     LABEL "Job" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-flag-8 AS LOGICAL INITIAL no 
     LABEL "Vendor" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-flag-9 AS LOGICAL INITIAL no 
     LABEL "Estimate" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE lv-prompt AS LOGICAL INITIAL no 
     LABEL "Prompt for update screen?" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-prompt AT ROW 4.1 COL 15
     lv-flag-1 AT ROW 5.29 COL 15
     lv-flag-2 AT ROW 6.24 COL 15
     lv-flag-3 AT ROW 7.19 COL 15
     lv-flag-4 AT ROW 8.14 COL 15
     lv-flag-5 AT ROW 9.1 COL 15
     lv-flag-6 AT ROW 10.05 COL 15
     lv-flag-10 AT ROW 11 COL 15 WIDGET-ID 2
     lv-flag-7 AT ROW 11.95 COL 15
     lv-flag-8 AT ROW 12.91 COL 15
     lv-flag-9 AT ROW 13.86 COL 15
     Btn_OK AT ROW 16.48 COL 37
     lv-text-1 AT ROW 1.71 COL 3 COLON-ALIGNED NO-LABEL
     lv-text-2 AT ROW 2.67 COL 3 COLON-ALIGNED NO-LABEL
     RECT-43 AT ROW 1 COL 1
     SPACE(0.19) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Finished Goods Item Update"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
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

/* SETTINGS FOR FILL-IN lv-text-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-text-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Finished Goods Item Update */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   do with frame {&frame-name}.
      assign lv-flag-1 lv-flag-2
             lv-flag-3 lv-flag-4
             lv-flag-5 lv-flag-6
             lv-flag-7 lv-flag-8
             lv-flag-9 lv-flag-10
             lv-prompt
             .
   end.
   op-flag = "".
   assign  substring(op-flag,1,1) = string(lv-flag-1,"Y/N") 
           substring(op-flag,2,1) = string(lv-flag-2,"Y/N") 
           substring(op-flag,3,1) = string(lv-flag-3,"Y/N")
           substring(op-flag,4,1) = string(lv-flag-4,"Y/N")
           substring(op-flag,5,1) = string(lv-flag-5,"Y/N")
           substring(op-flag,6,1) = string(lv-flag-6,"Y/N")
           substring(op-flag,7,1) = string(lv-flag-7,"Y/N")
           substring(op-flag,8,1) = string(lv-flag-8,"Y/N")
           substring(op-flag,9,1) = string(lv-flag-9,"Y/N") 
           substring(op-flag,10,1) = string(lv-flag-10,"Y/N") 
           .

   if ip-type = "new" then do:
      assign  substring(sys-ctrl.char-fld,1,1) = string(lv-flag-1,"Y/N") 
           substring(sys-ctrl.char-fld,2,1) = string(lv-flag-2,"Y/N") 
           substring(sys-ctrl.char-fld,3,1) = string(lv-flag-3,"Y/N")
           substring(sys-ctrl.char-fld,4,1) = string(lv-flag-4,"Y/N")
           substring(sys-ctrl.char-fld,5,1) = string(lv-flag-5,"Y/N")
           substring(sys-ctrl.char-fld,6,1) = string(lv-flag-6,"Y/N")
           substring(sys-ctrl.char-fld,7,1) = string(lv-flag-7,"Y/N")
           substring(sys-ctrl.char-fld,8,1) = string(lv-flag-8,"Y/N")
           substring(sys-ctrl.char-fld,9,1) = string(lv-flag-9,"Y/N") 
           substring(sys-ctrl.char-fld,10,1) = string(lv-flag-10,"Y/N") 
           .

       assign sys-ctrl.int-fld  = int(lv-flag-9)
              sys-ctrl.log-fld  = lv-prompt.
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

  find sys-ctrl where recid(sys-ctrl) = ip-recid.

  assign lv-flag-1 = substring(sys-ctrl.char-fld,1,1) = "Y"
         lv-flag-2 = substring(sys-ctrl.char-fld,2,1) = "Y"
         lv-flag-3 = substring(sys-ctrl.char-fld,3,1) = "Y"
         lv-flag-4 = substring(sys-ctrl.char-fld,4,1) = "Y"
         lv-flag-5 = substring(sys-ctrl.char-fld,5,1) = "Y"
         lv-flag-6 = substring(sys-ctrl.char-fld,6,1) = "Y"
         lv-flag-7 = substring(sys-ctrl.char-fld,7,1) = "Y"
         lv-flag-8 = substring(sys-ctrl.char-fld,8,1) = "Y"
         lv-flag-9 = sys-ctrl.int-fld = 1 and ip-est-no <> ""
         lv-flag-10 = substring(sys-ctrl.char-fld,10,1) = "Y"
         lv-prompt = sys-ctrl.log-fld 
         .
  if ip-type = "exist" then
     assign lv-prompt:hidden in frame {&frame-name} = yes
            lv-prompt:sensitive in frame {&frame-name} = no
            lv-text-1 = ""
            lv-text-2 = "".
            
                  
  RUN enable_UI.
  /*
  if ip-type = "new" then 
        assign lv-prompt:hidden in frame {&frame-name} = no
               lv-text-1:hidden = no
               lv-text-2:hidden = no
               lv-prompt:sensitive = yes
               lv-text-1:visible = yes
               lv-text-2:visible = yes
               lv-prompt:visible = yes.
 */
  if ip-type = "exist" then   
     assign lv-prompt:hidden in frame {&frame-name} = yes
            lv-text-1:hidden in frame {&frame-name}= yes
            lv-text-2:hidden in frame {&frame-name}= yes
            lv-prompt:visible in frame {&frame-name} = no
            lv-text-1:visible in frame {&frame-name}= no
            lv-text-2:visible in frame {&frame-name}= no
            lv-prompt:sensitive in frame {&frame-name} = no
            

            .
 
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY lv-prompt lv-flag-1 lv-flag-2 lv-flag-3 lv-flag-4 lv-flag-5 lv-flag-6 
          lv-flag-10 lv-flag-7 lv-flag-8 lv-flag-9 lv-text-1 lv-text-2 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-43 lv-prompt lv-flag-1 lv-flag-2 lv-flag-3 lv-flag-4 lv-flag-5 
         lv-flag-6 lv-flag-10 lv-flag-7 lv-flag-8 lv-flag-9 Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

