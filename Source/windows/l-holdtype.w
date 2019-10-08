&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows/l-holdtype.w

  Description: Lookup browser for order status hold type codes.

  Input Parameters:  Type of lookup (usergrps.usergrps)
  Output Parameters: Selected code

  Author: Stacey Brooks

  Created: Feb/Mar 2012
  
  Notes:  Builds a temp-table with the list of codes from the usrgrps table.
          When input is "ORDER STATUS HOLD", the table is built using a
          predefined list in include file: oe/ordholdstat.i because the
          codes will be used for programming logic and cannot be defined
          by the user.
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER op-char-val   AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR vi AS INT NO-UNDO INIT 0.

{oe/ordholdstat.i}

DEFINE TEMP-TABLE tt-holdtype
    FIELD holdtype-code AS CHAR FORMAT "x(2)"
    FIELD holdtype-descr AS CHAR FORMAT "x(35)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME brUsrGrp

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-holdtype

/* Definitions for BROWSE brUsrGrp                                      */
&Scoped-define FIELDS-IN-QUERY-brUsrGrp tt-holdtype.holdtype-code tt-holdtype.holdtype-descr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brUsrGrp   
&Scoped-define SELF-NAME brUsrGrp
&Scoped-define QUERY-STRING-brUsrGrp FOR EACH tt-holdtype NO-LOCK
&Scoped-define OPEN-QUERY-brUsrGrp OPEN QUERY {&SELF-NAME} FOR EACH tt-holdtype NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brUsrGrp tt-holdtype
&Scoped-define FIRST-TABLE-IN-QUERY-brUsrGrp tt-holdtype


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-brUsrGrp}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brUsrGrp BtnOK BtnCancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 9 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 7 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brUsrGrp FOR 
      tt-holdtype SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brUsrGrp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brUsrGrp Dialog-Frame _FREEFORM
  QUERY brUsrGrp NO-LOCK DISPLAY
      tt-holdtype.holdtype-code COLUMN-LABEL "Group Item" FORMAT "x(2)"
      tt-holdtype.holdtype-descr COLUMN-LABEL "Description" FORMAT "x(35)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 55 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     brUsrGrp AT ROW 1 COL 1
     BtnOK AT ROW 12.43 COL 38.4 WIDGET-ID 2
     BtnCancel AT ROW 12.43 COL 46.2 WIDGET-ID 4
     SPACE(1.19) SKIP(0.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Order Status Hold Type Lookup"
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.


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
/* BROWSE-TAB brUsrGrp 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brUsrGrp
/* Query rebuild information for BROWSE brUsrGrp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-holdtype NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE brUsrGrp */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Order Status Hold Type Lookup */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brUsrGrp
&Scoped-define SELF-NAME brUsrGrp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brUsrGrp Dialog-Frame
ON DEFAULT-ACTION OF brUsrGrp IN FRAME Dialog-Frame
DO:
   APPLY "choose" TO btnOk.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK Dialog-Frame
ON CHOOSE OF BtnOK IN FRAME Dialog-Frame /* OK */
DO:
    IF AVAILABLE tt-holdtype THEN 
            ASSIGN op-char-val = TRIM(tt-holdtype.holdtype-code).
        
                  .
    APPLY "window-close" TO FRAME {&FRAME-NAME}. 

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
  RUN init-proc.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Build-Table Dialog-Frame 
PROCEDURE Build-Table :
/*------------------------------------------------------------------------------
  Purpose:     Build table using pre-defined order status list (oe/ordholdstat.i).
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


 DEFINE VARIABLE vi AS INT NO-UNDO INIT 0.

  DO vi = 1 TO NUM-ENTRIES(gcOrdStatList,","):
      CREATE tt-holdtype.
      ASSIGN tt-holdtype.holdtype-code = TRIM(ENTRY(vi,gcOrdStatList))
             tt-holdtype.holdtype-descr = TRIM(ENTRY(vi,gcOrdDescList)).
  END.


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
  ENABLE brUsrGrp BtnOK BtnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc Dialog-Frame 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  EMPTY TEMP-TABLE tt-holdtype.

  RUN Build-Table.

  {&OPEN-QUERY-brUsrGrp}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

