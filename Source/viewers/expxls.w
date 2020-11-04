&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
DEF VAR v-whereamI AS CHAR NO-UNDO.

ASSIGN v-whereamI = PROGRAM-NAME(3).
{methods/defines/hndlset.i}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-excel-exp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-excel-exp 
     IMAGE-UP FILE "Graphics/32x32/file_excel.png":U
     IMAGE-DOWN FILE "Graphics/32x32/file_excel_hover.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/file_excel_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 7.8 BY 1.81 TOOLTIP "Export to Excel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btn-excel-exp AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE
         BGCOLOR 21 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 1.52
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn-excel-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExcel s-object
ON CHOOSE OF btn-excel-exp IN FRAME F-Main
DO:
   DEF VAR char-hdl as CHAR no-undo.
   DEF VAR vcCust-From  AS CHAR NO-UNDO INIT "".
   DEF VAR vcCust-To    AS CHAR NO-UNDO INIT "".
   DEF VAR vcItem-From  AS CHAR NO-UNDO INIT "".
   DEF VAR vcItem-To    AS CHAR NO-UNDO INIT "".
   DEF VAR vcPart-From  AS CHAR NO-UNDO INIT "".
   DEF VAR vcPart-To    AS CHAR NO-UNDO INIT "".

   DEF VAR viOrder-From  AS INT NO-UNDO INIT 0.
   DEF VAR viOrder-To    AS INT NO-UNDO INIT 0.

   DEF VAR vdDate-From  AS DATE NO-UNDO INIT ?.
   DEF VAR vdDate-To    AS DATE NO-UNDO INIT ?.
  

/*    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'excel-source':U,OUTPUT char-hdl). */
/*                                                                                             */
/*    RUN export-excel in WIDGET-HANDLE(char-hdl).                                             */
 
  IF v-whereamI MATCHES "*oeinq/w-ordinq*" OR v-whereamI MATCHES "*oe/w-order*" OR v-whereamI MATCHES "*oe/w-cloord*"
  THEN DO: 
      /* If order inquiry, then pass current sort data to the report window. */
      IF v-whereamI MATCHES "*oeinq/w-ordinq*" THEN DO:

          /* Get handle to b-ordinq.w */
          RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'sort-data-target':U,OUTPUT char-hdl).

          /* Get current sort options from b-ordinq.w. */
          IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
              RUN send-sort-data IN WIDGET-HANDLE(char-hdl) (OUTPUT vcCust-From, 
                                                             OUTPUT vcCust-To,
                                                             OUTPUT vcItem-From,
                                                             OUTPUT vcItem-To,
                                                             OUTPUT vcPart-From,
                                                             OUTPUT vcPart-To,
                                                             OUTPUT viOrder-From,
                                                             OUTPUT viOrder-To,
                                                             OUTPUT vdDate-From,
                                                             OUTPUT vdDate-To).
      END. /* IF v-whereamI MATCHES "*oeinq/w-ordinq*" */
          
      /* Run the report passing filled or blank data. */
      RUN oerep/rd-order.w (INPUT vcCust-From,
                            INPUT vcCust-To,
                            INPUT vcItem-From,
                            INPUT vcItem-To,
                            INPUT vcPart-From,
                            INPUT vcPart-To,
                            INPUT viOrder-From,
                            INPUT viOrder-To,
                            INPUT vdDate-From,
                            INPUT vdDate-To). 
      
  END.
  ELSE 
   IF v-whereamI MATCHES "*jcrep/w-wipmt*"
     THEN RUN jcrep/r-wipmt.w.
  ELSE 
      IF v-whereamI MATCHES "*jc/w-jobcst*" OR v-whereamI MATCHES "*jcinq/w-jobinq*"
          THEN do:

          /* Get handle to w-jobcst.w */
          RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'sort-data-target':U,OUTPUT char-hdl).

          /* Get current sort options from w-jobcst.w. */
          IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
              RUN send-page-data IN WIDGET-HANDLE(char-hdl) (OUTPUT vcCust-From) . 

          IF vcCust-From = "3" THEN
              RUN jcrep/r-jobexlm.w.           
          ELSE IF vcCust-From = "4" THEN
              RUN jcrep/r-jobexlr.w.
          ELSE
              RUN jcrep/r-jobexl.w.   /*Task# 08111405*/
         END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

RUN Tool_Tips IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-button s-object 
PROCEDURE disable-button :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
  btn-excel-exp:SENSITIVE = NO.
 END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

