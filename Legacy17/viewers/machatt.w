&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: viewers/machatt.w

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
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES mach-attach mach-attach-pat
&Scoped-define FIRST-EXTERNAL-TABLE mach-attach


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mach-attach, mach-attach-pat.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mach-attach.att-dscr mach-attach.setup ~
mach-attach.qty mach-attach.run-speed mach-attach-pat.caliperMin ~
mach-attach-pat.caliperMax mach-attach-pat.blankWidthMin ~
mach-attach-pat.blankWidthMax mach-attach-pat.internalCellMin ~
mach-attach-pat.internalCellMax mach-attach-pat.endCellMin ~
mach-attach-pat.endCellMax mach-attach-pat.slotWidth ~
mach-attach-pat.dieWidth mach-attach-pat.onHandDieQty ~
mach-attach-pat.priorityLevel 
&Scoped-define ENABLED-TABLES mach-attach mach-attach-pat
&Scoped-define FIRST-ENABLED-TABLE mach-attach
&Scoped-define SECOND-ENABLED-TABLE mach-attach-pat
&Scoped-Define ENABLED-OBJECTS RECT-11 
&Scoped-Define DISPLAYED-FIELDS mach-attach.m-code mach-attach.style ~
mach-attach.att-type mach-attach.att-dscr mach-attach.setup mach-attach.qty ~
mach-attach.run-speed mach-attach-pat.caliperMin mach-attach-pat.caliperMax ~
mach-attach-pat.blankWidthMin mach-attach-pat.blankWidthMax ~
mach-attach-pat.internalCellMin mach-attach-pat.internalCellMax ~
mach-attach-pat.endCellMin mach-attach-pat.endCellMax ~
mach-attach-pat.slotWidth mach-attach-pat.dieWidth ~
mach-attach-pat.onHandDieQty mach-attach-pat.priorityLevel 
&Scoped-define DISPLAYED-TABLES mach-attach mach-attach-pat
&Scoped-define FIRST-DISPLAYED-TABLE mach-attach
&Scoped-define SECOND-DISPLAYED-TABLE mach-attach-pat


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS mach-attach.m-code mach-attach.style ~
mach-attach.att-type 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|asi.mach-attach.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 16.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     mach-attach.m-code AT ROW 1.24 COL 30 COLON-ALIGNED
          LABEL "Machine Code"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     mach-attach.style AT ROW 2.38 COL 30 COLON-ALIGNED
          LABEL "Style Code"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     mach-attach.att-type AT ROW 3.52 COL 30 COLON-ALIGNED
          LABEL "Adder Type"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     mach-attach.att-dscr AT ROW 4.67 COL 30 COLON-ALIGNED
          LABEL "Adder Description"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     mach-attach.setup AT ROW 5.81 COL 30 COLON-ALIGNED FORMAT ">>9.9999"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     mach-attach.qty AT ROW 5.81 COL 54 COLON-ALIGNED
          LABEL "For Qty" FORMAT ">>>>>"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     mach-attach.run-speed AT ROW 6.95 COL 30 COLON-ALIGNED
          LABEL "Run Speed Reduction%" FORMAT ">9.99"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     mach-attach-pat.caliperMin AT ROW 9.33 COL 30 COLON-ALIGNED WIDGET-ID 10
          LABEL "Caliper Thickness"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.caliperMax AT ROW 9.33 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.blankWidthMin AT ROW 10.29 COL 30 COLON-ALIGNED WIDGET-ID 6
          LABEL "Blank Width / Height"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.blankWidthMax AT ROW 10.29 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.internalCellMin AT ROW 11.24 COL 30 COLON-ALIGNED WIDGET-ID 20
          LABEL "Internal Cell Size"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.internalCellMax AT ROW 11.24 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.endCellMin AT ROW 12.19 COL 30 COLON-ALIGNED WIDGET-ID 16
          LABEL "End Cell Size"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.endCellMax AT ROW 12.19 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.slotWidth AT ROW 13.38 COL 30 COLON-ALIGNED WIDGET-ID 26
          LABEL "Partition Slot Width"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.dieWidth AT ROW 14.33 COL 30 COLON-ALIGNED WIDGET-ID 12
          LABEL "Partition Die Width"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.onHandDieQty AT ROW 15.33 COL 30 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     mach-attach-pat.priorityLevel AT ROW 15.38 COL 66 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     "Maximum" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 8.62 COL 51 WIDGET-ID 30
     "Minimum" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 8.62 COL 34 WIDGET-ID 28
     RECT-11 AT ROW 1.05 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.mach-attach,asi.mach-attach-pat
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 16.19
         WIDTH              = 78.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN mach-attach.att-dscr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach.att-type IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN mach-attach-pat.blankWidthMax IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach-pat.blankWidthMin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach-pat.caliperMax IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach-pat.caliperMin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach-pat.dieWidth IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach-pat.endCellMax IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach-pat.endCellMin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach-pat.internalCellMax IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach-pat.internalCellMin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach.m-code IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN mach-attach.qty IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach-attach.run-speed IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach-attach.setup IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mach-attach-pat.slotWidth IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach-attach.style IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
  DEF VAR lw-focus AS WIDGET NO-UNDO.
  DEF VAR char-val AS CHAR NO-UNDO.

  lw-focus = FOCUS.

  CASE lw-focus:NAME:
      WHEN "m-code" THEN
      DO:
         run windows/l-mach.w (g_company,g_loc, lw-focus:screen-value, output char-val).

         IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO: 
            lw-focus:SCREEN-VALUE = ENTRY(1,char-val).

         END.
      END.
      WHEN "style" THEN
      DO:
         run windows/l-style.w (g_company,lw-focus:screen-value, output char-val).
         if char-val <> "" then 
            lw-focus:screen-value = entry(1,char-val).
      END.

  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach-attach.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach-attach.m-code V-table-Win
ON LEAVE OF mach-attach.m-code IN FRAME F-Main /* Machine Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-m-code (mach-attach.m-code:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach-attach.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach-attach.style V-table-Win
ON LEAVE OF mach-attach.style IN FRAME F-Main /* Style Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-style (mach-attach.style:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN dispatch ('add-record').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "mach-attach"}
  {src/adm/template/row-list.i "mach-attach-pat"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mach-attach"}
  {src/adm/template/row-find.i "mach-attach-pat"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF adm-new-record THEN DO:
     ASSIGN mach-attach-pat.att-type = mach-attach.att-type
            mach-attach-pat.m-code = mach-attach.m-code
            mach-attach-pat.style = mach-attach.style
            .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN mach-attach.company = cocode
         mach-attach-pat.company = cocode.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-m-code (mach-attach.m-code:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-style (mach-attach.style:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "mach-attach" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "mach-attach"}
  {src/adm/template/snd-list.i "mach-attach-pat"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-m-code V-table-Win 
PROCEDURE valid-m-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST mach
                    WHERE mach.company EQ cocode
                      AND mach.m-code  EQ mach-attach.m-code:SCREEN-VALUE)
    THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style V-table-Win 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SCREEN-VALUE NE "" AND
       NOT CAN-FIND(FIRST style
                    WHERE style.company EQ cocode
                      AND style.style   EQ ip-focus:SCREEN-VALUE)
    THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

