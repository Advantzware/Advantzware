&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: jc\d-jclose.w

  Description: Close Orders at end of FG Posting

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

DEF VAR v-dummy AS LOG NO-UNDO.
DEF TEMP-TABLE tt-mach LIKE mach .
DEF TEMP-TABLE tt-mmty LIKE mmty .
DEF TEMP-TABLE tt-mmtx LIKE mmtx .
DEF TEMP-TABLE tt-mach-calendar LIKE mach-calendar .
DEF TEMP-TABLE tt-mmtx2 LIKE mmtx2 .
DEFINE TEMP-TABLE tt-mstd LIKE mstd .

DEF STREAM st-mstd .
DEF STREAM st-mmtx .
DEF STREAM st-mmty .
DEF STREAM st-cal .
DEF STREAM st-mmtx2 .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-company to-company mach-list ~
mach-selected-list btn_select-all btn_close-all-2 btn_add btn_remove ~
btn_selected-all btn_close-all Btn_Cancel btn_ok 
&Scoped-Define DISPLAYED-OBJECTS cb-company to-company mach-list ~
mach-selected-list v-dumpfile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_add 
     LABEL "Add >>" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE BUTTON Btn_Cancel DEFAULT 
     LABEL "&Cancel" 
     SIZE 16 BY 1.24
     BGCOLOR 8 FONT 6.

DEFINE BUTTON btn_close-all 
     LABEL "Clear All >>" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE BUTTON btn_close-all-2 
     LABEL "<< Clear All" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE BUTTON btn_ok AUTO-GO 
     LABEL "OK" 
     SIZE 16 BY 1.14
     BGCOLOR 8 FONT 6.

DEFINE BUTTON btn_remove 
     LABEL "<< Remove" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE BUTTON btn_select-all 
     LABEL "<< Select  All" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE BUTTON btn_selected-all 
     LABEL "Select  All >>" 
     SIZE 17 BY 1.14
     FONT 6.

DEFINE VARIABLE cb-company AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 17 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE to-company AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 17 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE v-dumpfile AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\machine.dat" 
     LABEL "Export to" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE mach-list AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     SIZE 25 BY 20.71 NO-UNDO.

DEFINE VARIABLE mach-selected-list AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     SIZE 23 BY 20.71 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cb-company AT ROW 2 COL 3.2 COLON-ALIGNED NO-LABEL
     to-company AT ROW 2 COL 52 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     mach-list AT ROW 3.86 COL 4 HELP
          "Jobs to be closed" NO-LABEL
     mach-selected-list AT ROW 3.86 COL 54 NO-LABEL
     btn_select-all AT ROW 6.24 COL 33.6
     btn_close-all-2 AT ROW 7.67 COL 33.6
     btn_add AT ROW 10.76 COL 33.6
     btn_remove AT ROW 12.19 COL 33.6
     btn_selected-all AT ROW 15.05 COL 33.6
     btn_close-all AT ROW 16.48 COL 33.6
     Btn_Cancel AT ROW 21.95 COL 34 HELP
          "Use this function to CANCEL field selecition"
     btn_ok AT ROW 23.38 COL 34
     v-dumpfile AT ROW 25.29 COL 17 COLON-ALIGNED
     "Copy To Company" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.29 COL 54.4 WIDGET-ID 4
          FONT 6
     "Available Machine" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 2.91 COL 5
          FONT 6
     "Machine Selected" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 2.91 COL 54
          FONT 6
     "Company" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.29 COL 5.6
          FONT 6
     SPACE(64.19) SKIP(25.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Machines"
         DEFAULT-BUTTON btn_ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-dumpfile IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Export Machines */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "choose" TO btn_ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_add D-Dialog
ON CHOOSE OF btn_add IN FRAME D-Dialog /* Add >> */
DO:
    RUN add-list.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_close-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_close-all D-Dialog
ON CHOOSE OF btn_close-all IN FRAME D-Dialog /* Clear All >> */
DO:
  RUN clear-list (mach-selected-list:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_close-all-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_close-all-2 D-Dialog
ON CHOOSE OF btn_close-all-2 IN FRAME D-Dialog /* << Clear All */
DO:
  RUN clear-list (mach-list:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok D-Dialog
ON CHOOSE OF btn_ok IN FRAME D-Dialog /* OK */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.
  DEF VAR v-fin-qty AS INT NO-UNDO.
  DEF VAR v AS INT NO-UNDO.

  ASSIGN v-dumpfile cb-company to-company.

  IF NOT v-process THEN
    MESSAGE "Are you sure you want to dump all the selected machines to " v-dumpfile " ?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN DO WITH FRAME {&FRAME-NAME}:
    SESSION:SET-WAIT-STATE("general").
    RUN dump-mach.

    SESSION:SET-WAIT-STATE("").
        
    APPLY "close" TO THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_remove D-Dialog
ON CHOOSE OF btn_remove IN FRAME D-Dialog /* << Remove */
DO:
  RUN remove-list.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_select-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_select-all D-Dialog
ON CHOOSE OF btn_select-all IN FRAME D-Dialog /* << Select  All */
DO:
  RUN SELECT-ALL (mach-list:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_selected-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_selected-all D-Dialog
ON CHOOSE OF btn_selected-all IN FRAME D-Dialog /* Select  All >> */
DO:
  RUN SELECT-ALL (mach-selected-list:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-company D-Dialog
ON VALUE-CHANGED OF cb-company IN FRAME D-Dialog
DO:
   RUN load-list.
   RUN clear-list (mach-selected-list:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach-list D-Dialog
ON DEFAULT-ACTION OF mach-list IN FRAME D-Dialog
DO:
  APPLY "choose" TO btn_add.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach-selected-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach-selected-list D-Dialog
ON DEFAULT-ACTION OF mach-selected-list IN FRAME D-Dialog
DO:
  APPLY "choose" TO btn_remove.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to-company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to-company D-Dialog
ON VALUE-CHANGED OF to-company IN FRAME D-Dialog
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-list D-Dialog 
PROCEDURE add-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR i AS INT NO-UNDO.
 DEF VAR v-list AS cha NO-UNDO.
 DEF VAR lv-num-items AS INT NO-UNDO.
 DEF VAR lv-item-list AS cha NO-UNDO.
 DEF VAR lv-screen-value AS cha NO-UNDO.

 /*
 DO i = 1 TO NUM-ENTRIES(mach-list:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
    IF LOOKUP(ENTRY(i,mach-list:SCREEN-VALUE),mach-selected-list:LIST-ITEMS) <= 0 or
        mach-selected-list:LIST-ITEMS = ? 
    THEN DO:
      v-list = v-list + ENTRY(i,mach-list:SCREEN-VALUE) + ",".
    END.
 END.

 IF length(v-list) > 0 THEN DO:
    IF SUBSTRING(v-list,LENGTH(v-list),1) = "," THEN
        v-list = SUBSTRING(v-list,1,LENGTH(v-list) - 1).
    mach-selected-list:LIST-ITEMS IN FRAME {&FRAME-NAME} = (IF mach-selected-list:LIST-ITEMS IN FRAME {&FRAME-NAME} <> ? THEN mach-selected-list:LIST-ITEMS + "," ELSE "") + 
                                                      v-list.
 END.
 DO i = mach-list:NUM-ITEMS IN FRAME {&FRAME-NAME} TO 1 BY -1:
    IF mach-list:IS-SELECTED(i) THEN 
       v-dummy = mach-selected-list:DELETE(i).
 END.
 */
 DO i = 1 TO mach-list:NUM-ITEMS IN FRAME {&FRAME-NAME}:
    IF mach-list:IS-SELECTED(i) AND
      /*(NOT CAN-DO(mach-selected-list:LIST-ITEMS,mach-list:ENTRY(i)) */
       (LOOKUP(ENTRY(i,mach-list:list-items),mach-selected-list:LIST-ITEMS) <= 0
       OR
       mach-selected-list:NUM-ITEMS = 0 )
    THEN DO:
       v-dummy = mach-selected-list:ADD-LAST(mach-list:ENTRY(i)).
    END.
  END.
  lv-num-items = mach-list:NUM-ITEMS IN FRAME {&FRAME-NAME}.
  lv-item-list = mach-list:LIST-ITEMS.
  lv-screen-value = mach-list:SCREEN-VALUE.
  DO i = 1 TO lv-num-items:
     IF lookup(ENTRY(i,lv-item-list),lv-screen-value) > 0 THEN
        /*IF mach-list:IS-SELECTED(i)*/   mach-list:DELETE(ENTRY(i,lv-item-list)).     
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-list D-Dialog 
PROCEDURE clear-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-handle AS WIDGET-HANDLE NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
 /*
  DO i = 1 TO ip-handle:num-items:
      IF ip-handle:IS-SELECTED(i) THEN
          v-dummy = ip-handle:DESELECT-SELECTED-ROW(i) NO-ERROR.
  END.
 */ 
  ip-handle:SCREEN-VALUE = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dump-mach D-Dialog 
PROCEDURE dump-mach :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.
  EMPTY TEMP-TABLE tt-mach .
  EMPTY TEMP-TABLE tt-mstd .
  EMPTY TEMP-TABLE tt-mmty .
  EMPTY TEMP-TABLE tt-mmtx .
  EMPTY TEMP-TABLE tt-mach-calendar .
  EMPTY TEMP-TABLE tt-mmtx2 .
  
  OUTPUT TO VALUE(v-dumpfile).
  OUTPUT STREAM st-mstd TO VALUE(v-dumpfile + "1").
  OUTPUT STREAM st-mmty TO VALUE(v-dumpfile + "2").
  OUTPUT STREAM st-mmtx TO VALUE(v-dumpfile + "3").
  OUTPUT STREAM st-cal TO VALUE(v-dumpfile + "4").
  OUTPUT STREAM st-mmtx2 TO VALUE(v-dumpfile + "5").

  DO i = 1 TO mach-selected-list:NUM-ITEMS IN FRAME {&FRAME-NAME}:
     FOR EACH mach NO-LOCK WHERE mach.company = cb-company 
                            AND mach.m-code = ENTRY(i,mach-selected-list:LIST-ITEMS) :
         CREATE tt-mach .
         BUFFER-COPY mach EXCEPT company TO tt-mach .
         ASSIGN tt-mach.company = to-company .

         EXPORT  tt-mach  .
         FOR EACH mstd OF mach NO-LOCK:
             CREATE tt-mstd .
                 BUFFER-COPY mstd EXCEPT company TO tt-mstd .
                 ASSIGN 
                     tt-mstd.company = to-company .
             EXPORT STREAM  st-mstd tt-mstd  .

             FOR EACH mmty OF mstd NO-LOCK:
                 CREATE tt-mmty .
                 BUFFER-COPY mmty EXCEPT company TO tt-mmty .
                 ASSIGN 
                     tt-mmty.company = to-company .
                 EXPORT STREAM st-mmty tt-mmty.

             END.
             FOR EACH mmtx OF mstd NO-LOCK:
                 CREATE tt-mmtx .
                 BUFFER-COPY mmtx EXCEPT company TO tt-mmtx .
                 ASSIGN 
                     tt-mmtx.company = to-company .
                 EXPORT STREAM st-mmtx tt-mmtx.
             END.
         END.
         FOR EACH mach-calendar NO-LOCK WHERE mach-calendar.company = mach.company
                                          AND mach-calendar.m-code = mach.m-code :
             CREATE tt-mach-calendar .
                 BUFFER-COPY mach-calendar EXCEPT company TO tt-mach-calendar .
                 ASSIGN 
                     tt-mach-calendar.company = to-company .
             export STREAM st-cal tt-mach-calendar.
         END.
         FOR EACH mmtx2 NO-LOCK WHERE mmtx2.company = mach.company
                                  AND mmtx2.m-code = mach.m-code.
             CREATE tt-mmtx2 .
                 BUFFER-COPY mmtx2 EXCEPT company TO tt-mmtx2 .
                 ASSIGN 
                     tt-mmtx2.company = to-company .
             EXPORT STREAM st-mmtx2 tt-mmtx2.
         END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-buttons D-Dialog 
PROCEDURE enable-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&frame-name}:
/*    DISABLE btn_close btn_open btn_close-all btn_open-all.

    IF close-list:NUM-ITEMS GT 0 THEN ENABLE btn_open  btn_open-all.
    IF open-list:NUM-ITEMS  GT 0 THEN ENABLE btn_close btn_close-all.
    */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY cb-company to-company mach-list mach-selected-list v-dumpfile 
      WITH FRAME D-Dialog.
  ENABLE cb-company to-company mach-list mach-selected-list btn_select-all 
         btn_close-all-2 btn_add btn_remove btn_selected-all btn_close-all 
         Btn_Cancel btn_ok 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-screen D-Dialog 
PROCEDURE init-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

  RUN load-comp.
  RUN load-list.

  DO WITH FRAME {&FRAME-NAME}:
    DISPLAY mach-list mach-selected-list.
  END.

  RUN enable-buttons.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-comp D-Dialog 
PROCEDURE load-comp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  FOR EACH company NO-LOCK:
      v-dummy = cb-company:ADD-LAST(company.company) IN FRAME {&FRAME-NAME}.
      v-dummy = to-company:ADD-LAST(company.company) IN FRAME {&FRAME-NAME}.
  END.
  cb-company:SCREEN-VALUE IN FRAME {&FRAME-NAME} = g_company.
  to-company:SCREEN-VALUE IN FRAME {&FRAME-NAME} = g_company.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-list D-Dialog 
PROCEDURE load-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-list AS CHAR NO-UNDO.
  
  v-list = "".
  FOR EACH mach NO-LOCK WHERE mach.company = cb-company:SCREEN-VALUE IN FRAME {&FRAME-NAME} BY mach.m-code:
      v-list = v-list + mach.m-code + ",".
  END.
  mach-list:LIST-ITEMS IN FRAME {&FRAME-NAME} = v-list.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN init-screen.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE remove-list D-Dialog 
PROCEDURE remove-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEF VAR i AS INT NO-UNDO.

  DO i = mach-selected-list:NUM-ITEMS IN FRAME {&FRAME-NAME} TO 1 BY -1:
    IF mach-selected-list:IS-SELECTED(i) THEN DO:
       mach-list:SCREEN-VALUE = mach-list:SCREEN-VALUE + "," + 
                       ENTRY(i,mach-selected-list:LIST-ITEMS).
       v-dummy = mach-list:ADD-LAST(mach-list:ENTRY(i)).
       v-dummy = mach-selected-list:DELETE(i).
    END.
  END.
*/
 DEF VAR i AS INT NO-UNDO.
 DEF VAR v-list AS cha NO-UNDO.
 DEF VAR lv-num-items AS INT NO-UNDO.
 DEF VAR lv-item-list AS cha NO-UNDO.
 DEF VAR lv-screen-value AS cha NO-UNDO.

 DO i = 1 TO mach-selected-list:NUM-ITEMS IN FRAME {&FRAME-NAME}:
    IF mach-selected-list:IS-SELECTED(i)
       THEN v-dummy = mach-list:ADD-LAST(mach-selected-list:ENTRY(i)).
  END.
  lv-num-items = mach-selected-list:NUM-ITEMS IN FRAME {&FRAME-NAME}.
  lv-item-list = mach-selected-list:LIST-ITEMS.
  lv-screen-value = mach-selected-list:SCREEN-VALUE.
  DO i = 1 TO lv-num-items:
     IF lookup(ENTRY(i,lv-item-list),lv-screen-value) > 0 
        THEN mach-selected-list:DELETE(ENTRY(i,lv-item-list)).     
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-all D-Dialog 
PROCEDURE select-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-handle AS WIDGET-HANDLE NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

/*  DO i = 1 TO ip-handle:num-items:
     ip-handle:SELECTED NO-ERROR.
  END.
*/
  ip-handle:SCREEN-VALUE = ip-handle:LIST-ITEMS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

