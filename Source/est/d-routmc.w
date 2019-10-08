&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

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
def input parameter v-recid1 as recid.
def input parameter v-recid2 as recid.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.
 
def buffer xrout-mtx for routing-mtx.

def var v-invalid as log no-undo.
def var v-process as log no-undo.

def var lv-style    like routing-mtx.style no-undo.
def var lv-industry like style.industry no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_style end_style ~
tg_overwrite-routing Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_style end_style tg_overwrite-routing 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_msf AS INTEGER FORMAT ">,>>>":U INITIAL 0 
     LABEL "From MSF" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_style AS CHARACTER FORMAT "X(6)":U 
     LABEL "From Style" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_msf AS INTEGER FORMAT ">,>>>":U INITIAL 0 
     LABEL "Into MSF" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_style AS CHARACTER FORMAT "X(6)":U 
     LABEL "Into Style" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 8.57.

DEFINE VARIABLE tg_overwrite-routing AS LOGICAL INITIAL no 
     LABEL "Overwrite Routing?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     begin_style AT ROW 3.86 COL 18 COLON-ALIGNED
     end_style AT ROW 3.86 COL 47 COLON-ALIGNED
     begin_msf AT ROW 5.76 COL 18 COLON-ALIGNED
     end_msf AT ROW 5.76 COL 47 COLON-ALIGNED
     tg_overwrite-routing AT ROW 7.38 COL 20
     Btn_OK AT ROW 10.29 COL 16
     Btn_Cancel AT ROW 10.29 COL 41
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.71 COL 4
     RECT-17 AT ROW 1 COL 1
     SPACE(0.00) SKIP(2.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Copy Default Machine Routings"
         CANCEL-BUTTON Btn_Cancel.


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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN begin_msf IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       begin_msf:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN end_msf IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       end_msf:HIDDEN IN FRAME D-Dialog           = TRUE.

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
ON HELP OF FRAME D-Dialog /* Copy Default Machine Routings */
DO:
  DEF VAR char-val AS CHAR NO-UNDO.


  CASE FOCUS:NAME:
    WHEN "begin_style" OR WHEN "end_style" THEN DO:
      IF lv-industry EQ "1" THEN
        RUN windows/l-stylef.w (cocode,FOCUS:SCREEN-VALUE,OUTPUT char-val).
      ELSE
        RUN windows/l-stylec.w (cocode,FOCUS:SCREEN-VALUE,OUTPUT char-val).
      IF char-val NE "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
  END.

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Copy Default Machine Routings */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_msf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_msf D-Dialog
ON LEAVE OF begin_msf IN FRAME D-Dialog /* From MSF */
DO: 
  assign {&self-name}.
  
  if lastkey ne -1 then do:
    run valid-begin_msf.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_msf D-Dialog
ON return OF begin_msf IN FRAME D-Dialog /* From MSF */
DO:
     apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_style D-Dialog
ON LEAVE OF begin_style IN FRAME D-Dialog /* From Style */
DO:
  assign {&self-name}.
  
  if lastkey ne -1 then do:
    run valid-begin_style.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_style D-Dialog
ON return OF begin_style IN FRAME D-Dialog /* From Style */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  if begin_msf:hidden or end_msf:hidden then
    apply "close" to this-procedure.
    
  else do:
    assign
     begin_style:sensitive = yes
     end_style:sensitive   = yes
     begin_msf:hidden      = yes
     end_msf:hidden        = yes.
        
    apply "entry" to begin_style.
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  run valid-begin_style.
  if v-invalid then return no-apply.
  
  run valid-end_style.
  if v-invalid then return no-apply.
  
  if begin_style eq end_style then do:
    if begin_msf:hidden or end_msf:hidden then do:
      assign
       begin_style:sensitive = no
       end_style:sensitive = no
       begin_msf:hidden      = no
       end_msf:hidden      = no.
       
      find first routing-mtx
          where routing-mtx.company eq cocode
             and routing-mtx.loc    eq locode
             and routing-mtx.style  eq begin_style
          no-lock.
      begin_msf = routing-mtx.msf.
       
      display begin_msf end_msf
          with frame {&frame-name}.
        
      ENABLE begin_msf end_msf 
          with frame {&frame-name}.
        
      apply "entry" to begin_msf.
      return no-apply.
    end.
    
    else do:
      run valid-begin_msf.
      if v-invalid then return no-apply.
  
      run valid-end_msf.
      if v-invalid then return no-apply.
    end.   
  end.
  
  run copy-records.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_msf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_msf D-Dialog
ON LEAVE OF end_msf IN FRAME D-Dialog /* Into MSF */
DO: 
  assign {&self-name}.
  
  if lastkey ne -1 then do:
    run valid-end_msf.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_msf D-Dialog
ON return OF end_msf IN FRAME D-Dialog /* Into MSF */
DO:
     apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_style D-Dialog
ON LEAVE OF end_style IN FRAME D-Dialog /* Into Style */
DO: 
  assign {&self-name}.
  
  if lastkey ne -1 then do:
    run valid-end_style.
    if v-invalid then return no-apply.
  end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_style D-Dialog
ON return OF end_style IN FRAME D-Dialog /* Into Style */
DO:
     apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_overwrite-routing
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_overwrite-routing D-Dialog
ON VALUE-CHANGED OF tg_overwrite-routing IN FRAME D-Dialog /* Overwrite Routing? */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
find style       where recid(style)       eq v-recid2.
find routing-mtx where recid(routing-mtx) eq v-recid1 no-lock no-error.
   
assign
 lv-style    = style.style
 lv-industry = style.industry
 begin_style  = lv-style
 end_style  = lv-style.
 
if avail routing-mtx then begin_msf = routing-mtx.msf.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-records D-Dialog 
PROCEDURE copy-records :
/* --------------------------------------------- sys/ref/routmtxc.p 05/96 JLF */
/* Routing Matrix Copy -                                                      */
/* -------------------------------------------------------------------------- */

v-process = no.

message "Are you sure you want to copy using these parameters?"
        view-as alert-box question button yes-no update v-process.
        
if v-process then do:
  for each routing-mtx
      where routing-mtx.company eq cocode
        and routing-mtx.loc     eq locode
        and routing-mtx.style   eq begin_style
        and (routing-mtx.msf    eq begin_msf or
             begin_style         ne end_style)
      no-lock:

    find first xrout-mtx
        where xrout-mtx.company eq cocode
          and xrout-mtx.loc     eq locode
          and xrout-mtx.style   eq end_style
          and ((xrout-mtx.msf   eq end_msf        and
                begin_style      eq end_style)          or
               (xrout-mtx.msf   eq routing-mtx.msf and
                begin_style      ne end_style))
        no-lock no-error.

    if not avail xrout-mtx then do:
      create xrout-mtx.
      buffer-copy routing-mtx to xrout-mtx
      assign
       xrout-mtx.style = end_style
       xrout-mtx.msf   = if begin_style eq end_style then end_msf
                                                     else routing-mtx.msf.
    end.
    ELSE IF tg_overwrite-routing AND begin_style ne end_style THEN
    DO:
       FIND CURRENT xrout-mtx.
       DELETE xrout-mtx.
       buffer-copy routing-mtx to xrout-mtx
       assign
          xrout-mtx.style = end_style.
    END.
  end.
   
  message "Copy Process Is Completed" view-as alert-box.
  apply "close" to this-procedure.
end.

return no-apply.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

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
  DISPLAY begin_style end_style tg_overwrite-routing 
      WITH FRAME D-Dialog.
  ENABLE RECT-17 begin_style end_style tg_overwrite-routing Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-begin_msf D-Dialog 
PROCEDURE valid-begin_msf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

v-invalid = no.

find first routing-mtx
    where routing-mtx.company eq cocode
      and routing-mtx.loc     eq locode
      and routing-mtx.style   eq begin_style
      and routing-mtx.msf     eq begin_msf
    no-lock no-error.

if not avail routing-mtx then do:
  message "No Routing Matrix exists with Style/MSF:"
          trim(begin_style) + "/" + trim(string(begin_msf,">,>>>"))
          view-as alert-box error.
  v-invalid = yes.
end.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-begin_style D-Dialog 
PROCEDURE valid-begin_style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

v-invalid = no.

find first routing-mtx
    where routing-mtx.company eq cocode
      and routing-mtx.loc     eq locode
      and routing-mtx.style   eq begin_style
    no-lock no-error.
if not avail routing-mtx then do:
  message "No Routing Matrix exists with Style:" begin_style
          view-as alert-box error.
  v-invalid = yes.    
end.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-end_msf D-Dialog 
PROCEDURE valid-end_msf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

v-invalid = no.

find first routing-mtx
    where routing-mtx.company eq cocode
      and routing-mtx.loc     eq locode
      and routing-mtx.style   eq end_style
      and routing-mtx.msf     eq end_msf
    no-lock no-error.

if end_msf eq 0 or avail routing-mtx then do:
  if end_msf eq 0 then
    message "Into MSF may not be zeroes" view-as alert-box error.
  else
    message "Routing Matrix already exists with Style/MSF:"
            trim(end_style) + "/" + trim(string(end_msf,">,>>>"))
            view-as alert-box error.
  v-invalid = yes.    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-end_style D-Dialog 
PROCEDURE valid-end_style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ind-list AS CHAR INIT "Fold,Corr,Foam" NO-UNDO.


v-invalid = no.

find first style
    where style.company  eq cocode
      and style.style    eq end_style
      and style.industry eq lv-industry
    no-lock no-error.
      
if end_style eq "" or not avail style then do:
  if end_style eq "" then
    message "Into Style may not be spaces"
        view-as alert-box error.
  else
    message "No Style exists with Style/Industry:" + " " +
            trim(end_style) + "/" + trim(entry(int(lv-industry),ind-list))
        view-as alert-box error.
  v-invalid = yes.    
end.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

