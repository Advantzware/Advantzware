&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: fg\v-fgimg.w

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

def var li-cnt as int no-undo.
def var li-line-no as int extent 99 no-undo.

def new shared var cocode as cha no-undo.
def new shared buffer xest for est.
def new shared buffer xef  for ef.
def new shared buffer xeb  for eb.
{cec/descalc.i new}
def TEMP-TABLE w-box-h NO-UNDO like box-design-hdr.
def TEMP-TABLE w-box-l NO-UNDO like box-design-line.
def var lv-wscore like box-design-hdr.wscore no-undo.
def var lv-wcum-score like box-design-hdr.wcum-score no-undo.
DEF VAR ll-box-refreshed AS LOG NO-UNDO.

PROCEDURE ShellExecuteA EXTERNAL "shell32":u :
      define input parameter hwnd as long.
      define input parameter lpOperation as char.
      define input parameter lpFile as CHAR .
      define input parameter lpParameters as char.
      define input parameter lpDirectory as char.
      define input parameter nShowCmd as long.
      define return parameter hInstance as long.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg.box-image 
&Scoped-define ENABLED-TABLES itemfg
&Scoped-define FIRST-ENABLED-TABLE itemfg
&Scoped-Define ENABLED-OBJECTS box-image-2 RECT-40 
&Scoped-Define DISPLAYED-FIELDS itemfg.box-image 
&Scoped-define DISPLAYED-TABLES itemfg
&Scoped-define FIRST-DISPLAYED-TABLE itemfg


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE IMAGE box-image-2
     SIZE 111 BY 12.38.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 17.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     itemfg.box-image AT ROW 1.48 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 98 BY 1
     box-image-2 AT ROW 4.57 COL 2
     RECT-40 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.itemfg
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.67
         WIDTH              = 148.4.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
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

&Scoped-define SELF-NAME itemfg.box-image
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.box-image V-table-Win
ON HELP OF itemfg.box-image IN FRAME F-Main /* Image File */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.

   /* gdm - 11110806 */
   DEF VAR v_path    AS CHAR FORMAT "X(100)" NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
   
      ASSIGN
         v_path = itemfg.box-image:SCREEN-VALUE.

      IF TRIM(v_path) EQ "" THEN
      DO:
          FIND FIRST sys-ctrl NO-LOCK                       
             WHERE sys-ctrl.company EQ itemfg.company
               AND sys-ctrl.name EQ "GRAPHIC" NO-ERROR.     
          IF AVAIL sys-ctrl
              THEN ASSIGN v_path = TRIM(sys-ctrl.char-fld).
              ELSE ASSIGN v_path = "boximage\".
      END.
           
      SYSTEM-DIALOG GET-FILE ls-filename 
                    TITLE "Select Image File to insert"
                    FILTERS "Acrobat Files(*.pdf)" "*.pdf",
                            "JPG Files    (*.jpg)" "*.jpg",
                            "Bitmap files (*.bmp)" "*.bmp",
                            "JPEG Files   (*.jpeg)" "*.jpeg",
                            "TIF Files    (*.tif)" "*.tif",                       
                            "All Files    (*.*) " "*.*"
                    INITIAL-DIR v_path
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE ll-ok.
         
       IF ll-ok THEN self:screen-value = ls-filename.
    
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT PARAMETER programname AS cha.
    DEFINE INPUT PARAMETER visualstyle AS long.
    DEFINE RETURN PARAM statuscode AS LONG.
END.


  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-box V-table-Win 
PROCEDURE build-box :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
session:set-wait-state("general").
  /* copied from cec/est-6-re.p */
def input parameter v-rebuild as char.

def buffer xbox-design-hdr  for box-design-hdr.
def buffer xbox-design-line for box-design-line.

cocode = eb.company.
find xeb where recid(xeb) = recid(eb) no-lock.               

for each box-design-hdr where box-design-hdr.design-no = 0 and
                              box-design-hdr.company = xeb.company 
                          and box-design-hdr.est-no = xeb.est-no
    /*{cec/est-6W.i box-design-hdr}*/
      and box-design-hdr.form-no   eq xeb.form-no
      and box-design-hdr.blank-no  eq xeb.blank-no
    no-lock:
  /*      
  for each box-design-line of box-design-hdr:
    create w-box-l.
    buffer-copy box-design-line to w-box-l.
  end.
  */

  create w-box-h.
  buffer-copy box-design-hdr to w-box-h.
end.

{cec/est-6del.i}

find first xest where xest.company = xeb.company and
                      xest.est-no = xeb.est-no
                      no-lock.
find first xef where xef.company = xeb.company 
                 and xef.est-no   eq xeb.est-no
                 and xef.form-no eq xeb.form-no  no-lock.

find first style where style.company eq xeb.company
                   and style.style   eq xeb.style
                 no-lock no-error.
if avail style then
  find first xbox-design-hdr where xbox-design-hdr.design-no eq style.design-no
  			       and xbox-design-hdr.company   eq style.company 
                               and xbox-design-hdr.est-no    eq ""
             no-lock no-error.

if avail xbox-design-hdr then do:
   run cec/descalc.p (recid(xest), recid(xeb)).
   create box-design-hdr.
   assign  box-design-hdr.design-no   = 0
           box-design-hdr.company = xeb.company
           box-design-hdr.est-no      = xeb.est-no
           box-design-hdr.form-no     = xeb.form-no
           box-design-hdr.blank-no    = xeb.blank-no
           box-design-hdr.description = if avail xbox-design-hdr then
                                          xbox-design-hdr.description else ""
           box-design-hdr.lscore      = v-lscore-c
           box-design-hdr.lcum-score  = v-lcum-score-c
/*           fil_id                     = recid(box-design-hdr). */
           box-design-hdr.wscore = xbox-design-hdr.wscore
           box-design-hdr.wcum-score = xbox-design-hdr.wcum-score
           box-design-hdr.box-text = xbox-design-hdr.box-text
           .

   for each xbox-design-line of xbox-design-hdr no-lock:
      create box-design-line.
      assign box-design-line.design-no  = box-design-hdr.design-no
             box-design-line.company = box-design-hdr.company
             box-design-line.est-no      = box-design-hdr.est-no
             box-design-line.form-no    = box-design-hdr.form-no
             box-design-line.blank-no   = box-design-hdr.blank-no
             box-design-line.line-no    = xbox-design-line.line-no
             box-design-line.line-text  = xbox-design-line.line-text.

      find first w-box-design-line
           where w-box-design-line.line-no eq box-design-line.line-no   no-error.
      if avail w-box-design-line then
         assign  box-design-line.wscore     = w-box-design-line.wscore-c
                 box-design-line.wcum-score = w-box-design-line.wcum-score-c.
   end.
 
   if v-rebuild ne "B" then do:
      if v-rebuild eq "S" then
         box-design-hdr.description = w-box-h.description.
      else  assign box-design-hdr.lscore      = w-box-h.lscore
                   box-design-hdr.lcum-score  = w-box-h.lcum-score
                   box-design-hdr.wscore      = w-box-h.wscore
                   box-design-hdr.wcum-score  = w-box-h.wcum-score.

      for each w-box-l of box-design-hdr no-lock,
          first box-design-line of w-box-l:
      
          if v-rebuild eq "S" then
             assign box-design-line.line-no    = w-box-l.line-no
                     box-design-line.line-text  = w-box-l.line-text.
          else do:
             find first w-box-design-line
                  where w-box-design-line.line-no eq w-box-l.line-no   no-error.
             if avail w-box-design-line then
                assign box-design-line.wscore     = w-box-l.wscore
                       box-design-line.wcum-score = w-box-l.wcum-score.
          end.     
      end.
   end.
end.
/*
def var char-hdl as cha no-undo.
run get-link-handle in adm-broker-hdl (this-procedure,"record-source", output char-hdl).
run dispatch in widget-handle(char-hdl) ('open-query').  
*/
run build-screen.
session:set-wait-state("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN refresh-boximg.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN refresh-boximg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-boximg V-table-Win 
PROCEDURE refresh-boximg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR ll-dummy AS LOG NO-UNDO.
  

   FIND CURRENT itemfg NO-LOCK NO-ERROR.
   IF NOT AVAIL itemfg THEN RETURN.
  /*
   ll-dummy = box-image-2:load-image("") in frame {&frame-name} no-error.
  /*
   if box-design-hdr.box-3d-image <> "" then do:
     /*  box-image:auto-resize = yes. */
     ll-dummy = box-image-2:load-image(box-design-hdr.box-3d-image) in frame {&frame-name}.
     /*assign box-image:height-pixels = box-image:height-pixels - 10
            box-image:width-pixels =  box-image:width-pixels - 10.
            
     */
   end.
   ELSE */
        if itemfg.box-image <> "" then do:
     /*  box-image:auto-resize = yes. */
     ll-dummy = box-image-2:load-image(itemfg.box-image) in frame {&frame-name}.
     /*assign box-image:height-pixels = box-image:height-pixels - 10
            box-image:width-pixels =  box-image:width-pixels - 10.
            
     */
   end.
   ll-box-refreshed = YES.
*/
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
  {src/adm/template/snd-list.i "itemfg"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-fgitem-img V-table-Win 
PROCEDURE update-fgitem-img :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-cmd AS cha NO-UNDO.
  DEF VAR lv-quote AS cha INIT '"' NO-UNDO.
  DEF VAR lv-return AS INT NO-UNDO.
  DEF VAR tInt AS INT NO-UNDO.
  DEF VAR ls-image1 AS cha FORM "x(300)" NO-UNDO.
  DEF VAR v-program AS CHAR NO-UNDO.
  
  v-program = "mspaint.exe" .
  lv-cmd = "custom\mspaint.exe".

  IF AVAIL itemfg AND itemfg.box-image <> "" THEN DO:

     IF itemfg.box-image MATCHES "*.pdf*"  THEN DO:
        RUN ShellExecuteA(0, "open", itemfg.box-image, "", "", 0, OUTPUT tInt).
      
        IF tInt LE 32 THEN
        DO:
           RUN custom/runapdf.p (OUTPUT lv-cmd).
           lv-cmd = lv-cmd + chr(32) + itemfg.box-image.
           RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).
        END.
     END.
     ELSE do: 
         ASSIGN ls-image1 = itemfg.box-image .
         lv-cmd = chr(34) + ls-image1 + " " + CHR(34).

         RUN ShellExecuteA(0, "open", lv-cmd, "", "", 0, OUTPUT tInt).

        IF tInt LE 32 THEN
            DO:
            IF ls-image1 <> "" THEN do:
                
                OS-COMMAND SILENT START value(trim(v-program)) value(lv-cmd).
           END.
        END.
       /* OS-COMMAND SILENT value(itemfg.box-image). */
      END.
  END.

  ll-box-refreshed = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-image V-table-Win 
PROCEDURE update-image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-cmd AS cha NO-UNDO.
  DEF VAR lv-quote AS cha INIT '"' NO-UNDO.
  DEF VAR lv-return AS INT NO-UNDO.

  lv-cmd = "custom\mspaint.exe".

  IF AVAIL itemfg AND itemfg.box-image <> "" THEN DO:

     IF itemfg.box-image MATCHES "*.pdf*"  THEN DO:
        RUN custom/runapdf.p (OUTPUT lv-cmd).
        /* RUN custom/runapdf.p (INPUT itemfg.box-image). */
     END.           
     /*
     lv-cmd = "c:\progra~~1\adobe\acroba~~1.0\reader\acrord32.exe".
     MESSAGE lv-cmd itemfg.box-image VIEW-AS ALERT-BOX.
     */

     /* not working on window 98/me    */
     OS-COMMAND  VALUE('"' + lv-cmd + " " + itemfg.box-image + '"').
     

    /* lv-cmd = ('"' + lv-cmd + " " + itemfg.box-image + '"'). */
     /* for win 98/me 
     lv-cmd = lv-cmd + chr(32) + itemfg.box-image.
     
     RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).     
                                                   */


  END.
  ll-box-refreshed = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

