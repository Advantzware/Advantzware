&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

/* Local Variable Definitions ---  
                                     */

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR ll-do-deactivate AS LOG NO-UNDO.
DEF VAR hStatus AS HANDLE NO-UNDO.
DEF VAR llCancel AS LOG NO-UNDO.
DEF TEMP-TABLE tt-inactive-list 
    FIELD tt-i-no AS CHAR.
DEF VAR gviCnt AS INT NO-UNDO.
DEF STREAM excel.

def var v-i-no     like itemfg.i-no     extent 2  init ["","zzzzzzzzzzzzzzz"].
def var v-cust     like itemfg.cust-no  extent 2  init ["","zzzzzzzz"].
def var v-procat   like itemfg.procat   extent 2  init ["","zzzzz"].
def var v-break    as   char format "!" init "N".
def var v-prt-cost as   log format "Cost/Value" init no.
def var v-custown  as   log format "Y/N" init no.
def var v-sort-by  as   log format "Y/N" init no.
def var v-zero     as   log format "Y/N" init no.
def var v-sho-cost as   log format "Y/N" init no.

def var v-first       as log extent 2 init yes.
def var v-page-break  as char.
def var v-label1      as char format "x(14)" extent 3.
def var v-label2      as char format "x(14)".
def var v-price       as dec.
def var v-cost        as dec.
def var v-tq-onh      as dec extent 2.
def var v-tq-ono      like v-tq-onh.
def var v-tq-alloc    like v-tq-onh.
def var v-tq-avail    like v-tq-onh.
def var v-tprice      like v-tq-onh.
def var v-qty-onh     like itemfg.q-onh.
DEF VAR iCnt          AS INT  NO-UNDO.
DEF VAR jCnt          AS INT  NO-UNDO.
DEF VAR v-status      AS CHAR NO-UNDO FORMAT "x(1)".
DEF VAR lvdCutOffDate AS DATE NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_rm-i-no end_rm-i-no btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-i-no end_rm-i-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_rm-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rm-i-no AT ROW 3.86 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_rm-i-no AT ROW 3.86 COL 66 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     btn-ok AT ROW 7.91 COL 21
     btn-cancel AT ROW 7.91 COL 57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 10.86.


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
         TITLE              = "Set Bin PO Number"
         HEIGHT             = 10.86
         WIDTH              = 96.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
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
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_rm-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Set Bin PO Number */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Set Bin PO Number */
DO:
  IF VALID-HANDLE(hStatus) THEN
      DELETE OBJECT hStatus.

  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.


  RUN windows/w-message.w PERSISTENT SET hStatus.
  RUN setWindowTitle IN hStatus (INPUT "Searching for Items").


  run process-items. 

  IF VALID-HANDLE(hStatus) THEN
      DELETE OBJECT hStatus.

  RUN windows/w-message.w PERSISTENT SET hStatus.
  RUN setWindowTitle IN hStatus (INPUT "Deactivating Items").

  IF VALID-HANDLE(hStatus) THEN
     DELETE OBJECT hStatus.
  MESSAGE "Done!"
     VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF VALID-HANDLE(hStatus) THEN
      DELETE OBJECT hStatus.


    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-i-no C-Win
ON LEAVE OF end_rm-i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
APPLY "entry" TO begin_rm-i-no .

  RUN enable_UI.

SUBSCRIBE TO "CancelIt" ANYWHERE.
SUBSCRIBE TO "NumDel" ANYWHERE.
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:

    APPLY "entry" TO begin_rm-i-no .
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll": 
DEFINE INPUT PARAMETER hWndLock AS LONG NO-UNDO. 
END PROCEDURE. /* LockWindowUpdate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelIt C-Win 
PROCEDURE cancelIt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* respond to cancel event */
llCancel = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY begin_rm-i-no end_rm-i-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_rm-i-no end_rm-i-no btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE numDel C-Win 
PROCEDURE numDel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiCnt AS INT NO-UNDO.
Run LockWindowUpdate(input CURRENT-WINDOW:HWND). 
IF VALID-HANDLE(hStatus) THEN
  RUN process-message IN hStatus (INPUT ipcTable + ": " + STRING(ipiCnt)).
Run LockWindowUpdate(input 0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-items C-Win 
PROCEDURE process-items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      DEF VAR iCnt AS INT NO-UNDO.
      DEF VAR jCnt AS INT NO-UNDO.
      DEF VAR iCntIncr AS INT NO-UNDO INIT 100.
      DEF VAR cCalcPoNo AS CHAR NO-UNDO.



       FOR EACH rm-rcpth 
         WHERE rm-rcpth.company   EQ cocode
           AND rm-rcpth.i-no      GE begin_rm-i-no 
           AND rm-rcpth.i-no      LE end_rm-i-no
           AND rm-rcpth.po-no     GT ""
           AND rm-rcpth.rita-code EQ "R"
           NO-LOCK,
           EACH rm-rdtlh fields() WHERE
                 rm-rdtlh.r-no EQ rm-rcpth.r-no 
                 AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code

               NO-LOCK
               BREAK
               BY rm-rcpth.company
               BY rm-rcpth.i-no
               BY rm-rcpth.trans-date
               BY rm-rdtlh.tag
               BY rm-rcpth.job-no
               BY rm-rcpth.job-no2          
               BY rm-rdtlh.loc
               BY rm-rdtlh.loc-bin           
               BY rm-rdtlh.trans-time DESC:

            iCnt = iCnt + 1. 
            IF iCnt GT iCntIncr THEN DO:  
              iCnt = 0. PROCESS EVENTS. 
              jCnt = jCnt + iCntIncr + 1.
              IF jCnt GT 900 THEN
                  iCntIncr = 999.
              PUBLISH "NUMDEL" (rm-rcpth.i-no, jCnt). 
              IF llCancel THEN
                  RETURN.
            END. 

            cCalcPoNo = rm-rcpth.po-no.
            /* If there is a tag, update all the bins for that tag */
            /* since tag may have been transferred                 */
            IF LAST-OF(rm-rdtlh.tag) AND rm-rdtlh.tag GT "" THEN DO:

              /* Assign PO number, tag index matches these fields */
              FOR EACH rm-bin 
                 WHERE rm-bin.company       EQ rm-rdtlh.company
                     AND rm-bin.i-no        EQ rm-rcpth.i-no
                     AND rm-bin.tag         EQ rm-rdtlh.tag 
                 EXCLUSIVE-LOCK.

                 /* this is done below, so sticking with it */
/*                  IF rm-bin.job-no GT "" THEN */
/*                     NEXT.                    */
                 IF cCalcPoNo GT "" THEN
                   rm-bin.po-no = int(cCalcPoNo).                                               

              END.


              cCalcPoNo = "".

            END.  /* last of bin */
            cCalcPoNo = rm-rcpth.po-no.

            IF LAST-OF(rm-rdtlh.loc-bin) THEN DO:
              /* Assign PO number */
              FIND FIRST rm-bin 
                 WHERE rm-bin.company       EQ rm-rdtlh.company
                     AND rm-bin.i-no        EQ rm-rcpth.i-no
                     AND rm-bin.loc         EQ rm-rdtlh.loc   
                     AND rm-bin.loc-bin     EQ rm-rdtlh.loc-bin  
                     AND rm-bin.tag         EQ rm-rdtlh.tag 
/*                      AND rm-bin.job-no      EQ rm-rcpth.job-no  */
/*                      AND rm-bin.job-no2     EQ rm-rcpth.job-no2 */
/*                      AND rm-bin.cust-no     EQ rm-rdtlh.cust-no */
/*                      AND fg-bin.bol-no      EQ fg-rdtlh.bol-no */
/*                      AND fg-bin.inv-no      EQ fg-rdtlh.inv-no */
                 EXCLUSIVE-LOCK NO-ERROR.

/*               IF AVAIL rm-bin AND rm-bin.job-no GT "" THEN DO: */
/*                   NEXT.                                        */
/*               END.                                             */

              IF AVAIL rm-bin AND cCalcPoNo GT "" THEN
                  rm-bin.po-no = int(cCalcPoNo).                                               

              cCalcPoNo = "".

            END.  /* last of bin */


       END. /* each rm-rcpth */      


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

