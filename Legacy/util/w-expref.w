&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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
def var i as int.
def var cnt1 as int.
def var cnt2 as int.
def var cnt3 as int.
def stream s1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btExport btImport 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExport 
     LABEL "Export Records" 
     SIZE 28 BY 1.14.

DEFINE BUTTON btImport 
     LABEL "Import Records" 
     SIZE 27 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btExport AT ROW 3.62 COL 23 WIDGET-ID 2
     btImport AT ROW 8.38 COL 24 WIDGET-ID 4
     "Export records from the original database to c:~\temp" VIEW-AS TEXT
          SIZE 55 BY .62 AT ROW 5.29 COL 15 WIDGET-ID 6
     "Import to the new database from c:~\temp" VIEW-AS TEXT
          SIZE 55 BY .62 AT ROW 10.05 COL 16 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.2 BY 12.86 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Import / Export Reftable Recid"
         HEIGHT             = 12.86
         WIDTH              = 77.2
         MAX-HEIGHT         = 47
         MAX-WIDTH          = 336
         VIRTUAL-HEIGHT     = 47
         VIRTUAL-WIDTH      = 336
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" W-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Import / Export Reftable Recid */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Import / Export Reftable Recid */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExport W-Win
ON CHOOSE OF btExport IN FRAME F-Main /* Export Records */
DO:

  RUN exportEmailCode (output cnt1).
  RUN exportJobHdr (output cnt2).
  RUN exportPhoneRec (output cnt3).  
  
  message "Exported " cnt1 "Email Records" skip
          cnt2 "Job Hdr Records" skip
          cnt3 "PHone Records" skip
          "To the c:\temp folder."
          view-as alert-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImport W-Win
ON CHOOSE OF btImport IN FRAME F-Main /* Import Records */
DO:

  RUN importEmail (output cnt1).
  RUN importJobHdr (output cnt2).
  RUN importPhone (output cnt3).  
  
  message "Imported " cnt1 "Email Records" skip
          cnt2 "Job Hdr Records" skip
          cnt3 "PHone Records" skip
          "from the c:\temp folder."
          view-as alert-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  ENABLE btExport btImport 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportEmailCode W-Win 
PROCEDURE exportEmailCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output parameter opcnt as int.



output stream s1 to c:\temp\emailcod-rec.d.
i = 0.
for each phone:
  for each emailcod:
                     
        find FIRST reftable NO-LOCK
       WHERE reftable.rec_key = STRING (recid(phone))
         AND reftable.CODE    = emailcod.emailcod
          no-error.
       if avail reftable then do:
          export stream s1 reftable.rec_key reftable.code phone.rec_key phone.table_rec_key
          phone.attention emailcod.emailcod.
          i = i + 1.
       end.
  end.
end.

output stream s1 close.
opCnt = i.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportJobHdr W-Win 
PROCEDURE exportJobHdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output parameter opCnt as int.
i = 0.
output stream s1 to c:\temp\jobdata.d.
for each reftable where reftable.reftable eq 
"ts/jobdata.p":
  find job-hdr where recid(job-hdr) eq int(reftable.code) no-lock no-error.
  if avail job-hdr then do:
    export reftable.reftable reftable.company reftable.code job-hdr.rec_key job-hdr.job-no job-hdr.job-no2.
    i = i + 1.
  end.
end.

output stream s1 close.
opCnt = i.



/*
def var cref as char.
def var ccomp as char.
def var ccode as char.
def var crec_key as char.
def var cjob as char.
def var cJob2 as int.
input from c:\temp\jobdata.d.
repeat:
  import 
    cRef cComp cCode cRec_key cJob cJob2.
  find first reftable where reftable.reftable = cRef
  and reftable.company = cComp
  and reftable.code = cCode
  exclusive-lock no-error.
  find first job-hdr where job-hdr.rec_key eq cRec_key
  and job-hdr.job-no eq cJob
  and job-hdr.job-no2 eq cJob2
  no-lock no-error.
  if avail reftable and avail job-hdr then 
   reftable.code     = STRING(RECID(job-hdr)).
end.
input close.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportPhoneRec W-Win 
PROCEDURE exportPhoneRec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output parameter opCnt as int.
i = 0.
output stream s1 to c:\temp\phone-rec.d.
for each phone:

 find FIRST reftable NO-LOCK
                 WHERE reftable.rec_key = phone.table_rec_key
                   AND reftable.CODE    = STRING (RECID (phone))
                   no-error.
 if avail reftable then do:
  i = i + 1.
  export stream s1 reftable.rec_key reftable.code phone.table_rec_key phone.rec_key
  phone.attention.
 end.
end.
output stream s1 close.
opCnt = i.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importEmail W-Win 
PROCEDURE importEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output parameter opCnt as int.


 def var cRec_key as char.
 def var cCode as char.
 def var cPhone_rec_key as char.
 def var cPHone_table_rec_key as char.
 def var cAttention as char.
 def var cEmailCod as char.
 
 
DISABLE TRIGGERS FOR LOAD OF reftable.
i = 0.
input stream s1 from c:\temp\emailcod-rec.d.
repeat:

 
 import stream s1  cRec_key cCode cPhone_rec_Key cPhone_table_rec_key
                   cAttention cEmailCod.
        
 find first reftable where reftable.rec_key = cRec_key
                      and reftable.code = cCode  no-error.
                      
 find first phone where phone.rec_key = cPhone_rec_key
                   and phone.table_rec_key = cPhone_table_rec_key
                   and phone.attention = cAttention 
                no-lock no-error.
 if avail phone and avail reftable then do:
  reftable.code = string(recid(phone)).
  i = i + 1.
 end.
end.
input stream s1 close.

opCnt = i.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importJobHdr W-Win 
PROCEDURE importJobHdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output parameter opCnt as int.



def var cref as char.
def var ccomp as char.
def var ccode as char.
def var crec_key as char.
def var cjob as char.
def var cJob2 as int.


input stream s1 from c:\temp\jobdata.d.
repeat:

  import stream s1
    cRef cComp cCode cRec_key cJob cJob2.
    
  find first reftable where reftable.reftable = cRef
    and reftable.company = cComp
    and reftable.code = cCode
  exclusive-lock no-error.
  
  find first job-hdr where job-hdr.rec_key eq cRec_key
    and job-hdr.job-no eq cJob
    and job-hdr.job-no2 eq cJob2
  no-lock no-error.
  
  if avail reftable and avail job-hdr then do:
   i = i + 1.
   reftable.code     = STRING(RECID(job-hdr)).
  end.
   
end.
input stream s1 close.


opCnt = i.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importPhone W-Win 
PROCEDURE importPhone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output parameter opCnt as int.
 def var cRec_key as char.
 def var cCode as char.
 def var cphone_rec_key as char.
 def var cAttention as char.
 def var cPHone_table_rec_key as char.
 
 
input stream s1 from c:\temp\phone-rec.d.
DISABLE TRIGGERS FOR LOAD OF reftable.

repeat:

 import stream s1 cRec_key cCode cPhone_table_rec_key cPHone_rec_key cattention .

 find first phone where phone.rec_key = cPhone_rec_key
   and phone.table_rec_key = cPhone_table_rec_key
  and phone.attention = cAttention no-lock no-error.
  
  
  if avail reftable then do:
    reftable.rec_key = string(recid(phone)).
    i = i + 1.
  end.
  
  
end.
input stream s1 close.
opCnt = i.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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

