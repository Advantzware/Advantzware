&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
def input param ip-company like sys-ctrl.company no-undo.
def input param ip-name like sys-ctrl.name no-undo.
def output param op-char-val as cha no-undo.
def temp-table tt-sys-val field name like sys-ctrl.name
                          field name-val as cha.

def var name-fld-list   as char init "quoprint,invprint,cerun,msfcalc,2piececd,fastoe,setprint,poprint,relprint,bolfmt,chkfmt,addrelse,ackhead,jobcard,boxdesum,job qty,job#,bolwhse,closejob,relpost,pocost,loadtag,arexp,cemenu,ceroute,oeautopo,sht calc,ceround,bolcert,cerunc,cerunf,jobcardc,jobcardf,oecomm,oecarier,autopost,apinq,poqty,arinq,schedule".
def var str-init        as char extent 40.

assign
 str-init[1]  = "Portrait,LandScap,ABox,10 Pitch,Rudd,ContSrvc,HOP,General," +
                "Brick,Midwest,Triad,Fibre"
 str-init[2]  = "ASI,Beeler,Argrov,ILWalker,1/2 Page,Phoenix,Color,Royal," +
                "Livngstn,ContSrvc,Rudd,Premier,MultiWll,Imperial,PAC 1/2," +
                "Colonial,Clev 1/2,Triad,Danbury,TriState,Blueridg,Sonoco," +  
                "P&P,HOP,Empire,Brick,Acme,Allpkg,Valley,Century,Express," +
                "MaxPak,Fibre,Abox,Harwell"
 str-init[3]  = "ASI,Clevelnd,McLean,Suthrlnd,HOP,Brick"
 str-init[4]  = "Foldware,Corrware"
 str-init[5]  = "D Pallet,Z Trailr"
 str-init[6]  = "ASI,Argrov,Beeler,ILWalker"
 str-init[7]  = "ASI,McLean"
 str-init[8]  = "16th's,32nd's,Century,Middlesx,RFC,Sonoco,Rudd,Brick,Fibre,P&P"
 str-init[9]  = "ASI,Argrov,Century,HOP,MultiWll,Sonoco,TriState,Fibre," +
                "Premier"
 str-init[10] = "ASI,1/2 Page,Royal,ContSrvc,Superior,Premier,Warren,PAC 1/2," +
                "Imperial,P&P,Triad,TriState,BlueRidg,Danbury,Sonoco,HOP," +
                "Boxtech,Empire,Brick,AllPkg,Fibre,Maxpak,Oracle,Harwell,Inland"
 str-init[11] = "ASI,Laser,P&P,n,Raritan,ContSrvc,Royal,Triad,Danbury,Rudd," +
                "Hartford,AIHalper,Brick,Fibre,Herman,ASILaser,TriadLas"
 str-init[12] = "ASI,Royal"
 str-init[13] = "ASI,WesInd,ILWalker,HOP,Brick"
 str-init[14] = "10 Pitch,17 Pitch,Boxtech,Phoenix,TriState,Triad,RFC,HOP," +
                "Brick,Hartford"
 str-init[15] = "Inches,MM,Both"
 str-init[16] = ",Net Shts"
 str-init[17] = ",Order#"
 str-init[18] = "FG BIN,SHIPTO"
 str-init[19] = "Manually,FGPost,OrdClose"
 str-init[20] = "Nothing,Invoice,BOL,BOL/TAG,BOL/REL"
 str-init[21] = "JobFile,Vendor"
 str-init[22] = "ASI,Triad"
 str-init[23] = "ASI,Sonoco"
 str-init[24] = "Corrware,Foldware,Both"
 str-init[26] = "Auto,Manual,AutoRM"
 str-init[27] = "QOH>QEst,AllItems"
 str-init[28] = "Penny,Dollar"
 str-init[29] = ",Brick"
 str-init[30] = "ASI,Clevelnd,McLean,Suthrlnd,Brick"
 str-init[31] = "ASI,McLean,HOP"
 str-init[32] = "ASI,Boxtech,Brick,Phoenix,RFC,TriState,Triad,Hartford," +
                "Corrugat"   
 str-init[33] = "ASI,HOP"
 str-init[34] = "Manual,Matrix"
 str-init[35] = "ShipTo,Header"
 str-init[36] = "ShipTo,FGFile"
 str-init[37] = "Triad,Brick"
 str-init[38] = "JobQty,Net Shts"
 str-init[39] = "ASI,Fibre"
 str-init[40] = "None,Planner,Kiwi".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-sys-val

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-sys-val.name-val   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-sys-val       WHERE true  NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-sys-val
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-sys-val


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-sys-val SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-sys-val.name-val label "Character!Value"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 48 BY 11.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     SPACE(0.39) SKIP(0.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "System Parameter Character Values Information".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
 /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* System Parameter Character Values Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   op-char-val = tt-sys-val.name-val:screen-value in browse {&browse-name} .
   apply "window-close" to frame {&frame-name}. 
      
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
  
  run create-sys-val.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-sys-val Dialog-Frame 
PROCEDURE create-sys-val :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var li-cnt as int no-undo.
 
  if can-do(name-fld-list,ip-name) then do:
     do li-cnt = 1 to num-entries(str-init[lookup(ip-name,name-fld-list)]):
        create tt-sys-val.
        assign tt-sys-val.name = ip-name
               tt-sys-val.name-val = entry(li-cnt, str-init[lookup(ip-name,name-fld-list)]).
     end.          
  end.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  ENABLE BROWSE-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


