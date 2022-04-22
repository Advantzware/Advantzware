&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/smartobj/qtyUnits.w

  Description: from SMART.W - Template for basic SmartObject

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
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

DEFINE VARIABLE oKeyboard  AS system.Keyboard  NO-UNDO.
DEFINE VARIABLE glShipNotesExpanded AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rQuantitiesRectangle rQuantitiesRectangle-2 
&Scoped-Define DISPLAYED-OBJECTS fiBolNo fiBolDate ~
fiOrdered fiBolStatus ship_note fiRelease fiCarrier fiSeal tb_signed ~
fiCustomer fiLoc fiTrailer tb_posted fiShipTo fiRateWt tb_printed fiAdd1 ~
fiTotalWeight fiAdd2 fiTotalPallet fiAdd3 fiAdd4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE ship_note AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 68 BY 8.14
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiAdd4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiAdd1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiAdd2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiAdd3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiBolDate AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Bol Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiBolNo AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Bol#" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1 TOOLTIP "Total Quantity"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiBolStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bol Status" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiCarrier AS CHARACTER FORMAT "X(256)":U 
     LABEL "Carrier" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiCustomer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiLoc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Loc" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FGCOLOR 0 NO-UNDO.

DEFINE VARIABLE fiOrdered AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordered By" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiRateWt AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Rate/100 Wt" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiRelease AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Release#" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiSeal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Seal#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiShipTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ship To#" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiTotalPallet AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Total Pallets" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiTotalWeight AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Weight" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiTrailer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Trailer#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE rQuantitiesRectangle
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 139.8 BY 9.76.

DEFINE RECTANGLE rQuantitiesRectangle-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 76 BY 9.76.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Posted" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_printed AS LOGICAL INITIAL no 
     LABEL "Printed" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_signed AS LOGICAL INITIAL no 
     LABEL "Signed" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main    
     fiBolNo AT ROW 1.95 COL 22 COLON-ALIGNED WIDGET-ID 26
     fiBolDate AT ROW 1.95 COL 55 COLON-ALIGNED WIDGET-ID 32
     fiOrdered AT ROW 1.95 COL 89 COLON-ALIGNED WIDGET-ID 52
     fiBolStatus AT ROW 1.95 COL 118 COLON-ALIGNED WIDGET-ID 88
     ship_note AT ROW 2.62 COL 150 NO-LABEL WIDGET-ID 130
     fiRelease AT ROW 3.1 COL 22 COLON-ALIGNED WIDGET-ID 92
     fiCarrier AT ROW 3.1 COL 55 COLON-ALIGNED WIDGET-ID 90
     fiSeal AT ROW 3.1 COL 89 COLON-ALIGNED WIDGET-ID 94
     tb_signed AT ROW 3.24 COL 113 WIDGET-ID 96
     fiCustomer AT ROW 4.24 COL 22 COLON-ALIGNED WIDGET-ID 100
     fiLoc AT ROW 4.24 COL 55 COLON-ALIGNED WIDGET-ID 98
     fiTrailer AT ROW 4.24 COL 89 COLON-ALIGNED WIDGET-ID 102
     tb_posted AT ROW 4.38 COL 113 WIDGET-ID 104
     fiShipTo AT ROW 5.38 COL 22 COLON-ALIGNED WIDGET-ID 106
     fiRateWt AT ROW 5.38 COL 89 COLON-ALIGNED WIDGET-ID 108
     tb_printed AT ROW 5.52 COL 113 WIDGET-ID 110
     fiAdd1 AT ROW 6.52 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     fiTotalWeight AT ROW 6.52 COL 89 COLON-ALIGNED WIDGET-ID 112
     fiAdd2 AT ROW 7.62 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     fiTotalPallet AT ROW 7.62 COL 89 COLON-ALIGNED WIDGET-ID 118
     fiAdd3 AT ROW 8.76 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 120
     fiAdd4 AT ROW 9.86 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 122
     "Notes:" VIEW-AS TEXT
          SIZE 10 BY .95 AT ROW 1.67 COL 145 WIDGET-ID 126
     rQuantitiesRectangle AT ROW 1.48 COL 1.2 WIDGET-ID 74
     rQuantitiesRectangle-2 AT ROW 1.48 COL 143 WIDGET-ID 124
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15 FONT 33 WIDGET-ID 100.


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
         HEIGHT             = 10.33
         WIDTH              = 219.8.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiAdd4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAdd1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAdd2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAdd3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiBolDate IN FRAME F-Main
   NO-ENABLE                                                            */

/* SETTINGS FOR FILL-IN fiBolNo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiBolNo:PRIVATE-DATA IN FRAME F-Main     = 
                "Quantity".

/* SETTINGS FOR FILL-IN fiBolStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCarrier IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCustomer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLoc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOrdered IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRateWt IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRelease IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSeal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiShipTo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotalPallet IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotalWeight IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTrailer IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       rQuantitiesRectangle:PRIVATE-DATA IN FRAME F-Main     = 
                "QuantitiesRectangle".

ASSIGN 
       rQuantitiesRectangle-2:PRIVATE-DATA IN FRAME F-Main     = 
                "QuantitiesRectangle".

/* SETTINGS FOR EDITOR ship_note IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_posted IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       tb_posted:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_printed IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       tb_printed:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_signed IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       tb_signed:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

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

&Scoped-define SELF-NAME fiAdd4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAdd4 s-object
ON ENTRY OF fiAdd4 IN FRAME F-Main
DO:
    SELF:BGCOLOR = 30.

    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:OpenKeyboard (SELF, "Numeric").         
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */



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




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable s-object 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE No-Resize s-object 
PROCEDURE No-Resize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayBOLInfo s-object 
PROCEDURE pDisplayBOLInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBOLID      AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.       
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
    DO WITH FRAME {&FRAME-NAME}:
    
      FIND FIRST bf-oe-bolh NO-LOCK 
           WHERE bf-oe-bolh.company EQ ipcCompany
           AND bf-oe-bolh.bol-no EQ ipiBOLID NO-ERROR .
      IF AVAILABLE bf-oe-bolh THEN
      do:      
        ASSIGN
            fiBolNo:SCREEN-VALUE     = STRING(bf-oe-bolh.bol-no)
            fiBolDate:SCREEN-VALUE   = STRING(bf-oe-bolh.bol-date)           
            fiBolStatus:SCREEN-VALUE = IF bf-oe-bolh.stat EQ "R" THEN "Released" ELSE "Hold"
            fiRelease:SCREEN-VALUE   = STRING(bf-oe-bolh.release#)
            fiCarrier:SCREEN-VALUE   = bf-oe-bolh.carrier
            fiSeal:SCREEN-VALUE      = bf-oe-bolh.airway-bill
            tb_signed:SCREEN-VALUE   = IF bf-oe-bolh.spare-int-1 EQ 0 THEN "No" ELSE "Yes"
            fiCustomer:SCREEN-VALUE  = bf-oe-bolh.cust-no
            fiLoc:SCREEN-VALUE       = bf-oe-bolh.loc
            fiTrailer:SCREEN-VALUE   = bf-oe-bolh.trailer
            tb_posted:SCREEN-VALUE   = STRING(bf-oe-bolh.posted)
            fiShipTo:SCREEN-VALUE    = bf-oe-bolh.ship-id
            fiRateWt:SCREEN-VALUE    = STRING(bf-oe-bolh.cwt) 
            tb_printed:SCREEN-VALUE  = STRING(bf-oe-bolh.printed)            
            fiTotalWeight:SCREEN-VALUE = STRING(bf-oe-bolh.tot-wt)           
            fiTotalPallet:SCREEN-VALUE = STRING(bf-oe-bolh.tot-pallets) 
            
            .
            
        FIND FIRST shipto NO-LOCK
             WHERE shipto.company EQ ipcCompany
               AND shipto.cust-no EQ bf-oe-bolh.cust-no
               AND shipto.ship-id EQ bf-oe-bolh.ship-id
             NO-ERROR.
           IF AVAIL shipto THEN
           ASSIGN 
                fiAdd1:SCREEN-VALUE = shipto.ship-name
                fiAdd2:SCREEN-VALUE  = shipto.ship-addr[1]
                fiAdd3:SCREEN-VALUE  = shipto.ship-addr[2]
                fiAdd4:SCREEN-VALUE  = shipto.ship-city  + "  " + shipto.ship-state + " " +  shipto.ship-zip. 
         
        RUN pSetShipToExpanded(bf-oe-bolh.company). 
        IF glShipNotesExpanded THEN DO:
            RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.
            RUN GetNoteOfType IN hNotesProcs (bf-oe-bolh.rec_key, "ES", OUTPUT ship_note).
            DELETE OBJECT hNotesProcs.
            ship_note:SCREEN-VALUE = ship_note.
        END. 
        ELSE DO:
             ship_note:SCREEN-VALUE = bf-oe-bolh.ship-i[1] + "  " + bf-oe-bolh.ship-i[2] + "  " +
                                      bf-oe-bolh.ship-i[3] + "  " + bf-oe-bolh.ship-i[4].
        END.
        
        FIND FIRST oe-boll NO-LOCK
             WHERE oe-boll.company EQ ipcCompany 
             AND oe-boll.b-no = bf-oe-bolh.b-no NO-ERROR.
             
        IF AVAIL oe-boll THEN     
           FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ ipcCompany 
                AND oe-ord.ord-no EQ oe-boll.ord-no NO-ERROR.
        IF AVAIL oe-ord AND AVAIL oe-boll THEN
           ASSIGN fiOrdered:SCREEN-VALUE  = oe-ord.entered-id.
  
       END.         
    END. 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmptyBOL s-object 
PROCEDURE EmptyBOL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:       
        ASSIGN
            fiBolNo:SCREEN-VALUE       = ""
            fiBolDate:SCREEN-VALUE     = ""
            fiBolStatus:SCREEN-VALUE   = ""
            fiRelease:SCREEN-VALUE     = ""
            fiCarrier:SCREEN-VALUE     = ""
            fiSeal:SCREEN-VALUE        = ""
            tb_signed:SCREEN-VALUE     = "No"
            fiCustomer:SCREEN-VALUE    = ""
            fiLoc:SCREEN-VALUE         = ""
            fiTrailer:SCREEN-VALUE     = ""
            tb_posted:SCREEN-VALUE     = "No"
            fiShipTo:SCREEN-VALUE      = ""
            fiRateWt:SCREEN-VALUE      = "" 
            tb_printed:SCREEN-VALUE    = "No"          
            fiTotalWeight:SCREEN-VALUE = ""           
            fiTotalPallet:SCREEN-VALUE = ""
            fiAdd1:SCREEN-VALUE        = ""
            fiAdd2:SCREEN-VALUE        = ""
            fiAdd3:SCREEN-VALUE        = ""
            fiAdd4:SCREEN-VALUE        = ""
            ship_note:SCREEN-VALUE     =  ""
            fiOrdered:SCREEN-VALUE     =  ""            
            . 
     END.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetKeyboard s-object 
PROCEDURE SetKeyboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipoKeyboard AS system.Keyboard NO-UNDO.
    
    oKeyboard = ipoKeyboard.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetShipToExpanded s-object 
PROCEDURE pSetShipToExpanded :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO .

    RUN sys/ref/nk1look.p (ipcCompany, "ShipNotesExpanded", "L" /* Logical */, NO /* check by cust */, 
              INPUT NO /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
              OUTPUT cRtnChar, OUTPUT lRecFound).
    glShipNotesExpanded = LOGICAL(cRtnChar) NO-ERROR.
    
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

