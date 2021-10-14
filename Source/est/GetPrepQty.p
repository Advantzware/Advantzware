&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : est/GetPrepQty.p  
    Purpose     : Gets Prep Qty based on prep material type

    Syntax      :

    Description : Gets the initial qty for a estimate prep

    Author(s)   : BV
    Created     : 10/21/2014
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipri-est   AS   ROWID          NO-UNDO.
DEFINE INPUT PARAMETER ipcMatType AS   CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER ipiFormNo  LIKE est-prep.s-num NO-UNDO.

DEFINE OUTPUT PARAMETER opdQty    LIKE est-prep.qty   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE BUFFER bf-est FOR est.
DEFINE BUFFER bf-ef  FOR ef.
DEFINE BUFFER bf-eb  FOR eb.

DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

FIND bf-est 
    WHERE ROWID(bf-est) EQ ipri-est 
    NO-LOCK NO-ERROR.

IF AVAIL bf-est THEN 
DO:
    FOR FIRST bf-ef
        WHERE bf-ef.company EQ bf-est.company
        AND bf-ef.est-no EQ bf-est.est-no
        AND bf-ef.form-no EQ ipiFormNo
        NO-LOCK:
        CASE ipcMatType:
            WHEN "B" THEN 
                opdQty = bf-ef.nsh-wid * bf-ef.nsh-len.
            WHEN "R" THEN
                opdQty  = bf-ef.die-in.
            WHEN "P" OR 
            WHEN "F" THEN
                DO:
                    IF bf-est.est-type NE 4 AND bf-est.est-type NE 8 THEN
                        FOR EACH bf-eb 
                            WHERE bf-eb.company EQ bf-ef.company
                            AND bf-eb.est-no  EQ bf-ef.est-no
                            AND bf-eb.form-no EQ bf-ef.form-no
                            NO-LOCK 
                            BREAK BY bf-eb.form-no:
                            /*sum up the coating and color counts for blanks*/
                            opdQty = opdQty + 
                                (IF bf-est.est-type NE 3 OR FIRST(bf-eb.form-no) THEN
                                bf-eb.i-coat + bf-eb.i-col 
                                ELSE bf-eb.yld-qty).
                        END. /*each bf-eb*/
                    ELSE /*If a combo just add form coating and form color fields*/
                        opdQty = opdQty + bf-ef.f-coat + bf-ef.f-col.
                    
                    /*subtract out any aqueous ink types from the count*/
                    FOR EACH bf-eb 
                        WHERE bf-eb.company EQ bf-ef.company 
                        AND bf-eb.est-no  EQ bf-ef.est-no
                        AND bf-eb.form-no EQ bf-ef.form-no
                        NO-LOCK 
                        BREAK BY bf-eb.form-no:
                        IF bf-eb.est-type LE 4 THEN
                        DO iCount = 1 TO 20:
                            IF CAN-FIND(FIRST item 
                                WHERE item.company  EQ bf-eb.company
                                AND item.i-no     EQ bf-eb.i-code2[iCount]
                                AND item.ink-type EQ 'A') THEN 
                                opdQty = opdQty - 1.
                        END.
                        ELSE
                        DO iCount = 1 TO 10:
                            IF CAN-FIND(FIRST item 
                                WHERE item.company  EQ bf-eb.company
                                AND item.i-no     EQ bf-eb.i-code[iCount]
                                AND item.ink-type EQ "A") THEN 
                                opdQty = opdQty - 1.
                        END.
                    END. /*each bf-eb*/
                    IF opdQty LT 0 THEN opdQty = 0.        
                END.
            OTHERWISE 
                opdQty = 1.
        END CASE. /*case*/
    END. /*for first bf-ef*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetQtyB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetQtyB Procedure 
/*PROCEDURE GetQtyB :                                                             */
/*/*------------------------------------------------------------------------------*/
/*  Purpose: Get Qty for Wood Die Board                                           */
/*  Parameters:  ef buffer , output qty                                           */
/*  Notes:                                                                        */
/*------------------------------------------------------------------------------*/*/
/*DEFINE PARAMETER BUFFER ipbf-ef FOR ef.                                         */
/*DEFINE OUTPUT PARAMETER opdQtyB LIKE est-prep.qty NO-UNDO.                      */
/*                                                                                */
/*IF AVAIL ipbf-ef THEN                                                           */
/*    opdQtyB = ipbf-ef.nsh-wid * ipbf-ef.nsh-len.                                */
/*                                                                                */
/*END PROCEDURE.                                                                  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetQtyP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetQtyP Procedure 
/*PROCEDURE GetQtyP :                                                             */
/*/*------------------------------------------------------------------------------*/
/*  Purpose:  Calculate Qty for Print Plates                                      */
/*  Parameters:  est, ef, output qty                                              */
/*  Notes:                                                                        */
/*------------------------------------------------------------------------------*/*/
/*DEFINE PARAMETER BUFFER ipbf-est FOR est.                                       */
/*DEFINE PARAMETER BUFFER ipbf-ef FOR ef.                                         */
/*DEFINE OUTPUT PARAMETER opiQtyP LIKE est-prep.qty NO-UNDO.                      */
/*                                                                                */
/*DEFINE BUFFER bf-eb FOR eb.                                                     */
/*DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.                                  */
/*                                                                                */
/*IF AVAIL ipbf-est AND AVAIL ipbf-ef THEN DO:                                    */
/*                                                                                */
/*    /*If not a combo*/                                                          */
/*    IF ipbf-est.est-type NE 4                                                   */
/*        AND ipbf-est.est-type NE 8 THEN                                         */
/*        FOR EACH bf-eb                                                          */
/*            WHERE bf-eb.company EQ ipbf-ef.company                              */
/*              AND bf-eb.est-no  EQ ipbf-ef.est-no                               */
/*              AND bf-eb.form-no EQ ipbf-ef.form-no                              */
/*        NO-LOCK                                                                 */
/*        BREAK BY bf-eb.form-no:                                                 */
/*            /*sum up the coating and color counts for blanks*/                  */
/*            opiQtyP = opiQtyP +                                                 */
/*                (IF ipbf-est.est-type NE 3 OR FIRST(bf-eb.form-no) THEN         */
/*                   bf-eb.i-coat + bf-eb.i-col ELSE bf-eb.yld-qty).              */
/*        END. /*each bf-eb*/                                                     */
/*    ELSE /*If a combo just add form coating and form color fields*/             */
/*        opiQtyP = opiQtyP + ipbf-ef.f-coat + ipbf-ef.f-col.                     */
/*                                                                                */
/*    /*subtract out any aqueous ink types from the count*/                       */
/*    FOR EACH bf-eb                                                              */
/*        WHERE bf-eb.company EQ ipbf-ef.company                                  */
/*          AND bf-eb.est-no  EQ ipbf-ef.est-no                                   */
/*          AND bf-eb.form-no EQ ipbf-ef.form-no                                  */
/*        NO-LOCK                                                                 */
/*        BREAK BY bf-eb.form-no:                                                 */
/*            IF bf-eb.est-type LE 4 THEN                                         */
/*                DO iCount = 1 TO 20:                                            */
/*                    IF CAN-FIND(FIRST item                                      */
/*                                    WHERE item.company  EQ bf-eb.company        */
/*                                      AND item.i-no     EQ bf-eb.i-code2[iCount]*/
/*                                      AND item.ink-type EQ 'A') THEN            */
/*                        opiQtyP = opiQtyP - 1.                                  */
/*                END.                                                            */
/*            ELSE                                                                */
/*                DO iCount = 1 TO 10:                                            */
/*                    IF CAN-FIND(FIRST item                                      */
/*                                    WHERE item.company  EQ bf-eb.company        */
/*                                      AND item.i-no     EQ bf-eb.i-code[iCount] */
/*                                      AND item.ink-type EQ "A") THEN            */
/*                        opiQtyP = opiQtyP - 1.                                  */
/*                END.                                                            */
/*    END. /*each bf-eb*/                                                         */
/*    IF opiQtyP LT 0 THEN opiQtyP = 0.                                           */
/*                                                                                */
/*END. /*avail ipbf-est and ipbf-ef*/                                             */
/*                                                                                */
/*END PROCEDURE.                                                                  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetQtyR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetQtyR Procedure 
/*PROCEDURE GetQtyR :                                                             */
/*/*------------------------------------------------------------------------------*/
/*  Purpose: Get Qty for Rotary Dies                                              */
/*  Parameters:  ef buffer , output qty                                           */
/*  Notes:                                                                        */
/*------------------------------------------------------------------------------*/*/
/*DEFINE PARAMETER BUFFER ipbf-ef FOR ef.                                         */
/*DEFINE OUTPUT PARAMETER opdQtyR LIKE est-prep.qty NO-UNDO.                      */
/*                                                                                */
/*IF AVAIL ipbf-ef THEN                                                           */
/*    opdQtyR  = ipbf-ef.die-in.                                                  */
/*                                                                                */
/*END PROCEDURE.                                                                  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

