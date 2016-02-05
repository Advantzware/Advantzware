&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : fg/ValidFgRcptTag.p
    Purpose     : Validate that a tag is valid for a new receipt.  
                  Must handle cases where tags are added to the Set Parts
                  browse as well

    Syntax      :

    Description :

    Author(s)   : BPV
    Created     : 08/09/2013
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAM ipri-fg-rctd AS ROWID. /*rowid of subject record*/
DEFINE INPUT PARAM ipcTag AS CHAR NO-UNDO.  /*tag to validate*/
DEFINE INPUT PARAM ipiQty AS INT NO-UNDO.
DEFINE INPUT PARAM ipcCocode AS CHAR NO-UNDO. /*company code*/
DEFINE INPUT PARAM iplAllowBlankTag AS LOG NO-UNDO.

DEFINE OUTPUT PARAM oplValid AS LOG NO-UNDO.
DEFINE OUTPUT PARAM opcMessage AS CHAR NO-UNDO.
DEFINE OUTPUT PARAM opcJobNo AS CHAR NO-UNDO.
DEFINE OUTPUT PARAM opcJobNo2 AS CHAR NO-UNDO.
DEFINE OUTPUT PARAM opcLoc AS CHAR NO-UNDO.
DEFINE OUTPUT PARAM opcLocBin AS CHAR NO-UNDO.

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

IF ipcTag NE "" THEN DO:
    oplValid = YES.
    RUN ValidateSetPartQtys.
END.
ELSE IF iplAllowBlankTag THEN oplValid = YES.
ELSE ASSIGN 
    opcMessage = "Tag # Cannot Be Blank"
    oplValid = NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ValidateSetPartQtys) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateSetPartQtys Procedure 
PROCEDURE ValidateSetPartQtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-fg-rctd FOR fg-rctd.
DEF BUFFER bf-fg-bin FOR fg-bin.

DEF VAR iTotalQty AS INT NO-UNDO.

FIND FIRST bf-fg-bin 
        WHERE bf-fg-bin.company EQ ipcCocode
            AND bf-fg-bin.tag   EQ ipcTag 
            AND bf-fg-bin.qty   GT 0 NO-LOCK NO-ERROR.
IF AVAIL bf-fg-bin THEN
    ASSIGN
        opcJobNo = bf-fg-bin.job-no
        opcJobNo2 = string(bf-fg-bin.job-no2)
        opcLoc = bf-fg-bin.loc
        opcLocBin = bf-fg-bin.loc-bin.
ELSE
    ASSIGN
        opcMessage = "Invalid Tag#, try help or scan valid tag#"
        oplValid = NO.
               
/* check for assembled and unassembled set parts on-hand or pending receipt*/
/* IF ipiQty < 0 THEN DO:                                      */
/*     iTotalQty = 0.                                          */
/*     FOR EACH bf-fg-rctd                                     */
/*         WHERE bf-fg-rctd.company EQ ipcCocode               */
/*              AND bf-fg-rctd.tag     EQ ipcTag               */
/*              AND ROWID(bf-fg-rctd) NE ipri-fg-rctd NO-LOCK: */
/*         iTotalQty = iTotalQty + bf-fg-rctd.t-qty.           */
/*     END.                                                    */
/*     IF AVAIL bf-fg-bin THEN                                 */
/*         ASSIGN                                              */
/*             iTotalQty = iTotalQty + bf-fg-bin.qty.          */
/*     IF oplValid AND iTotalQty LT ABS(ipiQty) THEN           */
/*         ASSIGN                                              */
/*             opcMessage = "Insufficient quantity in bin"     */
/*             oplValid = NO.                                  */
/*   END.                                                      */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

