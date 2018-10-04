&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiOrdNo AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipcNewPo    LIKE oe-ordl.po-no NO-UNDO.
DEFINE INPUT  PARAMETER ipcINo     AS CHARACTER   NO-UNDO.

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

RUN mainProcess (INPUT ipcCompany,
                 INPUT ipiOrdNo,
                 INPUT ipcINo,
                 INPUT ipcNewPO).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-mainProcess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mainProcess Procedure 
PROCEDURE mainProcess :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
   No po-no on oe-relh 
   po-no on oe-bolh is always blank 
   No po-no on inv-head 
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiOrdNo AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipcIno AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcNewPo AS CHARACTER   NO-UNDO.

DEF BUFFER bf-oe-rel FOR oe-rel.
DEF BUFFER bf-oe-rell FOR oe-rell.
DEF BUFFER bf-oe-boll FOR oe-boll.
DEF BUFFER bf-inv-line FOR inv-line.
DEF BUFFER bf-oe-bolh FOR oe-bolh.
DEF BUFFER bf-inv-head FOR inv-head.

/* Starting with oe-boll for index reasons */
FOR EACH bf-oe-boll 
  WHERE bf-oe-boll.company EQ ipcCompany
    AND bf-oe-boll.ord-no  EQ ipiOrdNo
    AND (IF ipcIno GT "" THEN bf-oe-boll.i-no EQ ipcIno ELSE TRUE)
  EXCLUSIVE-LOCK,
  FIRST bf-oe-bolh 
    WHERE bf-oe-bolh.b-no EQ bf-oe-boll.b-no
      AND bf-oe-bolh.posted EQ TRUE
    NO-LOCK
  BREAK BY bf-oe-boll.bol-no:
    
    FOR EACH bf-inv-head 
      WHERE bf-inv-head.company EQ ipcCompany
        AND bf-inv-head.bol-no  EQ bf-oe-boll.bol-no
      NO-LOCK,
      EACH bf-inv-line
        WHERE bf-inv-line.r-no EQ bf-inv-head.r-no
          AND bf-inv-line.ord-no  EQ ipiOrdNo
          AND (IF ipcIno GT "" THEN bf-inv-line.i-no EQ ipcIno ELSE TRUE)
          AND bf-inv-line.b-no EQ bf-oe-boll.b-no
        EXCLUSIVE-LOCK.
          
       IF bf-inv-line.b-no EQ bf-oe-boll.b-no THEN DO:
           
          ASSIGN bf-inv-line.po-no = ipcNewPo
                 bf-oe-boll.po-no = ipcNewPo.
          FOR EACH bf-oe-rell 
            WHERE bf-oe-rell.company EQ ipcCompany
              AND bf-oe-rell.ord-no  EQ ipiOrdNo
              AND (IF ipcIno GT "" THEN bf-oe-rell.i-no EQ ipcIno ELSE TRUE)
              AND bf-oe-rell.r-no EQ bf-oe-boll.r-no
            EXCLUSIVE-LOCK.
              
              bf-oe-rell.po-no = ipcNewPo.
              FOR EACH bf-oe-rel 
                WHERE bf-oe-rel.company EQ ipcCompany
                  AND bf-oe-rel.ord-no  EQ ipiOrdNo
                  AND (IF ipcIno GT "" THEN bf-oe-rel.i-no EQ ipcIno ELSE TRUE)
                  AND INDEX("SIL", bf-oe-rel.stat) EQ 0
                  AND bf-oe-rel.r-no EQ bf-oe-rell.link-no
                EXCLUSIVE-LOCK.                  
                  bf-oe-rel.po-no = ipcNewPo.
              END. /* each bf-oe-rel */
          END. /* each bf-oe-rell */
       END.  /* if inv-line.b-no eq oe-boll.b-no */
    END. /* for each inv-head */
END. /* each oe-boll */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

