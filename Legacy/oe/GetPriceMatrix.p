&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : BusinessLogic/OrderEntry/GetPriceMatrix.p 
    Purpose     : Set a buffer for the appropriate price matrix,
                  based on item and customer entered.

    Syntax      : Run GetPriceMatrix(BUFFER itemfg, 
                                     BUFFER cust, 
                                     BUFFER oe-prmtx,
                                     INPUT lInitialCheckOnly,
                                     OUTPUT lMatrixExists)

    Description :

    Author(s)   :  BV
    Created     :  1/14
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*main oe-prmtx buffer - will be returned avail if found*/
DEFINE PARAMETER BUFFER ipbf-oe-prmtx FOR oe-prmtx.

/*Qualifiers for finding the right matrix*/
DEFINE INPUT PARAMETER ipri-itemfg AS ROWID.
DEFINE INPUT PARAMETER ipri-cust AS ROWID.

/*for only getting initial exact match in the case of LASTPRICE method*/
DEFINE INPUT PARAMETER iplInitialCheckOnly AS LOG NO-UNDO.

/*output*/
DEFINE OUTPUT PARAMETER oplMatrixExists AS LOG NO-UNDO.

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

DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-cust FOR cust.

FIND FIRST bf-itemfg
    WHERE ROWID(bf-itemfg) EQ ipri-itemfg
    NO-LOCK NO-ERROR.
FIND FIRST bf-cust
    WHERE ROWID(bf-cust) EQ ipri-cust
    NO-LOCK NO-ERROR.

IF NOT AVAIL bf-itemfg OR NOT AVAIL bf-cust THEN RETURN.

FOR EACH ipbf-oe-prmtx
    WHERE ipbf-oe-prmtx.company EQ bf-itemfg.company
      AND ipbf-oe-prmtx.custype EQ bf-cust.TYPE
      AND ipbf-oe-prmtx.cust-no EQ bf-cust.cust-no
      AND (ipbf-oe-prmtx.procat EQ bf-itemfg.procat 
           OR SUBSTR(ipbf-oe-prmtx.i-no,01,100) NE "")
      AND ipbf-oe-prmtx.i-no BEGINS bf-itemfg.i-no
      AND SUBSTR(ipbf-oe-prmtx.i-no,01,100) EQ bf-itemfg.i-no
      AND ipbf-oe-prmtx.eff-date LE TODAY
      AND (ipbf-oe-prmtx.exp-date GE TODAY OR ipbf-oe-prmtx.exp-date EQ ?
            OR ipbf-oe-prmtx.exp-date EQ 01/01/0001 )
    BY ipbf-oe-prmtx.eff-date DESC:
    LEAVE.
END.

IF NOT iplInitialCheckOnly THEN DO:
    IF NOT AVAIL ipbf-oe-prmtx THEN
        FOR EACH ipbf-oe-prmtx
            WHERE ipbf-oe-prmtx.company EQ bf-itemfg.company
              AND ipbf-oe-prmtx.custype EQ bf-cust.TYPE
              AND ipbf-oe-prmtx.cust-no EQ ""
              AND (ipbf-oe-prmtx.procat EQ bf-itemfg.procat 
                OR bf-itemfg.i-no NE "")
              AND ipbf-oe-prmtx.i-no BEGINS bf-itemfg.i-no
              AND SUBSTR(ipbf-oe-prmtx.i-no,01,100) EQ bf-itemfg.i-no
              AND ipbf-oe-prmtx.eff-date LE TODAY
              AND (ipbf-oe-prmtx.exp-date GE TODAY OR ipbf-oe-prmtx.exp-date EQ ?
                    OR ipbf-oe-prmtx.exp-date EQ 01/01/0001 )
            BY ipbf-oe-prmtx.eff-date DESC:            
            LEAVE.
        END.

    IF NOT AVAIL ipbf-oe-prmtx THEN
        FOR EACH ipbf-oe-prmtx
            WHERE ipbf-oe-prmtx.company EQ bf-itemfg.company
              AND ipbf-oe-prmtx.custype EQ bf-cust.TYPE
              AND ipbf-oe-prmtx.cust-no EQ ""
              AND ipbf-oe-prmtx.procat EQ bf-itemfg.procat
              AND SUBSTR(ipbf-oe-prmtx.i-no,01,100) EQ ""
              AND ipbf-oe-prmtx.eff-date LE TODAY
              AND (ipbf-oe-prmtx.exp-date GE TODAY OR ipbf-oe-prmtx.exp-date EQ ?
                    OR ipbf-oe-prmtx.exp-date EQ 01/01/0001 )
            BY ipbf-oe-prmtx.eff-date DESC:            
            LEAVE.
        END.

    IF NOT AVAIL ipbf-oe-prmtx THEN
        FOR EACH ipbf-oe-prmtx
            WHERE ipbf-oe-prmtx.company EQ bf-itemfg.company
              AND ipbf-oe-prmtx.custype EQ ""
              AND ipbf-oe-prmtx.cust-no EQ ""
              AND (ipbf-oe-prmtx.procat EQ bf-itemfg.procat 
                OR SUBSTR(ipbf-oe-prmtx.i-no,01,100) NE "")
              AND ipbf-oe-prmtx.i-no BEGINS bf-itemfg.i-no
              AND SUBSTR(ipbf-oe-prmtx.i-no,01,100) EQ bf-itemfg.i-no
              AND ipbf-oe-prmtx.eff-date LE TODAY
              AND (ipbf-oe-prmtx.exp-date GE TODAY OR ipbf-oe-prmtx.exp-date EQ ?
                   OR ipbf-oe-prmtx.exp-date EQ 01/01/0001 )
            BY ipbf-oe-prmtx.eff-date DESC:            
            LEAVE.
        END.

    IF NOT AVAIL ipbf-oe-prmtx THEN
        FOR EACH ipbf-oe-prmtx
            WHERE ipbf-oe-prmtx.company EQ bf-itemfg.company
              AND ipbf-oe-prmtx.custype EQ ""
              AND ipbf-oe-prmtx.cust-no EQ ""
              AND ipbf-oe-prmtx.procat EQ ""
              AND ipbf-oe-prmtx.i-no BEGINS bf-itemfg.i-no
              AND SUBSTR(ipbf-oe-prmtx.i-no,01,100) EQ bf-itemfg.i-no
              AND ipbf-oe-prmtx.eff-date LE TODAY
              AND (ipbf-oe-prmtx.exp-date GE TODAY OR ipbf-oe-prmtx.exp-date EQ ?
                    OR ipbf-oe-prmtx.exp-date EQ 01/01/0001 )
            BY ipbf-oe-prmtx.eff-date DESC:            
            LEAVE.
        END.

END.
oplMatrixExists = AVAIL(ipbf-oe-prmtx).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


