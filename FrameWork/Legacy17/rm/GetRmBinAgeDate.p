&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : rm/GetRMBinAgeDate.p
    Purpose     : Retrieve the age of a given RM Bin

    Syntax      :  Run rm/GetRMBinAgeDate.p(input rowid(rm-bin,
                                            output age-date).

    Description :

    Author(s)   : BV
    Created     : 12/12/14
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriRmBin AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER opdtAgeDate AS DATE NO-UNDO.

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
DEFINE BUFFER bf-rm-bin FOR rm-bin.
DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
DEFINE BUFFER bf-reftable FOR reftable.

FIND FIRST bf-rm-bin 
    WHERE ROWID(bf-rm-bin) EQ ipriRmBin
    NO-LOCK NO-ERROR.

IF NOT AVAIL bf-rm-bin THEN RETURN.

FIND FIRST bf-reftable NO-LOCK
    WHERE bf-reftable.reftable EQ "rm-bin.age-date"
      AND bf-reftable.company  EQ bf-rm-bin.company
      AND bf-reftable.loc      EQ bf-rm-bin.i-no
      AND bf-reftable.code     EQ STRING(bf-rm-bin.loc,"x(50)") +
                                  STRING(bf-rm-bin.loc-bin,"x(50)")
      AND bf-reftable.code2    EQ bf-rm-bin.tag
    NO-ERROR.

IF AVAIL bf-reftable THEN 
    opdtAgeDate = DATE(INT(bf-reftable.val[1])).
ELSE
    IF TRIM(bf-rm-bin.tag) EQ "" THEN DO:
    
        /* Match loc, loc-bin, PO and blank tag */      
        FOR EACH bf-rm-rcpth NO-LOCK
            WHERE bf-rm-rcpth.company   EQ bf-rm-bin.company
              AND bf-rm-rcpth.i-no      EQ bf-rm-bin.i-no
              AND bf-rm-rcpth.rita-code EQ "R"
              AND bf-rm-rcpth.po-no     EQ string(bf-rm-bin.po-no)
            USE-INDEX i-no,
            EACH bf-rm-rdtlh NO-LOCK
            WHERE bf-rm-rdtlh.r-no    EQ bf-rm-rcpth.r-no
              AND bf-rm-rdtlh.loc     EQ bf-rm-bin.loc
              AND bf-rm-rdtlh.loc-bin EQ bf-rm-bin.loc-bin
              AND bf-rm-rdtlh.tag     EQ bf-rm-bin.tag
            USE-INDEX rm-rdtl
            BREAK BY bf-rm-rcpth.trans-date
            BY bf-rm-rdtlh.rec_key 
            BY bf-rm-rcpth.r-no:
            LEAVE.
        END.

        /* Match on PO # (non-blank or blank), loc, bin */
        IF NOT AVAIL bf-rm-rcpth AND bf-rm-bin.po-no GT 0 THEN DO:
          FOR EACH bf-rm-rcpth NO-LOCK
              WHERE bf-rm-rcpth.company   EQ bf-rm-bin.company
                AND bf-rm-rcpth.i-no      EQ bf-rm-bin.i-no
                AND bf-rm-rcpth.po-no     EQ string(bf-rm-bin.po-no)
                AND bf-rm-rcpth.rita-code EQ "R"
              USE-INDEX i-no,
              EACH bf-rm-rdtlh NO-LOCK
              WHERE bf-rm-rdtlh.r-no    EQ bf-rm-rcpth.r-no
                AND bf-rm-rdtlh.loc     EQ bf-rm-bin.loc
                AND bf-rm-rdtlh.loc-bin EQ bf-rm-bin.loc-bin
              USE-INDEX rm-rdtl
              BREAK BY bf-rm-rcpth.trans-date
              BY bf-rm-rdtlh.rec_key 
              BY bf-rm-rcpth.r-no:
       
              LEAVE.
          END.
        END.

        /* Match on PO # (non-blank or blank) only */
        IF NOT AVAIL bf-rm-rcpth AND bf-rm-bin.po-no GT 0 THEN DO:
          FOR EACH bf-rm-rcpth NO-LOCK
              WHERE bf-rm-rcpth.company   EQ bf-rm-bin.company
                AND bf-rm-rcpth.i-no      EQ bf-rm-bin.i-no
                AND bf-rm-rcpth.po-no     EQ string(bf-rm-bin.po-no)
                AND bf-rm-rcpth.rita-code EQ "R"
              USE-INDEX i-no,
              EACH bf-rm-rdtlh NO-LOCK
              WHERE bf-rm-rdtlh.r-no    EQ bf-rm-rcpth.r-no
              USE-INDEX rm-rdtl
              BREAK BY bf-rm-rcpth.trans-date
              BY bf-rm-rdtlh.rec_key 
              BY bf-rm-rcpth.r-no:
            
              LEAVE.
          END.
        END.

        /* Match just loc, loc-bin Only */
        IF NOT AVAIL bf-rm-rcpth THEN DO:
          FOR EACH bf-rm-rcpth NO-LOCK
              WHERE bf-rm-rcpth.company   EQ bf-rm-bin.company
                AND bf-rm-rcpth.i-no      EQ bf-rm-bin.i-no
                AND bf-rm-rcpth.rita-code EQ "R"
              USE-INDEX i-no,
              EACH bf-rm-rdtlh NO-LOCK
              WHERE bf-rm-rdtlh.r-no    EQ bf-rm-rcpth.r-no
                AND bf-rm-rdtlh.loc     EQ bf-rm-bin.loc
                AND bf-rm-rdtlh.loc-bin EQ bf-rm-bin.loc-bin
              USE-INDEX rm-rdtl
              BREAK BY bf-rm-rcpth.trans-date
              BY bf-rm-rdtlh.rec_key 
              BY bf-rm-rcpth.r-no:
            
              LEAVE.
          END.
        END.

    END.
    ELSE
        FOR EACH bf-rm-rdtlh NO-LOCK
            WHERE bf-rm-rdtlh.company   EQ bf-rm-bin.company
              AND bf-rm-rdtlh.tag       EQ bf-rm-bin.tag
              AND bf-rm-rdtlh.rita-code EQ "R"
            USE-INDEX tag,
            FIRST bf-rm-rcpth
            WHERE bf-rm-rcpth.r-no    EQ bf-rm-rdtlh.r-no
              AND bf-rm-rcpth.i-no    EQ bf-rm-bin.i-no
            USE-INDEX r-no
            BREAK BY bf-rm-rcpth.trans-date
            BY bf-rm-rdtlh.rec_key 
            BY bf-rm-rcpth.r-no:
            LEAVE.
        END.
   
IF AVAIL bf-rm-rcpth THEN 
    opdtAgeDate = bf-rm-rcpth.trans-date.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


