{rc/loginv.i}
/*  RPro EDI Shared Variables */
DEFINE {1} SHARED VARIABLE ws_company   LIKE edco.company   NO-UNDO LABEL "Company".
DEFINE {1} SHARED VARIABLE ws_partner   LIKE edmast.partner NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_partner_grp LIKE edmast.partnerGrp NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_docid     LIKE eddoc.docid    NO-UNDO
    LABEL "DocID".
DEFINE {1} SHARED VARIABLE ws_setid     LIKE edcode.setid   NO-UNDO
    LABEL "Set".
DEFINE {1} SHARED VARIABLE ws_direction LIKE edcode.direction NO-UNDO
    LABEL "Dir".
DEFINE {1} SHARED VARIABLE ws_print-opt AS LOGICAL NO-UNDO
        LABEL "Print?" INITIAL TRUE.

DEFINE {1} SHARED VARIABLE ws_edco_rec      AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edmast_rec    AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edcode_rec    AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_eddoc_rec     AS RECID NO-UNDO.

DEFINE {1} SHARED VARIABLE ws_edpotran_rec  AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edpoline_rec  AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edpoaddon_rec AS RECID NO-UNDO.

DEFINE {1} SHARED VARIABLE ws_edivtran_rec  AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edivline_rec  AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edivaddon_rec AS RECID NO-UNDO.

DEFINE {1} SHARED VARIABLE ws_edshtran_rec  AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edshord_rec   AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edshpack_rec  AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edshtare_rec  AS RECID NO-UNDO.
DEFINE {1} SHARED VARIABLE ws_edshline_rec  AS RECID NO-UNDO.

DEFINE {1} SHARED VARIABLE ws_edi_path  LIKE edco.path-in   NO-UNDO LABEL "EDI Path".

/* 9704 CAH: If set true then 856 is generated simultaneous with invoice */
DEFINE {1} SHARED VARIABLE ws_856_from_invoice AS LOGICAL NO-UNDO INITIAL FALSE.



/* ************************  Function Prototypes ********************** */

FUNCTION fCheckFolderOfPath RETURNS LOGICAL 
	(INPUT ipcPath AS CHARACTER  ) FORWARD.

FUNCTION fOutputFileName RETURNS CHARACTER 
	(  ) FORWARD.


/* ************************  Function Implementations ***************** */

FUNCTION fCheckFolderOfPath RETURNS LOGICAL 
	(INPUT ipcPath AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cFolder AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lResult AS LOGICAL   NO-UNDO.

    cFolder = REPLACE(ipcPath,"~\", "/"). 
   
    IF INDEX(cFolder, "/") GT 0 THEN
        cFolder = SUBSTRING(cFolder, 1, R-INDEX(cFolder, "/") - 1). 
    ELSE
        cFolder = ".".
     
    /* Use \ for WIN, / for Linux */
    IF OPSYS BEGINS "win" THEN  
        cFolder = REPLACE(cFolder, "/", "\").
   
    FILE-INFO:FILE-NAME = cFolder.
  
    IF   NOT FILE-INFO:FILE-TYPE BEGINS "D" OR FILE-INFO:FILE-TYPE EQ ? THEN 
    DO:       
        OS-COMMAND SILENT "md " VALUE(cFolder).        
    END.  

    FILE-INFO:FILE-NAME = cFolder.

    lResult = FILE-INFO:FILE-TYPE BEGINS "D".
    IF lResult EQ ? THEN
      lResult = NO.
  
    RETURN lResult.
		
END FUNCTION.

FUNCTION fOutputFileName RETURNS CHARACTER 
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

   DEFINE VARIABLE cresult AS CHARACTER NO-UNDO.
   cresult = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") 
        + string(DAY(TODAY), "99") + STRING(TIME) + string(RANDOM(1, 10000)).
		RETURN cresult.


		
END FUNCTION.
