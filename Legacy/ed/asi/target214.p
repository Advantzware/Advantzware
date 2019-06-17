
/*------------------------------------------------------------------------
    File        : target214.p
    Purpose     : 

    Syntax      :

    Description : Exports 214 json data

    Author(s)   : WFK
    Created     : Tue May 21 22:06:29 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER iprBolRecid AS RECID NO-UNDO.
DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cResult AS CHARACTER NO-UNDO.

DEFINE STREAM sOut.
OUTPUT STREAM sOUt TO VALUE(ipcOutputFile).

DEFINE VARIABLE cSendingLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSendingLocation AS INTEGER NO-UNDO.
DEFINE VARIABLE cProNum AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSCAC AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTrailer AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReceiveLocID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstTimeArrival AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInStoreDate AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDetContainerCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE iTotPalletCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE iTotalWeight AS INTEGER NO-UNDO.
DEFINE VARIABLE cStObj AS CHARACTER NO-UNDO INIT "~{".
DEFINE VARIABLE cEndObj AS CHARACTER NO-UNDO INIT "}".
DEFINE VARIABLE cStArray AS CHARACTER NO-UNDO INIT "[".
DEFINE VARIABLE cEndArray AS CHARACTER NO-UNDO INIT "]".
DEFINE VARIABLE cSep AS CHARACTER NO-UNDO INIT ": ".
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fDtTm RETURNS CHARACTER 
	( ipdDate AS DATE ) FORWARD.

FUNCTION fGetSCAC RETURNS CHARACTER 
	( ipcStore AS CHARACTER ) FORWARD.

FUNCTION fInt RETURNS CHARACTER 
	( ipiLevel AS INTEGER ) FORWARD.

FUNCTION fQuoter RETURNS CHARACTER 
	( ipcString AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */

/* ************************  Function Implementations ***************** */


FUNCTION fDtTm RETURNS CHARACTER 
	( ipdDate AS DATE ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
		DEFINE VARIABLE cTime AS CHARACTER NO-UNDO.
		DEFINE VARIABLE cTimeFormatted AS CHARACTER NO-UNDO.
		
        cTime =  string(time, "hh:mm:ss").
        cTimeFormatted = substring(cTime, 1, 2) + substring(cTime, 4, 2) + substring(cTime, 7, 2).
        cResult = STRING(YEAR(ipdDate)) + STRING(MONTH(ipdDate)) + STRING(DAY(ipdDate))
                  + cTimeFormatted.
		RETURN cResult.

		
END FUNCTION.

FUNCTION fGetSCAC RETURNS CHARACTER 
	( ipcStore AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
        FIND FIRST edShipVia NO-LOCK 
            WHERE edShipVia.carrier EQ ipcStore 
            NO-ERROR.
        IF AVAIL edShipVia THEN 
          cResult = edshipVia.carrier-code.
		RETURN cResult.
		
END FUNCTION.

FUNCTION fInt RETURNS CHARACTER 
	( ipiLevel AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose: Indent some # of spaces per level
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
        cResult = FILL(" ", ipiLevel * 4).
		RETURN cResult.


		
END FUNCTION.

FUNCTION fQuoter RETURNS CHARACTER 
	( ipcString AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
        cResult = '"' + ipcString + '"'.
		RETURN cResult.

		
END FUNCTION.

FOR EACH oe-bolh NO-LOCK
    WHERE RECID(oe-bolh) EQ iprBolRecid
/*    ,                                     */
/*    EACH oe-boll NO-LOCK                  */
/*        WHERE oe-boll.b-no EQ oe-bolh.b-no*/
     BREAK BY oe-bolh.ship-id 
           BY oe-bolh.carrier
           BY oe-bolh.trailer
           BY oe-bolh.bol-no  /* Assuming using bol# as the pro# */
     :
    IF FIRST-OF(oe-bolh.bol-no) THEN DO:
        FIND FIRST oe-boll NO-LOCK 
            WHERE oe-boll.b-no EQ oe-bolh.b-no
            NO-ERROR.
        IF AVAIL oe-boll THEN 
            cSendingLocation = oe-boll.loc.
        PUT STREAM sOut UNFORMATTED fInt(0) cStObj SKIP.
            PUT STREAM sOut UNFORMATTED fInt(1) fQuoter("non_retail_shipment") + cSep     + cStObj SKIP.
                PUT STREAM sOut UNFORMATTED fInt(2) fQuoter("sending_location_id") + cSep + fQuoter(cSendingLocation) + "," SKIP.
                PUT STREAM sOut UNFORMATTED fInt(2) fQuoter("proNumber") + cSep           + fQuoter(STRING(oe-bolh.bol-no)) + "," SKIP.
                PUT STREAM sOut UNFORMATTED fInt(2) fQuoter("scac") + cSep                + fQuoter(fGetSCAC(oe-bolh.carrier)) + "," SKIP.
                PUT STREAM sOut UNFORMATTED fInt(2) fQuoter("trailer_id") + cSep          + fQuoter(oe-bolh.trailer) + "," SKIP.
                   PUT STREAM sOut UNFORMATTED fInt(2) fQuoter("non_retail_store_shipment") + cSep + cStArray SKIP.
                      PUT STREAM sOut UNFORMATTED fInt(2) cStObj SKIP.
                         PUT STREAM sOut UNFORMATTED fInt(3) fQuoter("receiving_location_id") + cSep     + fQuoter(oe-bolh.ship-id) + "," SKIP.
                         PUT STREAM sOut UNFORMATTED fInt(3) fQuoter("estimated_time_of_arrival") + cSep + fDtTm(oe-bolh.bol-date + 2) + "," SKIP.
                         PUT STREAM sOut UNFORMATTED fInt(3) fQuoter("in_store_date")             + cSep + fDtTm(oe-bolh.bol-date + 4) + "," SKIP.
                         PUT STREAM sOut UNFORMATTED fInt(3) fQuoter("detatched_container_count") + cSep + STRING(oe-bolh.tot-pallets) + "," SKIP.
                         PUT STREAM sOut UNFORMATTED fInt(3) fQuoter("total_pallet_count") + cSep        + STRING(oe-bolh.tot-pallets) + "," SKIP.
                         PUT STREAM sOut UNFORMATTED fInt(3) fQuoter("total_weight") + cSep              + STRING(INTEGER(oe-bolh.tot-wt)) SKIP.
                      PUT STREAM sOut UNFORMATTED fInt(2) cEndObj SKIP.
                   PUT STREAM sOut UNFORMATTED fInt(2) cEndArray  SKIP.
            PUT STREAM sOut UNFORMATTED fInt(1) cEndObj SKIP.
        PUT STREAM sOut UNFORMATTED fInt(0) cEndObj SKIP .
    END.
END.
OUTPUT STREAM sOut CLOSE.