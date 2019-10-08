/*------------------------------------------------------------------------
    File        : edi/sp856xml.p  SOPI ASN
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Feb 28 09:07:49 EST 2017
    Notes       :
        
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER ipBOLNumber AS INTEGER.
DEFINE INPUT PARAMETER ipPartner AS CHARACTER.
DEFINE INPUT PARAMETER ipSeq AS INTEGER.
   
DEFINE VARIABLE hSAXWriter AS HANDLE  NO-UNDO.
DEFINE VARIABLE lOK        AS LOGICAL NO-UNDO.
DEFINE VARIABLE iHlCnt     AS INTEGER NO-UNDO.
DEFINE VARIABLE iLineCnt   AS INTEGER NO-UNDO.
DEFINE VARIABLE dQtyTotal  AS DECIMAL NO-UNDO.
DEFINE VARIABLE cQtyHash   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iQtyHash   AS INTEGER NO-UNDO.
DEFINE VARIABLE cXMLOutputfile AS CHARACTER NO-UNDO.


    
FUNCTION getDate RETURNS CHARACTER (ipDate AS DATE):
  DEFINE VARIABLE cDate AS CHARACTER.
  IF ipDate EQ ? THEN 
    cDate = "".
  ELSE 
    cDate = STRING(YEAR(ipDate), "9999") 
            + string(MONTH(ipDate), "99")
            + string(DAY(ipDate), "99").
  RETURN cDate.
END FUNCTION.

FUNCTION numericPart RETURNS INTEGER (ipcNumStr AS CHARACTER):
    DEFINE VARIABLE iNumeric AS INTEGER.
    DEFINE VARIABLE cNumerStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iX AS INTEGER NO-UNDO.
    IF ipcNumStr EQ ? THEN 
        iNumeric = 0.
    ELSE DO:
        cNumerStr = "".
        DO iX = 1 TO LENGTH(ipcNumStr):
            IF INDEX("0123456789", SUBSTRING(ipcNumStr, iX, 1)) GT 0 THEN 
               cNumerStr = cNumerStr + SUBSTRING(ipcNumStr, iX, 1).
        END.
        iNumeric = INTEGER(TRIM(cNumerStr, "0")).
    END.
    RETURN iNumeric.
END FUNCTION.

FIND FIRST edmast  NO-LOCK
  WHERE edmast.partner EQ ipPartner
  NO-ERROR. 
IF AVAILABLE edmast THEN 
  FIND FIRST edcode NO-LOCK 
    WHERE edcode.partner EQ edmast.partnerGrp
      AND EDCode.SetID EQ "856"
    NO-ERROR.
cXMLOutputfile = (IF AVAILABLE edcode AND edcode.path-out GT "" THEN EDCode.Path-out ELSE "c:\tmp:\").
IF SUBSTRING(cXmlOutputFile, LENGTH(cXmlOutputFile), 1) NE "\" THEN 
  cXmlOutputFile = cXmloutputFile + "\".
cXmlOutputfile = cXmlOutputFile + "GE856_BOL#_" + string(ipBOLNumber) + ".xml".
    
CREATE SAX-WRITER hSAXWriter.
hSAXWriter:FORMATTED = TRUE.

lOK = hSAXWriter:SET-OUTPUT-DESTINATION("file", cXmlOutputfile).
lOK = hSAXWriter:START-DOCUMENT( ).
lOK = hSAXWriter:START-ELEMENT("TransactionSet").
iHlCnt = 0.
iLineCnt = 0.
dQtyTotal = 0.
iQtyHash = 0.
FOR EACH edshtran WHERE edshTran.seq = ipSeq:
    
  ASSIGN
    lOK = hSAXWriter:START-ELEMENT("TX-00401-856")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "TransactionSet")
   
    lOK = hSAXWriter:START-ELEMENT("Meta")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("ST01", "856")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("ST02", "0001") 
    lOK = hSAXWriter:END-ELEMENT("Meta")

    lOK = hSAXWriter:START-ELEMENT("BSN")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("BSN01", "00")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("BSN02", STRING(edShTran.bol-no))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("BSN03", getDate(TODAY))
    lOK = hSAXWriter:END-ELEMENT("BSN")
    
    lOK = hSAXWriter:START-ELEMENT("DTM")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("DTM01", "011")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("DTM02", getDate(edShTran.ship-date))
    lOK = hSAXWriter:END-ELEMENT("DTM")
    
    lOK = hSAXWriter:START-ELEMENT("DTM")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("DTM01", "017")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("DTM02", getDate(edShTran.ship-date + 1))
    lOK = hSAXWriter:END-ELEMENT("DTM")
    
    iHlCnt = iHlCnt + 1
    lOK = hSAXWriter:START-ELEMENT("HL")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("HL01", STRING(iHlCnt))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("HL02", "")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("HL03", "S")
    lOK = hSAXWriter:END-ELEMENT("HL")        
    
    lOK = hSAXWriter:START-ELEMENT("MEA")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA01", "WT")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA02", "G")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA03", STRING(edShTran.tot-wght))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA04", "LB")   
    lOK = hSAXWriter:END-ELEMENT("MEA")
    
    lOK = hSAXWriter:START-ELEMENT("MEA")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA01", "WT")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA02", "N")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA03", STRING(edShTran.tot-wght))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA04", "LB")   
    lOK = hSAXWriter:END-ELEMENT("MEA")
        
    lOK = hSAXWriter:START-ELEMENT("TD1")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("TD101", "PLT71")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("TD102", STRING(edShTran.tot-cartons))
    lOK = hSAXWriter:END-ELEMENT("TD1")  
    
    lOK = hSAXWriter:START-ELEMENT("TD5")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("TD501", "B")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("TD502", "2")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("TD503", edShTran.carrier-code)
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("TD504", "T")   
    lOK = hSAXWriter:END-ELEMENT("TD5")
              
    lOK = hSAXWriter:START-ELEMENT("REF")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("REF01", "BM")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("REF02", edShTran.bol-no)
    lOK = hSAXWriter:END-ELEMENT("REF")  

    lOK = hSAXWriter:START-ELEMENT("REF")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("REF01", "PK")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("REF02", edShTran.bol-no)
    lOK = hSAXWriter:END-ELEMENT("REF")  

    lOK = hSAXWriter:START-ELEMENT("FOB")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("FOB01", "DF")
    lOK = hSAXWriter:END-ELEMENT("FOB")  

    lOK = hSAXWriter:START-ELEMENT("N1")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("N101", "SF")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("N102", "")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("N103", "16")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("N104", "159197401")   
    lOK = hSAXWriter:END-ELEMENT("N1")
    
    lOK = hSAXWriter:START-ELEMENT("N1")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("N101", "SU")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("N102", "")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("N103", "91")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("N104", "PP530")   /* Premier supplier code */
    lOK = hSAXWriter:END-ELEMENT("N1")
    
    .
    ASSIGN iLineCnt = 0
           dQtyTotal = 0
           .
    FOR EACH edShLine WHERE edShLine.seq = edShTran.seq:
        iLineCnt = iLineCnt + 1.
        dQtyTotal = dQtyTotal + edShLine.qty-shipped.
        iQtyHash = iQtyHash + numericPart(STRING(edShLine.qty-shipped)).
        iHlCnt = iHlCnt + 1.
        
        ASSIGN
        lOK = hSAXWriter:START-ELEMENT("HL")
        lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("HL01", STRING(iHlCnt))
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("HL02", "1")
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("HL03", "I")
        lOK = hSAXWriter:END-ELEMENT("HL")  
    
        lOK = hSAXWriter:START-ELEMENT("LIN")
        lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("LIN01", STRING(iLineCnt))
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("LIN02", "BP")
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("LIN03", edShLIne.cust-item-no)
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("LIN04", "VP")
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("LIN05", edShLIne.item-no)
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("LIN06", "CH")        
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("LIN07", "US")            
        lOK = hSAXWriter:END-ELEMENT("LIN")  
    
        lOK = hSAXWriter:START-ELEMENT("SN1")
        lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("SN101", "")
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("SN102", STRING(EDSHLine.Qty-shipped))
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("SN103", EDSHLine.Uom-code)
        lOK = hSAXWriter:END-ELEMENT("SN1")  
    
        lOK = hSAXWriter:START-ELEMENT("PRF")
        lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("PRF01", edShLine.cust-po)
        lOK = hSAXWriter:END-ELEMENT("PRF") 
      
        lOK = hSAXWriter:START-ELEMENT("MEA")
        lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA01", "PD")
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA02", "G")
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA03", STRING(edShLine.tot-wght))
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("MEA04", "LB")   
        lOK = hSAXWriter:END-ELEMENT("MEA")
        /*
        lOK = hSAXWriter:START-ELEMENT("REF")
        lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("REF01", "HC")
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("REF02", "R05806")
        lOK = hSAXWriter:END-ELEMENT("REF")  
    
        lOK = hSAXWriter:START-ELEMENT("REF")
        lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("REF01", "LS")
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("REF02", "2127622A")
        lOK = hSAXWriter:END-ELEMENT("REF")
        */  
        .
        
    END. /* Each edShLine */
    cQtyHash = STRING(iQtyHash).
    IF LENGTH(cQtyHash) GT 3 THEN 
      cQtyHash = SUBSTRING(cQtyHash, 1, 3).
    ASSIGN
        lOK = hSAXWriter:START-ELEMENT("CTT")
        lOK = hSAXWriter:INSERT-ATTRIBUTE("type", "Segment")        
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("CTT01", STRING(iLineCnt))
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("CTT02", cQtyHash)
        lOK = hSAXWriter:END-ELEMENT("CTT")
        .   
    lOK = hSAXWriter:END-ELEMENT("TX-00401-856").
END.

lOK = hSAXWriter:END-ELEMENT("TransactionSet").
lOK = hSAXWriter:END-DOCUMENT( ).

DELETE OBJECT hSAXWriter.
