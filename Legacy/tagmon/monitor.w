/* monitor.w */
/* notes: */
/* include file must contain edi variables */
/* must compare length of tag number given to normal one */
/* Must determine if po number is the real one */
/* no po line number is given, must assume there is only one !!!*/

/* Count up to iWaitSeconds, then continue processing */ 
DEFINE VARIABLE iWaitCount AS INTEGER NO-UNDO.
DEFINE VARIABLE iWaitSeconds AS INTEGER INIT 30 NO-UNDO.
DEFINE STREAM sInputStream.
{ed/sharedv.i}
{ed/edivars.i}
{custom/monitor.w "tagMon" "tagMon"}

def var scr-vend-tag as char.
def var begin_po-no as int.
def var scr-po-line as int.
DEFINE VARIABLE scr-uom AS CHARACTER.
DEFINE VARIABLE scr-item-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE scr-qty AS INTEGER NO-UNDO.
DEFINE BUFFER b-company FOR company .
find first loc where
     loc.company eq cocode AND
     loc.loc eq locode
     no-lock no-error.

DEF VAR v-bin AS CHAR NO-UNDO.
DEF VAR v-loadtag AS CHAR NO-UNDO INIT "ASI". /* sys ctrl option */

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ g_company
       AND sys-ctrl.name    EQ "RMWHSBIN" NO-ERROR.
IF NOT AVAIL sys-ctrl THEN
DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = g_company
   sys-ctrl.name     = "RMWHSBIN"
   sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
   sys-ctrl.char-fld = "RMITEM".
  FIND CURRENT sys-ctrl NO-LOCK.
END.
v-bin = sys-ctrl.char-fld.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ g_company
       AND sys-ctrl.name    EQ "LOADTAG" NO-ERROR.

IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
       sys-ctrl.company  = g_company
       sys-ctrl.name     = "LOADTAG"
       sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
       sys-ctrl.char-fld = "ASI".
      MESSAGE "System control record NOT found. Please enter the load tag option"
              UPDATE sys-ctrl.char-fld.
      FIND CURRENT sys-ctrl NO-LOCK.
  END.

ASSIGN
   v-loadtag = sys-ctrl.char-fld.

/*DEF VAR ws_eddoc_rec AS RECID. */

DEFINE TEMP-TABLE w-po NO-UNDO
  FIELD i-no LIKE po-ordl.i-no
  FIELD i-name LIKE po-ordl.i-name
  FIELD over-pct LIKE po-ord.over-pct
  FIELD cost LIKE po-ordl.cost
  FIELD po-no LIKE po-ord.po-no
  FIELD b-num LIKE po-ordl.b-num
  FIELD cons-cost LIKE po-ordl.cons-cost
  FIELD cons-qty LIKE po-ordl.cons-qty
  FIELD cons-uom LIKE po-ordl.cons-uom
  FIELD job-no LIKE po-ordl.job-no
  FIELD job-no2 LIKE po-ordl.job-no2
  FIELD ord-no LIKE po-ordl.ord-no
  FIELD ord-qty LIKE po-ordl.ord-qty
  FIELD pr-uom LIKE po-ordl.pr-uom
  FIELD s-len LIKE po-ordl.s-len
  FIELD s-num LIKE po-ordl.s-num
  FIELD s-wid LIKE po-ordl.s-wid
  FIELD loc AS CHAR
  FIELD loc-bin LIKE item.loc-bin
  FIELD LINE AS INT
  FIELD rcpt-qty LIKE rm-rctd.qty
  FIELD tag-date AS DATE
  FIELD total-tags AS INT
  FIELD overrun-qty AS INT
  FIELD TYPE AS CHAR
  FIELD setup AS DEC
  FIELD add-setup AS LOG
  INDEX po IS PRIMARY po-no ASC.
  
FUNCTION checkWhsBin RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN CAN-FIND(FIRST loc
                  WHERE loc.company EQ ipCompany
                    AND loc.loc EQ ipLoc) AND
         CAN-FIND(FIRST rm-bin
                  WHERE rm-bin.company EQ ipCompany
                    AND rm-bin.loc EQ ipLoc
                    AND rm-bin.i-no EQ ''
                    AND rm-bin.loc-bin EQ ipLocBin).

END FUNCTION.
  


/* **********************  Internal Procedures  *********************** */


PROCEDURE clearTempTable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
FOR EACH ttHeader:
  DELETE ttHeader.
END.
FOR EACH ttBody:
  DELETE ttBody.
END.

END PROCEDURE.

PROCEDURE postMonitor:
    /*------------------------------------------------------------------------------
      Purpose:     import montiored file, create receipt record, post
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE monitorFile AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE attrList    AS CHARACTER FORMAT 'X(4)' NO-UNDO.
    DEFINE VARIABLE errStatus   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE saveMonitor AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lReturn     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hPDS        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE nextRNo     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE nextRelease AS INTEGER   NO-UNDO.
    DEFINE VARIABLE nextRelNo   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cPathIn     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPathout    AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-eddoc FOR EDDoc.
    DEFINE VARIABLE cRtnChar                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound                 AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE AsnHotFolderIn-char  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE AsnHotFolderOut-char AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullFilePath             AS CHARACTER NO-UNDO.
    
    
    
    AsnHotFolderIn-char = monitorImportdir.
    AsnHotFolderOut-char = AsnHotFolderIn-char + "\processed".
    IF SEARCH(AsnHotFolderIn-char) EQ ? THEN 
      OS-CREATE-DIR VALUE(AsnhotFolderIn-char).
    IF SEARCH(AsnHotFolderOut-char) EQ ? THEN 
      OS-CREATE-DIR VALUE(AsnhotFolderOut-char).
        
    
    cPathIn  = AsnHotFolderIn-char.   

   
    RUN monitorActivity ('Check New Tag Files ' + monitorImportDir,YES,'').
    
      
    RUN monitorActivity ('Check dir ' + cPathIn,YES,'').
    INPUT FROM OS-DIR(cPathIn).
    REPEAT:
        IMPORT monitorFile ^ attrList.
        IF attrList NE 'f' OR monitorFile BEGINS '.' OR
       INDEX(monitorFile,'.xml') EQ 0 THEN NEXT.
        cFullFilePath = cPathIn + "\" + monitorFile.
        
        /* Create ttHeader and ttBody from flat file */
        RUN processResultFlatFile (INPUT cPathin, INPUT monitorFile).
        
        /* Create EDI Records from ttHeader and ttBody */
        RUN processTemptable.
        RUN clearTempTable.
        
        RUN monitorActivity ('Processing ' + monitorFile,YES,'').
    END. /* os-dir repeat */
    INPUT CLOSE.
  
END PROCEDURE.

PROCEDURE processResultFlatFile:
    DEFINE INPUT PARAMETER cInputDir  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cInputFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullFilePath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNewFilePath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInput AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRecNum AS INT NO-UNDO.
    
    cFullFilePath = cInputDir + "\" + cInputFile.
    cNewFilePath = cInputDir + "\" + "processed" + "\" + cInputFile.
        
    INPUT STREAM sInputStream FROM VALUE(cFullFilePath).
    REPEAT:
        cInput = "".
        IMPORT STREAM sInputStream UNFORMATTED cInput.
        cInput = TRIM(cInput).
      
        IF cInput BEGINS "B" THEN DO:
          iRecNum = iRecNum + 1.
          CREATE ttHeader. 
          ASSIGN 
          /* Recordcount SUBSTRING(cInput,     2,    4)*/
          ttHeader.company            = SUBSTRING(cInput,     1,    1)
          ttHeader.AsnCreateDateTime  = SUBSTRING(cInput,    17,   12)
          ttHeader.TotalQtyCount      = INTEGER(SUBSTRING(cInput,     7,   10))
          ttHeader.RecNum             = iRecNum
          .                             
   
        END. /* H Record */
        ELSE IF cInput BEGINS "A" THEN DO:
          CREATE ttBody.
          ASSIGN                                                                  
          ttBody.company              = ""
          ttBody.TheirCompanyCode     = SUBSTRING(cInput,     1,    1)
          ttBody.PalletID             = SUBSTRING(cInput,     2,   25) 
          ttBody.DateInTransit        = SUBSTRING(cInput,    27,   12)
          ttBody.TheirVendor          = SUBSTRING(cInput,    39,    5)
          ttBody.PoNumber             = SUBSTRING(cInput,    44,    6) /* was a zero at 22 */
          ttbody.poLineNumber         = SUBSTRING(cInput,    51,    2)
          ttBody.OrderNumber          = SUBSTRING(cInput,    66,    7)
          ttBody.QuanityOnPallet      = SUBSTRING(cInput,    73,    4) 
          ttBody.BolNumber            = SUBSTRING(cInput,    77,   10)
          ttBody.TrailerID            = SUBSTRING(cInput,    87,   10)
          ttBody.NewCorOrder          = SUBSTRING(cInput,    97,   11) 
          ttBody.RecNum               = iRecNum
          .     
        END. /* B Record */
  END. /* Repeat */
  INPUT STREAM sInputStream CLOSE.
  OS-COPY VALUE(cFullFilePath) VALUE(cNewFilePath).    

  IF INTEGER(OS-ERROR) EQ 0 THEN  
      OS-DELETE VALUE(cFullFilePath).              
            
  RUN monitorActivity ('Processing Result File' ,YES,'').

    
END PROCEDURE.



procedure processTemptable:
    DEFINE VARIABLE iNextSeq AS INT NO-UNDO.
    FIND FIRST edcode NO-LOCK WHERE edcode.setid = "856"
                        AND edcode.direction EQ "I"
                      NO-ERROR.
    IF NOT AVAIL edcode THEN 
      RETURN.
      
    FIND FIRST edmast NO-LOCK OF edcode NO-ERROR.
    IF NOT AVAIL edmast THEN
      RETURN.
    FIND LAST eddoc NO-LOCK WHERE eddoc.partner EQ edmast.partner
       USE-INDEX bySeq
       NO-ERROR.
    IF AVAIL eddoc THEN
      iNextSeq = eddoc.seq + 1.
    ELSE
      iNextSeq = 1.
    ws_partner = EDMast.partner.
    FOR EACH ttHeader, EACH ttBody WHERE ttBody.RecNum EQ ttHeader.Recnum
      BREAK BY ttHeader.RecNum:
      
      IF FIRST-OF(ttHeader.RecNum) THEN DO:
        RUN ed/gendoc.p (RECID(edcode), ttBody.bolNumber, OUTPUT ws_eddoc_rec).
        FIND  eddoc WHERE RECID(eddoc) = ws_eddoc_rec EXCLUSIVE.
        ASSIGN
          EDDoc.AddDate     = TODAY
          EDDoc.AddTime     = TIME
          EDDoc.Direction   = "I" 
          EDDoc.DocID       = ttBody.poNumber          
          EDDoc.Posted      = no 
          EDDoc.SetID       = "856" 
          EDDoc.Stat        = 0
          EDDoc.Seq         = iNextSeq
          EDDoc.Status-Flag = "O"
          EDDoc.Version     = STRING(edcode.version)
          EDDOC.partner     = edcode.partner
          .
          CREATE EdShTran.
          ASSIGN
            EDSHTran.Partner = EDCode.partner
            EDSHTran.BOL-Adddate = TODAY
            EDSHTran.BOL-Addtime = TIME
            EDSHTran.BOL-No      = ttBody.bolnumber
            EDSHTran.Partner     = edcode.partner
            /* EDSHTran.Purpose-code */
            EDSHTran.Trailer-Number = ttBody.trailerID
            EDSHTran.seq         = iNextSeq
            .
          CREATE EdShOrd.
          ASSIGN
            EDSHOrd.Partner   = EDCode.partner
            EdShOrd.seq       = iNextSeq
            EdShOrd.Bol-No    = ttBody.bolnumber
            EdShOrd.Order-no  = ttBody.OrderNumber
            EdShOrd.Partner   = edcode.partner
            
            .
            
      END. /* FIRST OF */
      CREATE EDSHTare.
      ASSIGN 
       EDSHTare.Partner     = EDCode.partner
       EDSHTare.Seq         = iNextSeq
       EDSHTare.Pallet-mark = int(ttBody.PalletID)
       .
      CREATE edShLine.
      ASSIGN 
        EDSHLine.Partner      = EDCode.partner
        edShLine.Seq          = iNextSeq
        edShLine.Bol-No       = ttBody.bolNumber
        edShLine.cust-po      = TRIM(ttBody.poNumber)
        EDSHLine.Cust-po-line = ttBody.poLineNumber
        edShLine.pallet-mark  = int(ttBody.PalletID)
        edShLine.tot-Cartons  = int(ttBody.QuanityOnPallet)
        
        .
      scr-vend-tag = string(edShLine.pallet-mark).
      begin_po-no  = integer(REPLACE(edShLine.cust-po, "A","")) NO-ERROR .
      IF ERROR-STATUS:ERROR THEN 
      
      find first po-ordl 
        where po-ordl.company eq g_company
          and po-ordl.po-no   eq begin_po-no
        no-lock no-error.
      if avail po-ordl then 
        scr-po-line  = po-ordl.line.
      
    END.
END PROCEDURE.


