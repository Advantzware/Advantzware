
/*------------------------------------------------------------------------
    File        : oe850xml.p
    Purpose     : 

    Syntax      :

    Description : Export job-related xml data to Esko

    Author(s)   : 
    Created     : Wed Nov 09 19:32:08 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iprEddoc AS ROWID NO-UNDO.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemOnOrder    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustPart       AS CHARACTER NO-UNDO.
DEFINE VARIABLE rItemfgRow      AS ROWID NO-UNDO.
DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRetOK          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iIndex        AS INTEGER NO-UNDO.

DEFINE VARIABLE cSheetBlank     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUDFString      AS CHARACTER NO-UNDO.
/* Insert obtaining sys-ctrl values for jobXml */



DEFINE TEMP-TABLE ttTempJob
    FIELD company      AS CHARACTER
    FIELD jobID        AS CHARACTER
    FIELD jobNo        AS CHARACTER 
    FIELD jobNo2       AS INTEGER 
    FIELD orderNo      AS INTEGER 
    FIELD orderNoPrev  AS CHARACTER 
    FIELD newProject   AS INTEGER
    FIELD customerID   AS CHARACTER
    FIELD customerName AS CHARACTER 
    FIELD customerPoNo AS CHARACTER 
    FIELD FGItemCode   AS CHARACTER
    FIELD FGName       AS CHARACTER
    FIELD CustPart     AS CHARACTER
    FIELD NumUp        AS CHARACTER  
    FIELD ItemStatus   AS CHARACTER  
    FIELD itemRecKey   AS CHARACTER
    FIELD DateIssued   AS CHARACTER   
    FIELD ebWIDTH      AS CHARACTER 
    FIELD ebLENGTH     AS CHARACTER
    FIELD ebDepth      AS CHARACTER  
    FIELD FlatWidth    AS CHARACTER
    FIELD FlatLength   AS CHARACTER
    FIELD ColorsCoat   AS CHARACTER
    FIELD CCNumber     AS CHARACTER
    FIELD Weight       AS CHARACTER
    FIELD Caliper      AS CHARACTER
    FIELD Structure    AS CHARACTER
    FIELD Board        AS CHARACTER
    FIELD FgCategory   AS CHARACTER
    FIELD QcSpc        AS CHARACTER 
    FIELD CadNo        AS CHARACTER 
    FIELD PlateNo      AS CHARACTER 
    FIELD UPCNo        AS CHARACTER 
    FIELD DieNo        AS CHARACTER 
    FIELD SizeBlankWL  AS CHARACTER
    FIELD SizeBlankLW  AS CHARACTER 
    FIELD SizeUnit     AS CHARACTER 
    FIELD SizeFinished AS CHARACTER 
    FIELD SizeDie      AS CHARACTER 
    FIELD SizeSheet    AS CHARACTER 
    FIELD SizeSheetNet AS CHARACTER 
    FIELD QuantityJob  AS DECIMAL 
    FIELD QuantityMax  AS DECIMAL 
    FIELD QuantityMin  AS DECIMAL 
    FIELD QuantityOver  AS DECIMAL 
    FIELD QuantityUnder AS DECIMAL 
    FIELD EstNo        AS CHARACTER 
    FIELD Style        AS CHARACTER 
    FIELD VendorNo     AS CHARACTER 
    FIELD VendorItemID AS CHARACTER 
    FIELD CostMisc AS CHARACTER EXTENT 6
    FIELD MRWastePM AS DECIMAL 
    FIELD MRHoursPM AS DECIMAL 
    FIELD MRWastePR AS DECIMAL 
    FIELD MRHoursPR AS DECIMAL 
    FIELD RunSpeedPR AS DECIMAL 
    FIELD RunWastePR AS DECIMAL 
    FIELD MRWasteDC AS DECIMAL 
    FIELD MRHoursDC AS DECIMAL 
    FIELD RunSpeedDC AS DECIMAL 
    FIELD RunWasteDC AS DECIMAL 
    FIELD CCCode AS CHARACTER
    FIELD DateDue AS DATE 
    FIELD DateLastShip AS DATE 
    FIELD ShipID AS CHARACTER 
    FIELD FreightClass AS CHARACTER 
    FIELD FreightClassDesc AS CHARACTER 
    FIELD FreightCharge AS CHARACTER 
    FIELD Carrier AS CHARACTER 
    FIELD Warehouse AS CHARACTER 
    FIELD DockHours AS CHARACTER 
    FIELD DockAppt AS CHARACTER 
    FIELD DockContact AS CHARACTER 
    FIELD MachLayout AS CHARACTER 
    FIELD MachPR AS CHARACTER 
    .
 
DEFINE VARIABLE cocode AS CHARACTER NO-UNDO.
cocode = ipcCompany.

/* Open stream for xml */
{XMLOutput/XMLOutput.i &XMLOutput=XMLJOB &Company=cocode &NEW=}

 /* temp table populated with UDF data */   
{UDF/ttUDF.i}                

/* function to get UDF Group */
{UDF/fUDFGroup.i "itemfg."}                         
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fnStripInvalidChar RETURNS CHARACTER 
	(ipcInput AS CHARACTER  ) FORWARD.


/* ***************************  Main Block  *************************** */

/* ************************  Function Implementations ***************** */


FUNCTION fnStripInvalidChar RETURNS CHARACTER 
	(ipcInput AS CHARACTER  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPos          AS INTEGER NO-UNDO. 
    DEFINE VARIABLE lBadCharFound AS LOGICAL NO-UNDO.  
    DEFINE VARIABLE iNumTries     AS INTEGER NO-UNDO.  

    iNumTries = 0.   

    DO WHILE TRUE:   
        lBadCharFound = FALSE.
        
        iPos = INDEX(ipcInput, '"'). 
        IF iPos GT 0 THEN 
        DO:  
            lBadCharFound = TRUE. 
            ipcInput = REPLACE(ipcInput, '"', '&quot;').            
        END.  
        
        iPos = INDEX(ipcInput, "'"). 
        IF iPos GT 0 THEN 
        DO:  
            lBadCharFound = TRUE. 
            ipcInput = REPLACE(ipcInput, "'", '&apos;').            
        END.  
        
        iPos = INDEX(ipcInput, "<"). 
        IF iPos GT 0 THEN 
        DO:  
            lBadCharFound = TRUE. 
            ipcInput = REPLACE(ipcInput, "<", '&lt;').            
        END. 
        
        iPos = INDEX(ipcInput, ">"). 
        IF iPos GT 0 THEN 
        DO:  
            lBadCharFound = TRUE. 
            ipcInput = REPLACE(ipcInput, ">", '&gt;').            
        END. 
        iNumTries = iNumTries + 1.
        IF lBadCharFound EQ FALSE OR iNumTries GT 200 THEN
            LEAVE.  
    END. 
    cResult = ipcInput.
    RETURN cResult.
		
END FUNCTION.

EMPTY TEMP-TABLE ttTempjob.
FOR EACH EDDoc EXCLUSIVE-LOCK WHERE ROWID(EDDoc) EQ iprEdDoc,
    FIRST job NO-LOCK WHERE job.company EQ cocode
        AND job.job-no EQ EDDoc.setID
        AND job.job-no2 EQ EDDoc.docSeq
       :
        
        FOR EACH job-hdr NO-LOCK WHERE job-hdr.company EQ job.company
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2,
            FIRST itemfg NO-LOCK WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no
            :
            cItemOnOrder = itemfg.i-no.
            FIND FIRST eb NO-LOCK 
              WHERE eb.company EQ job-hdr.company
                AND eb.est-no  EQ job-hdr.est-no
                AND eb.stock-no = job-hdr.i-no
              NO-ERROR.
            IF NOT AVAIL eb THEN 
               FIND FIRST eb NO-LOCK 
                  WHERE eb.company     EQ job-hdr.company
                    AND eb.est-no      EQ job-hdr.est-no
                    AND eb.form-no     EQ job-hdr.frm
                    AND eb.blank-no    GT 0 NO-ERROR.              
            IF AVAILABLE eb THEN DO:        
                FIND FIRST oe-ord NO-LOCK
                    WHERE oe-ord.company EQ eb.company
                      AND oe-ord.est-no  EQ eb.est-no                  
                    NO-ERROR.             
                FIND FIRST ef NO-LOCK 
                   WHERE ef.company EQ eb.company
                     AND ef.est-no EQ eb.est-no
                     AND ef.form-no = eb.form-no
                   NO-ERROR.
                IF NOT AVAILABLE ef THEN 
                    FIND FIRST ef NO-LOCK 
                       WHERE ef.company EQ eb.company
                         AND ef.est-no EQ eb.est-no
                       NO-ERROR.
                
                FIND style NO-LOCK 
                           WHERE style.company = eb.company 
                             AND style.style = eb.style
                           NO-ERROR.
                /* stock-no overrides item number for sales order lookup as jobcard does */
                IF eb.stock-no GT "" THEN 
                  cItemOnOrder = eb.stock-no.
            END.
            
            cocode = job-hdr.company.
            FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ job-hdr.company
                AND sys-ctrl.name EQ "xmljob"
                NO-ERROR.
            IF NOT AVAILABLE sys-ctrl THEN RETURN.
       
            FIND FIRST cust NO-LOCK WHERE cust.company EQ job-hdr.company
                AND cust.cust-no EQ job-hdr.cust-no
                NO-ERROR.
            
            IF NOT AVAILABLE oe-ord THEN 
                FIND FIRST oe-ord NO-LOCK 
                    WHERE oe-ord.company EQ job-hdr.company
                      AND oe-ord.ord-no EQ job-hdr.ord-no
                      NO-ERROR.
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ job-hdr.company
                   AND oe-ordl.ord-no  EQ job-hdr.ord-no
                   AND oe-ordl.job-no  EQ job-hdr.job-no
                   AND oe-ordl.job-no2 EQ job-hdr.job-no2
                   AND oe-ordl.i-no    EQ job-hdr.i-no
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN 
                FIND FIRST oe-rel NO-LOCK
                    WHERE oe-rel.company EQ oe-ordl.company
                      AND oe-rel.ord-no  EQ oe-ordl.ord-no
                      AND oe-rel.i-no    EQ oe-ordl.i-no
                      AND oe-rel.line    EQ oe-ordl.line
                    NO-ERROR.
            IF AVAILABLE oe-rel THEN 
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ oe-rel.company
                  AND shipto.cust-no EQ oe-rel.cust-no
                  AND shipto.ship-id EQ oe-rel.ship-id
                NO-ERROR.  
            IF NOT AVAILABLE shipto AND AVAILABLE eb THEN 
                FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ eb.company
                  AND shipto.cust-no EQ eb.cust-no
                  AND shipto.ship-id EQ eb.ship-id
                NO-ERROR.  
           
            rItemfgRow = ROWID(itemfg).
            cCustPart = itemfg.part-no.
            RUN custom/getcpart.p (INPUT cust.company, INPUT cust.cust-no, 
                                   INPUT-OUTPUT cCustPart, INPUT-OUTPUT rItemfgRow).
    
            cSheetBlank = STRING(job-hdr.frm, "9") + string(job-hdr.blank-no, "9") .
    
            FIND FIRST ttTempJob EXCLUSIVE-LOCK WHERE ttTempJob.company EQ job-hdr.company
                AND ttTempJob.jobID = trim(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2, "99") + "-" + cSheetBlank
                AND ttTempJob.newProject EQ 1
                NO-ERROR.
    
            IF NOT AVAILABLE ttTempJob THEN 
            DO:
    
                CREATE ttTempJob.
                ASSIGN
                    ttTempjob.company    = job-hdr.company
                    ttTempjob.jobID      = TRIM(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2, "99") + "-" + cSheetBlank
                    ttTempjob.jobNo      = job-hdr.job-no
                    ttTempjob.jobNo2     = job-hdr.job-no2
                    ttTempJob.QuantityJob = job-hdr.qty
                    ttTempjob.orderNo    = job-hdr.ord-no
                    ttTempjob.EstNo      = job-hdr.est-no
                    ttTempjob.itemRecKey = itemfg.rec_key
                    ttTempjob.FGItemCode = cItemOnOrder
                    ttTempjob.FGName     = itemfg.i-name
                    ttTempjob.CustPart   = cCustPart
                    ttTempjob.ItemStatus = (IF itemfg.stat EQ "A" THEN "Active" ELSE "Inactive") 
                    ttTempjob.FgCategory = itemfg.procat-desc
                    ttTempJob.CCCode     = itemfg.cc-code
                    ttTempJob.FreightClass = itemfg.frt-class
                    ttTempJob.FreightClassDesc = itemfg.frt-class-dscr
                    ttTempJob.Warehouse = itemfg.def-loc
                    .
                
                /* Override of itemfg.procat-desc to match logic in viewers/itemfg.w */    
                FIND FIRST fgcat NO-LOCK WHERE fgcat.company = job-hdr.company 
                                           AND fgcat.procat = itemfg.procat
                                         NO-ERROR.
                ttTempjob.FgCategory = IF AVAIL fgcat THEN fgcat.dscr ELSE ttTempjob.FgCategory.
                                    
                IF AVAILABLE cust THEN 
                    ASSIGN 
                        ttTempjob.customerID   = cust.cust-no
                        ttTempJob.customerName = cust.name
                        ttTempJob.DockAppt = STRING(cust.area-code,"(999)") + "        " + STRING(cust.phone,"999-9999")
                        ttTempJob.DockContact = cust.contact
                        .
                IF AVAILABLE shipto THEN
                    ASSIGN 
                        ttTempJob.DockContact = shipto.contact
                        ttTempJob.DockHours = shipto.dock-hour
                        ttTempJob.DockAppt = STRING(shipto.area-code,"(999)") + "        " + STRING(shipto.phone,"999-9999")
                        .
                    
                IF AVAILABLE eb THEN 
                  ASSIGN                   
                    ttTempJob.ebWIDTH      = STRING(eb.wid)
                    ttTempJob.ebLENGTH     = STRING(eb.len)
                    ttTempJob.ebDepth      = STRING(eb.dep)
                    ttTempJob.FlatWidth    = STRING(eb.t-len)
                    ttTempJob.FlatLength   = STRING(eb.t-wid)
                    ttTempJob.NumUp        = STRING(eb.num-wid) + " X " + STRING(eb.num-len)
                    ttTempJob.ColorsCoat   = eb.i-dscr2[1]
                    ttTempJob.CCNumber     = eb.cad-no 
                    ttTempJob.CadNo        = eb.cad-no
                    ttTempJob.DieNo        = eb.die-no
                    ttTempJob.PlateNo      = eb.plate-no
                    ttTempJob.UPCNo        = eb.upc-no       
                    ttTempJob.QcSpc        = eb.spc-no     
                    ttTempjob.style        = eb.style 
                    ttTempJob.SizeBlankWL    = STRING(eb.t-wid) + " X " + STRING(eb.t-len)   
                    ttTempJob.SizeBlankLW     = STRING(eb.t-len) + " X " + STRING(eb.t-wid)
                    ttTempJob.SizeFinished = STRING(eb.len) + " X " + STRING(eb.wid) + " X " + STRING(eb.dep) 
                    ttTempJob.ShipID = eb.ship-id
                    ttTempJob.FreightCharge = eb.chg-method
                    ttTempJob.Carrier = eb.carrier
                  .
                IF AVAILABLE ef THEN DO:
                  ASSIGN  ttTempJob.Weight       = STRING(ef.weight)
                          ttTempJob.Caliper      = STRING(ef.cal)                    
                          ttTempJob.Board        = fnStripInvalidChar(ef.brd-dscr)
                          ttTempJob.SizeDie      = STRING(ef.trim-w) + " X " + STRING(ef.trim-l)
                          ttTempJob.SizeSheet    = STRING(ef.gsh-wid) + " X " + STRING(ef.gsh-len)
                          ttTempJob.SizeSheetNet = STRING(ef.lsh-wid) + " X " + STRING(ef.lsh-len)
                          .
                    DO iIndex = 1 TO 6:
                        IF ef.mis-cost[iIndex] <> "" THEN
                            ttTempJob.CostMisc[iIndex] = ef.mis-cost[iIndex].
                    END.
                END.
                IF ttTempjob.board NE "" THEN 
                    FIND FIRST e-itemfg-vend NO-LOCK 
                        WHERE e-itemfg-vend.company EQ job-hdr.company
                        AND e-itemfg-vend.i-no EQ ttTempJob.FGItemCode
                        AND e-itemfg-vend.vend-no NE ""
                        NO-ERROR.
                IF AVAILABLE e-itemfg-vend THEN 
                    ASSIGN 
                        ttTempJob.VendorItemID = e-itemfg-vend.vend-item
                        ttTempJob.VendorNo = e-itemfg-vend.vend-no
                        .
                        
                IF AVAILABLE oe-ord THEN 
                 ASSIGN 
                   ttTempJob.DateIssued   = STRING(oe-ord.ord-date)
                   ttTempJob.DateDue = oe-ord.due-date
                   ttTempJob.DateLastShip = oe-ord.last-date
                   ttTempjob.orderNoPrev = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2 ELSE STRING(oe-ord.pord-no)
                 .
                IF AVAILABLE oe-ordl THEN 
                    ASSIGN 
                        ttTempJob.QuantityOver = oe-ordl.over-pct
                        ttTempJob.QuantityUnder = oe-ordl.under-pct
                        ttTempJob.QuantityMax = oe-ordl.qty + oe-ordl.qty * (oe-ordl.over-pct / 100)
                        ttTempJob.QuantityMin = oe-ordl.qty - oe-ordl.qty * (oe-ordl.under-pct / 100)
                        ttTempJob.CustomerPoNo = oe-ordl.po-no
                        .
                IF AVAILABLE style THEN 
                  ASSIGN 
                  ttTempJob.Structure    = style.dscr
                  .      
            END.  /* create ttTempJob */
        END. /* Each job-hdr of job */
         
        EDDoc.stat = 1.
END.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ  cocode
    AND sys-ctrl.name    EQ 'XMLJOB' NO-ERROR.
    
lXmlOutput = TRUE.
IF AVAILABLE sys-ctrl THEN 
DO:

     RUN XMLOutput (lXMLOutput,'','','Header').
    
    
    FOR EACH ttTempJob BREAK BY ttTempJob.jobID:
        
        IF FIRST-OF(ttTempJob.jobID) THEN 
        DO:                        
            RUN XMLOutput (lXMLOutput,'JDF','','Row').
            RUN XMLOutput (lXMLOutput,'Company',ttTempJob.company,'Col').
            RUN XMLOutput (lXMLOutput,'JobID',ttTempJob.jobID,'Col').
            RUN XMLOutput (lXMLOutput,'NewProject',0,'Col').
            RUN XMLOutput (lXMLOutput,'CustomerID',ttTempJob.customerID,'Col').
            RUN XMLOutput (lXMLOutput,'CustomerName',ttTempJob.customerName,'Col').
        END.
        
        RUN XMLOutput (lXMLOutput,'ResourcePool','','Row').
        
            /* Process Itemfg Level */
            RUN XMLOutput (lXMLOutput,'Product','','Row').
                RUN XMLOutput (lXMLOutput,'FGItemCode', ttTempjob.FGItemCode,'Col').
                RUN XMLOutput (lXMLOutput,'FGName'    , ttTempjob.FGName,'Col').
                RUN XMLOutput (lXMLOutput,'CustPart'  , ttTempjob.CustPart,'Col').    
                RUN XMLOutput (lXMLOutput,'FGCategory', ttTempjob.FgCategory,'Col').
                RUN XMLOutput (lXMLOutput,'ItemStatus', ttTempjob.ItemStatus,'Col').  
                
                EMPTY TEMP-TABLE ttUDF.
                IF CAN-FIND(FIRST mfvalues
                    WHERE mfvalues.rec_key EQ ttTempjob.itemRecKey) THEN 
                DO:
                    /* get UDF records for this record */
                    RUN UDF/UDF.p (cUDFGroup, ttTempjob.itemRecKey, OUTPUT TABLE ttUDF).
                     
                     
                    /* process UDF data found */
                    FOR EACH ttUDF NO-LOCK 
                        WHERE ttUDF.udfEsko EQ YES
                        :                            
                        cUDFSTring = "SmartName " + 'Name="' + ttUDF.udfLabel + '" Value="' + ttUDF.udfValue + '"/'.
                        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
                    END.  /* each ttudf */     
                END.        
                
                /* Estimate Values */
                cUDFSTring = "SmartName " + 'Name="Date Issued" Value="' + ttTempJob.DateIssued + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').   
                cUDFSTring = "SmartName " + 'Name="Width" Value="' + ttTempJob.ebWIDTH    + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row'). 
                cUDFSTring = "SmartName " + 'Name="Length" Value="' + ttTempJob.ebLENGTH   + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
                cUDFSTring = "SmartName " + 'Name="Depth" Value="' + ttTempJob.ebDepth    + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').  
                cUDFSTring = "SmartName " + 'Name="Flat Width" Value="' + ttTempJob.FlatWidth  + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
                cUDFSTring = "SmartName " + 'Name="Flat Length" Value="' + ttTempJob.FlatLength + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
                cUDFSTring = "SmartName " + 'Name="Colors/Coating" Value="' + ttTempJob.ColorsCoat + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
                cUDFSTring = "SmartName " + 'Name="CC#" Value="' + ttTempJob.CCNumber   + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
                cUDFSTring = "SmartName " + 'Name="Weight" Value="' + ttTempJob.Weight     + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
                cUDFSTring = "SmartName " + 'Name="Caliper" Value="' + ttTempJob.Caliper    + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
                cUDFSTring = "SmartName " + 'Name="Style Code" Value="' + ttTempJob.Structure  + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
                cUDFSTring = "SmartName " + 'Name="Board" Value=' + "'" + ttTempJob.Board      + "'/".
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
       
            RUN XMLOutput (lXMLOutput,'/Product','','Row').     
                               
        RUN XMLOutput (lXMLOutput,'/ResourcePool','','Row').
        
        RUN XMLOutput (lXMLOutput,'JobTicket','','Row'). 
        
        RUN XMLOutput (lXMLOutput,'JobTicketHeader','','Row').
            RUN XMLOutput (lXMLOutput,'Job',ttTempJob.jobNo,'Col').
            RUN XMLOutput (lXMLOutput,'Job2',ttTempJob.jobNo2,'Col').
            RUN XMLOutput (lXMLOutput,'Order',ttTempJob.orderNo,'Col').
            RUN XMLOutput (lXMLOutput,'NUMBER_UP',ttTempJob.NumUp,'Col').
            RUN XMLOutput (lXMLOutput,'QC_SPC',ttTempJob.QcSpc,'Col').
            RUN XMLOutput (lXMLOutput,'Category',ttTempJob.FgCategory,'Col').
        RUN XMLOutput (lXMLOutput,'/JobTicketHeader','','Row').        
       
        RUN XMLOutput (lXMLOutput,'TicketPrinting','','Row'). 
            RUN XMLOutput (lXMLOutput,'Job_Item',ttTempJob.FGItemCode,'Col').
            RUN XMLOutput (lXMLOutput,'max_qty',ttTempJob.QuantityMax,'Col').
            RUN XMLOutput (lXMLOutput,'job_qty',ttTempJob.QuantityJob,'Col').
            RUN XMLOutput (lXMLOutput,'min_qty',ttTempJob.QuantityMin,'Col').
            RUN XMLOutput (lXMLOutput,'OverRun',ttTempJob.QuantityOver,'Col').
            RUN XMLOutput (lXMLOutput,'UnderRun',ttTempJob.QuantityUnder,'Col').
            RUN XMLOutput (lXMLOutput,'Account_code',ttTempJob.customerID,'Col').
            RUN XMLOutput (lXMLOutput,'Cust_item',ttTempJob.CustPart,'Col').
            RUN XMLOutput (lXMLOutput,'Description',ttTempjob.FGName,'Col').
            RUN XMLOutput (lXMLOutput,'Customer',ttTempjob.customerID,'Col').
            RUN XMLOutput (lXMLOutput,'Purchase_order',ttTempJob.CustomerPoNo,'Col').
            RUN XMLOutput (lXMLOutput,'Estimate',ttTempJob.estNo,'Col').
            RUN XMLOutput (lXMLOutput,'Finished_size',ttTempJob.SizeFinished,'Col').
            RUN XMLOutput (lXMLOutput,'Style',ttTempJob.Style,'Col').
            RUN XMLOutput (lXMLOutput,'vend_no',ttTempJob.VendorNo,'Col').
            RUN XMLOutput (lXMLOutput,'vendor_item',ttTempJob.VendorItemID,'Col').
            DO iIndex = 1 TO 4:
                RUN XMLOutput (lXMLOutput,'cost',ttTempJob.CostMisc[iIndex],'Col').
            END.
        RUN XMLOutput (lXMLOutput,'/TicketPrinting','','Row').
        
        RUN XMLOutput (lXMLOutput,'Graphics','','Row').
            RUN XMLOutput (lXMLOutput,'FGItem',ttTempJob.FGItemCode,'Col').
            RUN XMLOutput (lXMLOutput,'Structure_Number',ttTempJob.CadNo,'Col').
            RUN XMLOutput (lXMLOutput,'Artwork',ttTempJob.PlateNo,'Col').
            RUN XMLOutput (lXMLOutput,'BarCode',ttTempJob.UPCNo,'Col').
            RUN XMLOutput (lXMLOutput,'Lastship_order',ttTempJob.orderNoPrev,'Col').
        RUN XMLOutput (lXMLOutput,'/Graphics','','Row').
    
        RUN XMLOutput (lXMLOutput,'StructuralDesign','','Row').
            RUN XMLOutput (lXMLOutput,'FG_Item',ttTempJob.FGItemCode,'Col').
            RUN XMLOutput (lXMLOutput,'Die_Size',ttTempJob.SizeDie,'Col').
            RUN XMLOutput (lXMLOutput,'Style',ttTempJob.style,'Col').
            RUN XMLOutput (lXMLOutput,'Die',ttTempJob.DieNo,'Col').
            RUN XMLOutput (lXMLOutput,'Blank_size',ttTempJob.SizeBlankWL,'Col').
        RUN XMLOutput (lXMLOutput,'/StructuralDesign','','Row').       
                
        RUN XMLOutput (lXMLOutput,'PlateMaking','','Row').
/*            RUN XMLOutput (lXMLOutput,'MR_Waste',iMrWaste,'Col').       */
            RUN XMLOutput (lXMLOutput,'Artwork',ttTempJob.PlateNo,'Col').
/*            RUN XMLOutput (lXMLOutput,'MR_Hours',dMrHour,'Col').        */
            RUN XMLOutput (lXMLOutput,'fg_item',ttTempJob.FGItemCode,'Col').
            RUN XMLOutput (lXMLOutput,'Item_code',ttTempJob.CCCode,'Col').
        RUN XMLOutput (lXMLOutput,'/PlateMaking','','Row').
        
        RUN XMLOutput (lXMLOutput,'Printing','','Row').
/*            RUN XMLOutput (lXMLOutput,'Code',ef.m-code,'Col').                                                 */
/*            RUN XMLOutput (lXMLOutput,'MRWaste',iMrWaste,'Col').                                               */
/*            RUN XMLOutput (lXMLOutput,'Stock_Code',IF AVAIL job-mat THEN  job-mat.rm-i-no ELSE "",'Col').      */
            RUN XMLOutput (lXMLOutput,'Sheet_Size',ttTempJob.SizeSheet,'Col').
/*            RUN XMLOutput (lXMLOutput,'MRHours',dMrHour,'Col').                                                */
/*            RUN XMLOutput (lXMLOutput,'Board_Paper',cPoItemName,'Col').                                        */
            RUN XMLOutput (lXMLOutput,'NetSheet_Size',ttTempJob.SizeSheetNet,'Col').
            RUN XMLOutput (lXMLOutput,'UNIT_SIZE',ttTempJob.SizeBlankLW,'Col').
/*            RUN XMLOutput (lXMLOutput,'FTM',iSpeed,'Col').                                                     */
/*            RUN XMLOutput (lXMLOutput,'LBS_Stock',IF AVAIL job-mat THEN  job-mat.qty ELSE 0,'Col').            */
/*            RUN XMLOutput (lXMLOutput,'Flexo_Cylinder',ef.gsh-len,'Col').                                      */
/*            RUN XMLOutput (lXMLOutput,'Spoilage',dWstPrct,'Col').                                              */
/*            RUN XMLOutput (lXMLOutput,'Number_Out',ef.n-out,'Col').                                            */
            RUN XMLOutput (lXMLOutput,'Caliper',ttTempJob.Caliper,'Col').
        RUN XMLOutput (lXMLOutput,'/Printing','','Row').
        
        RUN XMLOutput (lXMLOutput,'DieCutting','','Row').
/*            RUN XMLOutput (lXMLOutput,'MR_Waste',iMrWaste,'Col').                                       */
            RUN XMLOutput (lXMLOutput,'Die_Size',ttTempJob.SizeDie,'Col').
/*            RUN XMLOutput (lXMLOutput,'MR_Hours',dMrHour,'Col').                                        */
            RUN XMLOutput (lXMLOutput,'Die',ttTempJob.DieNo,'Col').
/*            RUN XMLOutput (lXMLOutput,'Run_Speed',iSpeed,'Col').                                        */
/*            RUN XMLOutput (lXMLOutput,'Spoilage',dWstPrct,'Col').                                       */
        RUN XMLOutput (lXMLOutput,'/DieCutting','','Row').
        
        RUN XMLOutput (lXMLOutput,'RollExamining','','Row').
/*            RUN XMLOutput (lXMLOutput,'code',ef.m-code,'Col').                                                                                                  */
/*            RUN XMLOutput (lXMLOutput,'MR_Waste',iMrWaste,'Col').                                                                                               */
/*            RUN XMLOutput (lXMLOutput,'case_no',eb.cas-no,'Col').                                                                                               */
/*            RUN XMLOutput (lXMLOutput,'Size',string(item.case-w) + "  x  " + STRING(item.case-l) + "  x  " + STRING(item.case-d),'Col').                        */
/*            RUN XMLOutput (lXMLOutput,'MR_Hours',dMrHour,'Col').                                                                                                */
/*            RUN XMLOutput (lXMLOutput,'Units_perCase',eb.cas-cnt,'Col').                                                                                        */
/*            RUN XMLOutput (lXMLOutput,'QtyofCases',v-cases-qty,'Col').                                                                                          */
/*            RUN XMLOutput (lXMLOutput,'case_weight',v-cas-wt,'Col').                                                                                            */
/*            RUN XMLOutput (lXMLOutput,'Flat',STRING(eb.t-len) + " x " + STRING(eb.t-wid),'Col').                                                                */
/*            RUN XMLOutput (lXMLOutput,'Run_Speed',iSpeed,'Col').                                                                                                */
/*            RUN XMLOutput (lXMLOutput,'Qtyof_Cases',v-cases-qty,'Col').                                                                                         */
/*            RUN XMLOutput (lXMLOutput,'Spoilage',dWstPrct,'Col').                                                                                               */
/*            RUN XMLOutput (lXMLOutput,'Divider',string(eb.div-up) + "   " + string(eb.div-len) + " x " + STRING(eb.div-wid) + "   " + STRING(eb.divider),'Col').*/
/*            RUN XMLOutput (lXMLOutput,'Dividerper_Case',eb.div-up,'Col').                                                                                       */
/*            RUN XMLOutput (lXMLOutput,'Shrink_Wrap',v-shrink-wrap,'Col').                                                                                       */
/*            RUN XMLOutput (lXMLOutput,'Wind_Direction',itemfg.prod-code,'Col').                                                                                 */
/*            RUN XMLOutput (lXMLOutput,'Size',string(eb.lp-len) + "     x     " + STRING(eb.lp-wid),'Col').                                                      */
/*            RUN XMLOutput (lXMLOutput,'stock',IF AVAIL job-mat THEN  job-mat.rm-i-no ELSE "",'Col').                                                            */
/*            RUN XMLOutput (lXMLOutput,'Units_per_TrayLP',v-layer-qty,'Col').                                                                                    */
/*            RUN XMLOutput (lXMLOutput,'LBS_Stock',IF AVAIL job-mat THEN job-mat.qty ELSE 0,'Col').                                                              */
/*            RUN XMLOutput (lXMLOutput,'Qty_of_TraysLPs',STRING(dQtyTray) + "      " + STRING(eb.lp-up),'Col').                                                  */
            RUN XMLOutput (lXMLOutput,'Web_Width',ttTempJob.SizeSheet,'Col').
/*            RUN XMLOutput (lXMLOutput,'Size',string(eb.tr-len) + "     x     " + STRING(eb.tr-wid) + "       " + STRING(eb.tr-no),'Col').                       */
            RUN XMLOutput (lXMLOutput,'Quantity',ttTempJob.QuantityJob,'Col').
/*            RUN XMLOutput (lXMLOutput,'Max_Ht',cShpDoc,'Col').                                                                                                  */
/*            RUN XMLOutput (lXMLOutput,'class',itemfg.class,'Col').                                                                                              */
        RUN XMLOutput (lXMLOutput,'/RollExamining','','Row').
           
        RUN XMLOutput (lXMLOutput,'TicketPrint','','Row').
            RUN XMLOutput (lXMLOutput,'Requested_Date',ttTempJob.DateDue,'Col').
            RUN XMLOutput (lXMLOutput,'Ship_Due_Date',ttTempJob.DateLastShip,'Col').
            RUN XMLOutput (lXMLOutput,'Ship_id',ttTempJob.ShipID,'Col').
            RUN XMLOutput (lXMLOutput,'Freight_class',ttTempjob.FreightClass,'Col').
            RUN XMLOutput (lXMLOutput,'Freight_class_description',ttTempJob.FreightClassDesc,'Col').
            RUN XMLOutput (lXMLOutput,'Carrier',ttTempJob.Carrier,'Col').
            RUN XMLOutput (lXMLOutput,'Freight_Charge',ttTempJob.FreightCharge,'Col').
            RUN XMLOutput (lXMLOutput,'Warehouse',ttTempJob.Warehouse,'Col').
            RUN XMLOutput (lXMLOutput,'Available_Deliverly_Hours',ttTempJob.DockHours,'Col').
            RUN XMLOutput (lXMLOutput,'Dock_Appointment_Number',ttTempJob.DockAppt,'Col').
            RUN XMLOutput (lXMLOutput,'Dock_Appointment_Contact',ttTempJob.DockContact,'Col').
        RUN XMLOutput (lXMLOutput,'/TicketPrint','','Row').
        RUN XMLOutput (lXMLOutput,'/JobTicket','','Row').
        
        IF LAST-OF(ttTempJob.jobID) THEN
          RUN XMLOutput (lXMLOutput,'/JDF','','Row').
    END.
    
    {xmloutput/XMLOutput.i &XMLClose} 
END.

