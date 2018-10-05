
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

DEFINE VARIABLE cSheetBlank     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUDFString      AS CHARACTER NO-UNDO.
/* Insert obtaining sys-ctrl values for jobXml */



DEFINE TEMP-TABLE ttTempJob
    FIELD company      AS CHARACTER
    FIELD jobID        AS CHARACTER
    FIELD newProject   AS INTEGER
    FIELD customerID   AS CHARACTER
    FIELD customerName AS CHARACTER 
    FIELD FGItemCode   AS CHARACTER
    FIELD FGName       AS CHARACTER
    FIELD CustPart     AS CHARACTER 
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
                    ttTempjob.itemRecKey = itemfg.rec_key
                    ttTempjob.FGItemCode = cItemOnOrder
                    ttTempjob.FGName     = itemfg.i-name
                    ttTempjob.CustPart   = cCustPart
                    ttTempjob.ItemStatus = (IF itemfg.stat EQ "A" THEN "Active" ELSE "Inactive") 
                    ttTempjob.FgCategory = itemfg.procat-desc
                    .

                /* Override of itemfg.procat-desc to match logic in viewers/itemfg.w */    
                FIND FIRST fgcat NO-LOCK WHERE fgcat.company = job-hdr.company 
                                           AND fgcat.procat = itemfg.procat
                                         NO-ERROR.
                ttTempjob.FgCategory = IF AVAIL fgcat THEN fgcat.dscr ELSE ttTempjob.FgCategory.
                                    
                IF AVAILABLE cust THEN 
                    ASSIGN ttTempjob.customerID   = cust.cust-no
                        ttTempJob.customerName = cust.name
                        .
                IF AVAILABLE eb THEN 
                  ASSIGN                   
                    ttTempJob.ebWIDTH      = STRING(eb.wid)
                    ttTempJob.ebLENGTH     = STRING(eb.len)
                    ttTempJob.ebDepth      = STRING(eb.dep)
                    ttTempJob.FlatWidth    = STRING(eb.t-len)
                    ttTempJob.FlatLength   = STRING(eb.t-wid)
                    ttTempJob.ColorsCoat   = eb.i-dscr2[1]
                    ttTempJob.CCNumber     = eb.cad-no                  
                  .
                IF AVAILABLE ef THEN 
                  ASSIGN  ttTempJob.Weight       = STRING(ef.weight)
                          ttTempJob.Caliper      = STRING(ef.cal)                    
                          ttTempJob.Board        = fnStripInvalidChar(ef.brd-dscr)
                          .
                IF AVAILABLE oe-ord THEN 
                 ASSIGN 
                   ttTempJob.DateIssued   = STRING(oe-ord.ord-date)
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
            
        IF LAST-OF(ttTempJob.jobID) THEN
          RUN XMLOutput (lXMLOutput,'/JDF','','Row').
    END.
    
    {xmloutput/XMLOutput.i &XMLClose} 
END.

