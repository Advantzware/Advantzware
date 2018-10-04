
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


/* ***************************  Main Block  *************************** */

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
                                   
                             
            cocode = job-hdr.company.
            FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ job-hdr.company
                AND sys-ctrl.name EQ "xmljob"
                NO-ERROR.
            IF NOT AVAILABLE sys-ctrl THEN RETURN.
       
            FIND FIRST cust NO-LOCK WHERE cust.company EQ job-hdr.company
                AND cust.cust-no EQ job-hdr.cust-no
                NO-ERROR.
    
    
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
                    ttTempjob.FGItemCode = itemfg.i-no
                    ttTempjob.FGName     = itemfg.i-name
                    ttTempjob.CustPart   = itemfg.part-no
                    ttTempjob.ItemStatus = (IF itemfg.stat EQ "A" THEN "Active" ELSE "Inactive")             
                    .
                IF AVAILABLE cust THEN 
                    ASSIGN ttTempjob.customerID   = cust.cust-no
                        ttTempJob.customerName = cust.name
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
            RUN XMLOutput (lXMLOutput,'/Product','','Row').     
                                
        RUN XMLOutput (lXMLOutput,'/ResourcePool','','Row').
            
        IF LAST-OF(ttTempJob.jobID) THEN
          RUN XMLOutput (lXMLOutput,'/JDF','','Row').
    END.
    
    {xmloutput/XMLOutput.i &XMLClose} 
END.

