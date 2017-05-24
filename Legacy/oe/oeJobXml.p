
/*------------------------------------------------------------------------
    File        : oe850xml.p
    Purpose     : 

    Syntax      :

    Description : Export job-related xml data to Esko

    Author(s)   : 
    Created     : Wed Nov 09 19:32:08 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iprJobRowid AS ROWID NO-UNDO.

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
FIND FIRST job NO-LOCK WHERE ROWID(job) = iprjobRowid
    NO-ERROR.
DEFINE VARIABLE cocode AS CHARACTER NO-UNDO.
IF AVAILABLE job THEN 
    cocode = job.company.

{XMLOutput/XMLOutput.i &XMLOutput=XMLJOB &Company=cocode}
    
{UDF/ttUDF.i}                
                         
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



        FIND FIRST sys-ctrl NO-LOCK
             WHERE sys-ctrl.company EQ  cocode
               AND sys-ctrl.name    EQ 'XMLJOB' NO-ERROR.

        IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
          ASSIGN
            XMLTimeStamp = STRING(YEAR(TODAY),'9999')
                             + '-'
                             + STRING(MONTH(TODAY),'99')
                             + '-'
                             + STRING(DAY(TODAY),'99')
                             + 'T'
                             + STRING(TIME,'hh:mm:ss')
                             + '-05:00'
            cXMLOutput = sys-ctrl.char-fld
            iXMLOutput = sys-ctrl.int-fld
            .
          
          ASSIGN
            XMLFile = '/XMLJOB.' + STRING(TIME,'99999') + '.xml'
            XMLTemp = 'XMLOutput' + XMLFile
            .
          OUTPUT STREAM XMLOutput TO VALUE(XMLTemp).
        END.
        
IF AVAILABLE job THEN 
DO:
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
            AND ttTempJob.jobID = trim(job-hdr.job-no) + "-" + cSheetBlank
            AND ttTempJob.newProject EQ 1
            NO-ERROR.
    
        IF NOT AVAILABLE ttTempJob THEN 
        DO:
    
            CREATE ttTempJob.
            ASSIGN
                ttTempjob.company = job-hdr.company
                ttTempjob.jobID   = TRIM(job-hdr.job-no) + "-" + cSheetBlank
                ttTempjob.itemRecKey  = itemfg.rec_key
                ttTempjob.FGItemCode   = itemfg.i-no
                ttTempjob.FGName       = itemfg.i-name
                ttTempjob.CustPart     = itemfg.part-no
                ttTempjob.ItemStatus   = (IF itemfg.stat EQ "A" THEN "Active" ELSE "Inactive")             
                .
            IF AVAILABLE cust THEN 
                ASSIGN ttTempjob.customerID   = cust.cust-no
                    ttTempJob.customerName = cust.name
                    .
        
        END.  /* create ttTempJob */
    END. /* Each job-hdr of job */ 
END.
ELSE 
DO:
    RETURN.
END.
 
lXmlOutput = TRUE.
IF AVAILABLE sys-ctrl THEN 
DO:
    RUN XMLOutput (lXMLOutput,'JobRow','','Row').
    
    FOR EACH ttTempJob BREAK BY ttTempJob.jobID:
        
        IF FIRST-OF(ttTempJob.jobID) THEN DO:
                        
            RUN XMLOutput (lXMLOutput,'Company',ttTempJob.company,'Col').
            RUN XMLOutput (lXMLOutput,'JobID',ttTempJob.jobID,'Col').
            RUN XMLOutput (lXMLOutput,'NewProject',0,'Col').
            RUN XMLOutput (lXMLOutput,'CustomerID',ttTempJob.customerID,'Col').
            RUN XMLOutput (lXMLOutput,'CustomerName',ttTempJob.customerName,'Col').
        END.
        
        /* Process Itemfg Level */
        RUN XMLOutput (lXMLOutput,'ItemfgRow','','Row').
        RUN XMLOutput (lXMLOutput,'FGItemCode', ttTempjob.FGItemCode,'Col').
        RUN XMLOutput (lXMLOutput,'FGName'    , ttTempjob.FGName,'Col').
        RUN XMLOutput (lXMLOutput,'CustPart'  , ttTempjob.CustPart,'Col').     
        RUN XMLOutput (lXMLOutput,'ItemStatus', ttTempjob.ItemStatus,'Col').  
        
        EMPTY TEMP-TABLE ttUDF.
        RUN UDF/UDF.p ("Esko", ttTempjob.itemRecKey, OUTPUT TABLE ttUDF).
        
        FOR EACH ttUDF NO-LOCK WHERE ttUDF.udfEsko:
            RUN XMLOutput (lXMLOutput,ttUDF.udfLabel,ttUDF.udfValue,'Col').
        END. 
        RUN XMLOutput (lXMLOutput,'/ItemfgRow','','Row').                         
        
    END.
    RUN XMLOutput (lXMLOutput,'/JobRow','','Row').
    {xmloutput/XMLOutput.i &XMLClose} 
END.

