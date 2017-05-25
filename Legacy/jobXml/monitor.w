/* monitor.w */

{custom/monitor.w "jobXml" "jobXml"}
{XMLOutput/XMLOutput.i &Company=cocode &NEW=NEW}
lXmlOutput = TRUE.
PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:     import montiored file, create receipt record, post
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE monitorFile AS CHARACTER FORMAT 'X(50)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.
  DEFINE VARIABLE errStatus AS INTEGER NO-UNDO.
  DEFINE VARIABLE saveMonitor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hPDS AS HANDLE NO-UNDO.
  DEFINE VARIABLE nextRNo AS INTEGER NO-UNDO.
  DEFINE VARIABLE nextRelease AS INTEGER NO-UNDO.
  DEFINE VARIABLE nextRelNo AS INTEGER NO-UNDO.
  DEFINE VARIABLE cPathIn  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPathout AS CHARACTER NO-UNDO.
  DEFINE BUFFER bf-eddoc FOR EDDoc.
  DEFINE VARIABLE cRtnChar     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE PrePressHotFolderIn-char AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE PrePressHotFolderOut-char AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFullFilePath AS CHARACTER   NO-UNDO.
    
RUN sys/ref/nk1look.p (INPUT cocode, "PrePressHotFolderIn", "C" /* Char */, NO /* check by cust */, 
   INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
   OUTPUT cRtnChar, OUTPUT lRecFound).
    
IF lRecFound THEN
   PrePressHotFolderIn-char = cRtnChar NO-ERROR.
    
    
RUN sys/ref/nk1look.p (INPUT cocode, "PrePressHotFolderOut", "C" /* Char */, NO /* check by cust */, 
   INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
   OUTPUT cRtnChar, OUTPUT lRecFound).
    
IF lRecFound THEN
   PrePressHotFolderOut-char = cRtnChar NO-ERROR.
    
cPathIn  = PrePressHotFolderIn-char.
cPathOut = PrePressHotFolderOut-char.
            
RUN monitorActivity ('Check New Jobs ' + monitorImportDir,YES,'').
FOR EACH job NO-LOCK WHERE job.company EQ g_company 
                       AND job.opened EQ TRUE
                       AND job.due-date GE TODAY:
                           
    FIND FIRST EDDoc NO-LOCK WHERE EDDoc.SetID EQ job.job-no
      AND eddoc.docID EQ job.job-no + STRING(job.job-no2) NO-ERROR.
    IF NOT AVAILABLE EDDoc THEN DO:
        
        FIND FIRST EDMast EXCLUSIVE-LOCK WHERE EDMast.Partner EQ "Esko" NO-ERROR.
        IF NOT AVAILABLE EDMast THEN DO: 
          CREATE EDMast.
          ASSIGN EDMast.Partner = "Esko"
                 .
        END.
        EDMast.Seq = EDMast.Seq + 1.
        FIND CURRENT EDMast NO-LOCK.
        cPathIn = edmast.path-in.
        cPathOut = edmast.path-out.
        
        CREATE EDDoc.
        ASSIGN EDDoc.Unique-Order-No = EDMast.seq
               EDDoc.setID           = job.job-no
               EDDoc.DocID           = job.job-no + STRING(job.job-no2)
               EDDoc.DocSeq          = job.job-no2
               EDDoc.Partner         = "Esko"
               EDDoc.Seq             = EDMast.Seq
               EDDoc.stat            = 0
               EDDoc.FGID            = ""
               .
    END. /* not avail eddoc for this job */
   
    
END.
    
FOR FIRST EDDoc NO-LOCK WHERE EDDoc.stat EQ 0
                         AND EDDoc.FGID EQ ""
                         AND EDDoc.Partner EQ "Esko":
    FIND FIRST job NO-LOCK WHERE job.company EQ g_company
                             AND job.job-no EQ EDDoc.setID
                             AND job.job-no2 EQ EDDoc.docSeq
                           NO-ERROR.
    IF AVAILABLE job THEN DO:
        lXmlOutput = TRUE.
        RUN oe/oeJobXml.p (INPUT g_company).
     
      FIND FIRST bf-eddoc EXCLUSIVE-LOCK WHERE ROWID(bf-eddoc) EQ rowid(eddoc) NO-ERROR.
      IF AVAILABLE bf-eddoc THEN 
        bf-eddoc.stat = 1.
    END.
END. /* Each eddoc */
                  
                
      
      
RUN monitorActivity ('Check dir ' + monitorImportDir,YES,'').
  INPUT FROM OS-DIR(monitorImportDir) NO-ECHO.
  REPEAT:
    SET monitorFile ^ attrList.
    IF attrList NE 'f' OR monitorFile BEGINS '.' OR
       INDEX(monitorFile,'.xml') EQ 0 THEN NEXT.
    cFullFilePath = monitorImportDir + "\" + monitorFile.
    OS-COPY VALUE(cFullFilePath) VALUE(cPathOut).    

    IF INTEGER(OS-ERROR) EQ 0 THEN 
      OS-DELETE VALUE(cFullFilePath).
          
    RUN monitorActivity ('Moving ' + monitorFile,YES,'').
  END. /* os-dir repeat */
  INPUT CLOSE.
  
  RUN monitorActivity ('Check dir ' + cPathIn,YES,'').
  INPUT FROM OS-DIR(cPathIn).
    REPEAT:
    SET monitorFile ^ attrList.
    IF attrList NE 'f' OR monitorFile BEGINS '.' OR
       INDEX(monitorFile,'.xml') EQ 0 THEN NEXT.
     cFullFilePath = cPathIn + "\" + monitorFile.
     RUN processResultXML (INPUT cFullFilePath).
/*    IF OS-ERROR EQ 0 THEN          */
/*      OS-DELETE VALUE(monitorFile).*/
    RUN monitorActivity ('Processing ' + monitorFile,YES,'').
  END. /* os-dir repeat */
  INPUT CLOSE.
  
END PROCEDURE.

PROCEDURE processResultXML:
  DEFINE INPUT PARAMETER cInputFile AS CHARACTER NO-UNDO.
RUN monitorActivity ('Processing Result XML' ,YES,'').
END PROCEDURE.

