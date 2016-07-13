/*------------------------------------------------------------------------------
 File        : support/dasupport.p    
 Description : data access super 
 Notes       : 
             - fetchData supports conditional retrieval of tables
             - All request methods are statefree (attaches and detaches sources) 
               and passes dataset around  by-reference. (no get or set)
             - query is only supported for the first table   
 ------------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING Consultingwerk.OERA.Query.*.
USING Consultingwerk.Util.* .

/* ************ Functions ****************************************/        
/* To be used by generic super destroy  */
function getObjectType returns character ( ) :
  if target-procedure = this-procedure then 
    return "super":U. 
  else 
    return "DataAccessObject":U.                                            
end.
 

procedure fetchData:
/*------------------------------------------------------------------------------
  Purpose: Fetch data from the physical storage.
               
Parameters:  
     phDataSet      - Prodataset handle (NOT dataset-handle)
     pcContext      - context    
     pcTables       - Tables for which to retrieve data or definitions
     pcQueries      - List of queries that corresponds to the requested tables
     pcRequest      - The actual request for the tables   
     piNumRecords   - Batch sizes
 
   Notes:  uses static data sources   
 -----------------------------------------------------------------------------*/
  define input        parameter pcTables       as character no-undo.
  define input        parameter pcQueries      as character no-undo.
  define input        parameter pcRequest      as character no-undo.
  define input        parameter piBatchSize    as integer   no-undo.
  define input-output parameter pcContext      as character no-undo.
  define output       parameter dataset-handle phDataset.
  
  define variable iBuffer      as integer   no-undo.
  define variable iTable       as integer   no-undo.
  define variable hBuffer      as handle    no-undo.
  define variable cQuery       as character no-undo.
  define variable hQuery       as handle    no-undo. 
  define variable iDSbuffer    as integer   no-undo. 
  define variable oQueryString as class DSQueryStringProcedural.
   
  DEFINE VARIABLE hFillBuffer AS HANDLE NO-UNDO. 
   
  run attachDataSources in target-procedure 
                           (input-output dataset-handle phDataset by-reference). 
 
  IF ProcedureHelper:HasInternalProcedure (TARGET-PROCEDURE, "defineReadEvents":U) THEN 
      run defineReadEvents in target-procedure 
                               (input-output dataset-handle phDataset by-reference)
                               no-error.
     
  /* Mike Fechner, Consultingwerk Ltd. 03.04.2009
     Only return current result set */
  phDataset:EMPTY-DATASET () .
       
  do iBuffer = 1 to phDataset:num-buffers:
    assign /* set handle and check if table is requested */
      hBuffer = phDataSet:GET-BUFFER-HANDLE(iBuffer)
      iTable  = LOOKUP(hBuffer:NAME,pcTables).
    /* if table found (and valid datasource )*/ 
    IF iTable > 0 THEN
    DO: 
      hBuffer:FILL-MODE = "REPLACE":U.
        
      /* Mike Fechner, Consultingwerk Ltd. 23.07.2009
         The first table might not have a DATA-SOURCE, it might implement
         custom fill using a tables BEFORE-FILL event (before Change 10274) 
         this code was dependent on the DATA-SOURCE*/        
      IF iTable = 1 THEN 
          ASSIGN hFillBuffer = hBuffer .    
        
      if valid-handle(hBuffer:data-source) then 
      do:
        if iTable = 1 then
        do:                          
          /* set batchsize */
          hBuffer:batch-size = piBatchSize.
          /* if context set batch start */
          if ENTRY(1, pcContext, CHR(1)) > '' then 
            hBuffer:data-source:restart-rowid = to-rowid(ENTRY(1, pcContext, CHR(1))).
          /* query mapping with callback to sourcecolumn and source-default-query */  
          cQuery  = entry(1,pcQueries,chr(1)).
          
          oQueryString = new DSQueryStringProcedural
                        (cQuery, target-procedure, hBuffer:name).
          create query hQuery.
          do iDSbuffer = 1 to hBuffer:data-source:query:num-buffers:  
            hQuery:add-buffer(hBuffer:data-source:query:get-buffer-handle(iDsbuffer)).
          end.
          hBuffer:data-source:query = hQuery.    
          oQueryString:insertToQuery(hQuery).
          delete object oQueryString. 
          
/*          MESSAGE cQuery SKIP          */
/*                  hQuery:PREPARE-STRING*/
/*              VIEW-AS ALERT-BOX.       */
/*                                       */
        end. /* first table */               
        ELSE DO:
          IF NUM-ENTRIES(pcQueries, CHR(1)) >= iTable THEN DO:
    
              /* Get Querystring for this buffer and ensure 4 entries - separated by space */              
              cQuery = ENTRY(iTable ,pcQueries,CHR(1)) + "   ":U.
              
              IF ENTRY(1, cQuery, " ":U) = "FOR":U OR ENTRY(1, cQuery, " ":U) = "PRESELECT":U THEN
                   ASSIGN
                       ENTRY(1, cQuery, " ":U) = "":U
                       ENTRY(2, cQuery, " ":U) = "":U.

              IF ENTRY(3, cQuery, " ":U) = hBuffer:NAME THEN
                  ASSIGN
                      ENTRY(3, cQuery, " ":U) = "":U.

              IF ENTRY(4, cQuery, " ":U) = "WHERE":U THEN
                  ASSIGN
                      ENTRY(4, cQuery, " ":U) = "":U.

              /* Replace temp-table name with database table name and trim unneccessary spaces */
              ASSIGN cQuery = TRIM(REPLACE(cQuery, hBuffer:NAME, hBuffer:DATA-SOURCE:GET-SOURCE-BUFFER(1):NAME)) . 
 
              /* append cQuery to data-source fill-where-string */
              IF cQuery > "":U THEN DO:
                  IF hBuffer:DATA-SOURCE:FILL-WHERE-STRING > "":U THEN
                      hBuffer:DATA-SOURCE:FILL-WHERE-STRING = hBuffer:DATA-SOURCE:FILL-WHERE-STRING + 
                                                              (IF NOT cQuery BEGINS "BY":U THEN " AND ":U ELSE " ":U) + cQuery . 
                  ELSE
                      hBuffer:DATA-SOURCE:FILL-WHERE-STRING = "WHERE ":U + cQuery .

              END.
          END.              
        END.        
      end.   
    end.
    else
      hBuffer:fill-mode = "NO-FILL":U.     
  end.
  
  /* Mike Fechner, Consultingwerk Ltd. 03.04.2009
     FILL based on child table or dataset */
  
  IF ENTRY(1, pcTables) <> phDataset:GET-TOP-BUFFER(1):NAME THEN 
      hFillBuffer:FILL() .
  ELSE 
      phDataset:fill().
      
	/* Marko Rüterbories, Consultingwerk Ltd. 18.06.2009
	   If PARENT-RELATION:REPOSITION of a Buffer which is requested is TRUE
       a dedicated FILL operation for this Buffer has to be performed. */
    DO iBuffer = 1 TO phDataset:NUM-BUFFERS:
        IF CAN-DO (pcTables, phDataset:GET-BUFFER-HANDLE (iBuffer):NAME) 
           AND VALID-HANDLE (phDataset:GET-BUFFER-HANDLE (iBuffer):PARENT-RELATION) THEN
               
            IF phDataset:GET-BUFFER-HANDLE (iBuffer):PARENT-RELATION:REPOSITION THEN
                phDataset:GET-BUFFER-HANDLE (iBuffer):FILL ().
    END.
          
  /* set batch context for first table */
  hBuffer = phDataset:get-buffer-handle(entry(1,pcTables)).
  
  /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
     Support Tables with no DataSource  */  
  IF (NOT hBuffer:LAST-BATCH) AND VALID-HANDLE (hBuffer:DATA-SOURCE) THEN
    ENTRY(1, pcContext, CHR(1)) = string(hBuffer:data-source:next-rowid).
  else
    ENTRY(1, pcContext, CHR(1)) = ''.  
  run detachDataSources in target-procedure
             (input-output dataset-handle phDataset by-reference).
             
  /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
     Query won't be valid if primary table has no DATA-SOURCE */
  IF VALID-HANDLE (hQuery) THEN                   
      delete object hQuery.           
end. /* retrieveData */

PROCEDURE saveBuffer :
/*------------------------------------------------------------------------------
  Purpose:     Called from saveChanges to process changes
               for a single DataSet temp-table buffer.
  Parameters:  buffer handle
  Notes:       This is similar to the OERI BE version, but has no pre and
               post transaction hooks because:  
               - This method might very well be called inside a transaction.
               - The BE supports hooks outside of the transaction scope. 
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER phBuffer AS HANDLE NO-UNDO.

    DEFINE VARIABLE hBeforeBuff AS HANDLE NO-UNDO.
    DEFINE VARIABLE cLogicProc  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hDataSet    AS HANDLE     NO-UNDO.
    define variable hBeforeQry  as handle    no-undo.

    /* phBuffer is the after-table. Walk through the modified rows in
       the before-table, specifically to capture DELETEs, even though it
       is of course the after-table values that Progress writes to the db
       for CREATEs and MODIFYs. */
    hBeforeBuff = phBuffer:BEFORE-BUFFER.
    
    IF VALID-HANDLE(hBeforeBuff) THEN
    DO: 
      create query hBeforeQry.
      hBeforeQry:SET-BUFFERS(hBeforeBuff).
      hBeforeQry:QUERY-PREPARE("FOR EACH ":U + hBeforeBuff:NAME).
      hBeforeQry:QUERY-OPEN().
      hBeforeQry:GET-FIRST().
      /* Process all the updates for the current table. */
      DO WHILE NOT hBeforeQry:QUERY-OFF-END:
        RUN commitChanges IN target-procedure (INPUT phBuffer).
        hBeforeQry:GET-NEXT().
      END. /* END DO WHILE NOT QUERY-OFF-END */
    END. /* END DO IF VALID-HANDLE */
    
    FINALLY:
        DELETE OBJECT hBeforeQry.
    END FINALLY.
    
END PROCEDURE. /* saveBuffer */

procedure saveChanges:
/*------------------------------------------------------------------------------
  Purpose:  Save changes to physical storage                
Parameters: dataset dsOrder 
     Notes: This example of data access save traverses all records in order
            to: 
            - Allow the DA to control transactions, which cannot be directly 
              controlled in distributed BL. Transaction control is a concern of 
              the BL, but should not be contolled with TRANSACTION keyword.
            - Offer a course grained save interface from distributed BL    
          - The default operation here is just a transaction per record
          - This simple sample does not use child relations to traverse 
            the buffers as the OERI BE example. (A property that controls 
            update order would give the same effect).
          - Traversing of child data from a parent would make sense if done for 
            one parent record at a time.     
-----------------------------------------------------------------------------*/
  define input-output parameter dataset-handle phDataset.

  define variable lOk         as logical no-undo. 
  define variable iBuffer     as integer   no-undo.
  define variable hBuffer     as handle    no-undo.

  run attachDataSources in target-procedure
                           (input-output dataset-handle phDataset by-reference). 
  
  do transaction:
    do iBuffer = 1 TO phDataSet:num-buffers:
      hBuffer = phDataSet:get-buffer-handle(iBuffer).
      run saveBuffer in target-procedure(hBuffer).
    end.  /* do iBuffer = 1 */
  end.
  run detachDataSources in target-procedure
                           (input-output dataset-handle phDataset by-reference).  
end. /* saveChanges */ 

PROCEDURE commitChanges :
/*------------------------------------------------------------------------------
  Purpose: This procedure is called from saveChanges in the DA
           to do the actual database commit and any transaction-related
           validation logic for a buffer.      
  Parameters:  buffer handle.
  Notes:   This is taken directly from the OERI. 
        -  Note that in this sample that illustrates support for distributed 
           business logic the DA saves the whole dataset in SaveChanges, so 
           this is not currently called from the BE.  
        -  The event names are the same as in the OERI. The word "Trans" 
           is somewhat misleading as the DA need to support large transactions.
           One could consider using a name that indicates that this is a row 
           event instead.
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER phBuffer     AS HANDLE     NO-UNDO.

    DEFINE VARIABLE hDataSet    AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hBeforeBuff AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cLogicProc  AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO.
    DEFINE VARIABLE i           AS INTEGER    NO-UNDO.

    /* Mike Fechner, Consultingwerk Ltd. 02.07.2009
       Use the ORIGIN-HANDLE Table name for evaluating the table-name 
       Required if changes-dataset contains name prefix (i.e. "ch_") 
       When send form client */    
    IF VALID-HANDLE(phBuffer:TABLE-HANDLE:ORIGIN-HANDLE) THEN
        ASSIGN hTable = phBuffer:TABLE-HANDLE:ORIGIN-HANDLE .
    ELSE 
        ASSIGN hTable = phBuffer:TABLE-HANDLE .

    ASSIGN hBeforeBuff = phBuffer:BEFORE-BUFFER
           hDataSet    = phBuffer:DATASET
           cLogicProc  = hTable:NAME 
                     + IF hBeforeBuff:ROW-STATE = ROW-DELETED THEN "Delete":U
                       ELSE IF hBeforeBuff:ROW-STATE = ROW-CREATED THEN "Create":U
                       ELSE "Modify":U.
    /* This is a sub transaction, the actual transaction is in savechanges */                  
    DO TRANSACTION ON ERROR UNDO, LEAVE:
      IF ProcedureHelper:HasInternalProcedure (TARGET-PROCEDURE, cLogicProc + "BeginTrans":U) THEN          
          RUN VALUE (cLogicProc + "BeginTrans":U) IN TARGET-PROCEDURE
                (INPUT DATASET-HANDLE hDataSet BY-REFERENCE) NO-ERROR.
         
      IF NOT hBeforeBuff:ERROR THEN
      DO:
        hBeforeBuff:SAVE-ROW-CHANGES() NO-ERROR.
        
        /* Mike Fechner, Consultingwerk Ltd. 08.09.2009
           Error-Handling for errors raised from SAVE-ROW-CHANGES
           Errors will set the ERROR attribute on the row, and add
           a message to the ERROR-STATUS system handle. */        
        
        IF ERROR-STATUS:ERROR OR ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            IF hBeforeBuff:ERROR-STRING = ? THEN 
                hBeforeBuff:ERROR-STRING = "":U . 
            
            ASSIGN hBeforeBuff:ERROR = TRUE 
                   hDataSet:ERROR    = TRUE .
                
            /* Mike Fechner, Consultingwerk Ltd. 08.09.2009
               Handle message for optimistic locking  */
            /* Mike Fechner, Consultingwerk Ltd. 01.09.2010
               Added error message number, to simplify detailled error handling */
            IF ERROR-STATUS:GET-NUMBER (1) = 11913 THEN DO:
                hBeforeBuff:ERROR-STRING = hBeforeBuff:ERROR-STRING + 
                                           (IF hBeforeBuff:ERROR-STRING > "":U THEN "~n":U ELSE "":U) +
                                           "The current row has been changed by another user! (11913)":U .                                
            END.
            ELSE DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:                                      
                   hBeforeBuff:ERROR-STRING = hBeforeBuff:ERROR-STRING + 
                                              (IF hBeforeBuff:ERROR-STRING > "":U THEN "~n":U ELSE "":U) +
                                              ERROR-STATUS:GET-MESSAGE(i) .                   
            END.            
        END.
               
        IF NOT hBeforeBuff:ERROR THEN DO:
         IF ProcedureHelper:HasInternalProcedure (TARGET-PROCEDURE, cLogicProc + "EndTrans":U) THEN                     
             RUN VALUE (cLogicProc + "EndTrans":U) IN TARGET-PROCEDURE
                  (INPUT DATASET-HANDLE hDataSet BY-REFERENCE) NO-ERROR.
              
         /* Mike Fechner, Consultingwerk Ltd. 04.07.2009
            Errors flagged from EndTrans procedure should undo the transaction */
         IF hBeforeBuff:ERROR THEN 
             UNDO, RETURN .                             
        END.

      END. /* END DO IF no error after BeginTrans validation. */       
    END.     /* END RowTrans TRANSACTION block */
END PROCEDURE.   /* commitChanges */  

procedure detachDataSources:
  define input-output parameter dataset-handle phDataset.
  define variable hBuffer as handle    no-undo.
  define variable iBuffer as integer   no-undo.
  do iBuffer = 1 to phDataset:num-buffers:
    hBuffer = phDataset:get-buffer-handle(iBuffer).
    if valid-handle(hBuffer:data-source) then 
      hBuffer:detach-data-source(). 
  end.
end. 
  
procedure createObject:
  /* nothing yet. Could create property storage.. see beSupport  */
end. 

procedure destroyObject:
  delete procedure target-procedure.  
end.     

/* used by locateProcedure (see locateprocedure.i) */
procedure isRunning:
  define output parameter phThis as handle no-undo.
  phThis = this-procedure.
end.  
       
       
