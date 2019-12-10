/*------------------------------------------------------------------------
    File        : queryLib.p    
    Purpose     : Library with common functions for queryBuilder

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/

{queryLib.i &InSuper=*}

/* Make this super react to its name */
SUBSCRIBE TO "queryLib" ANYWHERE.

PROCEDURE queryLib:
  DEFINE OUTPUT PARAMETER phSelf AS HANDLE NO-UNDO.
  phSelf = THIS-PROCEDURE:HANDLE.
END PROCEDURE.


/* Placeholder solution for user type, should be implemented in 
** host application */
DEFINE VARIABLE gcUserType AS CHARACTER NO-UNDO INITIAL 'normal'.


PROCEDURE createQuery:
  /* Create an empty query
  */
  DEFINE INPUT PARAMETER pcUserId  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER piQueryNr AS INTEGER NO-UNDO.
  
  DEFINE BUFFER bQueryNew FOR qbQuery.

  piQueryNr = getNewQueryNr().
  
  CREATE bQueryNew.
  ASSIGN bQueryNew.queryNr    = piQueryNr
         bQueryNew.queryUser  = pcUserId
         bQueryNew.predefined = FALSE.

END PROCEDURE. /* createQuery */


PROCEDURE closeQuery:
  DEFINE INPUT PARAMETER phQuery AS HANDLE NO-UNDO.
  DEFINE VARIABLE iBuffer AS INTEGER NO-UNDO.

  IF NOT VALID-HANDLE(phQuery) THEN RETURN. 
  phQuery:QUERY-CLOSE NO-ERROR.

  DO iBuffer = 1 TO phQuery:NUM-BUFFERS:
    DELETE OBJECT phQuery:GET-BUFFER-HANDLE(iBuffer) NO-ERROR.
  END.

  DELETE OBJECT phQuery NO-ERROR.
END PROCEDURE. /* closeQuery */


PROCEDURE copyQuery :
  /* Copy a query to a new user
  */
  DEFINE INPUT PARAMETER piQueryNr AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pcUserId  AS CHARACTER NO-UNDO.

  DEFINE BUFFER bQueryOrg FOR qbQuery.
  DEFINE BUFFER bQueryNew FOR qbQuery.
  DEFINE BUFFER bTableOrg FOR qbTable.
  DEFINE BUFFER bTableNew FOR qbTable.
  DEFINE BUFFER bFieldOrg FOR qbField.
  DEFINE BUFFER bFieldNew FOR qbField.
  
  /* Find org query */
  FIND bQueryOrg NO-LOCK WHERE bQueryOrg.queryNr = piQueryNr NO-ERROR.
  IF NOT AVAILABLE bQueryOrg THEN RETURN. 

  /* Query itself */
  CREATE bQueryNew.
  BUFFER-COPY bQueryOrg TO bQueryNew 
    ASSIGN bQueryNew.queryNr    = getNewQueryNr()
           bQueryNew.queryUser  = pcUserId
           bQueryNew.predefined = FALSE.

  /* Tables of query */
  FOR EACH bTableOrg NO-LOCK WHERE bTableOrg.queryNr = piQueryNr:
  
    CREATE bTableNew.
    BUFFER-COPY bTableOrg TO bTableNew 
      ASSIGN bTableNew.queryNr = bQueryNew.queryNr.
  END.

  /* Fields of query */
  FOR EACH bFieldOrg NO-LOCK WHERE bFieldOrg.queryNr = piQueryNr:
    CREATE bFieldNew.
    BUFFER-COPY bFieldOrg TO bFieldNew 
      ASSIGN bFieldNew.queryNr = bQueryNew.queryNr.
  END.

END PROCEDURE. /* copyQuery */


PROCEDURE deleteQuery :
  /* Delete a query and its settings
  */
  DEFINE INPUT PARAMETER piQueryNr AS INTEGER   NO-UNDO.

  DEFINE BUFFER bQuery FOR qbQuery.
  DEFINE BUFFER bTable FOR qbTable.
  DEFINE BUFFER bField FOR qbField.

  /* Find query */
  FIND bQuery EXCLUSIVE-LOCK WHERE bQuery.queryNr = piQueryNr NO-ERROR.
  IF NOT AVAILABLE bQuery THEN RETURN. 

  /* Query itself */
  DELETE bQuery.

  /* Tables of query */
  FOR EACH bTable EXCLUSIVE-LOCK WHERE bTable.queryNr = piQueryNr:
    DELETE bTable.
  END.

  /* Fields of query */
  FOR EACH bField EXCLUSIVE-LOCK WHERE bField.queryNr = piQueryNr:
    DELETE bField.
  END.

END PROCEDURE. /* deleteQuery */


PROCEDURE getSchema:
  /* Return the schema of all connected databases
  */
  DEFINE OUTPUT PARAMETER DATASET FOR dsSchema.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  IF NOT TEMP-TABLE ttSchemaTable:HAS-RECORDS THEN
  DO:
    #Database:
    DO i = 1 TO NUM-DBS:
      IF DBTYPE(i) <> "PROGRESS" THEN NEXT #Database.
  
      CREATE ALIAS 'dictdb' FOR DATABASE VALUE(LDBNAME(i)).
      RUN getSchema.p(INPUT gcUserType, OUTPUT DATASET dsSchema BY-REFERENCE).    
      DATASET dsSchema:WRITE-XML("file", "c:\temp\schema.xml", YES, ?, ?, NO, NO).      
    END.    
  END.    
  
END PROCEDURE. /* getSchema */


PROCEDURE loadQuery :
  /* Load query from database into dataset
  */
  DEFINE INPUT PARAMETER piQueryNr AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER DATASET FOR dsQuery.

  DEFINE BUFFER bQuery FOR qbQuery.
  DEFINE BUFFER bTable FOR qbTable.
  DEFINE BUFFER bField FOR qbField.

  DATASET dsQuery:EMPTY-DATASET().
  
  /* Query itself */
  FIND bQuery NO-LOCK WHERE bQuery.queryNr = piQueryNr NO-ERROR.
  IF NOT AVAILABLE bQuery THEN RETURN.  
  CREATE ttQuery.
  BUFFER-COPY bQuery TO ttQuery.

  /* Tables of query */
  FOR EACH bTable NO-LOCK WHERE bTable.queryNr = piQueryNr:
    CREATE ttTable.
    BUFFER-COPY bTable TO ttTable.
  END.

  /* Fields of query */
  FOR EACH bField NO-LOCK WHERE bField.queryNr = piQueryNr:
    CREATE ttField.
    BUFFER-COPY bField TO ttField.
    IF ttField.fieldName MATCHES '*[*]*' THEN
      ttField.iExtent = INTEGER(ENTRY(1, ENTRY(2, ttField.fieldName, '['), ']')).
  END.

END PROCEDURE. /* loadQuery */


PROCEDURE openQuery:
  /* Open the query and return the handle
  */
  DEFINE INPUT PARAMETER DATASET FOR dsQuery.
  DEFINE INPUT PARAMETER piDepth AS INTEGER    NO-UNDO. /* how many joins, 0=all */
  DEFINE INPUT PARAMETER plQueryConditions AS LOGICAL NO-UNDO. /* "no" means table structure only */
  DEFINE OUTPUT PARAMETER phQuery AS HANDLE    NO-UNDO.
  DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE hBuffer  AS HANDLE      NO-UNDO.
  DEFINE VARIABLE cQuery   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cAnd     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lQueryOk AS LOGICAL     NO-UNDO.
  
  DEFINE BUFFER bTable FOR ttTable.
  DEFINE BUFFER bQuery FOR ttQuery.
  
  FIND bQuery.
  CREATE QUERY phQuery.

  #AddJoin:
  FOR EACH bTable BY bTable.orderNr:

    /* Build a limited set of joins */
    IF piDepth > 0 AND bTable.orderNr > piDepth THEN LEAVE #AddJoin.
  
    CREATE BUFFER hBuffer FOR TABLE bTable.tableName.
    phQuery:ADD-BUFFER(hBuffer).
    cAnd = ''.
    
    IF cQuery = '' THEN
      cQuery = SUBSTITUTE('FOR EACH &1 NO-LOCK', bTable.tableName).
    ELSE 
    DO:
      IF bTable.autoJoin THEN
        cQuery = SUBSTITUTE('&1, EACH &2 NO-LOCK OF &3', cQuery, bTable.tableName, bTable.parentTable).
      ELSE
        cQuery = SUBSTITUTE('&1, EACH &2 NO-LOCK', cQuery, bTable.tableName).      
    END.
      
    IF bTable.conditions <> '' THEN
    DO:
      cAnd = (IF cAnd = '' THEN 'WHERE' ELSE 'AND').
      cQuery = SUBSTITUTE('&1 &2 &3'
                         , cQuery
                         , (IF bTable.conditions BEGINS cAnd THEN '' ELSE cAnd)
                         , bTable.conditions).    
    END.
  END. /* FOR EACH bTable */

  /* Optionally check the conditions for the query as a whole 
  ** not needed if we only check a limited part of the query
  */
  IF piDepth = 0 AND plQueryConditions = TRUE AND bQuery.conditions <> '' THEN
  DO:
    cAnd = (IF cAnd = '' THEN 'WHERE' ELSE 'AND').
    cQuery = SUBSTITUTE('&1 &2 &3'
                       , cQuery
                       , (IF bQuery.conditions BEGINS cAnd THEN '' ELSE cAnd)
                       , bQuery.conditions).
  END.

  /* Strip strange combinations of operators */
  cQuery = REPLACE(cQuery,' WHERE AND ', ' WHERE ').
  
  /* Try to open it */
  lQueryOk = phQuery:QUERY-PREPARE(cQuery) NO-ERROR.
  IF NOT lQueryOk THEN 
  DO:
    pcError = ERROR-STATUS:GET-MESSAGE(1).
    RUN closeQuery(phQuery).
  END.    

END PROCEDURE. /* openQuery */


PROCEDURE saveQuery :
  /* Save query from dataset into db
  */
  DEFINE INPUT PARAMETER DATASET FOR dsQuery.

  DEFINE BUFFER bQuery FOR qbQuery.
  DEFINE BUFFER bTable FOR qbTable.
  DEFINE BUFFER bField FOR qbField.
  
  DEFINE VARIABLE iTableNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE iFieldNr AS INTEGER NO-UNDO.
  
  FIND ttQuery NO-ERROR.
  IF NOT AVAILABLE ttQuery THEN RETURN.
  
  /* Just delete the current query in the database. 
  ** This is a lot easier and saves some compare logic
  */
  RUN deleteQuery(ttQuery.queryNr).
  
  /* Create the query itself again */
  CREATE bQuery.
  BUFFER-COPY ttQuery TO bQuery.

  /* Tables of query */
  FOR EACH ttTable BY ttTable.orderNr:
    iTableNr = iTableNr + 1.
    CREATE bTable.
    BUFFER-COPY ttTable TO bTable ASSIGN bTable.orderNr = iTableNr.
  END.

  /* Fields of query */
  FOR EACH ttField BY ttField.orderNr:
    iFieldNr = iFieldNr + 1.
    CREATE bField.
    BUFFER-COPY ttField TO bField ASSIGN bField.orderNr = iFieldNr.
  END.
    
END PROCEDURE. /* saveQuery */


/* ************************  Function Implementations ***************** */
FUNCTION canHaveRelation RETURNS LOGICAL
  ( pcParent AS CHARACTER
  , pcChild AS CHARACTER) :

  DEFINE VARIABLE hQuery        AS HANDLE      NO-UNDO.
  DEFINE VARIABLE cWhere        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hChildBuffer  AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hParentBuffer AS HANDLE      NO-UNDO.
  DEFINE VARIABLE lQueryOpen    AS LOGICAL     NO-UNDO.

  CREATE QUERY hQuery.
  CREATE BUFFER hParentBuffer FOR TABLE pcParent NO-ERROR.
  CREATE BUFFER hChildBuffer  FOR TABLE pcChild NO-ERROR.
  IF NOT VALID-HANDLE(hParentBuffer) OR NOT VALID-HANDLE(hChildBuffer) THEN RETURN FALSE.
  
  hQuery:SET-BUFFERS(hParentBuffer,hChildBuffer).  
  cWhere = SUBSTITUTE('FOR EACH &1 NO-LOCK, EACH &2 NO-LOCK OF &1', hParentBuffer:NAME, hChildBuffer:NAME).
  lQueryOpen = hQuery:QUERY-PREPARE(cWhere) NO-ERROR.
  RETURN lQueryOpen.

  FINALLY:
    hQuery:QUERY-CLOSE.
    DELETE OBJECT hQuery NO-ERROR.
    DELETE OBJECT hParentBuffer NO-ERROR.
    DELETE OBJECT hChildBuffer NO-ERROR.
  END FINALLY.
  
END FUNCTION. /* canHaveRelation */


FUNCTION getLightGray RETURNS INTEGER
  ( /* no parameters */ ):

  /* Return the color nr for the color with RGB values 240,240,240
  ** (light gray). If it is not defined, then add it 
  */  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  /* See if already exists */
  DO i = 0 TO COLOR-TABLE:NUM-ENTRIES - 1:
    IF    COLOR-TABLE:GET-RED-VALUE(i)   = 240
      AND COLOR-TABLE:GET-GREEN-VALUE(i) = 240
      AND COLOR-TABLE:GET-BLUE-VALUE(i)  = 240 THEN RETURN i.
  END.

  /* Define new color */
  i = COLOR-TABLE:NUM-ENTRIES.
  COLOR-TABLE:NUM-ENTRIES = COLOR-TABLE:NUM-ENTRIES + 1.
  COLOR-TABLE:SET-DYNAMIC(i, TRUE).
  COLOR-TABLE:SET-RED-VALUE  (i, 240 ).
  COLOR-TABLE:SET-GREEN-VALUE(i, 240 ).
  COLOR-TABLE:SET-BLUE-VALUE (i, 240 ).
  
  RETURN i.  
END FUNCTION. /* getLightGray */


FUNCTION getNewQueryNr RETURNS INTEGER
  ( /* no parameters */ ) :
  
  /* This function will return a new nr for the query table.
  **
  ** The reason for using a next-value within a repeat loop
  ** is that it will auto-heal the sequence in case the 
  ** sequence is behind the value in the table (perhaps due 
  ** to dump/load and forgetting the sequence)
  */
  DEFINE VARIABLE iQueryNr AS INTEGER NO-UNDO.
  
  REPEAT:
    iQueryNr = NEXT-VALUE(QueryNr).
    IF NOT CAN-FIND(qbQuery WHERE qbQuery.queryNr = iQueryNr) THEN RETURN iQueryNr.  
  END.

END FUNCTION. /* getNewQueryNr */


FUNCTION getReadableQuery RETURNS CHARACTER
  ( INPUT pcQuery AS CHARACTER ):
  /* Return a query as a string that is readable for humans.
  */
  DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.

  /* Accept query or query-handle */
  hQuery = WIDGET-HANDLE(pcQuery) NO-ERROR.
  IF VALID-HANDLE( hQuery ) THEN
  DO:
    hQuery = WIDGET-HANDLE(pcQuery).
    pcQuery = hQuery:PREPARE-STRING.
  END.

  pcQuery = REPLACE(pcQuery,',' ,' ~n ,~n ').
  pcQuery = REPLACE(pcQuery,' EACH ' ,'  EACH ').
  pcQuery = REPLACE(pcQuery,' FIRST ',' FIRST ').
  pcQuery = REPLACE(pcQuery,' WHERE ',  '~n  WHERE ').
  pcQuery = REPLACE(pcQuery,' AND '  ,  '~n    AND ').
  pcQuery = REPLACE(pcQuery,'~nAND '  ,  '~n    AND ').
  pcQuery = REPLACE(pcQuery,' BY '   ,  '~n     BY ').
  pcQuery = REPLACE(pcQuery,' FIELDS ()','').
  pcQuery = REPLACE(pcQuery,'FOR EACH ' ,'FOR EACH ').
  pcQuery = REPLACE(pcQuery,' NO-LOCK',  ' NO-LOCK').
  pcQuery = REPLACE(pcQuery,' INDEXED-REPOSITION',  '').
  pcQuery = REPLACE(pcQuery,'FOR  EACH', 'FOR EACH').

  pcQuery = pcQuery + '~n'.

  RETURN pcQuery.
END FUNCTION. /* getReadableQuery */


FUNCTION getUserId RETURNS CHARACTER
  ( /* no parameters */ ) :
  
  /*
  ** This function will return the id of the user. 
  **
  ** Note that this should be done from within the host application,
  ** this function is only a placeholder. 
  */
  RETURN USERID(LDBNAME(1)).

END FUNCTION. /* getUserId */


FUNCTION getUserType RETURNS CHARACTER
  ( /* no parameters */ ) :
  
  /*
  ** This function will return the type of the user. This can be one of:
  ** 
  ** 'normal'     -> normal user
  ** 'admin'      -> admin user, can edit queries
  ** 'superadmin' -> super admin, has higher privileges than normal admin
  **
  ** Note that this should be done from within the host application,
  ** this function is only a placeholder. 
  */

  RETURN gcUserType. 

END FUNCTION. /* getUserType */


FUNCTION setUserType RETURNS LOGICAL
  ( pcUserType AS CHARACTER ) :
  
  /*
  ** This function will set the type of the user. This can be one of:
  ** 
  ** 'normal'     -> normal user
  ** 'admin'      -> admin user, can edit queries
  ** 'superadmin' -> super admin, has higher privileges than normal admin
  **
  ** Note that this should be done from within the host application,
  ** this function is only a placeholder. 
  */

  gcUserType = pcUserType. 

END FUNCTION. /* setUserType */



FUNCTION isQueryChanged RETURNS LOGICAL
  ( INPUT DATASET dsQuery ):
  /* Return wether the user has changed the query 
  */
  DEFINE VARIABLE lTheSame AS LOGICAL NO-UNDO.
  
  DEFINE BUFFER bQuery FOR qbQuery.
  DEFINE BUFFER bTable FOR qbTable.
  DEFINE BUFFER bField FOR qbField.
  
  /* Query itself */
  FIND ttQuery.
  FIND bQuery NO-LOCK WHERE bQuery.queryNr = ttQuery.QueryNr NO-ERROR.
  IF NOT AVAILABLE bQuery THEN RETURN YES.  
  
  BUFFER-COMPARE bQuery TO ttQuery SAVE RESULT IN lTheSame.
  IF NOT lTheSame THEN RETURN YES. 

  /* Tables of query */
  FOR EACH ttTable NO-LOCK WHERE ttTable.queryNr = bQuery.QueryNr:
    FIND bTable NO-LOCK 
      WHERE bTable.queryNr   = ttTable.QueryNr 
        AND bTable.tableName = ttTable.tableName NO-ERROR.
    IF NOT AVAILABLE bTable THEN RETURN YES.  
    BUFFER-COMPARE bTable TO ttTable SAVE RESULT IN lTheSame.
    IF NOT lTheSame THEN RETURN YES. 
  END.
  
  FOR EACH bTable NO-LOCK WHERE bTable.queryNr = bQuery.QueryNr:
    FIND ttTable NO-LOCK
      WHERE ttTable.queryNr   = bTable.QueryNr 
        AND ttTable.tableName = bTable.tableName NO-ERROR.
    IF NOT AVAILABLE ttTable THEN RETURN YES.  
    BUFFER-COMPARE ttTable TO bTable SAVE RESULT IN lTheSame.
    IF NOT lTheSame THEN RETURN YES. 
  END.

  /* Fields of query */
  FOR EACH ttField NO-LOCK:
    FIND bField NO-LOCK 
      WHERE bField.queryNr   = ttField.QueryNr 
        AND bField.tableName = ttField.tableName 
        AND bField.fieldName = ttField.fieldName NO-ERROR.
    IF NOT AVAILABLE bField THEN RETURN YES.  
    BUFFER-COMPARE bField TO ttField SAVE RESULT IN lTheSame.
    IF NOT lTheSame THEN RETURN YES. 
  END.
  
  FOR EACH bField NO-LOCK:
    FIND ttField NO-LOCK 
      WHERE ttField.queryNr   = bField.QueryNr 
        AND ttField.tableName = bField.tableName 
        AND ttField.fieldName = bField.fieldName NO-ERROR.
    IF NOT AVAILABLE ttField THEN RETURN YES.  
    BUFFER-COMPARE ttField TO bField SAVE RESULT IN lTheSame.
    IF NOT lTheSame THEN RETURN YES. 
  END.
  
  RETURN NO.
END FUNCTION. /* isQueryChanged */
