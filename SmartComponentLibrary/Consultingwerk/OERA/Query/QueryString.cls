/*********************************************************************
* Copyright (C) 2006 by Progress Software Corporation. All rights    *
* reserved.  Prior versions of this work may contain portions        *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*---------------------------------------------------------------------------- 
    File        : QueryString
    Purpose     : Manage query string tranformation and manipulation.
                  String based, need no handles or compilation.
           
                  Manages a query string with field expressions and allows a caller 
                  to add these expressions to the correct tables in an existing query.
           
                  The main purpose is to support query transformation from a query 
                  with field expressions against a domain table to a query expressed 
                  against physical tables. 
           
                  The mapping and transformation is optionally triggered by  
                  passing an object that implements IQueryMap as the second parameter.     
                       
    Parameters:   pcQueryWhere complete query expression  
                  pQueryMap optional IQueryMap (columnSource, defaultQuery) 
            
    Notes:        This is a copied subset of ADM2's query manipulation with 
                  additional parsing.   
                  - The object is in a rather unfinished, unintentended and unplanned...  
                    - ADM methods and the parsing has overlaps and is rather disjoint.
                    - extents... could be changed to classes, but 
                    - Several private variables, mcQueryWhere, mcTargetQuery,
                      mcTargetTables serves no current purpose, except as a 
                      safety/debug support (afraid to loose control) ...    
                      (I will of course forget to remove this comment when they are 
                       removed, so it may not be true ..) 
                  - The passed pcQueryWhere goes through a rather crude form of parsing 
                    that may fail with too complex queries ...  
                    There is still an attempt to handle anything by just giving up and 
                    treat complex expressions as a single expression while the 
                    transformation  continues.. . If it fails with expressions on 
                    functions with parenthesis try to add a parenthesis around 
                    functions that also have parenthesis (no promise..) or fix the code.  
                  - The passed query is expected to be a working (compilable) dynamic 
                    temp-table query with no database options like locking. Outer 
                    joins would not make sense in this context either.   
                  - It is crucial that all values are quoted to be correctly parsed,
                    (also needed to handle non american numeric and date formats,
                     assuming same session format settings here as when applied)   
                  - The transformation can split the expressions on different tables 
                    if they are using an AND operator. An expression against multiple 
                    tables with an OR operator between them will be added to the query 
                    entry of the last of the tables.
                  - Allthough some of the code is based on the ADM, it is still meant 
                    as sample of query manipulation. 
                 There are alternatives to full parsing.
                    The query syntax is after all is a subset of the ABL programming 
                    language. Maybe reconsider the design to see if some of the 
                    complexity could be better encapsulated in the data access or data 
                    source object. Even if this logic expects the query to be in 
                    compilable form it is not really necessary at this stage, so tokens 
                    and special patterns could be used to ease the 
                    transformation/mapping.           
----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW . 

{Consultingwerk/products.i}

USING Consultingwerk.OERA.Query.* FROM PROPATH . 
USING Consultingwerk.Util.*       FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .  
 
CLASS Consultingwerk.OERA.Query.QueryString 
    IMPLEMENTS IThrowsErrorsOnInvalidSourceMapping
    USE-WIDGET-POOL:

    /* These should be implemented as read-only getters,    
      (future native support for this could simplify implementation ) */
    DEFINE PUBLIC  VARIABLE SortExpression AS CHARACTER NO-UNDO.
    DEFINE PUBLIC  VARIABLE Tables         AS CHARACTER NO-UNDO.

    DEFINE PUBLIC  VARIABLE mcQueryWhere   AS CHARACTER NO-UNDO.
  
    DEFINE PRIVATE VARIABLE mcTargetTable  AS CHARACTER NO-UNDO EXTENT 18.
    DEFINE PRIVATE VARIABLE mcTargetQuery  AS CHARACTER NO-UNDO EXTENT 18.
    DEFINE PRIVATE VARIABLE mcEvaluation   AS CHARACTER NO-UNDO EXTENT 50 .
    DEFINE PRIVATE VARIABLE mcExpression   AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE PRIVATE VARIABLE mcExpTables    AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE PRIVATE VARIABLE miNumTables    AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE miParCount     AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE miExpCount     AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE mQueryRef      AS CLASS     IQueryMap.

    /*------------------------------------------------------------------------------
        Purpose: Instructs the Query-Builder in method BuildQueryString to remove
                 an eventually present OUTER-JOIN keyword from the source default
                 query when filtering on a ceratin table.                                                                       
        Notes:   This allows to filter Customers by SalesRep fields in a Query:
                 FOR EACH Customer, FIRST SalesRep OF Customer OUTER-JOIN
                 See Bug 2609 for a detailled discussion on the new functionality                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FilterUsingInnerJoin AS LOGICAL NO-UNDO INIT FALSE  
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Get and sets if this class throws an error when the client query 
                 string contains columns that are not mapped to a data source column                                                                     
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ThrowErrorsOnInvalidSourceMapping AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the QueryString class                                                                      
        Notes:                                       
        @param pcQueryWhere The query string to parse
        @param poQueryMap The reference to the IQueryMap instance that provides call backs for the query manipulation                                 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryString (pcQueryWhere AS CHARACTER,
                                    poQueryMap AS IQueryMap) :

        /* waiting for overload constructors */
        mQueryRef = poQueryMap.
        IF pcQueryWhere > "":U THEN
            THIS-OBJECT:ParseQuery(pcQueryWhere). 
            
    END CONSTRUCTOR.
  
    /*------------------------------------------------------------------------------
        Purpose: Clears the internal state to allow parsing another query string                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ClearQuery ():
        
        ASSIGN 
            mcQueryWhere  = "":U
            mcTargetTable = "":U
            mcTargetQuery = "":U
            mcEvaluation  = "":U
            mcExpression  = "":U 
            mcExpTables   = "":U 
            miParCount    = 0
            miExpCount    = 0
            miNumTables   = 0.  
            
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Insert the QueryString into the query                                                                       
        Notes:   Actually performs QUERY-PREPARE on the Query-Handle                 
        @param phQuery The HANDLE of the QUERY widget
        @return True if successful   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL InsertToQuery (phQuery AS HANDLE):  
      
        DEFINE VARIABLE cQuery AS CHARACTER NO-UNDO.  
      
        ASSIGN cQuery = THIS-OBJECT:BuildQueryString (phQuery) .  
      
        RETURN phQuery:QUERY-PREPARE (cQuery).
    
        /* Mike Fechner, Consultingwerk Ltd. 26.11.2010
           Informative error messages */
        CATCH err AS Progress.Lang.Error:
            DEFINE VARIABLE oError   AS InsertToQueryException NO-UNDO . 
            DEFINE VARIABLE iMessage AS INTEGER                NO-UNDO . 
        
            oError = NEW InsertToQueryException (err, 
                                                 err:GetMessage (1), 
                                                 err:GetMessageNum (1),
                                                 cQuery) . 
        
            DO iMessage = 2 TO err:NumMessages:
                oError:AddMessage (err:GetMessage (iMessage), 0) .
            END. 

            oError:AddMessage (SUBSTITUTE ("Prepare-String: &1":U, cQuery), 0) .
        
            UNDO, THROW oError .
        END CATCH.
    
    END.
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the Index information of the query used for a buffer                                                                     
        Notes:   For debugging/logging purposes only                                                            
        @param phBuffer The handle to the buffer to return index information for
        @return The index information of the query used for the buffer handle         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER CheckIndex (phBuffer AS HANDLE):   
    
        DEFINE VARIABLE hTmpQuery  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hTmpBuffer AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cIndexed   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cWhere     AS CHARACTER NO-UNDO.

        cWhere = THIS-OBJECT:TableEvaluation (1,phBuffer:NAME).
        
        IF cWhere > '':U THEN
        DO:
            CREATE QUERY hTmpQuery.
            CREATE BUFFER hTmpBuffer FOR TABLE phBuffer.
    
            hTmpQuery:ADD-BUFFER(hTmpBuffer).
            /* add dummy as the evaluation returns all expressions for last entry */
            cWhere = tableEvaluation(1,phBuffer:NAME + ',dummy':U).
   
            /* ok , no-lock does not change index usage .. but */
            cWhere = 'FOR EACH ':U + phBuffer:NAME + ' WHERE ':U + cWhere + ' NO-LOCK':U.
            hTmpQuery:QUERY-PREPARE(cWhere).
      
            cIndexed = hTmpQuery:INDEX-INFORMATION(1). 
      
            DELETE OBJECT hTmpBuffer.
            DELETE OBJECT hTmpQuery.
        END.

        RETURN cIndexed.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Builds the QueryString for the Query                                                                      
        Notes:                                       
        @param phQuery The HANDLE of the QUERY widget
        @return The Query String                                                                            
    ------------------------------------------------------------------------------*/  
    METHOD PUBLIC CHARACTER BuildQueryString (phQuery AS HANDLE):    
        
        DEFINE VARIABLE iTable        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cQuery        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cBufferWhere  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTables       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cNewQuery     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cNewWhere     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lBuildDefault AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iStart        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iLength       AS INTEGER   NO-UNDO.
    
        cQuery = phQuery:PREPARE-STRING.

        IF cQuery = ? AND VALID-OBJECT(mQueryRef) THEN
            cQuery = mQueryRef:DefaultQuery().
    
        IF cQuery = ? THEN
            lBuildDefault = TRUE.
    
        DO iTable = 1 TO phQuery:NUM-BUFFERS:  
            ASSIGN
                cTables = cTables 
                 + (IF iTable = 1 THEN '':U ELSE ',':U)
                 + phQuery:GET-BUFFER-HANDLE(iTable):NAME.
      
            IF lBuildDefault THEN
            DO:
                cQuery = IF iTable = 1 
                    THEN 'FOR EACH ':U + phQuery:GET-BUFFER-HANDLE(iTable):NAME
                    ELSE cQuery 
                    + ', FIRST ':U + phQuery:GET-BUFFER-HANDLE(iTable):NAME
                    + ' OF ':U + phQuery:GET-BUFFER-HANDLE(1):NAME.
            END.
        END.

        DO iTable = 1 TO NUM-ENTRIES(cTables):  
            ASSIGN 
                cNewWhere    = THIS-OBJECT:TableEvaluation (iTable,cTables)
                cBufferWhere = THIS-OBJECT:BufferWhereClause (ENTRY(iTable,cTables),cQuery).
                
            /* workaround*/
            DEFINE VARIABLE cNewString AS CHARACTER NO-UNDO.
            IF cNewWhere > '':U THEN
            DO:
                ASSIGN
                    iStart     = INDEX(cQuery,cBufferWhere)
                    iLength    = LENGTH(cBufferWhere) . 

                IF iStart = 0 THEN 
                    UNDO, THROW NEW InvalidSourceQueryException (SUBSTITUTE ("Invalid source query for buffer: &1":U, ENTRY (iTable, cTables)), 
                                                                 0, 
                                                                 ENTRY (iTable, cTables)) .
                    
                ASSIGN 
                    cNewString = InsertExpression (cBufferWhere,
                                         cNewWhere,
                                         'AND':U)  
                    cQuery     = InsertString (cNewString,cQuery,iStart,iLength).
                    
                    /* Mike Fechner, Consultingwerk Ltd. 15.04.2011
                       Bug 2609: When the client request contains filters for the current table, 
                                 the OUTER-JOIN option from the SourceDefaultQuery should be removed. */
                    IF THIS-OBJECT:FilterUsingInnerJoin AND 
                       cNewWhere > "":U AND 
                       NUM-ENTRIES (cQuery) >= iTable THEN
                        ASSIGN ENTRY (iTable, cQuery) = REPLACE (ENTRY (iTable, cQuery), "OUTER-JOIN":U, "":U) .

            /*
            SUBSTR(cQuery,iStart,iLength) = insertExpression
                                            (cBufferWhere,
                                             cNewWhere,
                                             'AND':U).*/
    
            END.
        END.

        IF SortExpression > '':U THEN 
            RETURN newQuerySort(cQuery,SortExpression).
    
        RETURN cQuery.
  
    END METHOD.
  
    /*------------------------------------------------------------------------------
        Purpose:     Inserts a new expression to query's prepare string for a 
                     specified buffer.
        Notes:
        @param pcBuffer The name of the Buffer  
        @param pcExpression The new expression 
        @param pcAndOr Specifies what operator is used to add the new expression to existing expression(s), AND (default) / OR 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddQueryWhere (pcBuffer     AS CHARACTER,   
                                      pcExpression AS CHARACTER,  
                                      pcAndOr      AS CHARACTER):
        DEFINE VARIABLE iStart       AS INT       NO-UNDO.
        DEFINE VARIABLE iLength      AS INT       NO-UNDO.
        DEFINE VARIABLE cBufferWhere AS CHAR      NO-UNDO.
        DEFINE VARIABLE cWhere       AS CHARACTER NO-UNDO.
    
        ASSIGN
            cWhere       = mcQueryWhere
            /* Find the buffer's 'expression-entry' in the query */
            cBufferWhere = bufferWhereClause(pcBuffer,cWhere).
    
        /* if we found it, replace it with itself with the new expression inserted */
        DEFINE VARIABLE cnew AS CHARACTER NO-UNDO.
        IF cBufferWhere <> '':U THEN
            ASSIGN
                iStart  = INDEX(cWhere,cBufferWhere)
                iLength = LENGTH(cBufferWhere)
                cNew    = insertExpression (cBufferWhere,
                                    pcExpression,
                                    pcAndOr)
                cWhere  = insertString(cNew,cWhere,iStart,iLength). 
        /*    
            SUBSTR(cWhere,iStart,iLength) = insertExpression
                                              (cBufferWhere,
                                               pcExpression,
                                               pcAndOr).  
        */
        
        THIS-OBJECT:ParseQuery(cWhere).

    END METHOD.

    /*------------------------------------------------------------------------------
          Purpose: Parses the logical buffer's target query and stores it for 
                   transformation to the datasource query.
          Notes:   See main comments 
          @param pcQuery The QueryString to parse     
    ------------------------------------------------------------------------------*/  
    METHOD PROTECTED VOID ParseQuery (pcQuery AS CHARACTER) :
        
        DEFINE VARIABLE i                 AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cChar             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lWord             AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cNewString        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lIdent            AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lChar             AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lToken            AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cToken            AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cWord             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lDouble           AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lSingle           AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lIgnoreNext       AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lNoFieldExp       AS LOGICAL   NO-UNDO.

        DEFINE VARIABLE lNewValue         AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lNewToken         AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lNewIdent         AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lNewOperator      AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lNewAndOR         AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lNewLeft          AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lNewRight         AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lTableNext        AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lLeft             AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lRight            AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cSort             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cWordOperatorList AS CHARACTER NO-UNDO
            INIT "EQ,GE,LE,GT,LT,BEGINS,MATCHES,NE":U.
        DEFINE VARIABLE cOperatorList     AS CHARACTER NO-UNDO
            INIT "=,>=,<=,>,<,<>":U.

        DEFINE VARIABLE lNewExpression    AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iParOwner         AS INTEGER   NO-UNDO EXTENT 100.
        DEFINE VARIABLE iPar              AS INTEGER   NO-UNDO.
 
        DEFINE VARIABLE iBadpar           AS INTEGER   NO-UNDO.

 
        DEFINE VARIABLE lAddToken         AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iByPos            AS INTEGER   NO-UNDO.

        DEFINE VARIABLE cExpression       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFieldExp         AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cColumnTable      AS CHARACTER NO-UNDO.
    
        DEFINE VARIABLE cDiff             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cAccExpress       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iLeftPar          AS INTEGER   NO-UNDO.

        /* 48,57 */
        /* 65,90 */
        /* 97,122 */
        /* ('a'..'z'|'_'|'$') ('a'..'z'|'_'|'-'|'0'..'9'|'$'|'#'|'%'|'&')* */

        /* Any BY ? */ 
        /*     clearQuery(). */
        iByPos = INDEX(pcQuery + " ":U, " BY ":U).
        iPar   = 1.
        IF miParCount = 0 THEN miParCount = 1.
        IF iByPos > 0 THEN
            ASSIGN
                cSort   = TRIM(SUBSTR(pcQuery, iByPos + 1), '.':U)
                pcQuery = TRIM(SUBSTR(pcQuery, 1, iByPos), ' .':U).
    
        IF cSort > '':U THEN
            parseSortExpression(cSort).

        pcQuery = pcQuery + ' ':U. /* last char  */
        DO i = 1 TO LENGTH(pcQuery):   
            ASSIGN 
                cChar = SUBSTRING (pcQuery, i, 1).
            CASE cChar:
                WHEN "~~":U THEN
                    DO:
                        IF lSingle OR lDouble THEN
                            lIgnoreNext = NOT lIgnoreNext. /* would be true if ~~*/
                    END. /* tilde */
                WHEN "'":U THEN
                    DO:
                        IF NOT lDouble THEN  
                        DO:
                            IF NOT lIgnoreNext THEN
                            DO:
                                IF lSingle THEN
                                    lNewValue = TRUE.
                                ELSE 
                                DO:
                                    IF lToken THEN
                                        lNewToken = TRUE.
                                    lSingle = TRUE.
                                END.
                            END.
                            ELSE 
                                lIgnoreNext = FALSE.
                        END.
                    END. /* single quote */
                WHEN '"':U THEN
                    DO:
                        IF NOT lSingle THEN
                        DO:
                            IF NOT lIgnoreNext THEN
                            DO:
                                IF lDouble THEN DO:
                                /* Mike Fechner, Consultingwerk Ltd. 17.02.2013
                                   Bug 2850: When within double quotes, a double double quote 
                                   should be ignored, so test the next character */
                                IF SUBSTRING (pcQuery, i + 1, 1) = '"':U THEN DO:
                                  ASSIGN lIgnoreNext = TRUE . 
                                END.    
                                ELSE 
                                    
                                    lNewValue = TRUE.
                                    
                                END.
                                ELSE 
                                DO:
                                    IF lToken THEN
                                        lNewToken = TRUE.
                                    lDouble = TRUE.
                                END.
                            END.
                            ELSE 
                                lIgnoreNext = FALSE.
                            IF NOT lDouble THEN
                                lNewValue = TRUE. 
                        END.
                    END. /* double quote*/
                    
                OTHERWISE 
                DO:
                    lIgnoreNext = FALSE.
                    IF NOT lSingle AND NOT lDouble THEN
                    DO:
                        IF cChar <> '':U THEN 
                        DO:
                            lChar = (ASC(cChar) GE 97 AND ASC(cChar) LE 122) /* lowercase chars */
                                OR
                                (ASC(cChar) GE 65 AND ASC(cChar) LE 90) /* uppercase chars */
                                OR
                                (ASC(cChar) GE 48 AND ASC(cChar) LE 57) /* numbers */
                                OR 
                                (cChar = '_':U)
                                OR
                                /* Mike Fechner, Consultingwerk Ltd. 08.04.2015
                                   SCL-744: Cosider ? a character when not in quotes,
                                            replaces abot fix for Bug 2817 */
                                (cChar = '?':U)
                                .
                            /* period as part of word keeps column qualified */
                            IF NOT lChar AND lIdent THEN
                                lChar = LOOKUP(cChar,'_,-,$,#,%,&,.,[,]':U) > 0.
                            IF lChar THEN 
                            DO:
                                IF lToken THEN
                                    lNewToken = TRUE.
                                lIdent = TRUE.
                            END.
                            ELSE 
                            DO : 
                                IF lIdent THEN
                                    lNewIdent = TRUE.
                                ELSE 
                                    lNewToken = LOOKUP(cChar,',|.|)|(|+':U, '|':U) > 0.
                                lToken = TRUE. 
                            END.
                        END. /* not blank */
                        ELSE 
                        DO:
                            IF lIdent THEN
                                lNewIdent = TRUE.
                            ELSE IF lToken THEN
                                    lNewToken = TRUE. 
                        END. /* else (blank) */
                    END. /* not quote*/
                END. /*otherwise*/
            END CASE.

/*            Consultingwerk.Util.LogManager:WriteMessage (SUBSTITUTE ("&1 &2 &3 &4 &5",*/
/*                            cChar, cToken, lNewValue, lAddToken)) .                   */

            /* The case above will set lNewValue, lNewIdent or lNewToken whenever 
               a new token is found, this block will turn off flags and pass the 
               token on for further treatment below */
            IF lNewValue THEN 
            DO:
                ASSIGN
                    cToken    = cToken + cChar
                    cChar     = '':U
                    lSingle   = FALSE
                    lDouble   = FALSE
                    lAddToken = TRUE.
            END.
            ELSE IF lNewIdent THEN
                DO:
                    IF LOOKUP(cToken, 'AND,OR':U) > 0 THEN
                        lNewExpression = TRUE.
                    ELSE
                        IF LOOKUP(cToken,cWordOperatorList) > 0 THEN
                            ASSIGN 
                                lNewOperator = TRUE
                                lNewIdent    = FALSE.
                    ASSIGN
                        lAddToken = TRUE
                        lIdent    = FALSE.
                END.
                ELSE IF lNewToken THEN
                    DO:
                        IF cToken = ')':U THEN
                        DO:
                            IF iBadPar > 0 THEN 
                                iBadPar = iBadPar - 1.
                            ELSE
                                lNewExpression = TRUE.
                        END.
                        ELSE
                            IF LOOKUP(cToken,cOperatorList) > 0 THEN
                                ASSIGN
                                    lNewOperator = TRUE
                                    lNewToken    = FALSE.

                        IF cChar = '':U OR lIdent THEN
                            lToken = FALSE.
         
                        lAddToken = cToken > '':U.
                    END.
      
            IF i = LENGTH(pcQuery) THEN
            DO:
                ASSIGN
                    lAddToken      = TRUE
                    lNewExpression = TRUE.
            END.

            /***         **/
       
            IF lAddToken THEN
            DO:
                IF LOOKUP(cToken,'EACH,FIRST,LAST':U) > 0 THEN
                DO:
                    ASSIGN
                        lNewExpression = TRUE
                        lTableNext     = TRUE.
          
                    /* The query comma separator is hard to filter with this simple parsing, 
                       as it is used in functions, but we know it's the last added 
                       token here  */
                    IF miNumTables > 0 THEN
                        ASSIGN
                            mcTargetQuery[miNumTables] = RIGHT-TRIM(mcTargetQuery[miNumTables], ' ,':U)
                            cExpression                = RIGHT-TRIM(cExpression, ',':U). 

                END.
                ELSE IF lTableNext THEN
                    DO:
                        miNumTables = miNumTables + 1.
                        mcTargetTable[miNumTables] = cToken.
                        lTableNext = FALSE.
                    END.
                    /* ignore keywords and the last blank (appended to end of query at top
                       to have a breakpoint also for the last expression) */  
                    ELSE IF LOOKUP(cToken,'FOR,PRESELECT,WHERE, ':U) = 0 THEN
                        DO:
                            IF LOOKUP(cToken,'AND,OR':U) = 0 THEN
                            DO:
                                IF lNewIdent THEN 
                                DO:
                                    IF NUM-ENTRIES(cToken,".":U) < 2 THEN
                                        ASSIGN
                                            lNoFieldExp = TRUE
                                            lNewIdent   = FALSE
                                            lNewToken   = TRUE.
                                    ELSE 
                                        /* Mike Fechner, Consultingwerk Ltd. Nov 29, 2013
                                           Causes call back to SourceDefaultQuery and SourceColumn
                                           in the actual Data Access object. Here it is assumed, that
                                           any "word" that contains at least a single period is a 
                                           table/field name combination. */
                                        
                                        /* get the column name from the map and store info about it */
                                        cToken = resolveColumn(cToken,miExpCount + 1 /* next expression */). 
                                END.
                                IF lnewIdent 
                                    OR lNewOperator 
                                    OR lNewValue  THEN
                                    cFieldExp = LEFT-TRIM(cFieldExp + ' ':U + cToken). 

                                ASSIGN
                                    cExpression = LEFT-TRIM(cExpression + ' ':U + cToken).
                            END.          
                            IF miNumTables = 0 THEN
                                miNumTables = 1.
                            mcTargetQuery[miNumTables] = IF mcTargetQuery[miNumTables] = '':U 
                                THEN cToken
                                ELSE mcTargetQuery[miNumTables] + ' ':U + cToken.

                        END. /* else if not keyword*/
        
                /* newexpression is triggered by left parenthesis, and/or or 
                   end of statement  */
                IF lNewExpression THEN
                DO:
                    IF cExpression > '':U  THEN
                    DO:
            
                        cDiff = IF cFieldExp = '':U   
                            THEN cExpression
                            ELSE REPLACE(cExpression,cFieldExp,'':U).
                        /* If only diff is spaces and parenthesis then we store 
                           the parenthesis as separate p-expressions */
                        IF cDiff <> '':U AND TRIM(cDiff,'( )':U) = '':U THEN
                        DO:
                            DO iLeftPar = 1 TO NUM-ENTRIES(cDiff,"(":U) - 1:
                                ASSIGN 
                                    miParCount            = miParCount + 1
                                    mcEvaluation[iPar]    = TRIM(mcEvaluation[iPar] 
                             + ' ':U 
                             + 'P':U + STRING(miParCount))
                                    iParOwner[miParCount] = iPar
                                    iPar                  = miParCount.
                            END.
                        END.
                        ELSE 
                            ASSIGN  /* number of lefties to ignore */
                                iBadPar = MAX(0,NUM-ENTRIES(cDiff,"(":U) - 2).
                        /* store expression  */
                        IF iBadpar = 0 THEN
                        DO:                            
                            ASSIGN
                                miExpCount               = miExpCount + 1
                                mcExpression[miExpCount] = IF cAccExpress > '':U  OR lNoFieldExp
                                   THEN cAccExpress + cExpression
                                   ELSE TRIM(cFieldExp)
                                mcEvaluation[iPar]       = TRIM(mcEvaluation[iPar] 
                                   + ' ':U 
                                   + 'E':U + STRING(miExpCount))
                                   + (IF lTableNext THEN ',':U ELSE '':U).
                            /* left parenthesis,  back up to parent parenthesis */
                            IF TRIM(cDiff) = ")":U OR cDiff = "(  )":U THEN
                                iPar = iParOwner[iPar].
                            ASSIGN
                                lNoFieldExp = FALSE
                                cFieldExp   = '':U
                                cExpression = '':U
                                caccExpress = '':U.
                        END.
                        ELSE
                            ASSIGN
                                cAccExpress = cAccExpress + cExpression
                                cExpression = '':U.

                    END. /* cExpression > '' */
                    ELSE IF lTableNext AND minumtables > 0 THEN
                            mcEvaluation[iPar] = mcEvaluation[iPar] + ',':U.

                    IF cAccExpress = '':U AND LOOKUP(ctoken,'AND,OR':U) > 0 THEN
                        mcEvaluation[iPar] = LEFT-TRIM(mcEvaluation[iPar] + ' ':U + cToken).

                END. /* newexpresssion */

                ASSIGN
                    mcQueryWhere   = mcQueryWhere + ' ':U + cToken
                    lNewToken      = FALSE
                    lNewExpression = FALSE
                    lNewIdent      = FALSE
                    lNewOperator   = FALSE
                    lNewValue      = FALSE
                    lAddToken      = FALSE
                    cToken         = '':U.
            END.  /* lAddToken */

            IF cChar > '':U OR lSingle OR lDouble THEN
                cToken = cToken + cChar.
        END.

/*        /* Mike Fechner, Consultingwerk Ltd. 08.04.2015  */
/*           Tracing of query expressions */               */
/*        DO i = 1 TO miExpCount:                          */
/*                                                         */
/*            MESSAGE "mcExpression[" i "]" mcExpression[i]*/
/*                VIEW-AS ALERT-BOX.                       */
/*                                                         */
/*        END.                                             */

    END METHOD.
  
    /*------------------------------------------------------------------------------
        Purpose: Parses the Sort Expression                                                                      
        Notes:                             
        @param pcSort The sort expression to parse                                           
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParseSortExpression (pcSort AS CHARACTER) :

        DEFINE VARIABLE iSort AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cWord AS CHARACTER NO-UNDO.
        DO iSort = 1 TO NUM-ENTRIES(pcSort,' ':U):
            cWord = ENTRY(iSort,pcSort,' ':U).
            IF cWord <> '':U THEN
            DO:
                IF LOOKUP(cWord, ",BY,DESCENDING":U) = 0 THEN
                    cWord = resolveColumn (cWord,0 /* not an expression */). 
                SortExpression = LEFT-TRIM (SortExpression + ' ':U + cWord).
            END. /* not blank */
        END. /* isort loop */

    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose:     Returns the complete query where clause for a specified buffer
                     INCLUDING leading and trailing blanks.
                     EXCLUDING commas and period.  
        Notes:       This is supported as a 'utility function' that doesn't use any 
                     properties. 
        @param pcBuffer The Buffer name
        @param pcWhere A complete query:prepare-string or ? to use the current query                              
        @return The expression when found, '' if nothing is found. 
    ------------------------------------------------------------------------------*/ 
    METHOD PRIVATE CHARACTER BufferWhereClause (pcBuffer AS CHARACTER,
                                                pcWhere  AS CHARACTER) :
        DEFINE VARIABLE iComma      AS INT       NO-UNDO. 
        DEFINE VARIABLE iCount      AS INT       NO-UNDO.
        DEFINE VARIABLE iStart      AS INT       NO-UNDO.
        DEFINE VARIABLE cString     AS CHAR      NO-UNDO.
        DEFINE VARIABLE cFoundWhere AS CHAR      NO-UNDO.
        DEFINE VARIABLE cNextWhere  AS CHAR      NO-UNDO.
        DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cBuffer     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iUseIdxPos  AS INT       NO-UNDO.        
        DEFINE VARIABLE iByPos      AS INT       NO-UNDO.        
        DEFINE VARIABLE iIdxRePos   AS INT       NO-UNDO.  
        DEFINE VARIABLE iOptionPos  AS INTEGER   NO-UNDO.
   
        ASSIGN
            cString = RIGHT-TRIM(pcWhere," ":U)  
            iStart  = 1.
   
        /* Keep our promises and ensure that trailing blanks BEFORE the period are 
           returned, but remove the period and trailing blanks AFTER it. 
           If the length of right-trim with blank and blank + period is the same 
           then there is no period, so just use the passed pcWhere as is. 
           (Otherwise the remaining period is right-trimmed with comma further down)*/  
        IF LENGTH(cString) = LENGTH(RIGHT-TRIM(pcWhere,". ":U)) THEN
            cString = pcWhere.
   
    
        /* The ADM resolves qualification at this stage ensurimg that the caller
           can use different qualification than used in the query   
          cBuffer = resolveBuffer(pcBuffer}. 
        
          IF cBuffer <> '':U AND cBuffer <> ? THEN
            pcBuffer = cBuffer.
        */
      
        DO WHILE TRUE:
            iComma  = INDEX(cString,",":U). 
      
            /* If a comma was found we split the string into cFoundWhere and cNextwhere */  
            IF iComma <> 0 THEN 
                ASSIGN
                    cFoundWhere = cFoundWhere + SUBSTR(cString,1,iComma)
                    cNextWhere  = SUBSTR(cString,iComma + 1)     
                    iCount      = iCount + iComma.       
            ELSE      
                /* cFoundWhere is blank if this is the first time or if we have moved on 
                   to the next buffer's where clause
                   If cFoundwhere is not blank the last comma that was used to split 
                   the string into cFoundwhere and cNextwhere was not a join, so we set 
                   them together again.  */     
                cFoundWhere = IF cFoundWhere = "":U 
                    THEN cString
                    ELSE cFoundWhere + cNextwhere.
             
            /* We have a complete table whereclause if there are no more commas
               or the next whereclause starts with each,first or last */    
            IF iComma = 0 
                OR ListHelper:EntryIsInList (ENTRY (1,TRIM (cNextWhere), " ":U),
                                             "EACH,FIRST,LAST":U) THEN
            DO:
                /* Remove comma or period before inserting the new expression */
                ASSIGN
                    cFoundWhere = RIGHT-TRIM(cFoundWhere, ",.":U). 
        
                IF whereClauseBuffer(cFoundWhere) = pcBuffer THEN
                DO:
                    ASSIGN
                        iByPos     = INDEX(cFoundWhere," BY ":U)    
                        iUseIdxPos = INDEX(cFoundWhere," USE-INDEX ":U)    
                        iIdxRePos  = INDEX(cFoundWhere + " ":U," INDEXED-REPOSITION ":U)
                        iOptionPos = MIN(IF iByPos > 0     THEN iByPos     ELSE LENGTH(cFoundWhere),
                               IF iUseIdxPos > 0 THEN iUseIdxPos ELSE LENGTH(cFoundWhere),
                               IF iIdxRePos > 0  THEN iIdxRePos  ELSE LENGTH(cFoundWhere)
                              )
                        .
  
                    RETURN TRIM(SUBSTR(cFoundWhere,1,iOptionPos)).
                END.
                ELSE
                    /* We're moving on to the next whereclause so reset cFoundwhere */ 
                    ASSIGN      
                        cFoundWhere = "":U                     
                        iStart      = iCount + 1.      
       
                /* No table found and we are at the end so we need to get out of here */  
                IF iComma = 0 THEN 
                    LEAVE.    
            END. /* if iComma = 0 or ListHelper:EntryIsInList */
            cString = cNextWhere.  
        END. /* do while true. */
  
        RETURN '':U.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the sort expression of the passed querystring 
        Notes:   Includes the first BY also (getQuerySort does not) and 
                 removes extra spaces.
        @param pcQueryString The complete QueryString
        @return The sort expression of the QueryString including the first BY
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER SortExpression (pcQueryString AS CHARACTER):
        
        DEFINE VARIABLE iByPos      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cExpression AS CHARACTER NO-UNDO.
  
        /* Any BY ? */ 
        iByPos = INDEX(pcQueryString + " ":U," BY ":U).

        IF iByPos > 0 THEN
        DO:
            /* Trim away blanks and period and remove indexed-reposition */
            cExpression = REPLACE(TRIM(SUBSTR(pcQueryString,iByPos)," .":U),
                ' INDEXED-REPOSITION':U,
                '':U).
            /* Remove extra blanks (this may be used to compare new and old 
               sort expression so it need to be consistent)*/
            DO WHILE INDEX(cExpression,'  ':U) > 0:
                cExpression = REPLACE(cExpression,'  ':U,' ':U).
            END.
        END.

        RETURN cExpression.
    END.
  
    /*------------------------------------------------------------------------------
          Purpose   :  Insert sort criteria (BY phrase) in a QueryString.
          Notes:
          @param pcQuery Query String to add sort to (current sort will be replaced)        
          @param pcSort New sort expression
          @return The new query string
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER NewQuerySort (pcQuery       AS CHARACTER,
                                           pcSort        AS CHARACTER):
                                               
        DEFINE VARIABLE iByPos  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iIdxPos AS INTEGER NO-UNDO.
        DEFINE VARIABLE iLength AS INTEGER NO-UNDO.
  
        IF pcQuery = '':U THEN
            RETURN '':U.

        ASSIGN   
            /* check for indexed-reposition  */
            iIdxPos = INDEX(RIGHT-TRIM(pcQuery,". ":U) + " ":U," INDEXED-REPOSITION ":U)          
      
            /* If no INDEX-REPOSITION is found, set the iLength (where to end insert)
               to the end of where-clause. (right-trim periods and blanks to find 
               the true end of the expression) Otherwise iLength is the position of 
               INDEX-REPOSITION. */
            iLength = (IF iIdxPos = 0 
               THEN LENGTH(RIGHT-TRIM(pcQuery,". ":U)) + 1     
               ELSE iIdxPos)    
            
            /* Any By ? */ 
            iByPos  = INDEX(pcQuery," BY ":U)                   
            /* Now find where we should start the insert; 
               We might have both a BY and an INDEXED-REPOSITION or only one of them 
               or none. So we make sure we use the MINIMUM of whichever of those 
               unless they are 0. */
            iByPos  = MIN(IF iByPos  = 0 THEN iLength ELSE iByPos,
                  IF iIdxPos = 0 THEN iLength ELSE iIdxPos) 
            .    
        /*
        SUBSTR(pcQuery,iByPos,iLength - iByPos) = IF pcSort <> '':U 
                                                  THEN " ":U + pcSort
                                                  ELSE "":U.  
        RETURN pcQuery. 
          */
        RETURN THIS-OBJECT:InsertString (IF pcSort <> '':U THEN " ":U + pcSort ELSE "":U,
                                         pcQuery,
                                         iByPos,
                                         iLength - iByPos).
    END METHOD.
 
    /*------------------------------------------------------------------------------
         Purpose:     Inserts an expression into ONE buffer's where-clause.      
         Notes:     - The new expression is embedded in parenthesis and a parentheses
                      is also placed around the existing one, which is different.  
                      than the original ADM's version which is just appended
                      This would be problematic in adm, as it would be to many 
                      parenthesises. 
                     (could be improved to check for any OR and avoid parenthesis if not 
                      found ) 
                    - Lock keywords must be unabbreviated or without -lock (i.e. SHARE
                      or EXCLUSIVE.)   
                    - Any keyword in comments may cause problems.
         @param pcWhere Complete where clause with or without the FOR keyword, but without any comma before or after.
         @param pcExpression New expression OR OF phrase (Existing OF phrase is replaced)
         @param pcAndOr Specifies what operator is used to add the new expression to existing ones. AND (default)/OR                       
         @return The new query string
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER InsertExpression (pcWhere      AS CHARACTER,   
                                               pcExpression AS CHARACTER,     
                                               pcAndOr      AS CHARACTER):     
                         
        DEFINE VARIABLE cTable      AS CHAR      NO-UNDO.  
        DEFINE VARIABLE cRelTable   AS CHAR      NO-UNDO.  
        DEFINE VARIABLE cJoinTable  AS CHAR      NO-UNDO.  
        DEFINE VARIABLE cWhereOrAnd AS CHAR      NO-UNDO.  
        DEFINE VARIABLE iTblPos     AS INT       NO-UNDO.
        DEFINE VARIABLE iWherePos   AS INT       NO-UNDO.
        DEFINE VARIABLE lWhere      AS LOG       NO-UNDO.
        DEFINE VARIABLE iOfPos      AS INT       NO-UNDO.
        DEFINE VARIABLE iRelTblPos  AS INT       NO-UNDO.  
        DEFINE VARIABLE iInsertPos  AS INT       NO-UNDO.    
  
        DEFINE VARIABLE iUseIdxPos  AS INT       NO-UNDO.        
        DEFINE VARIABLE iOuterPos   AS INT       NO-UNDO.        
        DEFINE VARIABLE iLockPos    AS INT       NO-UNDO.      
  
        DEFINE VARIABLE iByPos      AS INT       NO-UNDO.        
        DEFINE VARIABLE iIdxRePos   AS INT       NO-UNDO.        
        DEFINE VARIABLE cTrimExp    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lAddPar     AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cOldWhere   AS CHARACTER NO-UNDO.

        IF pcExpression BEGINS '(':U THEN
        DO:
            cTrimExp = TRIM(pcExpression, "()":U).
            IF  INDEX(cTrimExp, ")":U) = 0 
                AND INDEX(cTrimExp, "(":U) = 0 THEN
                pcExpression = cTrimExp.
        END.
    
        ASSIGN 
            /* Get rid of potential line break characters */   
            pcWhere     = REPLACE(pcWhere,CHR(10),' ':U)
            cTable      = whereClauseBuffer(pcWhere)
            iTblPos     = INDEX(pcWhere,cTable) + LENGTH(cTable,"CHARACTER":U)
    
            iWherePos   = INDEX(pcWhere," WHERE ":U) + 6    
            iByPos      = INDEX(pcWhere," BY ":U)    
            iUseIdxPos  = INDEX(pcWhere," USE-INDEX ":U)    
            iIdxRePos   = INDEX(pcWhere + " ":U," INDEXED-REPOSITION ":U)    
            iOuterPos   = INDEX(pcWhere + " ":U," OUTER-JOIN ":U)     
            iLockPos    = MAX(INDEX(pcWhere + " ":U," NO-LOCK ":U),
                        INDEX(pcWhere + " ":U," SHARE-LOCK ":U),
                        INDEX(pcWhere + " ":U," EXCLUSIVE-LOCK ":U),
                        INDEX(pcWhere + " ":U," SHARE ":U),
                        INDEX(pcWhere + " ":U," EXCLUSIVE ":U)
                        )    
            iInsertPos  = LENGTH(pcWhere) + 1 
            /* We must insert before the leftmoust keyword,
               unless the keyword is Before the WHERE keyword */ 
            iInsertPos  = MIN(
                      (IF iLockPos   > iWherePos THEN iLockPos   ELSE iInsertPos),
                      (IF iOuterPos  > iWherePos THEN iOuterPos  ELSE iInsertPos),
                      (IF iUseIdxPos > iWherePos THEN iUseIdxPos ELSE iInsertPos),
                      (IF iIdxRePos  > iWherePos THEN iIdxRePos  ELSE iInsertPos),
                      (IF iByPos     > iWherePos THEN iByPos     ELSE iInsertPos)
                       )                                                        
            lWhere      = INDEX(pcWhere," WHERE ":U) > 0 
            cWhereOrAnd = (IF NOT lWhere          THEN " WHERE ":U 
                     ELSE IF pcAndOr = "":U OR pcAndOr = ? THEN " AND ":U 
                     ELSE " ":U + pcAndOr + " ":U) 
            iOfPos      = INDEX(pcWhere," OF ":U)
            cOldWhere   = IF lWhere 
                    THEN SUBSTR(pcWhere,iWherePos + 1,iInsertPos - iWherePos) 
                    ELSE '':U.

        IF LEFT-TRIM(cOldWhere) BEGINS '(':U THEN
            ASSIGN 
                cOldWhere = TRIM(cOldWhere,"()":U)
                lAddPar   = INDEX(cOldWhere,"(":U) > 0 OR INDEX(cOldWhere,")":U) > 0.
        ELSE 
            lAddPar = cOldWhere > '':U.

        IF LEFT-TRIM(pcExpression) BEGINS "OF ":U THEN 
        DO:   
            /* If there is an OF in both the join and existing query we replace the 
               table unless they are the same */      
            IF iOfPos > 0 THEN 
            DO:
                ASSIGN
                    /* Find the table in the old join */               
                    cRelTable  = ENTRY(1,LEFT-TRIM(SUBSTRING(pcWhere,iOfPos + 4))," ":U)      
                    /* Find the table in the new join */       
                    cJoinTable = SUBSTRING(LEFT-TRIM(pcExpression),3).
        
                IF cJoinTable <> cRelTable THEN
                    ASSIGN 
                        iRelTblPos = INDEX(pcWhere + " ":U," ":U + cRelTable + " ":U) + 1                            
                        pcWhere    = insertString(cJointable,pcWhere,iRelTblPos,LENGTH(cRelTable)).
            /*  SUBSTRING(pcWhere,iRelTblPos,LENGTH(cRelTable)) = cJointable. */

            END. /* if iOfPos > 0 */ 
            ELSE 
                pcWhere = insertString(pcExpression,pcWhere,iTblPos,0).

        /*
        SUBSTRING(pcWhere,iTblPos,0) = " ":U + pcExpression.        
          */
        END. /* if left-trim(pcExpression) BEGINS "OF ":U */
        ELSE 
            pcwhere = insertString((IF lAddPar THEN ')':U ELSE '':U)
                +  cWhereOrAnd 
                + (IF lWhere THEN "(":U ELSE '':U)
                + pcExpression 
                + (IF lWhere THEN ")":U ELSE '':U),
                pcWhere,iInsertPos,0).
                    /*
            SUBSTRING(pcWhere,iInsertPos,0) = (IF lAddPar THEN ')' ELSE '')
            +  cWhereOrAnd 
            + (IF lWhere THEN "(" ELSE '')
            + pcExpression 
            + (IF lWhere THEN ")" ELSE ''). 
            */

        IF lAddPar THEN pcWhere = REPLACE(pcWhere,' WHERE ':U,' WHERE (':U).

        RETURN RIGHT-TRIM(pcWhere).
  
    END METHOD.
  
    /*------------------------------------------------------------------------------
          Purpose:     Returns the buffername of a where clause expression. 
                       This function avoids problems with leading or double blanks in 
                       where clauses.      
          Notes:      
          @param pcWhere Complete where clause for ONE table with or without the FOR keyword. The buffername must be the second token in the where clause as in "EACH order OF Customer" or if "FOR" is specified, the third token as in "FOR EACH order".
          @return The name of the buffer of the where clause expression
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER WhereClauseBuffer (pcWhere AS CHARACTER):
        pcWhere = LEFT-TRIM(pcWhere).

        /* Remove double blanks */
        DO WHILE INDEX(pcWhere,"  ":U) > 0:
            pcWhere = REPLACE(pcWhere,"  ":U, " ":U).
        END.
        /* Get rid of potential line break characters */   
        pcWhere = REPLACE(pcWhere,CHR(10),'':U). 

        RETURN (IF NUM-ENTRIES(pcWhere," ":U) > 1 
            THEN ENTRY(IF pcWhere BEGINS "FOR ":U THEN 3 ELSE 2, pcWhere, " ":U)
            ELSE "":U).

    END.

    /*------------------------------------------------------------------------------
        Purpose: Performs the Query evaluation for a table                                                                       
        Notes:                                            
        @param piTableEntry The entry of the table
        @param pcTables The list of tables to evaluate                           
        @return The evaluated query string 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER TableEvaluation (piTableEntry AS INTEGER,
                                              pcTables AS CHARACTER):
        
        RETURN THIS-OBJECT:GetTableEvaluation (1, piTableEntry, pcTables).
    
    END METHOD.
  
    /*------------------------------------------------------------------------------
        Purpose: Performs the Query evaluation for a table                                                                       
        Notes:                                            
        @param piEval 
        @param piTableEntry The entry of the table
        @param pcTables The list of tables to evaluate                           
        @return The evaluated query string 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER GetTableEvaluation (piEval AS INTEGER,
                                                 piTableEntry AS INTEGER,
                                                 pcTables AS CHAR):
     
        DEFINE VARIABLE iEval       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cRef        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iRef        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cEvaluation AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cExpression AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOperator   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lAnyOr      AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cEvalTables AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cCheckExp   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lAddPar     AS LOGICAL   NO-UNDO.

        IF piTableEntry > 0 THEN
        DO:
            /* if any OR only add the whole evaluation */ 
            lAnyOr = INDEX(mcEvaluation[piEval],' OR ':U) > 0. 
            IF lAnyOr THEN
            DO:
                cEvalTables = getEvaluationTables(piEval,'':U).
                
                IF NOT canAddRefTables(cEvalTables,piTableEntry,pcTables) THEN
                    RETURN '':U.
                piTableEntry = 0.
            END.
        END.

        DO iEval = 1 TO NUM-ENTRIES (mcEvaluation[piEval],' ':U):
            cRef = ENTRY(iEval,mcEvaluation[piEval],' ':U).
            IF LOOKUP(cRef,'AND,OR':U) > 0 THEN
            DO:
                cOperator =  cRef.
            END.
            ELSE
            DO:
                iRef = INT(SUBSTR(cRef,2)).
                IF cRef BEGINS 'E':U THEN
                DO:
                    IF piTableEntry = 0  
                        OR canAddRefTables(mcExpTables[iRef],piTableEntry,pcTables) THEN
                    DO:
                        cExpression = mcExpression[iRef].

                    /*              MESSAGE iRef  cExpression            */
                    /*                VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                    END.
                END.
                ELSE IF iRef > 0 THEN 
                    DO:
                        cExpression = getTableEvaluation(iRef,piTableEntry,pctables).
                        
                        IF cExpression > '':U THEN 
                        DO:
                            
                            IF cExpression BEGINS '(':U THEN
                                ASSIGN
                                    cCheckExp = TRIM(cExpression,"()":U)
                                    lAddPar   = INDEX(cCheckExp,"(":U) > 0 OR INDEX(cCheckExp,")":U) > 0.
                            ELSE 
                                lAddPar = TRUE.

                            cExpression = (IF lAddPar THEN "(":U ELSE '':U)
                                + cExpression
                                + (IF lAddPar THEN ")":U ELSE '':U).
                        END.
                    END.

                IF cExpression > '':U THEN
                DO:
                    IF cEvaluation = '':U THEN
                        cEvaluation = cExpression. 
                    ELSE IF cOperator > '':U THEN
                            cEvaluation = cEvaluation + ' ':U + cOperator + ' ':U + cExpression.
                        ELSE /* added for second table */
                            cEvaluation = cEvaluation + ' AND ':U + cExpression.
                END.
       
                ASSIGN
                    cOperator   = '':U
                    cExpression = '':U.
            END.
        END.
        
        RETURN cEvaluation.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:                                                                       
        Notes:
        @param piEval piEval
        @param pcTables pcTables                                                                        
        @return The return value                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER GetEvaluationTables (piEval AS INTEGER,
                                                  pcTables AS CHARACTER):
     
        DEFINE VARIABLE iEval       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cRef        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iRef        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cEvaluation AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cExpression AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOperator   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lAnyOr      AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cEvalTables AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cCheckExp   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lAddPar     AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iTable      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cTable      AS CHARACTER NO-UNDO.

        DO iEval = 1 TO NUM-ENTRIES(mcEvaluation[piEval],' ':U):
            cRef = ENTRY(iEval,mcEvaluation[piEval],' ':U).
            IF LOOKUP(cRef,'AND,OR':U) = 0 THEN
            DO:
                iRef = INT(SUBSTR(cRef,2)).
        
                IF cRef BEGINS 'E':U THEN
                DO iTable = 1 TO NUM-ENTRIES(mcExpTables[iRef]) :
                    cTable = ENTRY(iTable,mcExpTables[iRef]).
                    IF LOOKUP(cTable,pcTables) = 0 THEN
                        pcTables = pcTables 
                            + (IF pcTables = '':U THEN '':U ELSE ',':U) 
                            + cTable.  
                END.
                ELSE IF iRef > 0 THEN
                        pcTables = getEvaluationTables(iRef,pctables).
            END.
        END.
        RETURN pcTables.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Evaluates if pcRefTables can be added to piTableLevel entry in a 
                 pcTables ordered query                                                                       
        Notes:   
        @param pcRefTables The references tables
        @param piTableLevel The current table level  
        @param pcTables The tables to be added
        @return Logical value indicating if pcRefTables can be added to piTableLevel entry in a pcTables ordered query                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE LOGICAL CanAddRefTables (pcRefTables AS CHARACTER,
                                            piTableLevel AS INTEGER,
                                            pcTables AS CHARACTER):
                                                
        /* can pcRefTables be added to piTableLevel entry in a pcTables ordered query */
        DEFINE VARIABLE iNumRefTables AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iTable        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cTable        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cCurrentTable AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iTablePos     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iCurrent      AS INTEGER   NO-UNDO.
        
        cCurrentTable = ENTRY(piTableLevel, pcTables).
        iNumRefTables = NUM-ENTRIES (pcRefTables).  
    
        /* expression with no ref goes to first */ 
        IF iNumRefTables = 0 THEN
            RETURN piTableLevel = 1 .
    
        IF iNumRefTables = 1 THEN
            RETURN cCurrentTable = pcRefTables.

        /* if current is in ref include if the others are higher or external  */
        IF LOOKUP(cCurrentTable,pcTables) > 0 THEN
        DO:
            DO iTable = 1 TO NUM-ENTRIES(pcRefTables) :
                cTable = ENTRY(iTable,pcRefTables).
                IF cTable <> cCurrentTable THEN
                DO:
                    iTablePos = LOOKUP(cTable,pcTables).
                    IF iTablePos > piTableLevel THEN
                        RETURN FALSE.
                END.
            END.
            RETURN TRUE.
        END.

        RETURN FALSE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:                                                                       
        Notes:                                     
        @param piEval piEval
        @return The return value                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER GetEvaluation (piEval AS INTEGER):
        DEFINE VARIABLE iEval       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cRef        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iRef        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cEvaluation AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cExpression AS CHARACTER NO-UNDO.

        DO iEval = 1 TO NUM-ENTRIES (mcEvaluation[piEval],' ':U):
            cRef = ENTRY(iEval,mcEvaluation [piEval],' ':U).

            IF LOOKUP(cRef,'AND,OR':U) > 0 THEN
                cEvaluation = cEvaluation + ' ':U + cRef.
            ELSE
            DO:
                iRef = INT(SUBSTR(cRef,2)).
                IF cRef BEGINS 'E':U THEN
                    cEvaluation = LEFT-TRIM(cEvaluation + ' ':U + mcExpression[iRef]).
                ELSE 
                    cEvaluation = LEFT-TRIM(cEvaluation + " (":U + getEvaluation(iRef) + ")":U).
            END.
        END.

        RETURN cEvaluation.
    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose:                                                                       
        Notes:
        @param pcColumn pcColumn
        @param piExp piExp
        @return Return value                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER ResolveColumn (pcColumn AS CHARACTER, piExp AS INTEGER):
        /* called from parsequery and parseSortExpression */
        DEFINE VARIABLE cColumnTable AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cColumn      AS CHARACTER NO-UNDO.

        /* get the column name from the map */
        IF VALID-OBJECT(mQueryRef) THEN
        DO:
            cColumn = mQueryRef:ColumnSource(pcColumn). 

            IF cColumn > "":U THEN
                pcColumn = cColumn.
            ELSE 
                IF THIS-OBJECT:ThrowErrorsOnInvalidSourceMapping = TRUE THEN 
                    UNDO, THROW NEW InvalidSourceMappingException (SUBSTITUTE ("Invalid data access column mapping for: &1":U, pcColumn), 
                                                                   0,
                                                                   pcColumn). 
        END.
    
        IF pcColumn > '':U THEN
        DO:
            cColumnTable = ENTRY(1,pcColumn, ".":U).
            /* keep track of table references in the expression */
            IF piExp > 0 AND LOOKUP(cColumnTable,mcExpTables[piExp]) = 0 THEN
                mcExpTables[piExp] = mcExpTables[piExp]
                    + (IF mcExpTables[piExp] = '':U THEN '':U ELSE ',':U)
                    + cColumnTable.
            /* Trim is a bit dangerous with recycled variables if set to ' '.
            LEFT-TRIM(mcExpTables[piExp] + "," + cColumnTable,", ") <--USE-.
             */
            /* keep track of table references in the object */
            IF LOOKUP(cColumnTable,Tables) = 0 THEN
                Tables = LEFT-TRIM(Tables + ",":U + cColumnTable,",":U).
        END.
        /* as for now we just return in any case leaving error messages to 
           query prepare if mapping was unsuccessful  */
        RETURN pcColumn.
    END.
  
    /*------------------------------------------------------------------------------
        Purpose: Workaround for temporary core bug                                                                       
        Notes: 
        @param pcString The String to insert
        @param pcTargetString The String to instert into
        @param piPos The position to instert 
        @param piLength The length of the original string   
        @return The resulting string                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER InsertString (pcString AS CHARACTER,
                                           pcTargetString AS CHARACTER,
                                           piPos AS INTEGER,
                                           piLength AS INTEGER):

        RETURN SUBSTRING (pcTargetString,1,piPos - 1) + 
               pcString + 
               SUBSTRING (pcTargetString,piPos + piLength).
    END.

END CLASS.
