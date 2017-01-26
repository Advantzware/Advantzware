/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : QueryHelper
    Purpose     :
    Syntax      : Static methods only, Protected constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 04 21:15:06 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Assertion.*             FROM PROPATH .
USING Consultingwerk.Exceptions.*            FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Util.QueryHelper:

    /**
     * Purpose: Protected default constructor.
     * Notes:   There's no need to create instances of the helper classes
     */
    CONSTRUCTOR PROTECTED QueryHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose:     Returns the complete query where clause for a specified buffer
     *              INCLUDING leading and trailing blanks.
     *              EXCLUDING commas and period.
     * Notes:       This is supported as a 'utility function' that doesn't use any
     *              properties.
     *           -  RETURNs the expression immediately when found.
     *              RETURNs '' at bottom if nothing is found.
     * @param pcBuffer The buffer name to return the where clause from
     * @param pcWhere The complete query:prepare-string
     * @return The complete query where clause for the specified buffer
     */
    METHOD PUBLIC STATIC CHARACTER BufferWhereClause (pcBuffer AS CHARACTER,
                                                      pcWhere  AS CHARACTER):

        DEFINE VARIABLE iComma      AS INTEGER    NO-UNDO.
        DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
        DEFINE VARIABLE iStart      AS INTEGER    NO-UNDO.
        DEFINE VARIABLE cString     AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE cFoundWhere AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE cNextWhere  AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE iUseIdxPos  AS INTEGER    NO-UNDO.
        DEFINE VARIABLE iByPos      AS INTEGER    NO-UNDO.
        DEFINE VARIABLE iIdxRePos   AS INTEGER    NO-UNDO.
        DEFINE VARIABLE iOptionPos  AS INTEGER    NO-UNDO.

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
              cFoundWhere = cFoundWhere + SUBSTRING (cString,1,iComma)
              cNextWhere  = SUBSTRING (cString,iComma + 1)
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
          OR ListHelper:EntryIsInList (ENTRY(1,TRIM(cNextWhere)," ":U),
                                       "EACH,FIRST,LAST":U) THEN
          DO:
            /* Remove comma or period before inserting the new expression */
            ASSIGN
              cFoundWhere = RIGHT-TRIM(cFoundWhere,",.":U).

            IF whereClauseBuffer(cFoundWhere) = pcBuffer THEN
            DO:
              ASSIGN
                iByPos        = INDEX(cFoundWhere," BY ":U)
                iUseIdxPos    = INDEX(cFoundWhere," USE-INDEX ":U)
                iIdxRePos     = INDEX(cFoundWhere + " ":U," INDEXED-REPOSITION ":U)
                iOptionPos    = MINIMUM (IF iByPos > 0     THEN iByPos     ELSE LENGTH(cFoundWhere),
                                   IF iUseIdxPos > 0 THEN iUseIdxPos ELSE LENGTH(cFoundWhere),
                                   IF iIdxRePos > 0  THEN iIdxRePos  ELSE LENGTH(cFoundWhere)
                                  )
               .

              RETURN TRIM(SUBSTRING (cFoundWhere,1,iOptionPos)).
            END.
            ELSE
              /* We're moving on to the next whereclause so reset cFoundwhere */
              ASSIGN
                cFoundWhere = "":U
                iStart      = iCount + 1.

             /* No table found and we are at the end so we need to get out of here */
            IF iComma = 0 THEN
              LEAVE.
          END. /* if iComma = 0 or ListHelper:EntryIsInList (EACH,FIRST,LAST */
          cString = cNextWhere.
        END. /* do while true. */

        RETURN '':U.

    END METHOD.

    /**
     * Purpose: Returns the Index of the Queries Buffer the column belongs to
     * Notes:
     * @param pcColumnName The name of the Column to return the Index for
     * @param phQuery The handle of the query
     * @return The index of the queries buffer the column belongs to
     */
    METHOD PUBLIC STATIC INTEGER ColumnsBufferIndex (pcColumnName AS CHARACTER,
                                                     phQuery AS HANDLE):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE VARIABLE hField AS HANDLE NO-UNDO.

        IF NOT VALID-HANDLE (phQuery) THEN
            UNDO, THROW NEW AppError ("A valid query is required to get columns buffer index."{&TRAN}, 0) .

        DO i = 1 TO phQuery:NUM-BUFFERS:
            ASSIGN hField = phQuery:GET-BUFFER-HANDLE (i):BUFFER-FIELD (pcColumnName) NO-ERROR .

            IF VALID-HANDLE (hField) THEN
                RETURN i .
        END.

        RETURN ? .
    END METHOD.

    /**
     * Purpose: Creates, Prepares and Opens a new Query widget for the given
     *          buffer handle. The Query will be positioned at the first result row
     * Notes:   You need to manually delete the query widget. No garbage collection
     *          is provided for query widgets.
     * @param phBuffer The buffer handle
     * @return The handle of the prepared query
     */
    METHOD PUBLIC STATIC HANDLE CreatePreparedQuery (phBuffer AS HANDLE):

        IF NOT VALID-HANDLE(phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN
            RETURN ERROR NEW AppError ("Invalid Buffer handle."{&TRAN}, 0) .

        RETURN CreatePreparedQuery (phBuffer,
                                    SUBSTITUTE ("FOR EACH &1":U, phBuffer:NAME)).

    END METHOD.

    /**
     * Purpose: Creates, Prepares and Opens a new Query widget for the given
     *          buffer handle. The Query will be positioned at the first result row
     * Notes:   You need to manually delete the query widget. No garbage collection
     *          is provided for query widgets.
     * @param phBuffer The buffer handle
     * @param pcQueryString The prepare string for the query
     * @return The handle of the prepared query
     */
    METHOD PUBLIC STATIC HANDLE CreatePreparedQuery (phBuffer AS HANDLE,
                                                     pcQueryString AS CHARACTER):

        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.

        CREATE QUERY hQuery .
        hQuery:SET-BUFFERS (phBuffer) .
        hQuery:QUERY-PREPARE (pcQueryString) .
        hQuery:QUERY-OPEN () .
        hQuery:GET-FIRST () .

        RETURN hQuery.

    END METHOD.

    /**
     * Purpose: Returns a ROWID Array containing the rowids of the current
     *          Query result row.
     * Notes:   Can be used with reposition to rowid
     * @param phQuery The handle of the query
     * @param prRowids OUTPUT The array of the rowid of each query buffer
     */
    METHOD PUBLIC STATIC VOID GetCurrentRowids (phQuery AS HANDLE,
                                                OUTPUT prRowids AS ROWID EXTENT):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF NOT VALID-HANDLE (phQuery) OR phQuery:TYPE <> "QUERY":U THEN
            UNDO, THROW NEW AppError ("Invalid Query object."{&TRAN}, 0).

        EXTENT (prRowids) = phQuery:NUM-BUFFERS .

        DO i = 1 TO phQuery:NUM-BUFFERS:
            IF phQuery:GET-BUFFER-HANDLE(i):AVAILABLE THEN
                ASSIGN prRowids[i] = phQuery:GET-BUFFER-HANDLE(i):ROWID .
            ELSE
                ASSIGN prRowids[i] = ? .
        END.

    END METHOD .

    /**
     * Purpose: Returns the handle of a field from a queries buffer
     * Notes:   Field names to be specified as Buffername.Fieldname
     * @param phQuery The handle to the Query widget
     * @param pcFieldName the name of the buffer field in the form Buffername.Fieldname
     * @return The handle to the buffer field specified
     */
    METHOD PUBLIC STATIC HANDLE GetQueryFieldHandle (phQuery AS HANDLE,
                                                     pcFieldName AS CHARACTER):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phQuery WidgetTypeEnum:Query} .

        IF NUM-ENTRIES (pcFieldName, ".":U) <> 2 THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcFieldName":U,
                                                            pcFieldName,
                                                            "Consultingwerk.Util.BufferHelper":U) .

        ASSIGN hBuffer = phQuery:GET-BUFFER-HANDLE (ENTRY (1, pcFieldName, ".":U)) .

        Consultingwerk.Assertion.BufferAssert:HasField (hBuffer,
                                                        ENTRY (2, pcFieldName, ".":U)) .

        RETURN hBuffer:BUFFER-FIELD (ENTRY (2, pcFieldName, ".":U)) .

    END METHOD .

    /**
     * Purpose:     Inserts an expression into ONE buffer's where-clause.
     * Notes:     - The new expression is embedded in parenthesis and a parentheses
     *              is also placed around the existing one, which is different.
     *              than the original ADM's version which is just appended
     *              This would be problematic in adm, as it would be to many
     *              parenthesises.
     *             (could be improved to check for any OR and avoid parenthesis if not
     *              found )
     *            - Lock keywords must be unabbreviated or without -lock (i.e. SHARE
     *              or EXCLUSIVE.)
     *            - Any keyword in comments may cause problems.
     *            Creates unpredictable (false) results when used with a query string
     *            on multiple buffers
     * @param pcWhere The complete where clause with or without the FOR keyword, but without any comma before or after
     * @param pcExpression The new expression OR OF phrase (Existing OF phrase is replaced)
     * @param pcAndOr Specifies what operator is used to add the new expression to existing ones, AND (default) or OR
     * @return The new query string
     */
    METHOD PUBLIC STATIC CHARACTER InsertExpression (pcWhere      AS CHARACTER,
                                                     pcExpression AS CHARACTER,
                                                     pcAndOr      AS CHARACTER):

        DEFINE VARIABLE cTable        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cRelTable     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cJoinTable    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cWhereOrAnd   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iTblPos       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iWherePos     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lWhere        AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iOfPos        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iRelTblPos    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iInsertPos    AS INTEGER   NO-UNDO.

        DEFINE VARIABLE iUseIdxPos    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iOuterPos     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iLockPos      AS INTEGER   NO-UNDO.

        DEFINE VARIABLE iByPos        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iIdxRePos     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cTrimExp      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lAddPar       AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cOldWhere     AS CHARACTER NO-UNDO.

        IF pcExpression BEGINS '(':U THEN
        DO:
            cTrimExp = TRIM(pcExpression, "()":U).
            IF  INDEX(cTrimExp, ")":U) = 0
            AND INDEX(cTrimExp, "(":U) = 0 THEN
              pcExpression = cTrimExp.
        END.

        ASSIGN
            /* Get rid of potential line break characters */
            pcWhere       = REPLACE(pcWhere,CHR(10),' ':U)
            cTable
              = whereClauseBuffer(pcWhere)
            iTblPos       = INDEX(pcWhere,cTable) + LENGTH(cTable,"CHARACTER":U)

            iWherePos     = INDEX(pcWhere," WHERE ":U) + 6
            iByPos        = INDEX(pcWhere," BY ":U)
            iUseIdxPos    = INDEX(pcWhere," USE-INDEX ":U)
            iIdxRePos     = INDEX(pcWhere + " ":U," INDEXED-REPOSITION ":U)
            iOuterPos     = INDEX(pcWhere + " ":U," OUTER-JOIN ":U)
            iLockPos      = MAX(INDEX(pcWhere + " ":U," NO-LOCK ":U),
                                INDEX(pcWhere + " ":U," SHARE-LOCK ":U),
                                INDEX(pcWhere +
                                " ":U," EXCLUSIVE-LOCK ":U),
                                INDEX(pcWhere + " ":U," SHARE ":U),
                                INDEX(pcWhere + " ":U," EXCLUSIVE ":U)
                                )
            iInsertPos    = LENGTH(pcWhere) + 1
                            /* We must insert before the leftmoust keyword,
                               unless the keyword is Before the WHERE keyword */
            iInsertPos    = MINIMUM (
                              (IF iLockPos   > iWherePos THEN iLockPos   ELSE iInsertPos),
                              (IF iOuterPos  > iWherePos THEN iOuterPos  ELSE iInsertPos),
                              (IF iUseIdxPos > iWherePos THEN iUseIdxPos ELSE iInsertPos),
                              (IF iIdxRePos  > iWherePos THEN iIdxRePos  ELSE iInsertPos),
                              (IF iByPos     > iWherePos THEN iByPos     ELSE iInsertPos)
                               )
            lWhere        = INDEX(pcWhere," WHERE ":U) > 0
            cWhereOrAnd   = (IF NOT lWhere          THEN " WHERE ":U
                             ELSE IF pcAndOr = "":U OR pcAndOr = ? THEN " AND ":U
                             ELSE " ":U + pcAndOr + " ":U)
            iOfPos        = INDEX(pcWhere," OF ":U)
            cOldWhere     = IF lWhere
                            THEN SUBSTRING (pcWhere, iWherePos + 1, iInsertPos - iWherePos)
                            ELSE '':U.

        IF LEFT-TRIM(cOldWhere) BEGINS '(':U THEN
          ASSIGN
            cOldWhere = TRIM(cOldWhere, "()":U)
            lAddPar   = INDEX(cOldWhere, "(":U) > 0 OR INDEX(cOldWhere, ")":U) > 0.
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
                                pcWhere, iInsertPos, 0).
                                                        /*
          SUBSTRING(pcWhere,iInsertPos,0) = (IF lAddPar THEN ')':U ELSE '':U)
                                             +  cWhereOrAnd
                                             + (IF lWhere THEN "(":U ELSE '':U)
                                             + pcExpression
                                             + (IF lWhere THEN ")":U ELSE '':U).
             */

        IF lAddPar THEN pcWhere = REPLACE (pcWhere,' WHERE ':U,' WHERE (':U).

        RETURN RIGHT-TRIM (pcWhere).


    END METHOD.

    /**
     * Purpose: Inserts a free slot into a sequence of ordered records (e.g.
     *          OrderLine.LineNum). I.o.w. prepares a lists of records to insert
     *          a new record at the desired position
     * Notes:
     * @param phBufferHandle The handle of the buffer to insert a record
     * @param pcQueryPredicate The query predicate (without FOR EACH <buffer name>)
     * @param pcSortField The field by which the sequence is orderd
     * @param piInsertSequence The sequence number to make available
     */
    METHOD PUBLIC STATIC VOID InsertIntoOrder (phBufferHandle AS HANDLE,
                                               pcQueryPredicate AS CHARACTER,
                                               pcSortField AS CHARACTER,
                                               piInsertSequence AS INTEGER):

        DEFINE VARIABLE hQuery    AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBuffer   AS HANDLE  NO-UNDO.
        DEFINE VARIABLE iPosition AS INTEGER NO-UNDO.

        HandleAssert:WidgetType(phBufferHandle, WidgetTypeEnum:Buffer) .
        BufferAssert:HasField(phBufferHandle, pcSortField) .
        Assert:GT (piInsertSequence, 0) .

        IF pcQueryPredicate > "":U AND NOT pcQueryPredicate BEGINS "WHERE":U THEN
            ASSIGN pcQueryPredicate = SUBSTITUTE ("WHERE &1":U, pcQueryPredicate) .

        DO TRANSACTION ON ERROR UNDO, THROW:

            CREATE BUFFER hBuffer FOR TABLE phBufferHandle .

            ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hBuffer,
                                                             SUBSTITUTE ("PRESELECT EACH &1 &2 &3 &4 >= &5 BY &4 DESCENDING":U,
                                                                         hBuffer:NAME,
                                                                         pcQueryPredicate,
                                                                         (IF pcQueryPredicate > "":U THEN " AND ":U ELSE " WHERE ":U),
                                                                         pcSortField,
                                                                         piInsertSequence)) .

            hQuery:GET-FIRST (EXCLUSIVE-LOCK) .

            DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
                ASSIGN hBuffer:BUFFER-FIELD (pcSortField):BUFFER-VALUE = hBuffer:BUFFER-FIELD (pcSortField):BUFFER-VALUE + 1 .

                hQuery:GET-NEXT (EXCLUSIVE-LOCK) .
            END.

            ASSIGN iPosition = piInsertSequence .

            hQuery:QUERY-PREPARE (SUBSTITUTE ("PRESELECT EACH &1 &2 &3 &4 > &5 BY &4":U,
                                              hBuffer:NAME,
                                              pcQueryPredicate,
                                              (IF pcQueryPredicate > "":U THEN " AND ":U ELSE " WHERE ":U),
                                              pcSortField,
                                              piInsertSequence)) .

            hQuery:QUERY-OPEN () .
            hQuery:GET-FIRST (EXCLUSIVE-LOCK) .

            DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
                ASSIGN iPosition                                       = iPosition + 1
                       hBuffer:BUFFER-FIELD (pcSortField):BUFFER-VALUE = iPosition .

                hQuery:GET-NEXT (EXCLUSIVE-LOCK) .
            END.

            IF hBuffer:AVAILABLE THEN
                hBuffer:BUFFER-RELEASE .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject(hQuery)    .
            GarbageCollectorHelper:DeleteObject(hQuery) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Inserts a String into and existing string
     * Notes:
     * @param pcString The string to insert into the target string
     * @param pcTargetString The string to insert into
     * @param piPos The position to insert into
     * @param piLength The length of the string to insert to
     * @return The resulting string
     */
    METHOD PRIVATE STATIC CHARACTER InsertString (pcString AS CHARACTER,
                                                  pcTargetString AS CHARACTER,
                                                  piPos AS INTEGER,
                                                  piLength AS INTEGER):

    RETURN   SUBSTRING (pcTargetString,1 , piPos - 1)
           + pcString
           + SUBSTRING (pcTargetString, piPos + piLength).

    END METHOD.

    /**
     * Purpose: Returns the number of rowids assigned in a ROWID[]
     * Notes:   Will stop counting at the first ROWID = ?, useful for REPOSITION
     *          operations
     * @param prRowids The ROWID[] to return the effective number of rowids from
     * @return The effective number of rowids
     */
    METHOD PUBLIC STATIC INTEGER NumberOfRowids (prRowids AS ROWID EXTENT):

        DEFINE VARIABLE i       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.

        ASSIGN iExtent = EXTENT (prRowids) .

        IF iExtent = 0 OR iExtent = ? THEN
            RETURN 0 .

        DO i = 1 TO iExtent:
            IF prRowids[i] = ? THEN
                RETURN i - 1 .
        END.

        RETURN iExtent .

    END METHOD.

    /**
     * Purpose: Open a query that returns no records
     * Notes:   Query is open but returns no records
     * @param phQuery The handle of the query
     */
    METHOD PUBLIC STATIC VOID OpenNonMatchingQuery (phQuery AS HANDLE):

        DEFINE VARIABLE cPrepareString AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cQueryString   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i              AS INTEGER   NO-UNDO .

        ASSIGN cPrepareString = phQuery:PREPARE-STRING
               cQueryString   = "FOR ":U  .

        DO i = 1 TO phQuery:NUM-BUFFERS:
            ASSIGN cQueryString = cQueryString +
                                  (IF i > 1 THEN ",":U ELSE "":U) +
                                  " EACH ":U + phQuery:GET-BUFFER-HANDLE (i):NAME + " ":U +
                                  " WHERE ROWID(":U + phQuery:GET-BUFFER-HANDLE (i):NAME + ") = ? NO-LOCK ":U .
        END.

        phQuery:QUERY-PREPARE (cQueryString) .
        phQuery:QUERY-OPEN () .

        IF cPrepareString <> ? THEN
           phQuery:QUERY-PREPARE (cPrepareString) .

    END METHOD.

    /**
     * Purpose: Prepares a Query in reverse sort order
     * Notes:   This method is depricated. In the original implementation there was
     *          a typo in the name of this method. This was fixed, a method with the
     *          typo remains for now as a facade. However, customers should be calling
     *          directly into QueryHelper:PrepareQueryInReverseOrder ()
     * @param phQuery The handle of the query to process
     */
    METHOD PUBLIC STATIC VOID PrepareQueryInReserveOrder (phQuery AS HANDLE):

        QueryHelper:PrepareQueryInReverseOrder (phQuery) .

    END METHOD .

    /**
     * Purpose: Prepares a Query in reverse sort order
     * Notes:   As the Dataserver for ORACLE does not return the Queries index information
     *          this method will throw an NoIndexInformationAvailable when the query
     *          was not openend without an BY phrase and the ORACLE Tables are involed
     * @param phQuery The handle of the query to process
     */
    METHOD PUBLIC STATIC VOID PrepareQueryInReverseOrder (phQuery AS HANDLE):

        DEFINE VARIABLE cQueryString       AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oTokens            AS CharacterList NO-UNDO .
        DEFINE VARIABLE oTokenizer         AS Tokenizer     NO-UNDO .
        DEFINE VARIABLE iFirstBy           AS INTEGER       NO-UNDO .
        DEFINE VARIABLE iFirstAfterBy      AS INTEGER       NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER       NO-UNDO .
        DEFINE VARIABLE hBuffer            AS HANDLE        NO-UNDO .
        DEFINE VARIABLE cBY                AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cFieldName         AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cOrder             AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cIndexName         AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cIndexSort         AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oIndexSort         AS CharacterList NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phQuery WidgetTypeEnum:Query} .

        ASSIGN cQueryString = phQuery:PREPARE-STRING .

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i cQueryString} .

        oTokenizer = NEW Tokenizer () .
        oTokens = oTokenizer:Tokenize (cQueryString, TRUE) .

        ASSIGN i = 0 .

        {Consultingwerk/foreachPrimitiveList.i Character cEntry in oTokens}
            i = i + 1 .

            IF cEntry = "BY":U THEN DO:
                ASSIGN iFirstBy = i .
                LEAVE .
            END.
        END.

        /* First first token that follows BY clause */
        ASSIGN i = 0 .

        {Consultingwerk/foreachPrimitiveList.i Character cEntry in oTokens nodefine}
            i = i + 1 .

            IF (iFirstBy > 0 AND i > iFirstBy) OR (iFirstBy = 0) THEN
                IF cEntry = "INDEXED-REPOSITION":U OR cEntry = "MAX-ROWS":U OR cEntry BEGINS "QUERY-TUNING":U THEN DO:
                    ASSIGN iFirstAfterBy = i .
                    LEAVE .
                END.
        END.

        IF iFirstBy > 0 THEN DO:
            IF iFirstAfterBy = 0 THEN
                iFirstAfterBy = oTokens:Count + 1 .

            ASSIGN i = iFirstBy .

            DO WHILE i < iFirstAfterBy:

                ASSIGN cBY = oTokens:GetValue (i) .

                IF i < oTokens:Count THEN
                    cFieldName = oTokens:GetValue (i + 1) .
                ELSE
                    cFieldName = "":U .

                IF i < oTokens:Count - 1 THEN
                    cOrder = oTokens:GetValue (i + 2) .
                ELSE cOrder = "":U .

                IF cOrder <> "DESCENDING":U THEN
                    cOrder = "":U .

                IF cBY = "BY":U AND cFieldName > "":U THEN DO:

                    IF cOrder = "DESCENDING":U THEN DO:
                        /* replace Order with empty entry */
                        oTokens:Remove (i + 2) .
                        iFirstAfterBy = iFirstAfterBy - 1 .
                    END.
                    ELSE DO:
                        /* Insert descending keyword */
                        oTokens:Insert ("DESCENDING":U, i + 2) .
                        iFirstAfterBy = iFirstAfterBy + 1 .
                    END.
                END.

                i = i + 1 .
            END.
        END.
        ELSE DO:
            IF iFirstAfterBy = 0 THEN
                iFirstAfterBy = oTokens:Count + 1 .

            DO i = 1 TO phQuery:NUM-BUFFERS:
                ASSIGN hBuffer = phQuery:GET-BUFFER-HANDLE (i) .

                IF phQuery:INDEX-INFORMATION (i) > "":U THEN DO:
                    ASSIGN cIndexName = ENTRY (1, phQuery:INDEX-INFORMATION (i)) .

                    IF cIndexName = "WHOLE-INDEX":U THEN
                        ASSIGN cIndexName = ENTRY (2, phQuery:INDEX-INFORMATION (i)) .

                    cIndexSort = BufferHelper:BufferIndexByClause (hBuffer, cIndexName, TRUE, TRUE) .
                    oIndexSort = oTokenizer:Tokenize (cIndexSort, TRUE) .

                    {Consultingwerk/foreachPrimitiveList.i Character cSortEntry in oIndexSort}
                        oTokens:Insert (cSortEntry, iFirstAfterBy) .
                        iFirstAfterBy = iFirstAfterBy + 1.
                    END.
                END.
                ELSE DO:
                    UNDO, THROW NEW NoIndexInformationAvailable
                                    (hBuffer:NAME,
                                     "Unable to determine current sort order for the buffer."{&TRAN},
                                     0) .
                END.
            END.
        END.

        ASSIGN cQueryString = "":U .

        {Consultingwerk/foreachPrimitiveList.i Character cEntry in oTokens nodefine}
            cQueryString = cQueryString + " ":U + cEntry .
        END.

        IF cQueryString > "":U THEN
            phQuery:QUERY-PREPARE (cQueryString) .

    END METHOD .

    /**
     * Purpose: Returns the column name qualified with the first table name
     *          of the Queries buffers that contains the field name
     * Notes:   Returns ? if the field in not found in any of the queries buffers
     * @param pcColumnName The unqualified column name
     * @param phQuery The handle of the QUERY
     * @return The qualified column name or ?
     */
    METHOD PUBLIC STATIC CHARACTER QualifiedColumnName (pcColumnName AS CHARACTER,
                                                        phQuery AS HANDLE):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE VARIABLE hField AS HANDLE NO-UNDO.


        IF NOT VALID-HANDLE (phQuery) THEN
            UNDO, THROW NEW AppError ("A valid query is required to qualify the column name."{&TRAN}, 0) .

        DO i = 1 TO phQuery:NUM-BUFFERS:
            ASSIGN hField = phQuery:GET-BUFFER-HANDLE (i):BUFFER-FIELD (pcColumnName) NO-ERROR .

            IF VALID-HANDLE (hField) THEN
                RETURN hField:BUFFER-NAME + ".":U + hField:NAME .
        END.

        RETURN ? .
    END METHOD.

    /**
     * Purpose: Returns a comma delimited list of the Buffer Names in the Query
     * Notes:
     * @param phQuery The handle of the Query
     * @return The comma delimited list of Buffer Names in the Query
     */
    METHOD PUBLIC STATIC CHARACTER QueryBufferNames (phQuery AS HANDLE):

        DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cBuffers AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phQuery WidgetTypeEnum:Query} .

        DO i = 1 TO phQuery:NUM-BUFFERS:

            ASSIGN cBuffers = cBuffers + ",":U + phQuery:GET-BUFFER-HANDLE (i):NAME .

        END.

        RETURN TRIM (cBuffers, ",":U) .

    END METHOD .

    /**
     * Purpose: Returns the large columns of the buffers from the given query handle
     * Notes:   Uses by BufferHelper:BufferLargeColumns, please be aware that every
     *          column name will only appear once, like Column names of a joined
     *          ProBindingSource query
     * @param phQuery The handle of the query
     * @return The comma delimited list of names of the large columns in the query buffers
     */
    METHOD PUBLIC STATIC CHARACTER QueryLargeColumns (phQuery AS HANDLE):

        DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cLobs   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cBuffer AS CHARACTER NO-UNDO.

        IF NOT VALID-HANDLE (phQuery) OR phQuery:TYPE <> "QUERY":U THEN
            UNDO, THROW NEW AppError ("Invalid Query object."{&TRAN}, 0).

        ASSIGN cLobs = BufferHelper:BufferLargeColumns (phQuery:GET-BUFFER-HANDLE(1)) .

        DO i = 2 TO phQuery:NUM-BUFFERS:

            ASSIGN cBuffer = BufferHelper:BufferLargeColumns (phQuery:GET-BUFFER-HANDLE(i)) .

            DO j = 1 TO NUM-ENTRIES (cBuffer):

                IF NOT ListHelper:EntryIsInList (ENTRY (j, cBuffer), cLobs) THEN
                    ASSIGN cLobs = cLobs + ",":U + ENTRY (j, cBuffer) .

            END.
        END.

        RETURN TRIM (cLobs, ",":U) .

    END METHOD.

    /**
     * Purpose: Returns an array with the entries of a query string, uses "," as the
     *          delimiter, but does not consider comma's in quotes or brackets
     * Notes:   QueryStringEntries (2, "for each Customer, first SalesRep of Customer")
     *          returns ["for each Customer", "first SalesRep of Customer"].
     * @param pcQueryString The complete query string
     * @return Array of entries of the query string
     */
    METHOD PUBLIC STATIC CHARACTER EXTENT QueryStringEntries (pcQueryString AS CHARACTER):

        DEFINE VARIABLE cReturn  AS CHARACTER EXTENT     NO-UNDO .
        DEFINE VARIABLE cEntries AS CHARACTER EXTENT 100 NO-UNDO .

        DEFINE VARIABLE iPos     AS INTEGER              NO-UNDO .
        DEFINE VARIABLE iStart   AS INTEGER              NO-UNDO .
        DEFINE VARIABLE iEntry   AS INTEGER              NO-UNDO .
        DEFINE VARIABLE cChar    AS CHARACTER            NO-UNDO .

        ASSIGN iStart = 1
               iPos   = 1
               iEntry = 1.

        DO WHILE iPos <= LENGTH (pcQueryString) ON ERROR UNDO, THROW:

            ASSIGN cChar = SUBSTRING (pcQueryString, iPos, 1) .

            IF cChar = "(":U OR cChar = "~{":U OR cChar = "[":U THEN
                ASSIGN iPos = StringHelper:FindClosingBracket (pcQueryString, iPos) .

            IF cChar = "'":U OR cChar = "~"":U THEN
                ASSIGN iPos = StringHelper:FindClosingQuote (pcQueryString, iPos) .

            IF cChar = ",":U THEN

                ASSIGN cEntries [iEntry] = SUBSTRING (pcQueryString, iStart, iPos - iStart)
                       iEntry            = iEntry + 1
                       iStart            = iPos + 1 .

            FINALLY:
                ASSIGN iPos = iPos + 1 .
            END FINALLY.
        END.

        /* Assign the rest */
        ASSIGN cEntries [iEntry] = SUBSTRING (pcQueryString, iStart) .

        EXTENT (cReturn) = iEntry .

        DO iPos = 1 TO iEntry:
            cReturn[iPos] = TRIM (cEntries [iPos]) .
        END.

        RETURN cReturn .

    END METHOD .

    /**
     * Purpose: Moves a query objects result list pointer to the row corresponding
     *          to the specified ROWID or ROWIDs. To reposition to a particular row
     *          when the query is a join, supply the ROWIDs of the buffers that
     *          correspond to the desired row.
     * Notes:   The advantage of this method over the REPOSITION-TO-ROWID method
     *          of the Query widget is that this method works with a dynamic
     *          array.
     * @param phQuery The handle of the query
     * @param prRowids An Array of Rowids (maximum 18)
     * @return Logical value indicating if the method was successful
     */
    METHOD PUBLIC STATIC LOGICAL RepositionToRowidArray (phQuery AS HANDLE,
                                                         prRowids AS ROWID EXTENT):

        DEFINE VARIABLE iNumRowids AS INTEGER NO-UNDO.

        ASSIGN iNumRowids = QueryHelper:NumberOfRowids (prRowids) .

        IF NOT VALID-HANDLE (phQuery) OR phQuery:TYPE <> "QUERY":U THEN
            UNDO, THROW NEW AppError ("Invalid Query object."{&TRAN}, 0).

        IF IntegerType:IsNullOrEmpty (iNumRowids) THEN
            RETURN  TRUE .

        CASE iNumRowids:
            WHEN  1 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1]) .
            WHEN  2 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2]) .
            WHEN  3 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3]) .
            WHEN  4 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4]) .
            WHEN  5 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5]) .
            WHEN  6 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6]) .
            WHEN  7 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7]) .
            WHEN  8 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8]) .
            WHEN  9 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9]) .
            WHEN 10 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9], prRowids[10]) .
            WHEN 11 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9], prRowids[10],
                                                    prRowids[11]) .
            WHEN 12 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9], prRowids[10],
                                                    prRowids[11], prRowids[12]) .
            WHEN 13 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9], prRowids[10],
                                                    prRowids[11], prRowids[12], prRowids[13]) .
            WHEN 14 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9], prRowids[10],
                                                    prRowids[11], prRowids[12], prRowids[13], prRowids[14]) .
            WHEN 15 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9], prRowids[10],
                                                    prRowids[11], prRowids[12], prRowids[13], prRowids[14], prRowids[15]) .
            WHEN 16 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9], prRowids[10],
                                                    prRowids[11], prRowids[12], prRowids[13], prRowids[14], prRowids[15],
                                                    prRowids[16]) .
            WHEN 17 THEN
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9], prRowids[10],
                                                    prRowids[11], prRowids[12], prRowids[13], prRowids[14], prRowids[15],
                                                    prRowids[16], prRowids[17]) .
            /* Mike Fechner, Consultingwerk Ltd. 03.05.2012
               Bug 2826: An ABL Query supports more than 18 records (although the documentation
                         suggests otherwise). But the REPOSITION-TO-ROWID method errors out with more
                         than 18 parameters. */
            OTHERWISE
                RETURN phQuery:REPOSITION-TO-ROWID (prRowids[1], prRowids[2], prRowids[3], prRowids[4], prRowids[5],
                                                    prRowids[6], prRowids[7], prRowids[8], prRowids[9], prRowids[10],
                                                    prRowids[11], prRowids[12], prRowids[13], prRowids[14], prRowids[15],
                                                    prRowids[16], prRowids[17], prRowids[18]) .
        END CASE.

        RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Inserts a USE-INDEX option into a query string. Defaults to using
     *          the first buffer criteria for adding the query string
     * Notes:   When the query did already contain a USE-INDEX option, that option
     *          is replaced, otherwise a USE-INDEX option is injected
     * @param pcQueryString The base query string
     * @param pcIndexName The name of the index to inject
     * @return The resulting Query String
     */
    METHOD PUBLIC STATIC CHARACTER SetUseIndex (pcQueryString AS CHARACTER,
                                                pcIndexName AS CHARACTER):

        RETURN QueryHelper:SetUseIndex (pcQueryString, 1, pcIndexName) .

    END METHOD .

    /**
     * Purpose: Inserts a USE-INDEX option into a query string
     * Notes:   When the query did already contain a USE-INDEX option, that option
     *          is replaced, otherwise a USE-INDEX option is injected
     * @param pcQueryString The base query string
     * @param piBufferIndex The index of the query buffer criteria to add the USE-INDEX option to
     * @param pcIndexName The name of the index to inject
     * @return The resulting Query String
     */
    METHOD PUBLIC STATIC CHARACTER SetUseIndex (pcQueryString AS CHARACTER,
                                                piBufferIndex AS INTEGER,
                                                pcIndexName AS CHARACTER):

        DEFINE VARIABLE cEntries       AS CHARACTER     NO-UNDO EXTENT .
        DEFINE VARIABLE cQueryString   AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE i              AS INTEGER       NO-UNDO .
        DEFINE VARIABLE oTokenizer     AS Tokenizer     NO-UNDO .
        DEFINE VARIABLE oList          AS CharacterList NO-UNDO .
        DEFINE VARIABLE iPos           AS INTEGER       NO-UNDO .
        DEFINE VARIABLE iTmp           AS INTEGER       NO-UNDO .
        DEFINE VARIABLE cKeywords      AS CHARACTER     NO-UNDO EXTENT 10 INITIAL
            ["SHARE-LOCK":U, "EXCLUSIVE-LOCK":U, "NO-LOCK":U, "NO-PREFETCH":U, "QUERY-TUNING":U,
             "BREAK":U, "BY":U, "COLLATE":U, "INDEXED-REPOSITION":U, "MAX-ROWS":U].

        Assert:NotNullOrEmpty (pcQueryString, "pcQueryString":U) .
        Assert:NotNullOrEmpty (pcIndexName, "pcIndexName":U) .
        Assert:GreaterThanZero (piBufferIndex, "piBufferIndex":U) .

        ASSIGN cEntries = QueryHelper:QueryStringEntries (pcQueryString) .

        IF piBufferIndex > EXTENT (cEntries) THEN
            UNDO, THROW NEW InvalidParameterValueException ("piBufferIndex":U,
                                                            STRING (piBufferIndex),
                                                            "Consultingwerk.Util.QueryHelper":U) .

        ASSIGN cQueryString = cEntries [piBufferIndex]
               oTokenizer   = NEW Tokenizer ().

        oList = oTokenizer:Tokenize (cQueryString, TRUE) .

        /* There may already be a use-index */
        IF oList:ContainsValue ("USE-INDEX":U) THEN DO:
            iPos = oList:IndexOf ("USE-INDEX":U) .

            oList:SetValue (iPos + 1, pcIndexName) .
        END.
        /* Before SHARE-LOCK, EXCLUSIVE-LOCK, NO-LOCK, NO-PREFETCH, QUERY-TUNING, BREAK, BY, COLLATE, INDEXED-REPOSITION, MAX-ROWS */
        ELSE DO:
            ASSIGN iPos = 9999 .

            DO i = 1 TO EXTENT (cKeywords):
                ASSIGN iTmp = oList:IndexOf (cKeywords[i]) .

                IF iTmp > 0 THEN
                    ASSIGN iPos = MINIMUM (iTmp, iPos) .
            END.

            IF iPos < 9999 AND iPos > 0 THEN DO:
                oList:Insert ("USE-INDEX":U, iPos) .
                oList:Insert (pcIndexName, iPos + 1) .
            END.
            ELSE DO:
                oList:Add ("USE-INDEX":U) .
                oList:Add (pcIndexName) .
            END.
        END.

        /* Rebuild that entry */
        ASSIGN cQueryString = "":U .

        {Consultingwerk/foreachPrimitiveList.i Character cEntry in oList}
            ASSIGN cQueryString = cQueryString + cEntry + " ":U .
        END.

        ASSIGN cEntries [piBufferIndex] = TRIM (cQueryString) .

        /* Build return query string */
        ASSIGN cQueryString = cEntries [1] .

        DO i = 2 TO EXTENT (cEntries):
            ASSIGN cQueryString = SUBSTITUTE ("&1, &2":U, cQueryString, cEntries[i]) .
        END.

        RETURN cQueryString .

    END METHOD .

    /**
     * Purpose: Returns the buffername of a where clause expression.
     * Notes:   This function avoids problems with leading or double blanks in
     *          where clauses.
     * @param pcWhere Complete where clause for ONE table with or without the FOR keyword. The buffername must be the second token in the where clause as in "EACH order OF Customer" or if "FOR" is specified, the third token as in "FOR EACH order".
     * @return The buffer name of the where clause
     */
    METHOD PRIVATE STATIC CHARACTER WhereClauseBuffer (pcWhere AS CHARACTER):

        pcWhere = LEFT-TRIM (pcWhere) .

        /* Remove double blanks */
        DO WHILE INDEX (pcWhere,"  ":U) > 0:
          pcWhere = REPLACE (pcWhere,"  ":U," ":U) .
        END.

        /* Get rid of potential line break characters */
        pcWhere = REPLACE (pcWhere, CHR(10), "":U) .

        RETURN (IF NUM-ENTRIES (pcWhere," ":U) > 1
                THEN ENTRY (IF pcWhere BEGINS "FOR ":U THEN 3 ELSE 2, pcWhere, " ":U)
                ELSE "":U) .

    END METHOD.

END CLASS.
