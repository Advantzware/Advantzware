/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : JsonDataSource
    Purpose     : Abstract base class for JSON Data Sources for Kendo.UI
                  Controls
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 13 23:03:05 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH.
USING Consultingwerk.OERA.* FROM PROPATH.
USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.Web.*  FROM PROPATH.
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.Web.Data.JsonDataSource
    ABSTRACT
    USE-WIDGET-POOL:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the batch size
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BatchSize AS INTEGER NO-UNDO INIT 100
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the ProDataset
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    SET.
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY EntityName AS CHARACTER NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Entity Table
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY EntityTable AS CHARACTER NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Entity View (joined tables)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY EntityView AS CHARACTER NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the temp-table for the JSON Output
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableHandle AS HANDLE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Query String for the web request
        Notes:
        @return The query string
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER BuildQueryString ():

        DEFINE VARIABLE cQueryString         AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cFields              AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE i                    AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cSort                AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cSortDirection       AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cFilterField         AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cFilterOperator      AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cFilterValue         AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oListQueryExpression AS ListQueryExpression NO-UNDO .

        ASSIGN cFields      = WebUtilities:GetField (?)
               cQueryString = SUBSTITUTE ("FOR EACH &1":U,
                                          THIS-OBJECT:EntityTable)
               oListQueryExpression = NEW ListQueryExpression () .

        ASSIGN i = 0 .

        filterFieldLoop:
        REPEAT:
            ASSIGN cFilterField    = WebUtilities:GetField (SUBSTITUTE ("filter[filters][&1][field]":U, i))
                   cFilterOperator = WebUtilities:GetField (SUBSTITUTE ("filter[filters][&1][operator]":U, i))
                   cFilterValue    = WebUtilities:GetField (SUBSTITUTE ("filter[filters][&1][value]":U, i)).

            IF cFilterField > "":U THEN
                .
            ELSE
                LEAVE filterFieldLoop .

            CASE cFilterOperator:
                WHEN "eq":U THEN
                    oListQueryExpression:Add (NEW BufferFieldName (THIS-OBJECT:EntityTable, cFilterField),
                                              OperatorEnum:EQ,
                                              NEW CharacterHolder (cFilterValue)) .
                WHEN "neq":U THEN
                    oListQueryExpression:Add (NEW BufferFieldName (THIS-OBJECT:EntityTable, cFilterField),
                                              OperatorEnum:NE,
                                              NEW CharacterHolder (cFilterValue)) .
                WHEN "startswith":U THEN
                    oListQueryExpression:Add (NEW BufferFieldName (THIS-OBJECT:EntityTable, cFilterField),
                                              OperatorEnum:Begins,
                                              NEW CharacterHolder (cFilterValue)) .
                WHEN "contains":U THEN
                    oListQueryExpression:Add (NEW BufferFieldName (THIS-OBJECT:EntityTable, cFilterField),
                                              OperatorEnum:Matches,
                                              NEW CharacterHolder ("*":U + cFilterValue + "*":U)) .
                WHEN "endswith":U THEN
                    oListQueryExpression:Add (NEW BufferFieldName (THIS-OBJECT:EntityTable, cFilterField),
                                              OperatorEnum:Matches,
                                              NEW CharacterHolder (cFilterValue + "*":U)) .
            END CASE .

/*LogManager:WriteFormattedMessage ("filter field &1: &2", STRING (i), cFilterField)  .      */
/*LogManager:WriteFormattedMessage ("filter operator &1: &2", STRING (i), cFilterOperator)  .*/
/*LogManager:WriteFormattedMessage ("filter value &1: &2", STRING (i), cFilterValue)  .      */

            ASSIGN i = i + 1 .
        END.

/*LogManager:WriteFormattedMessage ("expression: &1", oListQueryExpression:GetExpression())  .*/

        IF NOT oListQueryExpression:IsEmpty THEN
            ASSIGN cQueryString = SUBSTITUTE ("&1 WHERE &2":U,
                                              cQueryString,
                                              oListQueryExpression:GetExpression()) .

        ASSIGN i = 0 .

        sortFieldLoop:
        REPEAT:
            ASSIGN cSort          = WebUtilities:GetField (SUBSTITUTE ("sort[&1][field]":U, i))
                   cSortDirection = WebUtilities:GetField (SUBSTITUTE ("sort[&1][dir]":U, i)).

            IF cSort > "":U THEN
                .
            ELSE
                LEAVE sortFieldLoop .

            ASSIGN cQueryString = cQueryString + SUBSTITUTE (" BY &1.&2":U,
                                                             THIS-OBJECT:EntityTable,
                                                             cSort) .

            IF cSortDirection = "desc":U THEN
                ASSIGN cQueryString = cQueryString + " DESCENDING":U .

            ASSIGN i = i + 1 .
        END.

        LogManager:WriteFormattedMessage ("Query-String: &1":U, cQueryString) .

        RETURN cQueryString .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetchs the Data from the Business Entity
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchData ():

        DEFINE VARIABLE oRequest      AS FetchDataRequest          NO-UNDO .
        DEFINE VARIABLE hDataset      AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE oTableFactory AS ResultsetTempTableFactory NO-UNDO .
        DEFINE VARIABLE oQueryBuilder AS ResultsetQueryBuilder     NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE hBuffer       AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE hSourceBuffer AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE iCount        AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER                   NO-UNDO .

        DEFINE VARIABLE cQueryString AS CHARACTER        NO-UNDO.

        ASSIGN cQueryString = THIS-OBJECT:BuildQueryString () .

        oRequest = NEW FetchDataRequest (TRIM (SUBSTITUTE ("&1,&2":U,
                                                           THIS-OBJECT:EntityTable,
                                                           THIS-OBJECT:EntityView), ",":U),
                                         cQueryString,
                                         THIS-OBJECT:BatchSize)  .

        ASSIGN hDataset = THIS-OBJECT:DatasetHandle .

        ServiceInterface:FetchData (THIS-OBJECT:EntityName,
                                    oRequest,
                                    OUTPUT DATASET-HANDLE hDataset) .

        ASSIGN THIS-OBJECT:DatasetHandle = hDataset .

        IF THIS-OBJECT:EntityView > "":U THEN DO:
            ASSIGN oTableFactory           = NEW ResultsetTempTableFactory ()
                   oQueryBuilder           = NEW ResultsetQueryBuilder ()
                   THIS-OBJECT:TableHandle = oTableFactory:BuildResultsetTempTable (hDataset,
                                                                                    THIS-OBJECT:EntityTable,
                                                                                    THIS-OBJECT:EntityView)
                   hQuery                  = oQueryBuilder:PrepareResultsetQuery (hDataset,
                                                                                  THIS-OBJECT:EntityTable,
                                                                                  THIS-OBJECT:EntityView,
                                                                                  FALSE)
                   hBuffer                 = THIS-OBJECT:TableHandle:DEFAULT-BUFFER-HANDLE .

            hQuery:QUERY-OPEN () .
            hQuery:GET-FIRST () .

            ASSIGN iCount = NUM-ENTRIES (THIS-OBJECT:EntityView) .

            DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

                hBuffer:BUFFER-CREATE ()  .

                DO i = 1 TO iCount:
                    hSourceBuffer = hDataset:GET-BUFFER-HANDLE (ENTRY (i, THIS-OBJECT:EntityView)) .

                    IF hSourceBuffer:AVAILABLE THEN
                        hBuffer:BUFFER-COPY (hSourceBuffer) .
                END.

                hBuffer:BUFFER-COPY (hDataset:GET-BUFFER-HANDLE (THIS-OBJECT:EntityTable)) .

                hQuery:GET-NEXT () .
            END.
        END.
        ELSE
            ASSIGN THIS-OBJECT:TableHandle = hDataset:GET-BUFFER-HANDLE (1):TABLE-HANDLE .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the JSON Output to the web stream
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OutputTable ():

        DEFINE VARIABLE hStream AS HANDLE NO-UNDO.

        ASSIGN hStream = WebContext:WebStream .

        THIS-OBJECT:TableHandle:WRITE-JSON ("stream-handle":U,
                                            hStream) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC JsonDataSource ():

        IF VALID-HANDLE (THIS-OBJECT:DatasetHandle) THEN
            DELETE OBJECT THIS-OBJECT:DatasetHandle NO-ERROR .

        IF VALID-HANDLE (THIS-OBJECT:TableHandle) THEN
            DELETE OBJECT THIS-OBJECT:TableHandle NO-ERROR .

    END DESTRUCTOR.

END CLASS.
