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
    File        : QueryRowIdentifier
    Purpose     : Provides abstraction methods for query row id representation
    Syntax      :
    Description : Consultingwerk default IQueryRowIdentifier implementation
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 03 15:35:20 CET 2013
    Notes       : Used in the Consultingwerk.OERA.DataAccess class to allow
                  customers extensions to the RowId context string used by the
                  Data Access
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.QueryRowIdentifier
    IMPLEMENTS IQueryRowIdentifier:

    DEFINE VARIABLE roRestartRowids AS ROWID EXTENT NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Assigns the internal storage with the restart row identifiers of
                 the given data source object handle
        Notes:
        @param phDataSource The handle of the DATA-SOURCE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetDataSourceNextRowids (phDataSource AS HANDLE):

        DEFINE VARIABLE i      AS INTEGER   NO-UNDO .

        EXTENT (roRestartRowids) = ? .
        EXTENT (roRestartRowids) = phDataSource:NUM-SOURCE-BUFFERS .

        DO i = 1 TO phDataSource:NUM-SOURCE-BUFFERS:
            ASSIGN roRestartRowids[i] = phDataSource:NEXT-ROWID(i) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the ROWID[]with the rowids of the current Query result row
        Notes:
        @param phQuery The handle of the query
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetQueryCurrentRowids (phQuery AS HANDLE):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF NOT VALID-HANDLE (phQuery) OR phQuery:TYPE <> "QUERY":U THEN
            UNDO, THROW NEW AppError ("Invalid Query object."{&TRAN}, 0).

        EXTENT (roRestartRowids) = ? .
        EXTENT (roRestartRowids) = phQuery:NUM-BUFFERS .

        DO i = 1 TO phQuery:NUM-BUFFERS:
            IF phQuery:GET-BUFFER-HANDLE(i):AVAILABLE THEN
                ASSIGN roRestartRowids[i] = phQuery:GET-BUFFER-HANDLE(i):ROWID .
            ELSE
                ASSIGN roRestartRowids[i] = ? .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the QueryRowIdentifier contains a valid row identifier
        Notes:   Verifies that a ROWID for the topmost buffer is available
        @return Logical value indicating if the QueryRowIdentifier contains a valid row identifier
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsBaseRowidSpecified ():

        IF EXTENT (roRestartRowids) > 0 THEN
            IF roRestartRowids[1] <> ? THEN
                RETURN TRUE .

        RETURN FALSE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses a CHR(5) delimited list of ROWIDs and returns an ROWID Array
        Notes:
        @param pcRowids The CHR(5) delimited list of ROWIDs
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ParseString (pcRowids AS CHARACTER):

        DEFINE VARIABLE i        AS INTEGER      NO-UNDO .

        EXTENT (roRestartRowids) = ? .
        EXTENT (roRestartRowids) = NUM-ENTRIES (pcRowids, CHR (5)) .

        DO i = 1 TO EXTENT (roRestartRowids):

            roRestartRowids[i] = TO-ROWID (ENTRY (i, pcRowids, CHR (5))) .

        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Compares the array of the current query ROWIDs with the array of
                 Restart ROWIDs
        Notes:
        @param phQuery The Query handle
        @return Logical value indicating if the two arrays are equal
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL QueryCurrentRowidsEquals (phQuery AS HANDLE):

        DEFINE VARIABLE roQueryRowids AS ROWID EXTENT NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER      NO-UNDO .

        QueryHelper:GetCurrentRowids (phQuery, OUTPUT roQueryRowids) .

        IF EXTENT (roRestartRowids) <> EXTENT (roQueryRowids) THEN
            RETURN FALSE .

        DO i = 1 TO EXTENT (roRestartRowids):

            IF roRestartRowids[i] <> roQueryRowids[i] THEN
                RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Moves a query objects result list pointer to the row corresponding
                 to the specified ROWID or ROWIDs. To reposition to a particular row
                 when the query is a join, supply the ROWIDs of the buffers that
                 correspond to the desired row.
        Notes:
        @param phQuery The handle of the query
        @return Logical value indicating if the method was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RepositionQuery (phQuery AS HANDLE):

        RETURN QueryHelper:RepositionToRowidArray (phQuery, roRestartRowids) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the RESTART-ROWID for the data source buffer(s)
        Notes:
        @param phBufferHandle The buffer handle
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetBufferRestartRowids (phBufferHandle AS HANDLE):

        DEFINE VARIABLE iRowidIndex AS INTEGER NO-UNDO.

        IF EXTENT (roRestartRowids) > 0 THEN
            IF roRestartRowids[1] <> ? THEN DO:
                DO iRowidIndex = 1 TO EXTENT (roRestartRowids):
                    phBufferHandle:DATA-SOURCE:RESTART-ROWID (iRowidIndex) = roRestartRowids [iRowidIndex].
                END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the ROWID[] representation of this instance
        Notes:   Used for backwards compatibility only
        @return The ROWID[] representation of this instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ROWID EXTENT ToArray ():

        RETURN roRestartRowids .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the CHARACTER representation (serialization) of this instance
        Notes:   The result of this method can be parsed by the method ParseString()
        @return The CHARACTER representation of this instance
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER ToString ():

        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO .

        DO i = 1 TO EXTENT (roRestartRowids):

            ASSIGN cValue = cValue + SUBSTITUTE ("&1&2":U,
                                                 STRING (roRestartRowids[i]),
                                                 CHR(5)) .
        END.

        RETURN TRIM (cValue, CHR(5)) .

    END METHOD .

END CLASS.
