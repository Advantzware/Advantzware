/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *    The original author of this class was Progress Software,        *
 *    Håvard Danielsen                                                *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : DSQueryString
    Purpose     : Extends QueryString to provide query transformation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 03 08:24:02 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.OERA.*       FROM PROPATH .
USING Consultingwerk.OERA.Query.* FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .

CLASS Consultingwerk.OERA.Query.DSQueryString
    INHERITS QueryString
    IMPLEMENTS IQueryMap, IDSQueryString
    USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE mhInstance AS IDSQueryStringSource NO-UNDO.
    DEFINE PRIVATE VARIABLE mcTable    AS CHARACTER            NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the owning IDSQueryStringSource (typically
                 a DataAccess class) for call backs
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY Instance AS IDSQueryStringSource NO-UNDO
    GET:
        RETURN mhInstance .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the temp-table this DSQueryString instance is
                 providing source query manipulation for
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY Table AS CHARACTER NO-UNDO
    GET:
        RETURN mcTable .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DSQueryString class
        Notes:
        @param pcQueryString The query string from the current read request
        @param phInstance The reference to the owning IDSQueryStringSource (typically a DataAccess class) for call backs
        @param pcTable The name of the temp-table this DSQueryString instance is providing source query manipulation for
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DSQueryString (pcQueryString AS CHARACTER,
                                      phInstance    AS IDSQueryStringSource,
                                      pcTable       AS CHARACTER):

        /* SUPER must be first call, so pass blank to postpone parsing untill
           we have set the object reference for the call backs */
        SUPER ("":U, THIS-OBJECT).

        ASSIGN mhInstance = phInstance
               mcTable    = pcTable.

        THIS-OBJECT:ParseQuery (pcQueryString).

    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DSQueryString class
        Notes:   Alternative Constructor that allows setting of the
                 FilterUsingInnerJoin property of the QueryString class
        @param pcQueryString The query string from the current read request
        @param phInstance The reference to the owning DataAccess class (for call backs)
        @param pcTable The name of the temp-table this DSQueryString instance is providing source query manipulation for
        @param plFilterUsingInnerJoin The value for the FilterUsingInnerJoin property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DSQueryString (pcQueryString AS CHARACTER,
                                      phInstance    AS DataAccess,
                                      pcTable       AS CHARACTER,
                                      plFilterUsingInnerJoin AS LOGICAL):

        THIS-OBJECT (pcQueryString,
                     phInstance,
                     pcTable) .

        THIS-OBJECT:FilterUsingInnerJoin = plFilterUsingInnerJoin .

    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the column from the source (database) column
        Notes:   Invokes the SourceColumn method in the DataAccess class
        @param pcColumn The name of the temp-table column
        @return The name of the source (database) column
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ColumnSource (pcColumn AS CHARACTER):

        DEFINE VARIABLE cSource          AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE oBufferFieldName AS Consultingwerk.BufferFieldName NO-UNDO .

        ASSIGN cSource = mhInstance:SourceColumn (mcTable, pcColumn) .

        /* Mike Fechner, Consultingwerk Ltd. 13.05.2014
           Support for mapping Array columns */
        IF cSource = "":U OR cSource = ? AND INDEX (pcColumn, "[":U) > 0 THEN DO:
            oBufferFieldName = BufferHelper:ParseFieldName (pcColumn) .

            ASSIGN cSource = mhInstance:SourceColumn (mcTable, SUBSTITUTE ("&1.&2":U, oBufferFieldName:TableName,
                                                                                    oBufferFieldName:FieldName)) .
            IF cSource > "":U THEN
                ASSIGN cSource = SUBSTITUTE ("&1[&2]":U,
                                             cSource, oBufferFieldName:Index).
        END.

        RETURN cSource .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the default query string for the data source
        Notes:   Invokes SourceDefaultQuery in the DataAccess class
        @return The default query string
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER DefaultQuery ():

        RETURN mhInstance:SourceDefaultQuery (mcTable) .

    END METHOD .

END CLASS.
