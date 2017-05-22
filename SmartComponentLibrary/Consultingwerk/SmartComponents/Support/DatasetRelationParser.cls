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
    File        : DatasetRelationParser
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Mar 10 10:13:34 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Data.*       FROM ASSEMBLY .
USING Progress.Lang.*       FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Support.DatasetRelationParser :

    /*------------------------------------------------------------------------------
        Purpose: Returns a new ColumnPropDesc Array with the given length
        Notes:
        @param piCount
        @return The ColumnPropDesc array with the given size
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE ColumnPropDesc EXTENT GetNewColumnPropDescArray (piCount AS INTEGER):

        DEFINE VARIABLE oNewArray AS ColumnPropDesc EXTENT NO-UNDO.

        EXTENT(oNewArray) = piCount.

        RETURN oNewArray.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the exact count of fields including array fields
        Notes:
        @param phBuffer The handle to the buffer
        @return The total number of columns, including array extents
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE INTEGER GetColumnCount (phBuffer AS HANDLE):

        DEFINE VARIABLE i      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

        IF phBuffer:NUM-FIELDS = 0 THEN
            RETURN 0.

        DO i = 1 TO phBuffer:NUM-FIELDS:
            IF phBuffer:BUFFER-FIELD(i):EXTENT > 0 THEN DO:
                iCount = iCount + phBuffer:BUFFER-FIELD(i):EXTENT.
            END.
            ELSE DO:
                iCount = iCount + 1.
            END.
        END.

        RETURN iCount.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an array of ColumnPropDesc from the given Buffer
        Notes:   New fields are appended to the eventually existing columns inside
                 the table
        @param phBuffer The handle to the record buffer
        @param poTable The reference to the TableDesc instance
        @param pcExceptList INPUT-OUTPUT The BindingSource except field list
        @param plUseFullFieldNames If or if not the BindingSource uses FullFieldNames
        @return The array of ColumnPropDesc instances
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ColumnPropDesc EXTENT GetTableFieldArray (phBuffer                  AS HANDLE,
                                                            poTable                   AS TableDesc,
                                                            INPUT-OUTPUT pcExceptList AS CHARACTER,
                                                            plUseFullFieldNames       AS LOGICAL):

        DEFINE VARIABLE j                 AS INTEGER                NO-UNDO.
        DEFINE VARIABLE arrayvar          AS ColumnPropDesc         EXTENT NO-UNDO.
        DEFINE VARIABLE cColumnName       AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE cColumnLabel      AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE oDatatype         AS Progress.Data.DataType NO-UNDO.
        DEFINE VARIABLE iIterate          AS INTEGER                NO-UNDO INITIAL 1 .
        DEFINE VARIABLE iIterateNewFields AS INTEGER                NO-UNDO INITIAL 1 .
        DEFINE VARIABLE arrayJoined       AS ColumnPropDesc  EXTENT NO-UNDO.
        DEFINE VARIABLE arrayFromTable    AS ColumnPropDesc  EXTENT NO-UNDO.
        DEFINE VARIABLE i                 AS INTEGER                NO-UNDO.

        IF NOT plUseFullFieldNames THEN
            RETURN THIS-OBJECT:GetTableFieldArray (phBuffer,
                                                   poTable,
                                                   INPUT-OUTPUT pcExceptList) .

        arrayvar = GetNewColumnPropDescArray (GetColumnCount (phBuffer)).

        DO j = 1 TO phBuffer:NUM-FIELDS:
            cColumnName     = SUBSTITUTE ("&1:&2", phBuffer:NAME, phBuffer:BUFFER-FIELD(j):NAME).
            cColumnLabel    = phBuffer:BUFFER-FIELD(j):LABEL.

            oDataType = Consultingwerk.Util.DataTypeHelper:ToEnum (phBuffer:BUFFER-FIELD(j):DATA-TYPE) .

            /* If the Datatype is unknown or the columnname begins with an underscore move on to the next */
            IF oDatatype = Progress.Data.DataType:UNKNOWN OR cColumnName BEGINS "_":U THEN
                NEXT.

            IF phBuffer:BUFFER-FIELD(j):EXTENT > 0 THEN
                DO i = 1 TO phBuffer:BUFFER-FIELD(j):EXTENT:
                    arrayvar[iIterateNewFields] = NEW ColumnPropDesc (SUBSTITUTE("&1[&2]":U, cColumnName, i),
                                                                      SUBSTITUTE("&1[&2]":U, cColumnLabel, i),
                                                                      oDatatype).

                    ASSIGN iIterateNewFields = iIterateNewFields + 1.
            END.
            ELSE DO:
                arrayvar[iIterateNewFields] = NEW ColumnPropDesc(cColumnName, cColumnLabel, oDatatype).
                ASSIGN iIterateNewFields = iIterateNewFields + 1.
            END.
        END.

        /* The NewFields counter has to be reduced by one to get the count of the existing items in the array */
        ASSIGN iIterateNewFields = iIterateNewFields - 1.

        IF VALID-OBJECT(poTable) THEN
            IF VALID-OBJECT(poTable:COLUMNS) THEN DO:
                arrayJoined    = GetNewColumnPropDescArray(poTable:Columns:Length + iIterateNewFields).
                arrayFromTable = poTable:COLUMNS.

                DO i = 1 TO poTable:Columns:Length :
                    arrayJoined[i] = arrayFromTable[i].
                END.

                ASSIGN iIterate = 1.
                DO i = poTable:Columns:Length + 1 TO poTable:Columns:Length + iIterateNewFields:
                    arrayJoined[i] = arrayvar[iIterate].
                    iIterate       = iIterate + 1.
                END.

                RETURN arrayJoined.
        END.

        arrayJoined  = GetNewColumnPropDescArray(iIterateNewFields).

        DO i = 1 TO iIterateNewFields:
            arrayJoined[i] = arrayvar[i].
        END.

        RETURN arrayJoined.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an array of ColumnPropDesc from the given Buffer
        Notes:   New fields are appended to the eventually existing columns inside
                 the table
        @param phBuffer The handle to the record buffer
        @param poTable The reference to the TableDesc instance
        @param pcExceptList INPUT-OUTPUT The BindingSource except field list
        @return The array of ColumnPropDesc instances
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ColumnPropDesc EXTENT GetTableFieldArray (phBuffer     AS HANDLE,
                                                            poTable      AS TableDesc,
                                                            INPUT-OUTPUT pcExceptList AS CHARACTER):

        DEFINE VARIABLE j                 AS INTEGER                NO-UNDO.
        DEFINE VARIABLE arrayvar          AS ColumnPropDesc         EXTENT NO-UNDO.
        DEFINE VARIABLE cColumnName       AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE cColumnLabel      AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE oDatatype         AS Progress.Data.DataType NO-UNDO.
        DEFINE VARIABLE iIterate          AS INTEGER                NO-UNDO INITIAL 1 .
        DEFINE VARIABLE iIterateNewFields AS INTEGER                NO-UNDO INITIAL 1 .
        DEFINE VARIABLE arrayJoined       AS ColumnPropDesc  EXTENT NO-UNDO.
        DEFINE VARIABLE arrayFromTable    AS ColumnPropDesc  EXTENT NO-UNDO.
        DEFINE VARIABLE i                 AS INTEGER                NO-UNDO.

        arrayvar = GetNewColumnPropDescArray (GetColumnCount (phBuffer)).

        DO j = 1 TO phBuffer:NUM-FIELDS:
            cColumnName     = phBuffer:BUFFER-FIELD(j):NAME.
            cColumnLabel    = phBuffer:BUFFER-FIELD(j):LABEL.

            oDataType = Consultingwerk.Util.DataTypeHelper:ToEnum (phBuffer:BUFFER-FIELD(j):DATA-TYPE) .

            /* If the Datatype is unknown or the columnname begins with an underscore move on to the next */
            IF oDatatype = Progress.Data.DataType:UNKNOWN OR cColumnName BEGINS "_":U THEN
                NEXT.

            /* Prevent that duplicated fieldnames are added to the RootTable when processing a QUERY */
            IF NOT ListHelper:EntryIsInList (cColumnName, pcExceptList) THEN DO:

                ASSIGN pcExceptList = TRIM(pcExceptList + ",":U + cColumnName, ",":U) .
                IF phBuffer:BUFFER-FIELD(j):EXTENT > 0 THEN
                    DO i = 1 TO phBuffer:BUFFER-FIELD(j):EXTENT:
                        arrayvar[iIterateNewFields] = NEW ColumnPropDesc (SUBSTITUTE("&1[&2]":U, cColumnName, i),
                                                                          SUBSTITUTE("&1[&2]":U, cColumnLabel, i),
                                                                          oDatatype).

                        ASSIGN iIterateNewFields = iIterateNewFields + 1.
                END.
                ELSE DO:
                    arrayvar[iIterateNewFields] = NEW ColumnPropDesc(cColumnName, cColumnLabel, oDatatype).
                    ASSIGN iIterateNewFields = iIterateNewFields + 1.
                END.
            END.
        END.

        /* The NewFields counter has to be reduced by one to get the count of the existing items in the array */
        ASSIGN iIterateNewFields = iIterateNewFields - 1.

        IF VALID-OBJECT(poTable) THEN
            IF VALID-OBJECT(poTable:COLUMNS) THEN DO:
                arrayJoined    = GetNewColumnPropDescArray(poTable:Columns:Length + iIterateNewFields).
                arrayFromTable = poTable:COLUMNS.

                DO i = 1 TO poTable:Columns:Length :
                    arrayJoined[i] = arrayFromTable[i].
                END.

                ASSIGN iIterate = 1.
                DO i = poTable:Columns:Length + 1 TO poTable:Columns:Length + iIterateNewFields:
                    arrayJoined[i] = arrayvar[iIterate].
                    iIterate       = iIterate + 1.
                END.

                RETURN arrayJoined.
        END.

        arrayJoined    = GetNewColumnPropDescArray(iIterateNewFields).

        DO i = 1 TO iIterateNewFields:
            arrayJoined[i] = arrayvar[i].
        END.

        RETURN arrayJoined.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a new TalbeDesc Array with the given length
        Notes:   The array will be returned empty
        @param piCount The size for the TableDesc array to return
        @return The new array for TableDesc instances of the given size
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE TableDesc EXTENT GetNewTableDescArray (piCount AS INTEGER):
        DEFINE VARIABLE oNewArray AS TableDesc EXTENT NO-UNDO.

        EXTENT(oNewArray) = piCount.

        RETURN oNewArray.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an Array of TableDesc
        Notes:   If the parent table has children the new table is appended
        @param poParentTable The reference of the TableDesc instance of the parent table
        @param poChildTable The reference of the TableDesc instance of the child table
        @return The array with the new TableDesc instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableDesc EXTENT PackChildTable (INPUT poParentTable AS TableDesc,
                                                   INPUT poChildTable AS TableDesc):

        DEFINE VARIABLE oArrayChildTables AS TableDesc EXTENT NO-UNDO.
        DEFINE VARIABLE arrayFromTable    AS TableDesc EXTENT NO-UNDO.
        DEFINE VARIABLE i                 AS INTEGER          NO-UNDO.

        IF VALID-OBJECT(poParentTable) THEN
            IF VALID-OBJECT(poParentTable:ChildTables) THEN DO ON ERROR UNDO, THROW:
                oArrayChildTables = GetNewTableDescArray(poParentTable:ChildTables:Length + 1).
                arrayFromTable    = poParentTable:ChildTables.

                DO i = 1 TO poParentTable:ChildTables:Length :
                    oArrayChildTables[i] = arrayFromTable[i].
                END.

                oArrayChildTables[EXTENT(oArrayChildTables)] = poChildTable.

                RETURN oArrayChildTables.
            END.

        EXTENT(oArrayChildTables) = 1.

        oArrayChildTables[1] = poChildTable.

        RETURN oArrayChildTables.

    END METHOD.

END CLASS.
