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
    File        : SmartBindingSourceArrayFactory
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Jun 02 17:30:04 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Data.*       FROM ASSEMBLY .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Support.SmartBindingSourceArrayFactory   :

    {Consultingwerk/SmartComponents/Support/ttTablesAndFields.i}

    /*------------------------------------------------------------------------------
        Purpose: Returns a new ColumnPropDesc Array with the given length.
        Notes:   Invoked by GetTableDescItem of this Class.
        @param piCount The size of the new array
        @return The ColumnPropDesc array with the given size
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE ColumnPropDesc EXTENT GetNewColumnPropDescArray (piCount AS INTEGER):

        DEFINE VARIABLE oNewArray AS ColumnPropDesc EXTENT NO-UNDO.

        EXTENT(oNewArray) = piCount.

        RETURN oNewArray.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an Array of ColumnPropDesc to be filled into the
                 TableDesc:Columns Property. If the TableDesc has valid Columns
                 the new Columns are appended.
        Notes:   This Method is invoked by the SmartBindingSources
                 DesignerImportSchema Method.
                 The table description for the given TableName has to be present
                 in the DATASET dsSchema to be able to fill the Array.
        @param poTableDesc The reference to the TableDesc instance
        @param pcTableName The name of the temp-table
        @param dsSchema The dataset with the schema
        @param pcChoosenSchema The comma delimited list of column names
        @param pcExceptList INPUT-OUTPUT The comma delimited list of columns to be excluded from the binding source
        @return The ColumnPropDesc array
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ColumnPropDesc EXTENT FillColumns (poTableDesc AS TableDesc,
                                                     pcTableName AS CHARACTER,
                                                     DATASET dsSchema,
                                                     pcChoosenSchema AS CHARACTER,
                                                     INPUT-OUTPUT pcExceptList AS CHARACTER):

        DEFINE VARIABLE oColumnPropDescExtent AS ColumnPropDesc EXTENT NO-UNDO.
        DEFINE VARIABLE oArrayFromTable       AS ColumnPropDesc EXTENT NO-UNDO.

        DEFINE VARIABLE iFieldCount  AS INTEGER                NO-UNDO.
        DEFINE VARIABLE iIterate     AS INTEGER                NO-UNDO.
        DEFINE VARIABLE i            AS INTEGER                NO-UNDO.

        DEFINE VARIABLE cColumnName  AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE cColumnLabel AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE oDatatype    AS Progress.Data.DataType NO-UNDO.

        ASSIGN
            iFieldCount = 0
            .

        FOR EACH ttFields WHERE ttFields.TableName = pcTableName NO-LOCK:

            IF Progress.Util.EnumHelper:AreEqual (Consultingwerk.Util.DataTypeHelper:ToEnum (ttFields.DataType),
                                                  Progress.Data.DataType:UNKNOWN) THEN
                NEXT .

            /* Prevent that duplicated fieldnames are counted to build an array with the correct size */
            IF pcChoosenSchema <> "":U THEN DO:
                IF ListHelper:EntryIsInList (ttFields.ColumnName, pcChoosenSchema) THEN
                    IF NOT ListHelper:EntryIsInList (ttFields.ColumnName, pcExceptList) THEN
                        ASSIGN iFieldCount = iFieldCount + 1.
            END.
            ELSE
                IF NOT ListHelper:EntryIsInList (ttFields.ColumnName, pcExceptList) THEN
                    ASSIGN iFieldCount = iFieldCount + 1.
        END.

        /* If there are Columns in the current TableDesc:Columns Property, append the new Columns
           else create a new Array to be returned */
        IF VALID-OBJECT(poTableDesc) THEN
            IF VALID-OBJECT(poTableDesc:Columns) THEN DO:
            oColumnPropDescExtent = GetNewColumnPropDescArray(poTableDesc:Columns:Length + iFieldCount).
            oArrayFromTable = poTableDesc:Columns.

            DO i = 1 TO poTableDesc:Columns:Length:
                oColumnPropDescExtent[i] = oArrayFromTable[i].
            END.

            ASSIGN
                iIterate    = poTableDesc:Columns:Length + 1
                .
        END.
        ELSE DO:
            oColumnPropDescExtent = GetNewColumnPropDescArray(iFieldCount).

            ASSIGN
                iIterate    = 1
                .
        END.

        fieldsLoop:
        FOR EACH ttFields WHERE ttFields.TableName = pcTableName NO-LOCK :
            cColumnName     = ttFields.ColumnName.
            cColumnLabel    = ttFields.ColumnLabel.

            oDataType = Consultingwerk.Util.DataTypeHelper:ToEnum (ttFields.DataType) .

            /* If the Datatype is unknown or the column is not selected to be added to the schema
               move on to the next */
            IF pcChoosenSchema <> "":U THEN
                IF oDatatype = Progress.Data.DataType:UNKNOWN
                OR NOT ListHelper:EntryIsInList (cColumnName, pcChoosenSchema) THEN
                    NEXT fieldsLoop.
            ELSE
                IF oDatatype = Progress.Data.DataType:UNKNOWN THEN
                    NEXT fieldsLoop.

            /* Prevent that duplicated fieldnames are added to the Table */
            IF NOT ListHelper:EntryIsInList (cColumnName, pcExceptList) THEN DO:
                ASSIGN pcExceptList = TRIM(pcExceptList + ",":U + cColumnName, ",":U) .

                oColumnPropDescExtent[iIterate] = NEW Progress.Data.ColumnPropDesc(cColumnName,
                                                                                   cColumnLabel,
                                                                                   oDatatype).

                ASSIGN iIterate = iIterate + 1.
            END.
        END.

        RETURN oColumnPropDescExtent.

    END METHOD.

 END CLASS.
