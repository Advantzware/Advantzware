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
    File        : DataRelationFieldsService
    Purpose     : Default IDataRelationFieldsService Implementation
                  Used to return the field names used by new DATA-RELATION
                  objects in the business entity generator
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Feb 20 22:52:47 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.DataRelationFieldsService IMPLEMENTS IDataRelationFieldsService:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    /*------------------------------------------------------------------------------
        Purpose: Generates a set of data-relation field names
        Notes:   Used by the Business Entity Generator when a new data-relation is
                 dragged to the design canvas
        @param pcParentTempTableName The name of the database talbe (source table)
        @param pcChildTempTableName The name of the database field (source field)
        @param dsBusinessEntity Dataset with the current Business Entity model
        @return The field pairs for the specified data-relation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateDataRelationFields (pcParentTempTableName AS CHARACTER,
                                                        pcChildTempTableName AS CHARACTER,
                                                        DATASET FOR dsBusinessEntity):

        DEFINE BUFFER b_ParentTable FOR eTable .
        DEFINE BUFFER b_ChildTable FOR eTable .
        DEFINE BUFFER b_ChildTableIndex FOR eIndex .
        DEFINE BUFFER b_ParentTableIndex FOR eIndex .
        DEFINE BUFFER b_ChildTableField FOR eField .
        DEFINE BUFFER b_ParentTableField FOR eField .

        DEFINE VARIABLE i               AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cRelationFields AS CHARACTER NO-UNDO.

        FIND FIRST eBusinessEntity.

        FIND FIRST b_ParentTable WHERE b_ParentTable.BusinessEntityName = eBusinessEntity.BusinessEntityName
                                   AND b_ParentTable.TempTableName      = pcParentTempTableName .

        FIND FIRST b_ChildTable  WHERE b_ChildTable.BusinessEntityName  = eBusinessEntity.BusinessEntityName
                                   AND b_ChildTable.TempTableName       = pcChildTempTableName .

        /* Relation Fields */
        FIND FIRST b_ChildTableIndex OF b_ChildTable
            WHERE b_ChildTableIndex.IndexPrimary = TRUE NO-ERROR .

        IF AVAILABLE b_ChildTableIndex THEN
        childprimaryindex: DO:
            DO i = 1 TO NUM-ENTRIES (b_ChildTableIndex.FieldNames):
                 IF NOT CAN-FIND (b_ParentTableField OF b_ParentTable
                            WHERE b_ParentTableField.FieldName = ENTRY (i, b_ChildTableIndex.FieldNames)) THEN
                         LEAVE childprimaryindex .
            END.

            DO i = 1 TO NUM-ENTRIES (b_ChildTableIndex.FieldNames):

                ASSIGN cRelationFields = cRelationFields +
                                         SUBSTITUTE ("&1,&1,":U, ENTRY (i, b_ChildTableIndex.FieldNames)).
            END.

            ASSIGN cRelationFields = TRIM (cRelationFields, ",":U) .

            IF cRelationFields > "":U THEN
                RETURN cRelationFields .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 20.01.2013
           When it's not the child's primary index, try the parent's primary index */
        parentindexloop1:
        FOR EACH b_ParentTableIndex OF b_ParentTable
            WHERE b_ParentTableIndex.IndexUnique  = TRUE
               BY b_ParentTableIndex.IndexPrimary DESCENDING ON ERROR UNDO, THROW: /* first true then false */

            DO i = 1 TO NUM-ENTRIES (b_ParentTableIndex.FieldNames):
                 IF NOT CAN-FIND (b_ChildTableField OF b_ChildTable WHERE
                                  b_ChildTableField.FieldName = ENTRY (i, b_ParentTableIndex.FieldNames)) THEN
                         NEXT parentindexloop1 .
            END.

            ASSIGN cRelationFields = "":U .

            DO i = 1 TO NUM-ENTRIES (b_ParentTableIndex.FieldNames):

                ASSIGN cRelationFields = cRelationFields +
                                         SUBSTITUTE ("&1,&1,":U, ENTRY (i, b_ParentTableIndex.FieldNames)).
            END.

            ASSIGN cRelationFields = TRIM (cRelationFields, ",":U) .
        END.

        IF cRelationFields > "":U THEN
            RETURN cRelationFields .

        childindexloop:
        FOR EACH b_ChildTableIndex OF b_ChildTable
            WHERE NOT b_ChildTableIndex.IndexWordIndex BY b_ChildTableIndex.IndexOrder ON ERROR UNDO, THROW:

            DO i = 1 TO NUM-ENTRIES (b_ChildTableIndex.FieldNames):
                 IF NOT CAN-FIND (b_ParentTableField OF b_ParentTable
                     WHERE b_ParentTableField.FieldName = ENTRY (i, b_ChildTableIndex.FieldNames)) THEN
                         NEXT childindexloop .
            END.

            ASSIGN cRelationFields = "":U .

            DO i = 1 TO NUM-ENTRIES (b_ChildTableIndex.FieldNames):

                ASSIGN cRelationFields = cRelationFields +
                                         SUBSTITUTE ("&1,&1,":U, ENTRY (i, b_ChildTableIndex.FieldNames)).
            END.

            ASSIGN cRelationFields = TRIM (cRelationFields, ",":U) .

            LEAVE childindexloop .
        END.

        IF cRelationFields > "":U THEN
            RETURN cRelationFields .

        /* Mike Fechner, Consultingwerk Ltd. 16.11.2010
           When there was not matching ChildTable's index, try the other way around */
        parentindexloop:
        FOR EACH b_ParentTableIndex OF b_ParentTable
            WHERE NOT b_ParentTableIndex.IndexWordIndex BY b_ParentTableIndex.IndexOrder ON ERROR UNDO, THROW:

            DO i = 1 TO NUM-ENTRIES (b_ParentTableIndex.FieldNames):
                 IF NOT CAN-FIND (b_ChildTableField OF b_ChildTable WHERE
                                          b_ChildTableField.FieldName = ENTRY (i, b_ParentTableIndex.FieldNames)) THEN
                         NEXT parentindexloop .
            END.

            ASSIGN cRelationFields = "":U .

            DO i = 1 TO NUM-ENTRIES (b_ParentTableIndex.FieldNames):

                ASSIGN cRelationFields = cRelationFields +
                                         SUBSTITUTE ("&1,&1,":U, ENTRY (i, b_ParentTableIndex.FieldNames)).
            END.

            ASSIGN cRelationFields = TRIM (cRelationFields, ",":U) .

            LEAVE parentindexloop .
        END.

        RETURN cRelationFields .

    END METHOD.

END CLASS.