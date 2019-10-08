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
    File        : ContextWrapper
    Purpose     : Wraps access to the context dataset
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 20 00:18:35 CEST 2011
    Notes       : SCL-776: Constructor is PROTECTED, so that customers may inherit
                  from this class and extend it by adding addional properties to it
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                   FROM PROPATH .
USING Consultingwerk.Assertion.*         FROM PROPATH .
USING Consultingwerk.Framework.Session.* FROM PROPATH .
USING Consultingwerk.OERA.*              FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.*   FROM PROPATH .
USING Consultingwerk.Util.*              FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.OERA.ContextWrapper:

    {Consultingwerk/OERA/ContextWrapperSessionProperty.i UserName CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i UserKey CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i LoginCompanyName CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i ClientPrincipal RAW}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i LoginCompanyKey CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i LoginCompanyShortName CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i LoginCompanyReferenceChar CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i LoginCompanyReferenceDecimal DECIMAL}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i LoginCompanyReferenceInteger INTEGER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i LanguageDisplayName CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i LanguageIsoCode CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i LanguageKey CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i ServerDebugMode LOGICAL}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i ServerCustomLogEntries CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i ServerLogEntryTypes CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i UserGroupKeys CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i UserGroupNames CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i ClientProVersion CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i AppServerProVersion CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i ClientType CHARACTER}
    {Consultingwerk/OERA/ContextWrapperSessionProperty.i AppServerType CHARACTER}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ContextWrapper class
        Notes:   Disallow instance creation
                 SCL-776: Constructor is PROTECTED, so that customers may inherit
                 from this class and extend it by adding addional properties to it
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED ContextWrapper ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds a temp-table to the context dataset
        Notes:   When the context-dataset is static, we will create a new dynamic
                 ProDataset and use that as the new context dataset. This new context
                 dataset will be built using new buffers on the temp-tables of the
                 previous (static) context dataset
                 When the temp-tables default-buffer-handle is already member of a
                 ProDataset we will create a new dynamic buffer for the table and
                 add that to the dataset
        @param phTempTable The handle of the Temp-Table to add to the context dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID AddTempTable (phTempTable AS HANDLE):

        DEFINE VARIABLE hBuffer  AS HANDLE  NO-UNDO .
        DEFINE VARIABLE hTables  AS HANDLE  NO-UNDO EXTENT .
        DEFINE VARIABLE hDataset AS HANDLE  NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER NO-UNDO .
        DEFINE VARIABLE iCount   AS INTEGER NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phTempTable """Temp-Table Handle"":U"} .

        IF phTempTable:TYPE =  WidgetTypeEnum:Buffer THEN
            ASSIGN phTempTable = phTempTable:TABLE-HANDLE .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phTempTable WidgetTypeEnum:TempTable} .
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i SessionManager:ContextDataset """Context Dataset"":U"} .

        ASSIGN hBuffer = phTempTable:DEFAULT-BUFFER-HANDLE .

        IF VALID-HANDLE (hBuffer:DATASET) THEN
            ASSIGN hBuffer = BufferHelper:CreateBuffer (hBuffer) .

        IF SessionManager:ContextDataset:DYNAMIC = TRUE THEN DO:
            SessionManager:ContextDataset:ADD-BUFFER (hBuffer) .
        END.
        ELSE DO:
            ASSIGN hTables = DatasetHelper:GetTableHandles (SessionManager:ContextDataset)
                   iCount = EXTENT (hTables).

            CREATE DATASET hDataset .
            hDataset:NAME = SessionManager:ContextDataset:NAME .

            DO i = 1 TO iCount:
                hDataset:ADD-BUFFER (BufferHelper:CreateBuffer (hTables[i])) .
            END.

            hDataset:ADD-BUFFER (hBuffer) .

            SessionManager:ContextDataset = hDataset .
        END.

        CATCH err AS Progress.Lang.Error:
            IF VALID-HANDLE (hDataset) THEN
                DatasetHelper:DeleteDatasetAndBuffers (hDataset) .

            UNDO, THROW err .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a handle to a field of the Session Context buffer
        Notes:
        @param pcFieldName The name of the context buffer field
        @return The handle of the conext buffer field
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC HANDLE GetSessionContextField (pcFieldName AS CHARACTER):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        DatasetAssert:HasBuffer (SessionManager:ContextDataset,
                                 "eSessionContext":U) .

        ASSIGN hBuffer = SessionManager:ContextDataset::eSessionContext .

        BufferAssert:HasField (hBuffer, pcFieldName) .

        hBuffer:FIND-FIRST () NO-ERROR .

        IF NOT hBuffer:AVAILABLE THEN
            UNDO, THROW NEW AppError ("No session context record available"{&TRAN}, 0) .

        RETURN hBuffer:BUFFER-FIELD (pcFieldName) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes all values from the context properties table where the
                 property name matches the given property name pattern (MATCHES)
        Notes:
        @param pcProperties The pattern (MATCHES) for the property names to delete
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RemoveContextValues (pcProperties AS CHARACTER):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO .
        DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.

        DatasetAssert:HasBuffer (SessionManager:ContextDataset,
                                 "eContextProperties":U) .

        ASSIGN hBuffer = SessionManager:ContextDataset::eContextProperties
               hQuery  = QueryHelper:CreatePreparedQuery (hBuffer,
                                                          SUBSTITUTE ("FOR EACH eContextProperties WHERE eContextProperties.PropertyName MATCHES &1":U,
                                                                      QUOTER (pcProperties))).

        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
            hBuffer:BUFFER-DELETE () .

            FINALLY:
                hQuery:GET-NEXT () .
            END FINALLY.
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes a temp-table from the context dataset
        Notes:   When the context-dataset is static, we will throw an error as we expect
                 that the "core" temp-tables of a static ProDataset should not be removed.
                 As dynamic ProDatasets do not support removing temp-tables we are creating
                 a new session dataset from new buffers for the temp-tables of the buffers
                 that are in the current context dataset.
        @param phTempTable The handle of the Temp-Table to remove from the context dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RemoveTempTable (phTempTable AS HANDLE):

        DEFINE VARIABLE hDataset    AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hTables     AS HANDLE  NO-UNDO EXTENT .
        DEFINE VARIABLE i           AS INTEGER NO-UNDO .
        DEFINE VARIABLE iCount      AS INTEGER NO-UNDO .
        DEFINE VARIABLE hOldDataset AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phTempTable """Temp-Table Handle"":U"} .

        IF phTempTable:TYPE =  WidgetTypeEnum:Buffer THEN
            ASSIGN phTempTable = phTempTable:TABLE-HANDLE .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phTempTable WidgetTypeEnum:TempTable} .
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i SessionManager:ContextDataset """Context Dataset"":U"} .

        ASSIGN hOldDataset = SessionManager:ContextDataset .

        IF hOldDataset:DYNAMIC = FALSE THEN
            UNDO, THROW NEW CannotRemoveTableFromStaticContextDatasetException () .

        ASSIGN hTables = DatasetHelper:GetTableHandles (hOldDataset)
               iCount = EXTENT (hTables).

        CREATE DATASET hDataset .
        hDataset:NAME = hOldDataset:NAME .

        DO i = 1 TO iCount:
            /*  When it's not the table to be removed, add it to
                the new ProDataset */
            IF hTables[i] <> phTempTable THEN
                hDataset:ADD-BUFFER (BufferHelper:CreateBuffer (hTables[i])) .
        END.

        SessionManager:ContextDataset = hDataset .

        DatasetHelper:DeleteDatasetAndBuffers (hOldDataset) .

        CATCH err AS Progress.Lang.Error:
            IF VALID-HANDLE (hDataset) THEN
                DatasetHelper:DeleteDatasetAndBuffers (hDataset) .

            ASSIGN SessionManager:ContextDataset = hOldDataset .

            UNDO, THROW err .
        END CATCH.

    END METHOD.

    {Consultingwerk/OERA/ContextWrapperProperty.i Character}
    {Consultingwerk/OERA/ContextWrapperProperty.i Longchar}
    {Consultingwerk/OERA/ContextWrapperProperty.i Integer}
    {Consultingwerk/OERA/ContextWrapperProperty.i Int64}
    {Consultingwerk/OERA/ContextWrapperProperty.i Decimal}
    {Consultingwerk/OERA/ContextWrapperProperty.i Logical}
    {Consultingwerk/OERA/ContextWrapperProperty.i Date}
    {Consultingwerk/OERA/ContextWrapperProperty.i DateTime}
    {Consultingwerk/OERA/ContextWrapperProperty.i Raw}

END CLASS.
