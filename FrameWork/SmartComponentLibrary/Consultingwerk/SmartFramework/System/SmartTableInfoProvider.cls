/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartTableInfoProvider
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 15 21:24:57 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.Assertion.*       FROM PROPATH .
USING Consultingwerk.Framework.*       FROM PROPATH .
USING Consultingwerk.OERA.*            FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*  FROM PROPATH .
USING Consultingwerk.Util.*            FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartTableInfoProvider
    IMPLEMENTS ITableInfoProvider:

    DEFINE VARIABLE lCachePopulated AS LOGICAL NO-UNDO INIT FALSE .

    {Consultingwerk/SmartFramework/System/dsTable.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTableInfoProvider class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTableInfoProvider ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Caches the SmartTable records
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CacheSmartTableRecords ():

        ASSIGN lCachePopulated = TRUE .

        IF CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            ServiceInterface:FetchData ("Consultingwerk.SmartFramework.System.TableBusinessEntity":U,
                                        NEW FetchDataRequest ("eSmartTable":U),
                                        OUTPUT DATASET dsTable) .
        ELSE
            FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                           "Consultingwerk.SmartFramework.System.TableBusinessEntity":U,
                                                           NEW FetchDataRequest ("eSmartTable":U),
                                                           OUTPUT DATASET dsTable) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Finds a buffer based on the unique key values
        Notes:
        @param phBuffer The buffer with the database record to return the description for
        @param pcUniqueKeyValues The unqiue key field values
        @param plExclusive Logical value indicating if the record should be accessed with exclusive-lock
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FindRecordByKeyValues (phBuffer AS HANDLE,
                                              pcUniqueKeyValues AS CHARACTER,
                                              plExclusive AS LOGICAL):

        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cFindString AS CHARACTER NO-UNDO.

        HandleAssert:WidgetType (phBuffer, WidgetTypeEnum:Buffer) .
        BufferAssert:IsDatabaseBuffer (phBuffer) .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:CacheSmartTableRecords () .

        FIND eSmartTable WHERE eSmartTable.DatabaseName = phBuffer:DBNAME
                           AND eSmartTable.TableName    = phBuffer:TABLE NO-ERROR .

        DO i = 1 TO NUM-ENTRIES (eSmartTable.UniqueKeyFields):

            IF i = 1 THEN
                ASSIGN cFindString = cFindString + SUBSTITUTE ("WHERE &1 = &2":U,
                                                               ENTRY (i, eSmartTable.UniqueKeyFields),
                                                               QUOTER (ENTRY (i, pcUniqueKeyValues, CHR (1)))) .
            ELSE
                ASSIGN cFindString = cFindString + SUBSTITUTE (" AND &1 = &2":U,
                                                               ENTRY (i, eSmartTable.UniqueKeyFields),
                                                               QUOTER (ENTRY (i, pcUniqueKeyValues, CHR (1)))) .

        END.

        DO ON ERROR UNDO, THROW:
            IF plExclusive THEN
                phBuffer:FIND-UNIQUE (cFindString, EXCLUSIVE-LOCK) .
            ELSE
                phBuffer:FIND-UNIQUE (cFindString, NO-LOCK) .

            CATCH err AS Progress.Lang.SysError:
                UNDO, THROW NEW RecordNotAvailableException (err:GetMessage (1), err:GetMessageNum (1)) .
            END CATCH.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the standard description value for a database record
        Notes:
        @param phBuffer The buffer with the database record to return the description for
        @return The standard description value for the record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecordDescription (phBuffer AS HANDLE):

        RETURN RecordDescription (phBuffer:DBNAME,
                                  phBuffer:TABLE,
                                  phBuffer) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the standard description value for a database record
        Notes:
        @param pcDatabaseName The name of the database
        @param pcTableName The name of the database table
        @param phBuffer The buffer with the database record to return the description for
        @return The standard description value for the record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecordDescription (pcDatabaseName AS CHARACTER,
                                               pcTableName AS CHARACTER,
                                               phBuffer AS HANDLE):

        DEFINE BUFFER eSmartTable FOR eSmartTable .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:CacheSmartTableRecords () .

        FIND eSmartTable WHERE eSmartTable.DatabaseName = pcDatabaseName
                           AND eSmartTable.TableName    = pcTableName NO-ERROR .

        IF NOT AVAILABLE eSmartTable THEN
            RETURN ? .

        RETURN BufferHelper:BufferFieldSubstituteFormatted (phBuffer,
                                                            eSmartTable.DescriptionFields,
                                                            eSmartTable.DescriptionSubstitute,
                                                            eSmartTable.DescriptionFormats) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the key field values for a database record
        Notes:
        @param phBuffer The buffer with the database record to return the description for
        @return The standard description value for the record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecordKeyFields (phBuffer AS HANDLE):

        RETURN RecordKeyFields (phBuffer:DBNAME,
                                phBuffer:TABLE,
                                phBuffer) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the key field values for a database record
        Notes:
        @param pcDatabaseName The name of the database
        @param pcTableName The name of the database table
        @param phBuffer The buffer with the database record to return the description for
        @return The standard description value for the record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecordKeyFields (pcDatabaseName AS CHARACTER,
                                             pcTableName AS CHARACTER,
                                             phBuffer AS HANDLE).

        DEFINE BUFFER eSmartTable FOR eSmartTable .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:CacheSmartTableRecords () .

        FIND eSmartTable WHERE eSmartTable.DatabaseName = pcDatabaseName
                           AND eSmartTable.TableName    = pcTableName NO-ERROR .

        IF NOT AVAILABLE eSmartTable THEN
            RETURN ? .

        RETURN BufferHelper:BufferFieldSubstitute (phBuffer,
                                                   eSmartTable.UniqueKeyFields,
                                                   "&1":U + CHR (1) + "&2":U + CHR (1) + "&3":U + CHR (1) +
                                                   "&4":U + CHR (1) + "&5":U + CHR (1) + "&6":U + CHR (1) +
                                                   "&7":U + CHR (1) + "&8":U + CHR (1) + "&9":U + CHR (1)) .

    END METHOD .

END CLASS.
