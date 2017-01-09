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
    File        : UiTranslationDataAccess
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 12.05.2013 18:40:10
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.UiTranslationDataAccess
    INHERITS SmartDataAccess:

    { Consultingwerk/SmartFramework/System/dsUiTranslation.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartUiTranslation FOR SmartUiTranslation .
    @DefineDataSourcesEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UiTranslationDataAccess class
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.
        @param phDataset The handle of the Business Entity dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UiTranslationDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN
            THIS-OBJECT:AssignSkipList = "UiTranslationGuid":U .

        THIS-OBJECT:TrackDeletions = TRUE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsUiTranslation:HANDLE, FALSE) .

        @AttachDataSourcesStart.
        BUFFER eSmartUiTranslation:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartUiTranslation:HANDLE, "UiTranslationGuid,SmartUiTranslation.UiTranslationGuid,LanguageGuid,SmartUiTranslation.LanguageGuid,ObjectName,SmartUiTranslation.ObjectName,ResourceKey,SmartUiTranslation.ResourceKey,OriginalString,SmartUiTranslation.OriginalString,TranslatedString,SmartUiTranslation.TranslatedString":U) .
        @AttachDataSourcesEnd.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsUiTranslation DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsUiTranslation BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsUiTranslation:HANDLE, FALSE) .

        @DetachDataSourcesStart.
        BUFFER eSmartUiTranslation:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the dataset with a single eSmartUiTranslation record
                 per object for which translations exist in the database
        Notes:
        @param pcLanguageKey The key of the language for which to return the list of translated objects
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetTranslatableControls (pcLanguageKey AS CHARACTER):

        DATASET dsUiTranslation:EMPTY-DATASET () .

        FOR EACH SmartUiTranslation WHERE SmartUiTranslation.LanguageGuid = pcLanguageKey NO-LOCK
                                 BREAK BY SmartUiTranslation.ObjectName:

            IF FIRST-OF (SmartUiTranslation.ObjectName) THEN DO:
                CREATE eSmartUiTranslation.
                ASSIGN eSmartUiTranslation.LanguageGuid      = pcLanguageKey
                       eSmartUiTranslation.ObjectName        = SmartUiTranslation.ObjectName
                       eSmartUiTranslation.UiTranslationGuid = ? .
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition)
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
                 TO-DO: Provide code for alternative mapping
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.

        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eSmartUiTranslation":U THEN
                cMap = DATA-SOURCE src_SmartUiTranslation:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

        END CASE .

        RETURN ENTRY(LOOKUP(pccolumn,cMap) + 1,cMap).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the base query string used to retrieve data for the temp
                 tables. This query string will be appended by the query provided by
                 the consumer (FetchDataRequest object).
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
                 TO-DO: Provide code to return the DATA-SOURCE root query string
        @param pcTable The temp table name to return the source default (database) query string for
        @return The base query string used to retrieve data for the given temp-table
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):

        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eSmartUiTranslation":U THEN
                RETURN "FOR EACH SmartUiTranslation INDEXED-REPOSITION":U.

        END CASE .

    END METHOD.

END CLASS.
