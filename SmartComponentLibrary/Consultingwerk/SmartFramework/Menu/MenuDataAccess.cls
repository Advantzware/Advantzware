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
    File        : MenuDataAccess
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories
    Created     : 11.10.2012 01:41:25
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.MenuDataAccess
    INHERITS SmartDataAccess:

    { Consultingwerk/SmartFramework/Menu/dsMenu.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE PRIVATE DATA-SOURCE src_SmartFunction FOR SmartFunction .
    DEFINE PRIVATE DATA-SOURCE src_SmartMenu FOR SmartMenu .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuDataAccess class
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.
        @param phDataset The handle of the Business Entity dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Fields that are not assigned to the database record when a new record is created. */
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN DO:
            THIS-OBJECT:AssignSkipDictionary:Add ("eSmartMenu":U, "MenuGuid":U).
            THIS-OBJECT:AssignSkipDictionary:Add ("eSmartFunction":U, "FunctionGuid":U).
        END.

        THIS-OBJECT:TrackDeletions = TRUE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsMenu:HANDLE, FALSE) .

        BUFFER eSmartFunction:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartFunction:HANDLE, "FunctionGuid,SmartFunction.FunctionGuid,FunctionName,SmartFunction.FunctionName,FunctionDescription,SmartFunction.FunctionDescription,FunctionCallParameter,SmartFunction.FunctionCallParameter,FunctionSmallImage,SmartFunction.FunctionSmallImage,FunctionLargeImage,SmartFunction.FunctionLargeImage":U) .
        BUFFER eSmartMenu:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartMenu:HANDLE, "MenuGuid,SmartMenu.MenuGuid,ParentMenuGuid,SmartMenu.ParentMenuGuid,MenuName,SmartMenu.MenuName,MenuStructureType,SmartMenu.MenuStructureType,MenuBeginsAGroup,SmartMenu.MenuBeginsAGroup,MenuSmallImage,SmartMenu.MenuSmallImage,MenuLargeImage,SmartMenu.MenuLargeImage,MenuSequence,SmartMenu.MenuSequence,FunctionGuid,SmartMenu.FunctionGuid":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsMenu DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsMenu BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

        BUFFER eSmartMenu:SET-CALLBACK ("AFTER-FILL":U, "eSmartMenuAfterFill":U, THIS-OBJECT).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsMenu:HANDLE, FALSE) .

        BUFFER eSmartFunction:DETACH-DATA-SOURCE () .
        BUFFER eSmartMenu:DETACH-DATA-SOURCE () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Set calculated fields of eSmartMenu
        Notes:
        @param dsMenu Input of the dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID eSmartMenuAfterFill (INPUT DATASET dsMenu):
        DEFINE BUFFER bufSmartMenu FOR SmartMenu.

        FOR EACH eSmartMenu:
            eSmartMenu.HasChild = CAN-FIND (FIRST bufSmartMenu WHERE bufSmartMenu.ParentMenuGuid = eSmartMenu.MenuGuid) .

            FIND FIRST SmartFunction WHERE SmartFunction.FunctionGuid = eSmartMenu.FunctionGuid NO-LOCK NO-ERROR.

            IF AVAILABLE (SmartFunction) THEN
                ASSIGN eSmartMenu.FunctionName = SmartFunction.FunctionName.
            ELSE
                ASSIGN ERROR-STATUS:ERROR = FALSE NO-ERROR .

            RELEASE SmartFunction.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Init the MenuSequence to highest number of sibling records + 1
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID eSmartMenuCreateBeginTrans ():

        DEFINE VARIABLE iMenuSequence AS INTEGER NO-UNDO INIT 1.
        DEFINE BUFFER b_SmartMenu FOR SmartMenu.

        FIND eSmartMenu WHERE ROWID(eSmartMenu) = BUFFER eSmartMenuBefore:AFTER-ROWID
            NO-ERROR .

        FIND LAST b_SmartMenu WHERE b_SmartMenu.ParentMenuGuid = eSmartMenu.ParentMenuGuid NO-LOCK NO-ERROR.

        IF AVAILABLE (b_SmartMenu) THEN
            iMenuSequence = b_SmartMenu.MenuSequence + 1.

        ASSIGN eSmartMenu.MenuSequence = iMenuSequence.

    END METHOD .

    &IF PROVERSION BEGINS "10" &THEN
    /*------------------------------------------------------------------------------
        Purpose: Overridable method that executes SAVE-ROW-CHANGES on the before buffer
        Notes:   Called from CommitChanges
                 Override to DataAccess:SaveRowChanges to workaround OE10 issue with
                 SAVE-ROW-CHANGES and LOB's: http://knowledgebase.progress.com/articles/Article/ErrorwithSAVEROWCHANGESsavingbufferswithCLOBBLOBfields
        @param phBeforeBuffer The handle of the before buffer
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID SaveRowChanges (phBeforeBuffer AS HANDLE):

        DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.

        DEFINE BUFFER SmartFunction FOR SmartFunction .

        ASSIGN cTableName = phBeforeBuffer:AFTER-BUFFER:NAME .

        /* Mike Fechner, Consultingwerk Ltd. 26.04.2011
           Skip-List for SAVE-ROW-CHANGES */
        IF THIS-OBJECT:AssignSkipDictionary:ContainsKey (cTableName) THEN
            phBeforeBuffer:SAVE-ROW-CHANGES (1, THIS-OBJECT:AssignSkipDictionary:GetValue (cTableName), TRUE) NO-ERROR .
        ELSE
            IF THIS-OBJECT:AssignSkipList > "":U THEN
                phBeforeBuffer:SAVE-ROW-CHANGES (1, THIS-OBJECT:AssignSkipList, TRUE) NO-ERROR.
        ELSE
            phBeforeBuffer:SAVE-ROW-CHANGES (1, "":U, TRUE) NO-ERROR.

        IF phBeforeBuffer:AFTER-BUFFER:NAME = "eSmartFunction":U AND phBeforeBuffer:ROW-STATE <> ROW-DELETED THEN
            DO FOR SmartFunction:
                Consultingwerk.Util.BufferHelper:FindAfterBuffer (phBeforeBuffer) .

                FIND SmartFunction WHERE SmartFunction.FunctionGuid = phBeforeBuffer::FunctionGuid EXCLUSIVE-LOCK NO-ERROR .
                IF NOT AVAILABLE SmartFunction THEN
                    FIND SmartFunction WHERE SmartFunction.FunctionGuid = phBeforeBuffer:AFTER-BUFFER::FunctionGuid EXCLUSIVE-LOCK NO-ERROR .

                COPY-LOB FROM phBeforeBuffer:AFTER-BUFFER::FunctionCallParameter TO SmartFunction.FunctionCallParameter .
            END.

    END METHOD.
    &ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition)
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.

        CASE pcTable:
            WHEN "eSmartFunction":U THEN
                cMap = DATA-SOURCE src_SmartFunction:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .
            WHEN "eSmartMenu":U THEN
                cMap = DATA-SOURCE src_SmartMenu:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

        END CASE .

        RETURN ENTRY(LOOKUP(pccolumn,cMap) + 1,cMap).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the base query string used to retrieve data for the temp
                 tables. This query string will be appended by the query provided by
                 the consumer (FetchDataRequest object).
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
        @param pcTable The temp table name to return the source default (database) query string for
        @return The base query string used to retrieve data for the given temp-table
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):

        CASE pcTable:
            WHEN "eSmartFunction":U THEN
                RETURN "FOR EACH SmartFunction INDEXED-REPOSITION":U.
            WHEN "eSmartMenu":U THEN
                RETURN "FOR EACH SmartMenu INDEXED-REPOSITION":U.

        END CASE .

    END METHOD.

END CLASS.
