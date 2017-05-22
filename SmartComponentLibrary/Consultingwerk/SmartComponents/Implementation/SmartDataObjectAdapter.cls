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
    File        : SmartDataObjectAdapter
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 31 10:03:56 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.*      FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Data.*                                 FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartDataObjectAdapter
    INHERITS SmartDataAdapter:

      /*------------------------------------------------------------------------------
        Purpose: Gets if the SmartDataObjectAdapter is currently adding a new
                 row
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AddingRecord AS LOGICAL NO-UNDO INIT FALSE
    GET.
    PRIVATE SET.

      /*------------------------------------------------------------------------------
        Purpose: Gets the RowIdent of the added row
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AddRowIdent AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

       /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the BatchSize used when reading data from the Business
                 Entity.
        Notes:   Set to 0 to fetch all data and disable Batching
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BatchSize AS INTEGER INITIAL 50 NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to the RowObject query of the SmartDataObject
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataHandle AS HANDLE NO-UNDO
    GET():
        DEFINE VARIABLE hDataHandle AS HANDLE NO-UNDO.
        DEFINE VARIABLE hSDO        AS HANDLE NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        IF NOT VALID-HANDLE (hSDO) THEN
            RETURN ? .

        {get DataHandle hDataHandle hSDO} .

        RETURN hDataHandle .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: ForeignFields related to the Parent SmartDataSource.
                 This Property is set by the SetSmartDataSource Method when a new
                 SmartDataSource is registered.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ForeignFields AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET (arg AS CHARACTER):
        THIS-OBJECT:ForeignFields = ListHelper:TrimEntries (arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: If Property is set a File selection Dialog is Shown to select a
                 SmartDataObject .w file and set the property SmartDataObjectName
                 to the selected filename.
        Notes:   The Value of this Property is not changed but the Dialog is started
                 when temporarily set to TRUE.
                 Workaround to be able to start a Dialog from Propertygrid.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OpenSmartDataObject AS LOGICAL NO-UNDO INIT FALSE
    GET():
        RETURN FALSE.
    END GET.
    SET(INPUT arg AS LOGICAL):
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lOk       AS LOGICAL NO-UNDO.

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        IF arg = TRUE THEN .
        ELSE RETURN .

        DesignerOpenSmartDataObject () .

    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to the RowObject temp-table of the SmartDataObject
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RowObject AS HANDLE NO-UNDO
    GET:
        DEFINE VARIABLE hRowObject AS HANDLE NO-UNDO.
        DEFINE VARIABLE hSDO       AS HANDLE NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        IF NOT VALID-HANDLE (hSDO) THEN
            RETURN ? .

        {get RowObject hRowObject hSDO} .

        RETURN hRowObject .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the BatchSize used when reading data from the Business
                 Entity.
        Notes:   This property has the same meaning/value as BatchSize. It's avialable
                 primarily to respect ADM2 terms.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RowsToBatch AS INTEGER NO-UNDO
    GET:
        RETURN THIS-OBJECT:BatchSize .
    END GET.
    SET (arg AS INTEGER):
        ASSIGN THIS-OBJECT:BatchSize = arg .
    END SET.

     /*------------------------------------------------------------------------------
        Purpose: Gets the handle to the SmartDataObject instance
        Notes:   PRIVATE SET, returns unknown value is SmartDataObject has not yet
                 been loaded
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataObjectHandle AS HANDLE NO-UNDO
    GET.
    PRIVATE SET.

     /*------------------------------------------------------------------------------
        Purpose: Returns if the SmartDataObject is initalized
        Notes:   Uses ObjectInitialized property of the SDO
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataObjectInitialized AS LOGICAL NO-UNDO
    GET:
        DEFINE VARIABLE lInitialized AS LOGICAL NO-UNDO.
        DEFINE VARIABLE hSDO AS HANDLE NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        IF NOT VALID-HANDLE (hSDO) THEN
            RETURN FALSE .

        {get ObjectInitialized lInitialized hSDO} .

        RETURN lInitialized .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: The Name of the SmartDataObject .
                 This Property can be set by hand (typing in a .w file) or using a
                 Dialog by setting the Property OpenSmartDataObject to TRUE.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataObjectName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDataObjectAdapter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataObjectAdapter ():
        SUPER ().

        THIS-OBJECT:SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDataObjectAdapter class
        Notes:
        @param poContainer The System.ComponentModel.IContainer used as the Container for this component
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataObjectAdapter (poContainer AS System.ComponentModel.IContainer):

        SUPER (INPUT poContainer).

        THIS-OBJECT:SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assigns selection criteria to the query and distributes the
                 column/value pairs to the corresponding buffer's where-clause.
                 Each buffer's expression will always be embedded in parenthesis.
        Notes:  Calls assignQuerySelection in the SmartDataObject
                Can be called before initialization, but the alternative string
                operator is not supported before the object is initialized.
                (datatype is not known)
              -  This procedure is designed to be called several times to build up
                 the the query's where clause
                 (storing intermediate results in the QueryString property) before
                 it is finally used in a Query-Prepare method.
              -  openDataQuery takes care of the preparation of the QueryString
                 property.
              -  The QueryColumns property is used to ensure that each column and
                 operator only will be added once to the QueryString. The property is
                 also used to store the offset and length of the corresponding values.
        @param pcColumns Column names (Comma separated), Fieldname of a table in the query in the form of TBL.FLDNM or DB.TBL.FLDNM (only if qualified with db is specified), (RowObject.FLDNM should be used for SDO's) If the fieldname isn't qualified it checks the tables in the TABLES property and assumes the first with a match.
        @param pcValues Corresponding Values (CHR(1) separated)
        @param pcOperators Operator one for all columns, blank - defaults to EQ, Use slash to define alternative string operator EQ/BEGINS etc., comma separated for each column/value
        @return Return value from ADM2 assignQuerySelection, always TRUE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL AssignQuerySelection (pcColumns   AS CHARACTER,
                                                pcValues    AS CHARACTER,
                                                pcOperators AS CHARACTER):

        IF NOT VALID-HANDLE (THIS-OBJECT:SmartDataObjectHandle) THEN
            RETURN FALSE .

        RETURN DYNAMIC-FUNCTION ("assignQuerySelection":U IN THIS-OBJECT:SmartDataObjectHandle,
                                 pcColumns,
                                 pcValues,
                                 pcOperators) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the CancelCreateRow event of the ProBindingSource
        Notes:   Comments from cancelRow in data.p:
                 cancelRow calls doUndoUpdate which restores the original values
                 of a modified row from the RowObjUpd record then deletes the
                 RowObjUdp record.  In the case of newMode, the new
                 RowObject is deleted and doUndoUpdate repositions the
                 RowObject temp-table to what was previously the current row.
        @param sender The reference to the System.Object that raised the event
        @param args The CAncelCreateRowEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID CancelCreateRowHandler (sender AS System.Object,
                                                           args AS CancelCreateRowEventArgs):

        DEFINE VARIABLE hSDO AS HANDLE NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        DYNAMIC-FUNCTION ("cancelRow":U IN hSDO) .

        FINALLY:
             THIS-OBJECT:AddingRecord = FALSE .
             THIS-OBJECT:AddRowIdent = "":U .
        END FINALLY.

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Empties the Resultset of this SmartDataAdapter. Used to clear grids
        Notes:   Empty the RowObject temp-table and reopens the query
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID CloseQuery(  ):

        DEFINE VARIABLE hSDO AS HANDLE NO-UNDO.
        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
        DEFINE VARIABLE hRowObject AS HANDLE NO-UNDO.

        ASSIGN
            hSDO       = THIS-OBJECT:SmartDataObjectHandle
            hRowObject = THIS-OBJECT:RowObject
            hQuery     = THIS-OBJECT:DataHandle .

        DYNAMIC-FUNCTION ("closeQuery":U IN hSDO) .

        /* Empty the RowObject temp-table and reopen the query*/
        hRowObject:EMPTY-TEMP-TABLE ().
        hQuery:QUERY-OPEN () .

        THIS-OBJECT:BindingSource:RefreshAll () .

        SUPER:CloseQuery () .

    END METHOD.


      /*------------------------------------------------------------------------------
        Purpose: Handler for the CreateRow event of the ProBindingSource
        Notes:   Comments from addRow in data.p:
                 Creates a new RowObject temp-table record, initializes it, and
                 returns CHR(1) delimited list of values for the requested
                 columns (in pcViewColList) of the new RowObject row.
                 The first entry in the list is the RowObect ROWID and db RowIds
                 separated with commas. (The db Rowids are blank as the record
                 has not been created)
        @param sender The reference to the System.Object that raised the event
        @param args The reference to the CreateRowEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID CreateRowHandler (sender AS System.Object,
                                                     args AS CreateRowEventArgs):
        DEFINE VARIABLE hSDO    AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cValues AS CHARACTER NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        cValues = DYNAMIC-FUNCTION ("addRow":U IN hSDO, "":U)  .

        args:Created = TRUE .

        THIS-OBJECT:AddingRecord = TRUE .
        THIS-OBJECT:AddRowIdent = ENTRY(1, cValues, CHR(1)) .

        /* Mike Fechner, Consultingwerk Ltd. 19.04.2016
           Ensure an updatable browser is in the correct state */
        THIS-OBJECT:AddRecordInUpdatableGrid () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deletes the current row.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID DeleteRow ():

        DEFINE VARIABLE hSDO      AS HANDLE  NO-UNDO.
        DEFINE VARIABLE lSuccess  AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iPosition AS INTEGER NO-UNDO.

        /* Remember postion */
        ASSIGN iPosition = THIS-OBJECT:BindingSource:Position .
               hSDO      = THIS-OBJECT:SmartDataObjectHandle .

        /* deleteRow in SDO needs RowObject ROWID, not RowIdent */
        lSuccess = DYNAMIC-FUNCTION ("deleteRow":U IN hSDO,
                                     STRING(THIS-OBJECT:RowObject:ROWID)).

        /* Throws an error containing the error message received from the SDO */
        IF NOT lSuccess THEN
            GetSmartDataObjectError () .

        /* Refresh and reposition */
        THIS-OBJECT:DataHandle:QUERY-OPEN () .

        THIS-OBJECT:BindingSource:RefreshAll () .

        IF THIS-OBJECT:BindingSource:Count > iPosition THEN
            THIS-OBJECT:BindingSource:Position = iPosition .
        ELSE IF THIS-OBJECT:BindingSource:Count > 1 THEN
            THIS-OBJECT:BindingSource:Position = THIS-OBJECT:BindingSource:Count - 1.
        ELSE IF THIS-OBJECT:BindingSource:Count = 1 THEN
            THIS-OBJECT:BindingSource:Position = 0.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DesignerOpenSmartDataObject(  ):

        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lOk       AS LOGICAL NO-UNDO.

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        ASSIGN cFileName = THIS-OBJECT:SmartDataObjectName
               lOk       = SmartDataObjectSupport:FileOpenSmartDataObject (INPUT-OUTPUT cFileName).

        IF NOT lOk THEN
            RETURN.

        IF cFileName > "":U THEN
            ASSIGN THIS-OBJECT:SmartDataObjectName = cFileName .

    END METHOD.

      /*------------------------------------------------------------------------------
        Purpose: Destroys the SmartDataObeject instance
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DestroySmartDataObject(  ):
        DEFINE VARIABLE hSDO AS HANDLE NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        IF VALID-HANDLE (hSDO) THEN DO:
            RUN destroyObject IN hSDO .

            IF VALID-HANDLE (hSDO) THEN
                APPLY "CLOSE" TO hSDO .

            IF VALID-HANDLE (hSDO) THEN
                DELETE PROCEDURE hSDO .
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: This Method moves to the last DataRow inside the BindingSource.
                 It is called by The Navigation Tools of the Toolbars.
                 Enforced by Interface ISmartNavigationTarget.
        Notes:   Local override that allows to fetch the last batch from the SDO
                 before moving the BindingSource
        @return Logical value, currently not used
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC LOGICAL FetchLast ():

        DEFINE VARIABLE hSDO        AS HANDLE NO-UNDO.
        DEFINE VARIABLE iLastRowNum AS INTEGER NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        {get LastRowNum iLastRowNum hSDO} .

        IF iLastRowNum = ? THEN DO:
            RUN fetchLast IN hSDO .
            THIS-OBJECT:BindingSource:Batching = FALSE .
        END.

         SUPER:FetchLast() .

        /* If we are not at the last row, this is probably because
           additional batches have been fetched */
        IF THIS-OBJECT:RecordPosition <> RecordPositionEnum:LastRecord THEN DO:
            SUPER:FetchLast().
        END.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Returns a CHR(1) delimited list of field values from the
                 DataSource
        Notes:   Uses SDO function colValues
        @param pcFields The comma delimited list of fields to return
        @return The CHR(1) delimited list of field values
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER GetFieldValues (INPUT pcFields AS CHARACTER ):

        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hSDO    AS HANDLE    NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        IF NOT VALID-HANDLE (hSDO) THEN
            RETURN ? .

        ASSIGN cReturn = DYNAMIC-FUNCTION ("colValues":U IN hSDO, pcFields) .

        RETURN SUBSTRING(cReturn, INDEX(cReturn, CHR(1)) + 1) .
    END METHOD.


     /*------------------------------------------------------------------------------
        Purpose: Evaluates the current record position property value
        Notes:   Translates QueryPosition from SDO to RecordPosition property
        @return The current record position as a CHARACTER representing an RecordPositionEnum value
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED CHARACTER GetRecordPosition ():

        DEFINE VARIABLE hSDO           AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cQueryPosition AS CHARACTER NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        IF NOT VALID-HANDLE(hSDO) THEN
            RETURN RecordPositionEnum:NoRecordAvailable .

        {get QueryPosition cQueryPosition hSDO} .

        CASE cQueryPosition:
            WHEN "FirstRecord":U THEN
                RETURN RecordPositionEnum:FirstRecord .
            WHEN "LastRecord":U THEN
                RETURN RecordPositionEnum:LastRecord .
            WHEN "NotFirstOrLast":U THEN
                RETURN RecordPositionEnum:NotFirstOrLast .
            WHEN "NoRecordAvailable" OR WHEN "NoRecordAvailableExt" THEN
                RETURN RecordPositionEnum:NoRecordAvailable .
            WHEN "OnlyRecord" THEN
                RETURN RecordPositionEnum:OnlyRecord .
            /* If RecordPosition from SDO unknown, build value based on
               BindingSource properties in SUPER class */
            OTHERWISE
                RETURN SUPER:GetRecordPosition () .
        END CASE.
    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Formats Error message returned from the SmartDataObject using
                 the SmartDataObject error class
                 Error messages are returned using the fetchMessages API
        Notes:   THORWS SmartDataObjectValidationError
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetSmartDataObjectError ():
        DEFINE VARIABLE cMessages   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cField      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFields     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTable      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTables     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iMsg        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iMsgCnt     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cSummary    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cText       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hSDO        AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cFirstField AS CHARACTER NO-UNDO.

        ASSIGN hSDO      = THIS-OBJECT:SmartDataObjectHandle
               cMessages = DYNAMIC-FUNCTION('fetchMessages':U IN hSDO).

        iMsgCnt = NUM-ENTRIES(cMessages, CHR(3)).

        msgCnt_blk:
        DO iMsg = 1 TO iMsgCnt:
        /* Format a string of messages; each has a first line of
           "Field:  <field>    "Table:  <table>"
           (if either of these is defined) plus the error message on a
            separate line. */
            ASSIGN cMessage = ENTRY(iMsg, cMessages, CHR(3))
                   cField = IF NUM-ENTRIES(cMessage, CHR(4)) > 1
                            THEN ENTRY(2, cMessage, CHR(4))
                            ELSE "":U
                   cTable = IF NUM-ENTRIES(cMessage, CHR(4)) > 2
                            THEN ENTRY(3, cMessage, CHR(4))
                            ELSE "":U
                   .
        cSummary = ENTRY(1,cMessage, CHR(4)).

        ASSIGN cText = cText
                       + (IF cField NE "":U
                          THEN DYNAMIC-FUNCTION('messageNumber':U IN hSDO, 10) + cField + "   ":U
                          ELSE "":U)
                       + (IF cTable NE "":U
                          THEN DYNAMIC-FUNCTION('messageNumber':U IN hSDO, 11) + cTable
                          ELSE "":U)
                       + (IF cField NE "":U OR cTable NE "":U THEN "~n":U + "  ":U ELSE "":U)
                       + cSummary + "~n":U.
        /* since we are displaying in a resizable dialog we can afford a blank line between fields */
        IF TRIM(cText) <> "":U THEN
            ASSIGN cText = cText + "~n":U.

        /* Return the field name from the first error message so the caller can
           use it to position the cursor. */
        IF iMsg = 1 THEN
            ASSIGN cFirstField = cField
                   cFields     = cField
                   cTables     = cTable .
        ELSE
            IF cField > "":U OR cTable > "":U THEN
                ASSIGN cFields = cFields + ",":U + cField
                       cTables = cTables + ",":U + cTables  .
      END.   /* END DO iMsg */

      UNDO, THROW NEW SmartDataObjectValidationError ( INPUT cText,
                                                       INPUT cFirstField,
                                                       INPUT cFields,
                                                       INPUT cTables) .
    END METHOD.


      /*------------------------------------------------------------------------------
        Purpose: Initializes the SmartDataObject by running initializeObject
        Notes:   PROTECTED method can be overridden in a child class, to add
                 customization
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeSmartDataObject(  ):

        IF NOT VALID-HANDLE (THIS-OBJECT:SmartDataObjectHandle) THEN
            LaunchSmartDataObject () .
        ELSE
            /* Needs to be at ELSE, because LaunchSmartDataObject calls into
               InitializeSmartDataObject as well and would cause double
               execution of initalizeObject */
            RUN initializeObject IN THIS-OBJECT:SmartDataObjectHandle .

    END METHOD.


      /*------------------------------------------------------------------------------
        Purpose: Launches the SmartDataObject associated with this Data Adapter
                 instance
        Notes:   Original code is in constructObject in adm2/containr.p
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LaunchSmartDataObject ():

        DEFINE VARIABLE cFileName    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iDB          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iDotPos      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cDotRFile    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cMemberFile  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cDBList      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcProcName   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hSDO         AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iRowsToBatch AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cPartition   AS CHARACTER NO-UNDO.

        IF VALID-HANDLE (THIS-OBJECT:SmartDataObjectHandle) THEN
            DestroySmartDataObject () .

        ASSIGN
            pcProcName  = THIS-OBJECT:SmartDataObjectName
            iDotPos     = R-INDEX(pcProcName, '.':U).

        ASSIGN /* Find the .r file */
            FILE-INFORMATION:FILE-NAME = SUBSTRING(pcProcName, 1, iDotPos) + "r":U
            cDotRFile           = FILE-INFORMATION:FULL-PATHNAME
            /* If .r in proc lib, get the member name (ie, filename). */
            cMemberFile         = MEMBER(cDotRFile).

        IF cMemberFile <> ? THEN
            cDotRFile = cMemberFile.

        IF cDotRFile NE ? THEN DO:  /* We have found the base .r */
            ASSIGN
                RCODE-INFORMATION:FILE-NAME = cDotRFile
                cDBList              = RCODE-INFORMATION:DB-REFERENCES.

            DB-CHECK: DO iDB = 1 TO NUM-ENTRIES(TRIM(cDBList)):   /* Remove blank when no db */
                IF NOT CONNECTED(ENTRY(iDB,cDBList)) THEN DO:
                    cDotRFile = ?.        /* Flag that we can't use the base file. */
                    LEAVE DB-CHECK.
                END.  /* Found a DB that needs to be connected that isn't */
            END.  /* Do for each required DB */
        END.  /* If we have the base .r */

        /* If the .r was not found or it was found and the databases it needs
           are not connected OR ClientOnly is a Run DO option then we look
           for the proxy */
        IF cDotRFile = ? THEN
        DO:
            /* Unable to locate the base .r file OR the required DBs are
               not connected - so try the _cl proxy instead. */
            ASSIGN
                pcProcName          = SUBSTRING(pcProcName, 1, iDotPos - 1) + "_cl.r":U
                FILE-INFORMATION:FILE-NAME = pcProcName
                cDotRFile           = FILE-INFORMATION:FULL-PATHNAME
                cMemberFile         = MEMBER(cDotRFile).
            IF cMemberFile <> ? THEN
                cDotRFile = cMemberFile.
        END.        /* END ELSE DO IF base .r not found. */

        RUN VALUE (pcProcName) PERSISTENT SET hSDO .

        ASSIGN THIS-OBJECT:SmartDataObjectHandle = hSDO
               iRowsToBatch                      = THIS-OBJECT:RowsToBatch
               cPartition                        = THIS-OBJECT:AppServerPartition .

        /* Set DataAdapter properties in SmartDataObject */
        {set OpenOnInit FALSE hSDO} .
        {set RebuildOnRepos FALSE hSDO} .
        {set RowsToBatch iRowsToBatch hSDO} .

        IF cPartition > "":U THEN
            {set AppService cPartition hSDO} .

        /* InitializeSmartDataObject, allows to be overridden */
        InitializeSmartDataObject () .

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Event handler for the OffEnd event of the ProBindingSource object
        Notes:   Overrides OffEndHandler in SmartDataAdapter base class
        @param sender The reference to the object that raised the event
        @param e The OffEndEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OffEndHandler (INPUT sender AS System.Object,
                                                  INPUT e AS Progress.Data.OffEndEventArgs):

        DEFINE VARIABLE hSDO              AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iLastRowNum       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cLastResultRow    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iLastRow          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iNewLastRow       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lWaitStateActive  AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cRowIdent         AS CHARACTER NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        {get LastRowNum iLastRowNum hSDO} .

        {get RowIdent cRowIdent hSDO} .

        IF iLastRowNum = ? THEN DO:

            {get LastResultRow cLastResultRow hSDO} .

            ASSIGN iLastRow = INTEGER(ENTRY(1, cLastResultRow, ";":U)) .

            /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
               Indicate WaitState */
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:SetWaitState ("RetrieveData":U, THIS-OBJECT) .

            ASSIGN lWaitStateActive = TRUE .

            /* Fetch next batch from SDO */
            RUN fetchBatch IN hSDO (TRUE) .

            /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
               Clear WaitState */
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .

            ASSIGN lWaitStateActive = FALSE .

            {get LastResultRow cLastResultRow hSDO} .

            ASSIGN iNewLastRow = INTEGER(ENTRY(1, cLastResultRow, ";":U)) .

            e:RowsAdded = iNewLastRow - iLastRow .

            /* Still batching ? */
            {get LastRowNum iLastRowNum hSDO} .

            /* Reposition SDO on previous row */
            DYNAMIC-FUNCTION ("fetchRowIdent":U IN hSDO, cRowIdent, "":U) .
        END.
        ELSE DO:
            /* Turn off batching, when SDO already had loaded
               the last result row. */
            THIS-OBJECT:BindingSource:Batching = FALSE .
        END.

        FINALLY:
            IF lWaitStateActive AND VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs
        Notes:   This method is intended to be overridden
        @param pcVerbText The text indicating the verb that was clicked
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID OnVerbClicked( INPUT pcVerbText AS CHARACTER ):

        SUPER:OnVerbClicked(INPUT pcVerbText).

        CASE pcVerbText:
            WHEN "Open SmartDataObject (SDO)":U THEN
                DesignerOpenSmartDataObject() .
        END CASE .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Opens the SmartDataObjects Query by calling RetrieveData
        Notes:
        @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL OpenQuery ():

        RETURN RetrieveData () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: This Method is invoked by the ParentPositionChangedBase Event Handler
                 of the SmartDataAdapters base class when the PositionChanged Event of
                 the SmartDataSources BindingSource is fired.
        Notes:   Refresh Data when a new line is selected inside the Parent
                 SmartDataSource.
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID ParentPositionChanged (sender AS System.Object,
                                                       e AS System.EventArgs)  :

        DEFINE VARIABLE hDataSource        AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cValue             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i                  AS INTEGER   NO-UNDO.

        ASSIGN hDataSource = oSmartDataSource:BindingSource:Handle .

        IF NOT VALID-HANDLE(hDataSource) THEN
            RETURN.

        IF hDataSource:GET-BUFFER-HANDLE(1):AVAILABLE THEN DO:
            IF NOT THIS-OBJECT:SmartDataObjectInitialized THEN
                THIS-OBJECT:InitializeSmartDataObject () .

            /* Request Foreign Field values,
               note ForeignFields stores the fields in the order
               parentfield1,childfield1,parentfield2,childfield2,.... */
            DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:ForeignFields) BY 2:
                ASSIGN cValue = oSmartDataSource:GetFieldValues (ENTRY(i, THIS-OBJECT:ForeignFields)) .

                   THIS-OBJECT:AssignQuerySelection (ENTRY(i + 1, THIS-OBJECT:ForeignFields), cValue, "EQ":U) .
            END.

            THIS-OBJECT:RetrieveData() .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handler for the PositionChanged event of the BindingSource attached
                 to this SmartDataAdapter instance.
        Notes:   The current RecordPosition is passed to the SmartNavigationSource.
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID PositionChangedHandler (sender AS System.Object,
                                                           e AS System.EventArgs):

        DEFINE VARIABLE hSDO AS HANDLE NO-UNDO.

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        RUN dataAvailable IN hSDO ("DIFFERENT":U) .

        /* Standard PositionChangedHandler in SUPER class */
        SUPER:PositionChangedHandler(sender, e).

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose:     Remove field expression(s) for specified column(s) and operator(s)
                    added by assignQuerySelection from the query.
       Notes:       This procedure modifies the QueryString property.
                    openQuery will prepare the query using this property.
                    The removal of the actual field expression is done by the help
                    of the position and length stored in the QueryColumns property.
       @param pcColumns Column names (Comma separated)
       @param pcOperators Operator - one for all columns, blank - defaults to (EQ), Use slash to define alternative string operator, EQ/BEGINS etc.., comma separated for each column/value
       @return Return value from ADM2 removeQuerySelection, always TRUE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RemoveQuerySelection (pcColumns   AS CHARACTER,
                                                pcOperators AS CHARACTER):

        IF NOT VALID-HANDLE (THIS-OBJECT:SmartDataObjectHandle) THEN
            RETURN FALSE .

        RETURN DYNAMIC-FUNCTION ("removeQuerySelection":U IN THIS-OBJECT:SmartDataObjectHandle,
                                 pcColumns,
                                 pcOperators) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Retrieves data from the SmartDataObject and binds the RowObject
                 temp-table to a ProBindingSource
        Notes:
        @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC LOGICAL RetrieveData ():

        DEFINE VARIABLE hSDO             AS HANDLE  NO-UNDO.
        DEFINE VARIABLE iLastRowNum      AS INTEGER NO-UNDO.
        DEFINE VARIABLE lWaitStateActive AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lPositionWasZero AS LOGICAL NO-UNDO INIT FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 21.07.2009
           Prevent loading of data/SDO in DesignTime */
        IF THIS-OBJECT:DesignTime THEN
            RETURN FALSE .

        IF THIS-OBJECT:BindingSource:Position = 0 THEN
            lPositionWasZero = TRUE .

        /* If no valid BindingSource exist a new one is created right here */
        IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource) THEN
            CreateBindingSource ().

        /* Launch and initialize SDO if required */
        IF NOT VALID-HANDLE (THIS-OBJECT:SmartDataObjectHandle)
            THEN LaunchSmartDataObject () .

        ASSIGN hSDO = THIS-OBJECT:SmartDataObjectHandle .

        IF NOT THIS-OBJECT:SmartDataObjectInitialized THEN
            InitializeSmartDataObject () .

        /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
           Indicate WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState ("RetrieveData":U, THIS-OBJECT) .

        ASSIGN lWaitStateActive = TRUE .

        /* Open the query and bind to it. */
        DYNAMIC-FUNCTION ("openQuery":U IN hSDO) .

        /* Mike Fechner, Consultingwerk Ltd. 13.06.2009
           Clear WaitState */
        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .

        ASSIGN lWaitStateActive = FALSE .

        THIS-OBJECT:BindingSource:Handle = THIS-OBJECT:DataHandle .

        /* Turn on or off batching */
        {get LastRowNum iLastRowNum hSDO} .

        IF iLastRowNum > 0 THEN
            THIS-OBJECT:BindingSource:Batching = FALSE .
        ELSE
            THIS-OBJECT:BindingSource:Batching = TRUE .

        /* Tell subscribers, that we got (new) data */
        OnAfterRetrieveData(System.EventArgs:Empty) .

        IF THIS-OBJECT:BindingSource:Position = 0 AND lPositionWasZero THEN
            THIS-OBJECT:OnCurrentChanged (System.EventArgs:Empty) .

        RETURN TRUE .

        FINALLY:
            IF lWaitStateActive AND VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("RetrieveData":U, THIS-OBJECT) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets Properties to customize Visual Designer behaviour
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        ASSIGN
           THIS-OBJECT:DesignerVerbs = TRIM (THIS-OBJECT:DesignerVerbs + ",Open SmartDataObject (SDO)":U, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: UpdateRow submits the changes to a buffer into the using the
                 SmartDataObject API
                 Called by a DataTarget when it's required to save data.

                 Enforced by Interface ISmartDataSource.
        Notes:   Do NOT call SUPER:UpdateRow() in the overriding method.

      Parameters required for submitRow in data.p:
        INPUT pcRowIdent  - "key" with row to update.
                             The RowObject ROWID, typically derrived from the visual
                             DataTarget's Rowident property, which often also
                             has an additional list of the ROWID(s) of the db
                             record(s). This appended list is not used here and is thus
                             not required.
                          - ?  indicates current record.
        INPUT pcValueList - CHR(1) delimited list of alternating column names
                            and values to be assigned.
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID UpdateRow ():

        DEFINE VARIABLE hSDO              AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hRowObject        AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cUpdatableColumns AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cChanges          AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i                 AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lResult           AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cField            AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField            AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lSame             AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cValue            AS CHARACTER NO-UNDO.

        ASSIGN hSDO       = THIS-OBJECT:SmartDataObjectHandle
               hRowObject = THIS-OBJECT:RowObject .

        {get UpdatableColumns cUpdatableColumns hSDO} .

        /* Mike Fechner, Consultingwerk Ltd. 03.06.2009
           During update, only send changed fields to SDO */
        IF THIS-OBJECT:AddingRecord = FALSE THEN DO:
            DO i = 1 TO NUM-ENTRIES(cUpdatableColumns):
                ASSIGN
                    cField = ENTRY(i, cUpdatableColumns)
                    hField = hRowObject:BUFFER-FIELD (cField) .

                /* Mike Fechner, Consultingwerk Ltd. 02.06.2009
                   DATE fields need special handling as the UNBOX function returns a DATETIME
                   value for System.DateTime fields.  */
                CASE hField:DATA-TYPE:
                    WHEN "DATE":U THEN
                        IF hField:BUFFER-VALUE <> DATE(UNBOX(THIS-OBJECT:BindingSource:InputValue [cField])) THEN
                            ASSIGN lSame = FALSE .
                        ELSE
                            ASSIGN lSame = TRUE .
                     OTHERWISE
                        IF hField:BUFFER-VALUE <> UNBOX(THIS-OBJECT:BindingSource:InputValue [cField]) THEN
                            ASSIGN lSame = FALSE .
                        ELSE
                            ASSIGN lSame = TRUE .
                END CASE.

                IF NOT lSame THEN DO:
                    ASSIGN cValue = STRING(UNBOX(THIS-OBJECT:BindingSource:InputValue [ENTRY(i, cUpdatableColumns)])) .

                    IF cValue = ? THEN
                        ASSIGN cValue = "?":U .

                    ASSIGN cChanges = cChanges +
                                      ENTRY(i, cUpdatableColumns) + CHR(1) +
                                      cValue + CHR(1) .
                END.
            END.

            ASSIGN cChanges = RIGHT-TRIM (cChanges, CHR(1)).
        END.
        /* Mike Fechner, Consultingwerk Ltd. 03.06.2009
           When adding a new row, send all fields to SDO */
        ELSE DO:
            DO i = 1 TO NUM-ENTRIES(cUpdatableColumns):
                ASSIGN
                    cField = ENTRY(i, cUpdatableColumns) .

                    ASSIGN cValue = STRING(UNBOX(THIS-OBJECT:BindingSource:InputValue [ENTRY(i, cUpdatableColumns)])) .

                    IF cValue = ? THEN
                        ASSIGN cValue = "?":U .

                    ASSIGN cChanges = cChanges +
                                      ENTRY(i, cUpdatableColumns) + CHR(1) +
                                      cValue + CHR(1) .
            END.

            ASSIGN cChanges = RIGHT-TRIM (cChanges, CHR(1)).
        END.

        IF cChanges > "":U THEN
            IF THIS-OBJECT:AddingRecord THEN
                ASSIGN lResult = DYNAMIC-FUNCTION ("submitRow":U IN hSDO,
                                                   THIS-OBJECT:AddRowIdent,
                                                   cChanges) .
            ELSE
                ASSIGN lResult = DYNAMIC-FUNCTION ("submitRow":U IN hSDO,
                                                   ?, /* Update current row as currently availalbe in the SDO */
                                                   cChanges) .

        /* Throws an error containing the error message received from the SDO */
        IF NOT lResult THEN DO:
            GetSmartDataObjectError () .

            UNDO, THROW NEW AppError ("An error has occurred when saving to the SDO.", 0) .
        END.

        /* Should only be done in case of success, so that wrong values remain
           in the viewer and the user can correct them. */
        THIS-OBJECT:BindingSource:Refresh () .

        THIS-OBJECT:AddingRecord = FALSE .
        THIS-OBJECT:AddRowIdent = "":U .

        CATCH e AS Progress.Lang.SysError:
            ErrorHelper:ShowErrorMessage (e, SmartComponentLibraryCustomizer:SmartDataObjectAdapter_UpdateRowSysError) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destroys the SmartDataObject instance by calling
                 DestroySmartDataObject
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartDataObjectAdapter ( ):

        DestroySmartDataObject () .

    END DESTRUCTOR.


END CLASS.