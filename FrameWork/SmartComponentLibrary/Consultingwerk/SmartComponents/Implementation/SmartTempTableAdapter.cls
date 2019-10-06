/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartTempTableAdapter
    Purpose     : A SmartDataAdapter implementation that can be used to
                  navigate and update any temp-table using the
                  SmartComponent Library classes
    Syntax      :
    Description : The SmartTempTableAdapter does not connect to any backend
                  by itself, if can be used to work with Temp-Tables that
                  are already available on the client.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 11 20:00:27 CEST 2010
    Notes       :
    Usage       : 1.) Create an Instance of the SmartTempTableAdapter
                      (this can be done using the VisualDesigner)
                  2.) Assign the handle of the temp-table to the
                      TempTableHandle property
                  3.) Call RetrieveData
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.Exceptions.*                     FROM PROPATH .
USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.*      FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Data.*                                 FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter
    INHERITS SmartDataAdapter
    IMPLEMENTS ISortableDataSource,
               ISupportsReposition,
               ISmartFilterTarget
    USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE hQuery                AS HANDLE    NO-UNDO .
    DEFINE PRIVATE VARIABLE lFirstTime            AS LOGICAL   NO-UNDO INIT TRUE .
    DEFINE PRIVATE VARIABLE roCreateRowid         AS ROWID     NO-UNDO .
    DEFINE PRIVATE VARIABLE iPositionBeforeCreate AS INTEGER   NO-UNDO .
    DEFINE PRIVATE VARIABLE roCurrentRowids       AS ROWID     NO-UNDO EXTENT .
    DEFINE PRIVATE VARIABLE cQuerySort            AS CHARACTER NO-UNDO .
    DEFINE PRIVATE VARIABLE cQueryString          AS CHARACTER NO-UNDO .

    DEFINE PRIVATE VARIABLE lSaving               AS LOGICAL   NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Event raised after a record has been assigned
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterAssignRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterAssignRecord SIGNATURE VOID (sender AS System.Object,
                                                          e AS AfterAssignRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired after the creation of a record has been cancelled
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterCancelCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterCancelCreateRecord SIGNATURE VOID (sender AS System.Object,
                                                                e AS AfterCancelCreateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired after a record has been created
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterCreateRecord SIGNATURE VOID (sender AS System.Object,
                                                          e AS AfterCreateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised after a record has been deleted
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterDeleteRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterDeleteRecord SIGNATURE VOID (sender AS System.Object,
                                                          e AS AfterDeleteRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised after a record has been updated
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterUpdateRecord SIGNATURE VOID (sender AS System.Object,
                                                          e AS AfterUpdateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised before assigning to a record
        Notes:
        @param sender The reference to the object that raised the event
        @param e The BeforeAssignRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeAssignRecord SIGNATURE VOID (sender AS System.Object,
                                                           e AS BeforeAssignRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised before the creation of the record will be cancelled
        Notes:
        @param sender The reference to the object that raised the event
        @param e The BeforeCancelCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeCancelCreateRecord SIGNATURE VOID (sender AS System.Object,
                                                                 e AS BeforeCancelCreateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired before a record will be created
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeCreateRecord SIGNATURE VOID (sender AS System.Object,
                                                           e AS BeforeCreateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised before a record will be deleted
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterDeleteRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeDeleteRecord SIGNATURE VOID (sender AS System.Object,
                                                           e AS BeforeDeleteRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised before the a record is updated
        Notes:
        @param sender The reference to the object that raised the event
        @param e The BeforeUpdateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeUpdateRecord SIGNATURE VOID (sender AS System.Object,
                                                           e AS BeforeUpdateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised to collect Filter Values from Filter Sources
        Notes:   Required by IFilterTarget
        @param sender The reference to the object that raised the event
        @param e The CollectFilterValuesEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT CollectFilterValues SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                            e AS CollectFilterValuesEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the TempTable buffer used for navigating
                 the temp-table
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferHandle AS HANDLE NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the SmartTempTableAdapter is going create a custom
                 Buffer for navigating the Temp-Table
        Notes:   When set to FALSE the SmartTempTableAdapter will navigate the
                 DEFAULT-BUFFER-HANDLE of the Temp table
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CreateNewBuffer AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of Query Expressions by Table of the previous
                 RetrieveData
        Notes:   Required for batching capabilities
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FetchFilterValues AS ListQueryExpressionByTable NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Indicates if a DataAdapters Query is being opened for the First time
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY FirstTime AS LOGICAL NO-UNDO
    GET:
        RETURN lFirstTime .
    END GET .
    SET (arg AS LOGICAL):
        ASSIGN lFirstTime = arg .
    END SET .

      /*------------------------------------------------------------------------------
        Purpose: Returns the handle to the Query used to navigate the temp table
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QueryHandle AS HANDLE NO-UNDO
    GET:
        RETURN hQuery .
    END GET.
    PROTECTED SET (arg AS HANDLE):
        ASSIGN hQuery = arg .
    END SET.

      /*------------------------------------------------------------------------------
        Purpose: Controls the sort-order used at the client
        Notes:   Right now QuerySort is only supported when batching is
                 turned off!
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QuerySort AS CHARACTER NO-UNDO
    GET():
        RETURN cQuerySort .
    END GET.
    SET (INPUT arg AS CHARACTER):
        SetQuerySort (arg) .
    END SET.

      /*------------------------------------------------------------------------------
        Purpose: This is a base query string.
        Notes:
    ------------------------------------------------------------------------------*/
     DEFINE PUBLIC PROPERTY QueryString AS CHARACTER NO-UNDO
    GET:
        RETURN cQueryString .
    END.
    SET (arg AS CHARACTER):
        ASSIGN cQueryString = arg .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Get's and sets the handle to the temp-table used by this adapter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TempTableHandle AS HANDLE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the schema to the BindingSource
        Notes:
        @param poBindingSource The BindingSource to attach the schema to
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachSchemaToBindingSource (poBindingSource AS Progress.Data.BindingSource):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        IF THIS-OBJECT:CreateNewBuffer = TRUE THEN
            CREATE BUFFER hBuffer FOR TABLE THIS-OBJECT:TempTableHandle .
        ELSE
            ASSIGN hBuffer = THIS-OBJECT:TempTableHandle:DEFAULT-BUFFER-HANDLE .

        ASSIGN THIS-OBJECT:BufferHandle = hBuffer .

        CREATE QUERY hQuery .
        hQuery:ADD-BUFFER (hBuffer) .

        IF cQueryString = "":U OR cQueryString = ? THEN
            cQueryString = "PRESELECT EACH ":U + hBuffer:NAME .

        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:QueryHandle:QUERY-PREPARE (cQueryString)  .

            CATCH querypreperr AS Progress.Lang.Error:
                UNDO, THROW Exception:FromErrorMessageAndError (SUBSTITUTE ("Error opening Query: &1"{&TRAN}, cQueryString),
                                                                0,
                                                                querypreperr) .
            END CATCH.
        END.

        IF NOT hQuery:IS-OPEN THEN hQuery:QUERY-OPEN () .

        ASSIGN poBindingSource:AutoSync = TRUE
               poBindingSource:Handle   = hQuery .

        poBindingSource:HANDLE = hQuery .

        FINALLY:
            /* BindingSource is already prepared */
            ASSIGN lFirstTime = FALSE .
        END FINALLY.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Invoked from the SmartDataTarget to cancel the creation of a new
                 row
        Notes:   Creation of rows and cancellation of rows in the
                 SmartTempTableAdapter now no longer handled using the AddNew()
                 method of the BindingSource. We are now creating rows and removing
                 them on cancel. (Bug 1836)
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID CancelCreateRecord ():

        DEFINE VARIABLE lBatching AS LOGICAL                           NO-UNDO.
        DEFINE VARIABLE hBuffer   AS HANDLE                            NO-UNDO.
        DEFINE VARIABLE eBefore   AS BeforeCancelCreateRecordEventArgs NO-UNDO .

        ASSIGN
            lBatching                          = THIS-OBJECT:BindingSource:Batching
            THIS-OBJECT:BindingSource:Batching = FALSE
            hBuffer                            = THIS-OBJECT:BufferHandle .

        hBuffer:FIND-BY-ROWID (roCreateRowid) .

        eBefore = NEW BeforeCancelCreateRecordEventArgs (?,
                                                         ?,
                                                         hBuffer,
                                                         THIS-OBJECT:QueryHandle) .

        OnBeforeCancelCreateRecord (eBefore) .

        IF eBefore:Cancel THEN DO:
            IF eBefore:CancelMessage > "":U THEN
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException (eBefore:CancelMessage, 0) .
            ELSE
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException () .
        END.

        IF hBuffer:AVAILABLE THEN
            hBuffer:BUFFER-DELETE () .

        hQuery:DELETE-RESULT-LIST-ENTRY () .

        hQuery:QUERY-OPEN () .

        THIS-OBJECT:BindingSource:RefreshAll() .

        /* 0 based binding source position */
        IF iPositionBeforeCreate > 0 THEN DO:
            ASSIGN THIS-OBJECT:BindingSource:Position = iPositionBeforeCreate .

            OnAfterQueryPositionChanged (System.EventArgs:Empty) .
        END.

        OnAfterCancelCreateRecord (NEW AfterCancelCreateRecordEventArgs (?,
                                                                         ?,
                                                                         hBuffer,
                                                                         THIS-OBJECT:QueryHandle)) .

        FINALLY:
            ASSIGN
                THIS-OBJECT:BindingSource:Batching = lBatching
                roCreateRowid                      = ?
                iPositionBeforeCreate              = ? .

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Empty the DATASET inside the DataAdapter and reopen Query.
        Notes:   SUPER:CloseQuery does not have to be invoked because it is an
                 abstract METHOD.
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID CloseQuery ():

        IF VALID-HANDLE (hQuery) THEN DO:
            hQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH &1 WHERE ROWID (&1) = TO-ROWID ('0x0')", THIS-OBJECT:BufferHandle:NAME)) .
            hQuery:QUERY-OPEN () .

            hQuery:QUERY-PREPARE (SUBSTITUTE ("PRESELECT EACH &1", THIS-OBJECT:BufferHandle:NAME)) .
        END.

        SUPER:CloseQuery () .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Creates a new record
        Notes:   Creation of rows and cancellation of rows in the
                 SmartTempTableAdapter now no longer handled using the AddNew()
                 method of the BindingSource. We are now creating rows and removing
                 them on cancel. (Bug 1836)

                 The OnBeforeCreateRecord event handle are NOT allowed to contain
                 MESSAGE statements. This will cause the hBuffer of to no longer
                 point to the right record!
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID CreateRecord ():

        DEFINE VARIABLE hBuffer       AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE hCreateBuffer AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE i             AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE cField        AS CHARACTER                   NO-UNDO.
        DEFINE VARIABLE hField        AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE iPosition     AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE iIndex        AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE cIndex        AS CHARACTER                   NO-UNDO.

        DEFINE VARIABLE eBefore       AS BeforeCreateRecordEventArgs NO-UNDO .

        /* If no valid BindingSource exist a new one is created right here */
        IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource) THEN
            CreateBindingSource ().

        /* Keep current position after cancel */
        ASSIGN iPositionBeforeCreate = THIS-OBJECT:BindingSource:Position .

        QueryHelper:GetCurrentRowids (hQuery, OUTPUT roCurrentRowids) .

        /* Create a new row in the EntityTalbe */
        hBuffer = THIS-OBJECT:BufferHandle .

        /* use an alternative buffer for creating */
        CREATE BUFFER hCreateBuffer FOR TABLE hBuffer .

        hCreateBuffer:BUFFER-CREATE() .

        /* Mike Fechner, Consultingwerk Ltd. 17.01.2010
           BeforeCreateRecord event */
        eBefore = NEW BeforeCreateRecordEventArgs (?,
                                                   ?,
                                                   hCreateBuffer,
                                                   THIS-OBJECT:QueryHandle) .

        /* Mike Fechner, Consultingwerk Ltd. 19.01.2010
           Note, that the OnBeforeCreateRecord event handle are NOT allowed
           to contain MESSAGE statements. This will cause the hBuffer of
           to no longer point to the right record! */

        OnBeforeCreateRecord (eBefore) .

        IF eBefore:Cancel THEN DO:
            IF hCreateBuffer:AVAILABLE THEN
                hCreateBuffer:BUFFER-DELETE () .

            IF eBefore:CancelMessage > "":U THEN
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException (eBefore:CancelMessage, 0) .
            ELSE
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException () .
        END.

        IF eBefore:SkipUniqueRecordValidation = TRUE THEN .
        ELSE DO:
            /* Mike Fechner, Consultingwerk Ltd. 30.09.2009
               Test if record can be created using Initial Values / Unique index conflicts */
            IF NOT BufferHelper:CanCreateDefaultRow (hCreateBuffer) THEN DO:
                indexloop: REPEAT ON ERROR UNDO, THROW:
                    ASSIGN iIndex = iIndex + 1
                           cIndex = hCreateBuffer:INDEX-INFORMATION (iIndex) .

                    IF cIndex > "":U THEN .
                    ELSE LEAVE indexloop.

                    IF ENTRY(2, cIndex) <> "1":U /* unique */ THEN
                        NEXT indexloop .

                    DO i = 5 TO NUM-ENTRIES (cIndex) BY 2:
                        ASSIGN cField = ENTRY(i, cIndex)
                               hField = hCreateBuffer:BUFFER-FIELD (cField) .

                        /* Mike Fechner, Consultingwerk Ltd. 30.09.2009
                           Test for mandatory index fields. */
                        IF hField:MANDATORY THEN
                            UNDO, THROW NEW AppError ("Unable to create a new row. The initial value of some index fields conflicts with unique keys and some fields are defined mandatory in the entity temp-table.", 0) .

                        ELSE hField:BUFFER-VALUE = ? .
                    END.
                END.
            END.
        END.

        ASSIGN roCreateRowid = hCreateBuffer:ROWID .

        hCreateBuffer:BUFFER-RELEASE () .

        /* Mike Fechner, Consultingwerk Ltd. 20.01.2010
           Add new record to query using default buffer */
        hBuffer:FIND-BY-ROWID (roCreateRowid) .
        hQuery:CREATE-RESULT-LIST-ENTRY () .

        ASSIGN iPosition = hQuery:CURRENT-RESULT-ROW .

        THIS-OBJECT:BindingSource:RefreshAll () .

        /* 1 based query current-result-row */
        IF iPosition > 0 THEN
            THIS-OBJECT:BindingSource:Position = iPosition - 1.

        /* Mike Fechner, Consultingwerk Ltd. 01.01.2010
           After creating a new record, update the roCurrentRowids.
           This is required so that a new record is actually available after
           adding it. */
        QueryHelper:GetCurrentRowids (hQuery, OUTPUT roCurrentRowids) .

        OnAfterQueryPositionChanged (System.EventArgs:Empty) .

        OnAfterCreateRecord (NEW AfterCreateRecordEventArgs (?,
                                                             ?,
                                                             hBuffer,
                                                             THIS-OBJECT:QueryHandle)) .

        CATCH e AS Progress.Lang.Error :
            IF VALID-HANDLE (hCreateBuffer) AND hCreateBuffer:AVAILABLE
                THEN hCreateBuffer:BUFFER-DELETE () .

            UNDO, THROW e.
        END CATCH.

        FINALLY:
            IF VALID-OBJECT (eBefore) THEN
                DELETE OBJECT eBefore .

            IF VALID-HANDLE (hCreateBuffer) THEN
                DELETE OBJECT hCreateBuffer .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deletes the current record from the EntityTable
        Notes:   Method enforced by ISmartDataSource
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID DeleteRow ():

        DEFINE VARIABLE hBuffer               AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE rRowids               AS ROWID                       NO-UNDO EXTENT .
        DEFINE VARIABLE iPositionBeforeDelete AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE eBefore               AS BeforeDeleteRecordEventArgs NO-UNDO .

        ASSIGN iPositionBeforeDelete = THIS-OBJECT:BindingSource:Position.

        ASSIGN hBuffer = hQuery:GET-BUFFER-HANDLE (1) .

        IF NOT hBuffer:AVAILABLE THEN
            UNDO, THROW NEW AppError (SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_DeleteNoRecordError, 0).

        QueryHelper:GetCurrentRowids (hQuery, OUTPUT rRowids) .

        /* Mike Fechner, Consultingwerk Ltd. 17.01.2010
           BeforeDeleteRecord event */
        eBefore = NEW BeforeDeleteRecordEventArgs (?,
                                                   ?,
                                                   hBuffer,
                                                   THIS-OBJECT:QueryHandle) .

        OnBeforeDeleteRecord (eBefore) .

        IF eBefore:Cancel THEN DO:
            IF eBefore:CancelMessage > "":U THEN
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException (eBefore:CancelMessage, 0) .
            ELSE
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException () .
        END.

        hBuffer:BUFFER-DELETE () .

        hQuery:QUERY-OPEN () .
        THIS-OBJECT:BindingSource:RefreshAll() .

        IF iPositionBeforeDelete < THIS-OBJECT:BindingSource:Count THEN DO:
            THIS-OBJECT:BindingSource:Position = iPositionBeforeDelete .

            OnAfterQueryPositionChanged (System.EventArgs:Empty) .
        END.
        ELSE DO:
            THIS-OBJECT:BindingSource:Position = THIS-OBJECT:BindingSource:Count - 1 .

            OnAfterQueryPositionChanged (System.EventArgs:Empty) .
        END.

        OnAfterDeleteRecord (NEW AfterDeleteRecordEventArgs (?,
                                                             ?,
                                                             hBuffer,
                                                             THIS-OBJECT:QueryHandle)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Current Rowids
        Notes:   Neeeds to be a Method instead of a Property Get to return an
                 undimensioned Array
        @return The array of the current query ROWID's
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ROWID EXTENT GetCurrentRowid ():

        RETURN roCurrentRowids.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CHR(1) delimited list of field values from the
                 DataSource
        Notes:   Override to GetFieldValues method. Make sure the Query is properly
                 positioned before calling SUPER method
        @param pcFields The comma delimited list of fields to return
        @return The CHR(1) delimited list of field values
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER GetFieldValues (pcFields AS CHARACTER):

        RepositionCurrentRow () .

        RETURN SUPER:GetFieldValues (pcFields).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Builds an empty ListQueryExpressionByTable instance and raises the
                 CollectFilterValues event
        Notes:   Will return ? when the event was cancelled by a subscriber
        @return The ListQueryExpressionByTable with the filters per table
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ListQueryExpressionByTable GetNewFilterValues ():

        DEFINE VARIABLE e            AS CollectFilterValuesEventArgs NO-UNDO .
        DEFINE VARIABLE oList        AS ListQueryExpressionByTable   NO-UNDO .

        oList = NEW ListQueryExpressionByTable () .

        oList:Add (THIS-OBJECT:TempTableHandle:NAME, NEW ListQueryExpression ()) .

        e = NEW CollectFilterValuesEventArgs (oList) .

        OnCollectFilterValues (e) .

        IF e:Cancel THEN
            RETURN ? .

        RETURN oList .

        FINALLY:
            IF VALID-OBJECT (e) THEN
                DELETE OBJECT e.

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Merges two Query Strings
        Notes:
        @param pcFetchQueryString The first QueryString
        @param poQueryExpressions The List of QueryExpressions by Table to be merged with the first QueryString
        @return The resulting Query String
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER MergeQueryStrings (pcFetchQueryString AS CHARACTER,
                                                  poQueryExpressions AS ListQueryExpressionByTable):

        DEFINE VARIABLE cBaseQuery       AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cTable           AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cReturn          AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oQueryExpression AS ListQueryExpression NO-UNDO .


        ASSIGN cTable           = THIS-OBJECT:TempTableHandle:NAME
               oQueryExpression = poQueryExpressions:GetItem (cTable) .

        IF oQueryExpression:IsEmpty THEN
            ASSIGN cReturn = ENTRY (1, pcFetchQueryString, CHR(1)) .
        ELSE DO:
            ASSIGN cBaseQuery = ENTRY (1, pcFetchQueryString, CHR(1)) .

            IF cBaseQuery = "":U THEN
                ASSIGN cBaseQuery = SUBSTITUTE ("FOR EACH &1":U, cTable) .

            cBaseQuery = QueryHelper:InsertExpression (cBaseQuery,
                                                       oQueryExpression:GetExpression(),
                                                       "AND":U) .

            ASSIGN cReturn = cBaseQuery .

        END.

        RETURN cReturn .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterAssignRecord event
        Notes:
        @param e The AfterAssignRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterAssignRecord (e AS AfterAssignRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event AfterAssignRecord requires valid event argument!", 0) .

        THIS-OBJECT:AfterAssignRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterCancelCreateRecord event
        Notes:
        @param e The AfterCancelCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterCancelCreateRecord (e AS AfterCancelCreateRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event AfterCancelCreateRecord requires valid event argument!", 0) .

        THIS-OBJECT:AfterCancelCreateRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterCreateRecord event
        Notes:
        @param e The AfterCreateRecordEventArgs with the data for thie event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterCreateRecord (e AS AfterCreateRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event AfterCreateRecord requires valid event argument!", 0) .

        THIS-OBJECT:AfterCreateRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterDeleteRecord event
        Notes:
        @param e The AfterDeleteRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterDeleteRecord (e AS AfterDeleteRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event AfterDeleteRecord requires valid event argument!", 0) .

        THIS-OBJECT:AfterDeleteRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterUpdateRecord event
        Notes:
        @param e The AfterUpdateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterUpdateRecord (e AS AfterUpdateRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event AfterUpdateRecord requires valid event argument!", 0) .

        THIS-OBJECT:AfterUpdateRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raise the BeforeAssignRecord event
        Notes:
        @param e The BeforeAssignRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeAssignRecord (e AS BeforeAssignRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event BeforeAssignRecord requires valid event argument!", 0) .

        THIS-OBJECT:BeforeAssignRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeCancelCreateRecord event
        Notes:
        @param e The BeforeCancelCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeCancelCreateRecord (e AS BeforeCancelCreateRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event BeforeCancelCreateRecord requires valid event argument!", 0) .

        THIS-OBJECT:BeforeCancelCreateRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeCreateRecord event
        Notes:
        @param e The BeforeCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeCreateRecord (e AS BeforeCreateRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event BeforeCreateRecord requires valid event argument!", 0) .

        THIS-OBJECT:BeforeCreateRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeDeleteRecord event
        Notes:
        @param e The BeforeDeleteRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeDeleteRecord (e AS BeforeDeleteRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event BeforeDeleteRecord requires valid event argument!", 0) .

        THIS-OBJECT:BeforeDeleteRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeUpdateRecord event
        Notes:
        @param e The BeforeUpdateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeUpdateRecord (e AS BeforeUpdateRecordEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Event BeforeUpdateRecord requires valid event argument!", 0) .

        THIS-OBJECT:BeforeUpdateRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the CollectFilterValues event
        Notes:
        @param e The CollectFilterValuesEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnCollectFilterValues (e AS CollectFilterValuesEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Unable to raise CollectFilterValues event without event argument object."{&TRAN}, 0) .

        THIS-OBJECT:CollectFilterValues:Publish (THIS-OBJECT, e) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Eventhandler for the PositionChanged event of the BindingSource
                 object
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID PositionChangedHandler (sender AS System.Object,
                                                           e AS System.EventArgs):

        SUPER:PositionChangedHandler (sender, e) .

        IF VALID-HANDLE (hQuery) AND NOT hQuery:QUERY-OFF-END THEN
            QueryHelper:GetCurrentRowids (hQuery, OUTPUT roCurrentRowids) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Repositions the Query to the current row
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RepositionCurrentRow ():

        DEFINE VARIABLE oError AS Progress.Lang.AppError NO-UNDO .

        IF NOT VALID-HANDLE (hQuery) THEN DO:
            oError = NEW Consultingwerk.Exceptions.InvalidHandleException ("Query":U) .
            oError:AddMessage ("Unable to reposition to current row before the Query has been created."{&TRAN}, 0) .
            UNDO, THROW oError .
        END .

        IF NOT hQuery:GET-BUFFER-HANDLE(1):AVAILABLE THEN DO:
            QueryHelper:RepositionToRowidArray (hQuery, roCurrentRowids) .
            hQuery:GET-NEXT () .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Repositions to a Rowid Array
        Notes:
        @param prRowid The array of ROWID's to reposition the query to
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID RepositionToRowid (INPUT prRowid AS ROWID EXTENT):

        SUPER:RepositionToRowid(INPUT prRowid).

        Consultingwerk.Util.QueryHelper:GetCurrentRowids (hQuery, OUTPUT roCurrentRowids) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Overridable Setter for QuerySort property
        Notes:
        @param arg The new value for the QuerySort property
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetQuerySort (arg AS CHARACTER):

        IF arg > "":U THEN .
        ELSE RETURN .

        ASSIGN cQuerySort = arg .

        IF NOT VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN
            RETURN .

        IF NOT THIS-OBJECT:Initializing THEN
            THIS-OBJECT:RetrieveData () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Assigns if the SmartTempTableAdapter is currently saving data
        Notes:
        @param plSaving Logical value indicating if the SmartTempTableAdapter is currently saving data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetSavingData (plSaving AS LOGICAL):

        lSaving = plSaving .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTempTableAdapter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTempTableAdapter ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTempTableAdapter class
        Notes:
        @param poContainer An System.Componentmodel.IContainer that represents the container for the component.
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTempTableAdapter (poContainer AS System.ComponentModel.IContainer):

        SUPER (INPUT poContainer).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Handler for the CancelCreateRow event of the ProBindingSource.
                 A Row that has been created usind CreateRowHandler gets reverted.
        Notes:
        @param sender The reference to the object that raised the event
        @param args The CancelCreateRowEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID CancelCreateRowHandler (sender AS System.Object,
                                                           args AS CancelCreateRowEventArgs):

        DEFINE VARIABLE hBuffer AS HANDLE                            NO-UNDO.
        DEFINE VARIABLE eBefore AS BeforeCancelCreateRecordEventArgs NO-UNDO .


        /* Remove the new row from the EntityTable */
        hBuffer = THIS-OBJECT:BufferHandle .

        eBefore = NEW BeforeCancelCreateRecordEventArgs (?,
                                                         ?,
                                                         hBuffer,
                                                         THIS-OBJECT:QueryHandle) .

        OnBeforeCancelCreateRecord (eBefore) .

        IF eBefore:Cancel THEN DO:
            IF eBefore:CancelMessage > "":U THEN
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException (eBefore:CancelMessage, 0) .
            ELSE
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException () .
        END.

        hQuery:DELETE-RESULT-LIST-ENTRY () .

        hBuffer:BUFFER-DELETE () .

        OnAfterCancelCreateRecord (NEW AfterCancelCreateRecordEventArgs (?,
                                                                         ?,
                                                                         hBuffer,
                                                                         THIS-OBJECT:QueryHandle)) .

        FINALLY:
            ASSIGN
                roCreateRowid         = ?
                iPositionBeforeCreate = ?.
        END FINALLY.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Handler for the CreateRow event of the ProBindingSource.
                 A new Row gets created.
        Notes:
        @param sender The reference to the object thta raised the event
        @param args The CreateRowEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID CreateRowHandler (sender AS System.Object,
                                                     args AS CreateRowEventArgs):

        DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO.

        /* Create a new row in the EntityTalbe */
        hBuffer = THIS-OBJECT:BufferHandle .

        hBuffer:TABLE-HANDLE:TRACKING-CHANGES = TRUE .
        hBuffer:BUFFER-CREATE() .

        ASSIGN roCreateRowid = hBuffer:ROWID .

        hQuery:CREATE-RESULT-LIST-ENTRY() .

        args:Created = TRUE .

        /* Mike Fechner, Consultingwerk Ltd. 19.04.2016
           Ensure an updatable browser is in the correct state */
        THIS-OBJECT:AddRecordInUpdatableGrid () .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: RetrieveData reads Data from a Datasource and links the DataHandle
                 with a BindingSource inside the Instance of
                 SmartTempTableAdapter.
        Notes:   If there is no valid instance of a BindigSource a new one is created.
                 After that a Dataset is cleared and then filled with the requested
                 data from the Database. Now a Query is build or reopened and the
                 Handle to this Query is assigned to the BindingSource:Handle.
                 Enforced by Interface ISmartDataSource
                 Remember to test if a BindingSource exists. If not call
                 CreateBindingSource from the base class.
        @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE LOGICAL RetrieveData ():

        DEFINE VARIABLE lPositionWasZero      AS LOGICAL                                   NO-UNDO INIT FALSE .
        DEFINE VARIABLE lBindingSourceCreated AS LOGICAL                                   NO-UNDO INIT FALSE .
        DEFINE VARIABLE oListQueryExpressions AS Consultingwerk.ListQueryExpressionByTable NO-UNDO .
        DEFINE VARIABLE cQueryString          AS CHARACTER                                 NO-UNDO .

        THIS-OBJECT:PreviousPosition = THIS-OBJECT:BindingSource:Position NO-ERROR .

        IF THIS-OBJECT:DesignTime THEN
            RETURN FALSE .

        /* If no valid BindingSource exist a new one is created right here */
        IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource) THEN DO:
            CreateBindingSource ().
            ASSIGN lBindingSourceCreated = TRUE .
        END.

        IF THIS-OBJECT:BindingSource:Position = 0 THEN
            lPositionWasZero = TRUE .

        THIS-OBJECT:BindingSource:Batching = FALSE .

        IF lFirstTime THEN DO:
            THIS-OBJECT:AttachSchemaToBindingSource (THIS-OBJECT:BindingSource) .

            /* Mike Fechner, Consultingwerk Ltd. 18.01.2013
               Bug 2845 setting FirstTime to FALSE does no longer happen in the FINALLY block
               so that in case of an error finally is not reset */
            THIS-OBJECT:FirstTime = FALSE .
        END.

        IF THIS-OBJECT:QuerySort > "":U THEN DO:
            ASSIGN cQueryString = THIS-OBJECT:QueryHandle:PREPARE-STRING .

            /* Mike Fechner, Consultingwerk Ltd. 02.02.2011
               Bug 2582: This manipulation of the QueryString is not 100% rock solid,
               but searching for _BY_ provides less false matches then just searching
               for BY. Ultimatively we need to parse the QueryString while respecting
               single and double quotes. */
            IF INDEX (cQueryString, " BY ":U) > 0 THEN
                cQueryString = SUBSTRING (cQueryString, 1, INDEX (cQueryString, " BY ":U) - 1) .

            /* Mike Fechner, Consultingwerk Ltd. 14.05.2015
               SCL-727 - Support for FilterRow etc. on SmartTempTableAdapter */
            ASSIGN oListQueryExpressions         = GetNewFilterValues ()
                   THIS-OBJECT:FetchFilterValues = oListQueryExpressions .

            IF VALID-OBJECT (oListQueryExpressions) AND NOT oListQueryExpressions:IsEmpty THEN
                ASSIGN cQueryString = THIS-OBJECT:MergeQueryStrings (cQueryString,
                                                                     oListQueryExpressions) .

            /* Marko Rüterbories, Consultingwerk Ltd. 30.06.2010
               Bug 2357: Added a whitespace between cQueryString and cQuerySort to prevent queries from
               beeing composed like "PRESELECT EACH fooBY foo.field" */
            cQueryString = SUBSTITUTE ("&1 &2", cQueryString, cQuerySort) .

            DO ON ERROR UNDO, THROW:
                THIS-OBJECT:QueryHandle:QUERY-PREPARE (cQueryString)  .

                CATCH querypreperr AS Progress.Lang.Error:
                    UNDO, THROW Exception:FromErrorMessageAndError (SUBSTITUTE ("Error opening Query: &1"{&TRAN}, cQueryString),
                                                                    0,
                                                                    querypreperr) .
                END CATCH.
            END.
        END.
        ELSE DO ON ERROR UNDO, THROW:
            IF THIS-OBJECT:QueryString = "":U THEN
                ASSIGN cQueryString = SUBSTITUTE ("preselect each &1":U, THIS-OBJECT:TempTableHandle:NAME) .
            ELSE
                ASSIGN cQueryString = THIS-OBJECT:QueryString .

            /* Mike Fechner, Consultingwerk Ltd. 14.05.2015
               SCL-727 - Support for FilterRow etc. on SmartTempTableAdapter */
            ASSIGN oListQueryExpressions         = GetNewFilterValues ()
                   THIS-OBJECT:FetchFilterValues = oListQueryExpressions .

            IF VALID-OBJECT (oListQueryExpressions) AND NOT oListQueryExpressions:IsEmpty THEN
                ASSIGN cQueryString = THIS-OBJECT:MergeQueryStrings (THIS-OBJECT:QueryString,
                                                                     oListQueryExpressions) .

            THIS-OBJECT:QueryHandle:QUERY-PREPARE (cQueryString)  .

            CATCH querypreperr AS Progress.Lang.Error:
                UNDO, THROW Exception:FromErrorMessageAndError (SUBSTITUTE ("Error opening Query: &1"{&TRAN}, cQueryString),
                                                                0,
                                                                querypreperr) .
            END CATCH.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 25.09.2009
           Catch and ignore Error 91, all other errors will be thrown for
           further handling.
           ** No  record is available. (91)
           ** Kein  Satz verfügbar. (91)
           Raised by Query-Open (probably from within the ProBindingSource).
           Errorstack shows IMPLICIT as line number */
        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:QueryHandle:QUERY-OPEN () .

            CATCH sysex AS Progress.Lang.SysError :
                IF sysex:GetMessageNum (1) = 91 THEN .
                ELSE UNDO, THROW sysex .
            END CATCH.
        END .

           THIS-OBJECT:BindingSource:RefreshAll () .

        /* Initialize the state of the NavigationButtons */
        IF VALID-OBJECT (oSmartNavigationSource) THEN
            OnSmartNavigationTargetPositionChanged(NEW SmartNavigationTargetPositionChangedEventArgs (THIS-OBJECT:RecordPosition)) .

        /* Tell subscribers, that we got (new) data */
        OnAfterRetrieveData(System.EventArgs:Empty) .

        /* Mike Fechner, Consultingwerk Ltd. 24.06.2010
           When BindingSource's postition has been zero and is now zero
           the PositionChanged event of the BindingSource has not been
           raised, so tell others */
        IF (THIS-OBJECT:BindingSource:Position = 0 AND lPositionWasZero OR lBindingSourceCreated) THEN
            InvokeParentPositionChanged (THIS-OBJECT, System.EventArgs:Empty) .

        IF THIS-OBJECT:BindingSource:Position = 0 AND lPositionWasZero THEN
            THIS-OBJECT:OnCurrentChanged (System.EventArgs:Empty) .

        RETURN TRUE .

        CATCH e AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessageBox (e, SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_RetrieveDataError) .
        END CATCH.

        FINALLY:
            THIS-OBJECT:PreviousPosition = THIS-OBJECT:BindingSource:Position NO-ERROR .
        END FINALLY.
    END METHOD.

       /*------------------------------------------------------------------------------
        Purpose: UpdateRow submits the changes to a buffer into the Database.
                 Called by SmartViewerControl

                 Enforced by Interface ISmartDataSource.
        Notes:   Calls UpdateRow (plAssignFromBindingSource = TRUE) .
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID UpdateRow ():

        THIS-OBJECT:UpdateRow (YES) .

    END METHOD .

      /*------------------------------------------------------------------------------
        Purpose: UpdateRow submits the changes to a buffer into the Database.
                 Called by SmartViewerControl
        Notes:   Data is updated into the Dataset from the BindingSource. After this
                 the Datasetchanges are submitted to the OERA Backend, merged into
                 the Dataset and the BindingSource gets refreshed.
        @param plAssignFromBindingSource Logical value indicating if values should be assigned from the BindingSource
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateRow (plAssignFromBindingSource AS LOGICAL):

        DEFINE VARIABLE hChangesDataset AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE hBuffer         AS HANDLE                      NO-UNDO.

        DEFINE VARIABLE rRowids         AS ROWID                       NO-UNDO EXTENT .
        DEFINE VARIABLE i               AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE j               AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE rRowid          AS ROWID                       NO-UNDO.
        DEFINE VARIABLE eBeforeUpdate   AS BeforeUpdateRecordEventArgs NO-UNDO.
        DEFINE VARIABLE eBeforeAssign   AS BeforeAssignRecordEventArgs NO-UNDO.
        DEFINE VARIABLE cKeyFields      AS CHARACTER                   NO-UNDO.

        DEFINE VARIABLE lResult         AS LOGICAL                     NO-UNDO.

        SetSavingData(TRUE) .

        /* Mike Fechner, Consultingwerk Ltd. 04.07.2009
           GET-CHANGES and REJECT-CHANGES might have invalidated Query position */
        RepositionCurrentRow () .

        /* Update Dataset from BindingSource */
        /* Mike Fechner, Consultingwerk Ltd. 13.07.2009
           Only assign the fields of the EntityTable,
           Assign will assign all fields from all tables. */
        ASSIGN hBuffer = THIS-OBJECT:BufferHandle .

        eBeforeUpdate = NEW BeforeUpdateRecordEventArgs (?,
                                                         ?,
                                                         hBuffer,
                                                         THIS-OBJECT:QueryHandle) .

        OnBeforeUpdateRecord (eBeforeUpdate) .

        IF VALID-OBJECT (eBeforeUpdate) AND eBeforeUpdate:Cancel THEN
            RETURN .

        eBeforeAssign = NEW BeforeAssignRecordEventArgs (?,
                                                         ?,
                                                         hBuffer,
                                                         THIS-OBJECT:QueryHandle) .

        OnBeforeAssignRecord (eBeforeAssign) .

        IF VALID-OBJECT (eBeforeAssign) AND eBeforeAssign:Cancel THEN
            RETURN .


        IF plAssignFromBindingSource = TRUE THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 21.10.2013
               Avoid unique key conflicts for new created records caused
               by sequential assignment of unique index fields of indexes
               with more than a single index component - and legitimate
               records already existing records with index components that
               have initial values */
            IF hBuffer:ROW-STATE = ROW-CREATED THEN DO:
                ASSIGN cKeyFields = BufferHelper:UniqueKeyFields (hBuffer, FALSE) .

                DO i = 1 TO NUM-ENTRIES (cKeyFields):
                    IF hBuffer:BUFFER-FIELD(ENTRY (i, cKeyFields)):BUFFER-VALUE = hBuffer:BUFFER-FIELD(ENTRY (i, cKeyFields)):INITIAL THEN
                        ASSIGN hBuffer:BUFFER-FIELD(ENTRY (i, cKeyFields)):BUFFER-VALUE = ? .
                END.
            END.

            fieldLoop: DO i = 1 TO hBuffer:NUM-FIELDS ON ERROR UNDO, THROW:
                /* Mike Fechner, Consultingwerk Ltd. 16.12.2013
                   Don't update the SmartFields in the list */
                IF ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (i):NAME,
                                             "SmartRecordKey,SmartAttachments,SmartComments,SmartCopiedFrom":U) THEN
                    NEXT .

                /* When using Exclude Fields, skip those fields */
                IF THIS-OBJECT:BindingSourceIncludeFields = "*" AND THIS-OBJECT:BindingSourceExcludeFields > "":U THEN
                    IF ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (i):NAME, THIS-OBJECT:BindingSourceExcludeFields) OR
                       ListHelper:EntryIsInList (hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD (i):NAME, THIS-OBJECT:BindingSourceExcludeFields) THEN
                        NEXT.

                /* When using Exclude Fields, skip those fields */
                IF THIS-OBJECT:BindingSourceIncludeFields <> "*" AND THIS-OBJECT:BindingSourceIncludeFields > "":U THEN
                    IF NOT ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (i):NAME, THIS-OBJECT:BindingSourceIncludeFields) AND
                       NOT ListHelper:EntryIsInList (hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD (i):NAME, THIS-OBJECT:BindingSourceExcludeFields) THEN
                        NEXT.

                IF hBuffer:BUFFER-FIELD(i):EXTENT > 1 THEN DO:
                    DO j = 1 TO hBuffer:BUFFER-FIELD(i):EXTENT:
                        ASSIGN hBuffer:BUFFER-FIELD(i):BUFFER-VALUE[j] = UNBOX (THIS-OBJECT:BindingSource:InputValue [hBuffer:BUFFER-FIELD(i):NAME + "[":U + STRING(j) + "]":U]) .
                    END.
                END.
                ELSE
                    ASSIGN hBuffer:BUFFER-FIELD(i):BUFFER-VALUE = UNBOX (THIS-OBJECT:BindingSource:InputValue [hBuffer:BUFFER-FIELD(i):Name]) .

                CATCH e AS System.IndexOutOfRangeException:
                    /* Ignore: Invalid fldName used with InputValue property: ... */

                    /* Mike Fechner, Consultingwerk Ltd. 18.08.2014
                       Questionable if it makes sense to show error to the developer here ... */
                    IF FrameworkSettings:DebugMode THEN
                        ErrorHelper:ShowErrorMessage (e) .

                    NEXT fieldLoop .
                END CATCH.

                CATCH ple AS Progress.Lang.Error:
                    IF ple:GetMessageNum(1) = 132 THEN DO:
                        /* Mike Fechner, Consultingwerk Ltd. 31.12.2009
                           When there have been unique key issues on the temp-table (error 132),
                           reset buffer to default-value, aka back out what has been assigned here */
                        resetloop: DO j = 1 TO hBuffer:NUM-FIELDS:
                            IF THIS-OBJECT:BindingSourceIncludeFields = "*":U AND THIS-OBJECT:BindingSourceExcludeFields > "":U THEN
                                IF ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (j):NAME, THIS-OBJECT:BindingSourceExcludeFields) OR
                                   ListHelper:EntryIsInList (hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD (j):NAME, THIS-OBJECT:BindingSourceExcludeFields) THEN
                                    NEXT resetloop.

                            /* When using Exclude Fields, skip those fields */
                            IF THIS-OBJECT:BindingSourceIncludeFields <> "*":U AND THIS-OBJECT:BindingSourceIncludeFields > "":U THEN
                                IF NOT ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (j):NAME, THIS-OBJECT:BindingSourceIncludeFields) AND
                                   NOT ListHelper:EntryIsInList (hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD (j):NAME, THIS-OBJECT:BindingSourceExcludeFields) THEN
                                    NEXT resetloop.

                            ASSIGN hBuffer:BUFFER-FIELD(j):BUFFER-VALUE = hBuffer:BUFFER-FIELD(j):DEFAULT-VALUE .
                        END.

                        UNDO, THROW ple .
                    END.
                END CATCH.
            END.
        END.

        OnAfterAssignRecord (NEW AfterAssignRecordEventArgs (?,
                                                             ?,
                                                             hBuffer,
                                                             THIS-OBJECT:QueryHandle)) .

        QueryHelper:GetCurrentRowids (hQuery, OUTPUT rRowids) .

        ASSIGN rRowid = rRowids[1] .

        THIS-OBJECT:QueryHandle:QUERY-OPEN () .
        THIS-OBJECT:BindingSource:RefreshAll() .
        lResult = THIS-OBJECT:QueryHandle:REPOSITION-TO-ROWID (rRowid) .

        IF lResult AND THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE(1):AVAILABLE = FALSE THEN
            THIS-OBJECT:QueryHandle:GET-NEXT () .

        OnAfterUpdateRecord (NEW AfterUpdateRecordEventArgs (?,
                                                             ?,
                                                             hBuffer,
                                                             THIS-OBJECT:QueryHandle)) .

        OnAfterQueryPositionChanged (System.EventArgs:Empty) .

        FINALLY:
            IF VALID-OBJECT (eBeforeUpdate) THEN
                DELETE OBJECT eBeforeUpdate .

            IF VALID-OBJECT (eBeforeAssign) THEN
                DELETE OBJECT eBeforeAssign .

            IF VALID-HANDLE (hChangesDataset) THEN
                DELETE OBJECT hChangesDataset .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartTempTableAdapter class
        Notes:   Clean up Query and ProDataset from memory.
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartTempTableAdapter ():
        IF VALID-HANDLE (hQuery) THEN
            DELETE OBJECT hQuery .

        IF VALID-HANDLE (THIS-OBJECT:BufferHandle) THEN
            DELETE OBJECT THIS-OBJECT:BufferHandle NO-ERROR .
    END DESTRUCTOR.

END CLASS.
