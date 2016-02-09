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
    File        : SmartDatasetAdapter
    Purpose     : Shared base class of the SmartBusinessEntityAdapter and 
                  the SmartDatasetChildAdapter
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 28 20:41:59 CET 2010
    Notes       : Contains common code from SmartBusinessEntityAdapter and
                  SmartDatasetChildAdapter as of change # 16369
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i} 

USING Consultingwerk.*                                                          FROM PROPATH .
USING Consultingwerk.Assertion.*                                                FROM PROPATH .
USING Consultingwerk.Exceptions.*                                               FROM PROPATH .
USING Consultingwerk.Framework.*                                                FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                                    FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*                           FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*                               FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupCreateSource FROM ASSEMBLY . 
USING Consultingwerk.SmartComponents.Resources.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*                                  FROM PROPATH .
USING Consultingwerk.Util.*                                                     FROM PROPATH .
USING Consultingwerk.Windows.API.*                                              FROM PROPATH .
USING Progress.Data.*                                                           FROM ASSEMBLY .
USING Progress.Lang.*                                                           FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.SmartDatasetAdapter
    INHERITS SmartDataAdapter
    IMPLEMENTS ISmartDataSourceWithDataset,
               ISortableDataSource,
               ISupportsReposition,
               ISupportsSubmitChanges,
               ISmartBusinessEntityAdapterEvents,
               IDatasetControllerConsumer,
               ISmartGroupCreateSource,
               IProvidesRecordKey,
               IStoresCopiedFrom,
               IDesignGroupCreateSource,
               ISmartFilterTarget
    ABSTRACT:

    DEFINE PROTECTED VARIABLE cQuerySort            AS CHARACTER NO-UNDO .
    DEFINE PRIVATE   VARIABLE roCreateRowid         AS ROWID     NO-UNDO .
    DEFINE PRIVATE   VARIABLE roCurrentRowids       AS ROWID     NO-UNDO EXTENT .
    DEFINE PRIVATE   VARIABLE iPositionBeforeCreate AS INTEGER   NO-UNDO .

    DEFINE PROTECTED VARIABLE lHasValidatedForeignFields AS LOGICAL NO-UNDO INIT FALSE .

    DEFINE PRIVATE   VARIABLE cPreviousFetchQueryString AS CHARACTER INIT "":U NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Event raised to collect Filter Values from Filter Sources
        Notes:   Required by IFilterTarget 
        @param sender The reference to the object that raised the event
        @param e The CollectFilterValuesEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT CollectFilterValues SIGNATURE VOID (sender AS Progress.Lang.Object, 
                                                            e AS CollectFilterValuesEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Binding the BusinessEntityBindingSource to a Query or a Dataset.
                 This Property is set by the Developer during DesignTime.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY BindTo AS CHARACTER NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if this SmartDatasetAdapter is currently creating a new
                 record
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CreatingRecord AS LOGICAL NO-UNDO INIT FALSE
    GET:
        IF roCreateRowid <> ? THEN
            RETURN TRUE .
        ELSE
            RETURN FALSE .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to the ProDataset instance managed by the
                 SmartBusinessEntityAdapter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY DataSet AS HANDLE NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: A Reference to the DatasetController that is is owning the Dataset
                 on which this SmartDatasetAdapter is operating.
        Notes:   The property can only be set at long as this
                 SmartBusinessEntityAdapter does not already have a valid dataset
                 handle (DatasetHandle property)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY DatasetController AS Consultingwerk.SmartComponents.Interfaces.IDatasetController NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: A List in the same order as EntityView which defines Join (YES or NO)
                 for each Table of pcEntityView.
                 This Property is set by the Developer during DesignTime.
        Notes:   Not exposed publicly for SmartDatasetChildAdapter
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED ABSTRACT PROPERTY EntityJoin AS CHARACTER NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Name of the BusinessEntity.
                 This Property is set by the Developer during DesignTime.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY EntityName AS CHARACTER NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: RootTable for this Instance of the BusinessEntity.
                 This Property is set by the Developer during DesignTime.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY EntityTable AS CHARACTER NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: List of Tables to read with the EntityTable.
                 This Property is set by the Developer during DesignTime.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY EntityView AS CHARACTER NO-UNDO
    GET.
            
    /*------------------------------------------------------------------------------
        Purpose: Returns the list of Query Expressions by Table of the previous
                 RetrieveData  
        Notes:   Required for batching capabilities
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FetchFilterValues AS ListQueryExpressionByTable NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: This is the QueryString for the next Fetch operation.
                 This Property is set by the ParentPositionChanged Method every time
                 the Parent Data changes.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY FetchQueryString AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Indicates if a DataAdapters Query is being opened for the First time
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY FirstTime AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: ForeignFields related to the Parent SmartDataSource.                 
        Notes:   This Property is set by the SetSmartDataSource Method when a new
                 SmartDataSource is registered.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ForeignFields AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET (arg AS CHARACTER):
        THIS-OBJECT:ForeignFields = ListHelper:TrimEntries (arg) .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Get's/set's Columns that will not get updated by UpdateRow (TRUE)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NonUpdatableColumns AS CHARACTER INITIAL "":U NO-UNDO
    GET.
    SET (arg AS CHARACTER):
        IF arg <> THIS-OBJECT:NonUpdatableColumns THEN DO:
            OnNonUpdatableColumnsChanged (System.EventArgs:Empty) .

            THIS-OBJECT:NonUpdatableColumns = arg .
        END.
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to the Query used to navigate the selected tables
                 of the ProDataset instance managed by the SmartBusinessEntityAdapter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QueryHandle AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls the sort-order used at the client
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QuerySort AS CHARACTER NO-UNDO INIT "":U
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
    DEFINE PUBLIC ABSTRACT PROPERTY QueryString AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: This property is depricated since the addition of Designer Verbs
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectTables AS LOGICAL NO-UNDO INIT FALSE
    GET ():
        RETURN FALSE .
    END GET.
    SET (arg AS LOGICAL):
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of SmartGroupAssignTargets as a generic list
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartGroupCreateTargets AS ListISmartGroupCreateTarget NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets if errors from RetrieveData (in childs of this abstract
                 base class) should be THROWN or Shown from the DataAdapter class
                 (Using ErrorHelper)
        Notes:   The default is to Show the error message (FALSE), not to throw it.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ThrowErrorsFromRetrieveData AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

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
        Purpose: Raised while creating a new record in a child SmartDatasetAdapter
        Notes:   The event is raised just before the foreign field values are assigned. 
                 Setting the Handled property of the event argument to TRUE will prevent 
                 the Adapter for assigning the foreign field values as it is assumed
                 that an event subscribed has done so already.  
        @param sender The object that raised the AssignForeignFields event
        @param e The AssignForeignFieldsEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AssignForeignFields SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                            e AS AssignForeignFieldsEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised when the SmartDatasetAdapter catches an error while assigning
                 the fields from the BindindSource
        Notes:   This event allows the UI to review the error and provide more appropriate
                 error handling than just showing the Progress Error message. From this
                 event you may actually throw an error message which is more informational
                 to the end user
        @param sender The object that raised the AssignKeyFieldError event
        @param e The AssignKeyFieldErrorEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AssignKeyFieldError SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                            e AS AssignKeyFieldErrorEventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Event raised when a ISmartGroupCreateSource needs to create a
                 record to allow creation of a child record in a ISmartGroupCreateTarget
                 instance.
        Notes:   The purpose of this event is that an event handler can create
                 meaningful key values. When done an event handler is supposed to
                 ASSIGN e:Assigned = TRUE .
        @param sender The reference to the object that raised the event
        @param e The AssignKeyFieldEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AssignKeyFields SIGNATURE VOID (sender AS System.Object,
                                                        e AS AssignKeyFieldsEventArgs) .

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
        Purpose: Raised before the a record is created
        Notes:   The BeforeCreateRecord event handler are NOT allowed to contain
                 MESSAGE statements. This will cause the Buffer to no longer
                 point to the (right) record!
        @param sender The reference to the object that raised the event
        @param e The BeforeCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeCreateRecord SIGNATURE VOID (sender AS System.Object,
                                                           e AS BeforeCreateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised before the a record is deleted
        Notes:
        @param sender The reference to the object that raised the event
        @param e The BeforeDeleteRecordEventArgs with the data for this event
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
        Purpose: Raised before the SmartDatasetAdapter is about to create a new record
                 in the CreateRecord method
        Notes:   Unlike the BeforeCreateRecord (which allows to assign new values 
                 to the newly created record), this event is raised before any new 
                 record is created
        @param sender The object that raised the CreatingRecord event
        @param e The Consultingwerk.CancelableEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeCreatingRecord SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                             e AS Consultingwerk.CancelableEventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Raised when the DatasetController property is changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DatasetControllerChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Raised when the NonUpdatableColumns property is changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT NonUpdatableColumnsChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Raised when the SmartDatasetAdapter is unable to reposition to the 
                 updated row after update (typically the key values of the record
                 have changed so that it does no longer match the query criteria)
        Notes:   For sample event handler code, see SCL-500
        @param sender The object that raised the UnableToRepositionAfterUpdateRow event
        @param e The UnableToRepositionAfterUpdateRowEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT UnableToRepositionAfterUpdateRow SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                         e AS UnableToRepositionAfterUpdateRowEventArgs).
    
	/*------------------------------------------------------------------------------
	    Purpose: Begins a transaction when starting update
	    Notes:   Allows SmartDatasetChildAdapter to initiate TransactionState in a 
	             parent SmartBusinessEntityAdapter
	------------------------------------------------------------------------------*/
	METHOD PUBLIC ABSTRACT VOID BeginTransactionState ().

    /*------------------------------------------------------------------------------
        Purpose: The Message to for instance Disable a Browser is send to all
                 SmartDataTargets except the calling one
        Notes:   This Method is called from the SmartViewerControl derived Class when
                 starting to Modify or Add data.
                 Enforced by Interface ISmartDataSource.
        @param poSmartDataTarget The reference to the ISmartDataTarget which is in UpdateState now
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID BeginUpdateState (poSmartDataTarget AS Consultingwerk.SmartComponents.Interfaces.ISmartDataTarget):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        SUPER:BeginUpdateState (poSmartDataTarget) .

        DO i = 1 TO THIS-OBJECT:SmartDataTargets:Count:

            IF THIS-OBJECT:SmartDataTargets:GetItem (i) = poSmartDataTarget THEN DO:

                THIS-OBJECT:BeginTransactionState() .

                RETURN .
            END.
        END.

    END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Clears the PreviousQueryString information to ensure that the 
	             ParentPositionChanged event handler does reopen the query regardless 
	             of identical key values
	    Notes:   SCL-402
	    @param plIncludeCurrent Clear cPreviousQueryString also for the current instance or only for Data Targets
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ClearPreviousQueryString (plIncludeCurrent AS LOGICAL):
		
		IF plIncludeCurrent THEN 
		    ASSIGN cPreviousFetchQueryString = "":U . 

        {Consultingwerk/foreachABL.i ISmartDataTarget oTarget in THIS-OBJECT:SmartDataTargets}
            IF TYPE-OF (oTarget, SmartDatasetChildAdapter) THEN 
                CAST (oTarget, SmartDatasetChildAdapter):ClearPreviousQueryString (TRUE) . 
        END.

	END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Returns the Class Name used for creating SmartBusinessEntityBindingSource
                 (or derived) components at design time (SCL-1000), using the 
                 "Create BindingSource" designer verb 
        Notes:   
        @return  The Class Name used for creating SmartBusinessEntityBindingSource components at design time   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER GetDesignTimeDataSourceClassName ():
		
		RETURN "Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource":U .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds an empty ListQueryExpressionByTable instance and raises the 
                 CollectFilterValues event
        Notes:   Will return ? when the event was cancelled by a subscriber                                 
        @return The ListQueryExpressionByTable with the filters per table
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ListQueryExpressionByTable GetNewFilterValues ():
        
        DEFINE VARIABLE e            AS CollectFilterValuesEventArgs NO-UNDO . 
        DEFINE VARIABLE iEntry       AS INTEGER                      NO-UNDO.
        DEFINE VARIABLE oList        AS ListQueryExpressionByTable   NO-UNDO .

        oList = NEW ListQueryExpressionByTable () . 
        
        oList:Add (THIS-OBJECT:EntityTable, NEW ListQueryExpression ()) .
    
        DO iEntry = 1 TO NUM-ENTRIES (THIS-OBJECT:EntityView):
            oList:Add (ENTRY (iEntry, THIS-OBJECT:EntityView), NEW ListQueryExpression ()) .
        END.

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
        Purpose: Initializes the SmartBusinessEntityAdpater
        Notes:   Useful to initalize the BindingSource and Query before calling 
                 RetrieveData
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ABSTRACT VOID InitializeAdapter () .

    /*------------------------------------------------------------------------------
        Purpose: Merges two Query Strings                                                                      
        Notes:                           
        @param pcFetchQueryString The first QueryString
        @param poQueryExpressions The List of QueryExpressions by Table to be merged with the first QueryString
        @return The resulting Query String          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER MergeQueryStrings (pcFetchQueryString AS CHARACTER, 
                                                  poQueryExpressions AS ListQueryExpressionByTable):
        
        DEFINE VARIABLE i                AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cBaseQuery       AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cTables          AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cTable           AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cReturn          AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oQueryExpression AS ListQueryExpression NO-UNDO .

        ASSIGN pcFetchQueryString  = pcFetchQueryString + FILL (CHR(1), NUM-ENTRIES (THIS-OBJECT:EntityView)) 
               cReturn             = FILL (CHR(1), NUM-ENTRIES (THIS-OBJECT:EntityView)).

        ASSIGN cTables = TRIM(THIS-OBJECT:EntityTable + ",":U + THIS-OBJECT:EntityView, ",":U) .

        DO i = 1 TO NUM-ENTRIES (cTables):
            
            ASSIGN cTable           = ENTRY (i, cTables)
                   oQueryExpression = poQueryExpressions:GetItem (cTable) .
            
            IF oQueryExpression:IsEmpty THEN 
                ASSIGN ENTRY (i, cReturn, CHR(1)) = ENTRY (i, pcFetchQueryString, CHR(1)) .
            ELSE DO:
                ASSIGN cBaseQuery = ENTRY (i, pcFetchQueryString, CHR(1)) .
                
                IF cBaseQuery = "":U THEN 
                    ASSIGN cBaseQuery = SUBSTITUTE ("FOR EACH &1":U, cTable) .
                    
                cBaseQuery = QueryHelper:InsertExpression (cBaseQuery, 
                                                           oQueryExpression:GetExpression(),
                                                           "AND":U) .
                
                ASSIGN ENTRY (i, cReturn, CHR(1)) = cBaseQuery .
            
            END.                                                                                  
        END.

        RETURN cReturn . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the UnableToRepositionAfterUpdateRow
        Notes:   
        @param e The UnableToRepositionAfterUpdateRowEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnUnableToRepositionAfterUpdateRow (e AS UnableToRepositionAfterUpdateRowEventArgs):
    		
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "UnableToRepositionAfterUpdateRow":U) .
     
        THIS-OBJECT:UnableToRepositionAfterUpdateRow:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDatasetAdapter component
        Notes:
        @param poContainer An System.Componentmodel.IContainer that represents the container for the component.
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDatasetAdapter (poContainer AS System.ComponentModel.IContainer):

        SUPER (poContainer).

        THIS-OBJECT:SmartGroupCreateTargets = NEW ListISmartGroupCreateTarget () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDatasetAdapter component
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDatasetAdapter ():
        SUPER ().

        THIS-OBJECT:SmartGroupCreateTargets = NEW ListISmartGroupCreateTarget () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the Query/ProDataset to the BindingSource
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ABSTRACT VOID AttachQueryToBindingSource ().

    /*------------------------------------------------------------------------------
        Purpose: Attaches the schema to the BindingSource
        Notes:
        @param poBindingSource The BindingSource to attach the schema to
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachSchemaToBindingSource (poBindingSource AS Progress.Data.BindingSource):

        DEFINE VARIABLE cExceptList  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cIncludeList AS CHARACTER NO-UNDO.

        IF THIS-OBJECT:BindTo = "Query":U THEN DO:
            THIS-OBJECT:QueryHandle = SmartBusinessEntityQuerySupport:PrepareBindingQuery (THIS-OBJECT:Dataset,
                                                                                           THIS-OBJECT:EntityTable,
                                                                                           THIS-OBJECT:EntityView,
                                                                                           THIS-OBJECT:EntityJoin,
                                                                                           TRUE,
                                                                                           OUTPUT cExceptList) .

            /* Mike Fechner, Consultingwerk Ltd. 07.09.2009
               Merge Developer defined Except List and Queries Except List */
            IF cExceptList > "":U OR THIS-OBJECT:BindingSourceIncludeFields > "":U OR THIS-OBJECT:BindingSourceExcludeFields > "":U THEN DO:
                IF cExceptList > "":U OR THIS-OBJECT:BindingSourceExcludeFields > "":U THEN
                    ASSIGN cIncludeList = "*":U
                           cExceptList = TRIM(cExceptList + ",":U + THIS-OBJECT:BindingSourceExcludeFields, ",":U).
                ELSE
                    cIncludeList = THIS-OBJECT:BindingSourceIncludeFields .

                poBindingSource:SetFields(cIncludeList, cExceptList, "":U) .
            END.

            THIS-OBJECT:BindingSource:SetFields (THIS-OBJECT:BindingSourceIncludeFields,
                                                 THIS-OBJECT:BindingSourceExcludeFields, "":U) .


            IF NOT THIS-OBJECT:QueryHandle:IS-OPEN THEN THIS-OBJECT:QueryHandle:QUERY-OPEN () .

            ASSIGN poBindingSource:AutoSync = TRUE
                   poBindingSource:Handle   = THIS-OBJECT:QueryHandle .
        END.
        ELSE DO:
            IF THIS-OBJECT:BindingSourceIncludeFields > "":U OR THIS-OBJECT:BindingSourceExcludeFields > "":U THEN DO:
                IF THIS-OBJECT:BindingSourceExcludeFields > "":U THEN
                    ASSIGN cIncludeList = "*":U
                           cExceptList = TRIM(cExceptList + "," + THIS-OBJECT:BindingSourceExcludeFields, ",").
                ELSE
                    cIncludeList = THIS-OBJECT:BindingSourceIncludeFields .

                poBindingSource:SetFields(cIncludeList, cExceptList, "":U) .
            END.

            ASSIGN THIS-OBJECT:QueryHandle  = THIS-OBJECT:Dataset:TOP-NAV-QUERY (1)
                   poBindingSource:AutoSync = TRUE
                   poBindingSource:HANDLE   = THIS-OBJECT:Dataset .
        END.

        FINALLY:
            /* BindingSource is already prepared */
            ASSIGN THIS-OBJECT:FirstTime = FALSE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked from the SmartDataTarget to cancel the creation of a new
                 row
        Notes:   Creation of rows and cancellation of rows in the
                 SmartBusinessEntityAdapter now no longer handled using the AddNew()
                 method of the BindingSource. We are now creating rows and removing
                 them on cancel. (Bug 1836)
                 For ISmartGroupCreateSource instances this event notifies the
                 associated ISmartGroupCreateTargets to also cancel the creation of
                 a new record
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID CancelCreateRecord ():

        DEFINE VARIABLE lBatching AS LOGICAL                           NO-UNDO.
        DEFINE VARIABLE hBuffer   AS HANDLE                            NO-UNDO.
        DEFINE VARIABLE oBefore   AS BeforeCancelCreateRecordEventArgs NO-UNDO .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO THIS-OBJECT:SmartGroupCreateTargets:Count:
            THIS-OBJECT:SmartGroupCreateTargets:GetItem (i):CancelCreateByGroupCreateSource (THIS-OBJECT) .
        END.

        ASSIGN
            lBatching                          = THIS-OBJECT:BindingSource:Batching
            THIS-OBJECT:BindingSource:Batching = FALSE
            hBuffer                            = THIS-OBJECT:Dataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable) .

        hBuffer:FIND-BY-ROWID (roCreateRowid) .


        oBefore = NEW BeforeCancelCreateRecordEventArgs (THIS-OBJECT:EntityName,
                                                         THIS-OBJECT:EntityTable,
                                                         hBuffer,
                                                         THIS-OBJECT:QueryHandle) .

        OnBeforeCancelCreateRecord (oBefore) .

        IF oBefore:Cancel THEN DO:
            IF oBefore:CancelMessage > "":U THEN
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException (oBefore:CancelMessage, 0) .
            ELSE
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException () .
        END.

        IF hBuffer:AVAILABLE THEN
            hBuffer:BUFFER-DELETE () .

        THIS-OBJECT:QueryHandle:DELETE-RESULT-LIST-ENTRY () .

        THIS-OBJECT:QueryHandle:QUERY-OPEN () .

        THIS-OBJECT:BindingSource:RefreshAll() .

        /* 0 based binding source position */
        IF iPositionBeforeCreate > 0 THEN DO:
            ASSIGN THIS-OBJECT:BindingSource:Position = iPositionBeforeCreate .

            OnAfterQueryPositionChanged (System.EventArgs:Empty) .
        END.

        OnAfterCancelCreateRecord (NEW AfterCancelCreateRecordEventArgs (THIS-OBJECT:EntityName,
                                                                         THIS-OBJECT:EntityTable,
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
        Purpose: Empties the Resultset of this SmartDataAdapter. Used to clear grids
        Notes:   Override to CloseQuery in SmartDataAdapter that resets the variable
                 cPreviousFetchQueryString to avoid issues of not reopening the
                 Query in the ParentPositionChanged handler when the same parent record
                 as before becomes available after closing the query inbetween
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID CloseQuery ():

        ASSIGN cPreviousFetchQueryString = "":U .

        SUPER:CloseQuery().

        /* Tell Viewers that/if we got a record */
        {Consultingwerk/foreachABL.i ISmartDataTarget oTarget in this-object:SmartDataTargets}
            oTarget:EvaluateTableIOState () .
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Recursively invokes CancelUpdate in all DataTargets that are currently 
                 updating
        Notes:   Throws an Exception, when of the DataTargets remains updating
                 Used during RejectChanges
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CancelInDataTargets ():

        DEFINE VARIABLE i              AS INTEGER                               NO-UNDO.
        DEFINE VARIABLE oTarget        AS Progress.Lang.Object                  NO-UNDO .
        DEFINE VARIABLE oTableIOTarget AS ISmartTableIOTarget                   NO-UNDO .  

        DO i = 1 TO THIS-OBJECT:SmartDataTargets:Count:

            ASSIGN oTarget = THIS-OBJECT:SmartDataTargets:GetItem (i) .
            
            IF VALID-OBJECT (oTarget) AND TYPE-OF (oTarget, ISmartTableIOTarget) THEN DO:
            
                oTableIOTarget = CAST (oTarget, ISmartTableIOTarget) .
                
                IF oTableIOTarget:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
                    oTableIOTarget:CancelUpdate () . 
             
                IF oTableIOTarget:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
                    THIS-OBJECT:RaiseUnableToRejectError (oTableIOTarget) .                                    
            END.
            ELSE IF VALID-OBJECT (oTarget) AND TYPE-OF (oTarget, SmartDatasetAdapter) THEN 
                CAST (oTarget, SmartDatasetAdapter):CancelInDataTargets () .      
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a parent record when needed for creating a record in a
                 SmartGroupCreateTarget
        Notes:   Creates a record and attempts to assign the primary key values
        @param pcKeyFields The Key Fields to assign
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateParentRecord (pcKeyFields AS CHARACTER):

        DEFINE VARIABLE oTableIOTarget     AS ISmartTableIOTarget      NO-UNDO .
        DEFINE VARIABLE oGroupCreateSource AS ISmartGroupCreateSource  NO-UNDO .
        DEFINE VARIABLE oDataTarget        AS ISmartDataTarget         NO-UNDO .
        DEFINE VARIABLE cForeignFields     AS CHARACTER                NO-UNDO .
        DEFINE VARIABLE cKeyFields         AS CHARACTER                NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER                  NO-UNDO .
        DEFINE VARIABLE e                  AS AssignKeyFieldsEventArgs NO-UNDO .
        DEFINE VARIABLE hBuffer            AS HANDLE                   NO-UNDO .
        DEFINE VARIABLE hField             AS HANDLE                   NO-UNDO .

        tableIOTargetLoop: DO i = 1 TO THIS-OBJECT:SmartDataTargets:Count:
            oDataTarget = THIS-OBJECT:SmartDataTargets:GetItem (i) .

            IF TYPE-OF (oDataTarget, ISmartTableIOTarget) THEN DO:

                ASSIGN oTableIOTarget = CAST (oDataTarget, ISmartTableIOTarget) .

                IF VALID-OBJECT (oTableIOTarget:SmartTableIOSource) THEN
                    LEAVE tableIOTargetLoop .
                ELSE
                    ASSIGN oTableIOTarget = ? .
            END.
        END.

        IF NOT VALID-OBJECT (oTableIOTarget) THEN
            UNDO, THROW NEW AppError ("Unable to create parent record when there is no active TableIOTarget", 0) .

        IF     TYPE-OF (THIS-OBJECT, ISmartGroupCreateTarget)
           AND VALID-OBJECT (CAST (THIS-OBJECT, ISmartGroupCreateTarget):SmartGroupCreateSource) THEN DO:

            ASSIGN oGroupCreateSource = CAST (THIS-OBJECT, ISmartGroupCreateTarget):SmartGroupCreateSource .

            IF oGroupCreateSource:RecordPosition = RecordPositionEnum:NoRecordAvailable THEN DO:

                ASSIGN cForeignFields = THIS-OBJECT:ForeignFields .

                DO i = 1 TO NUM-ENTRIES (cForeignFields) - 1:
                    ASSIGN cKeyFields = cKeyFields + SUBSTITUTE ("&1,":U, ENTRY (i, cForeignFields)).
                END.

                oGroupCreateSource:CreateParentRecord (TRIM (cKeyFields, ",":U)) .
            END.
        END.

        oTableIOTarget:AddRecord () .

        /* Mike Fechner, Consultingwerk Ltd. 07.01.2012
           Raise the AssignKeyFields event */
        ASSIGN hBuffer = THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1)
               e       = NEW AssignKeyFieldsEventArgs (hBuffer,
                                                       pcKeyFields) .

        OnAssignKeyFields (e) .

        IF NOT e:Assigned THEN DO:


            /* Assign key fields with default values */
            DO i = 1 TO NUM-ENTRIES (pcKeyFields):

                Consultingwerk.Assertion.BufferAssert:HasField (hBuffer, ENTRY (i, pcKeyFields)) .

                ASSIGN hField = hBuffer:BUFFER-FIELD (ENTRY (i, pcKeyFields)) .

                CASE hField:DATA-TYPE:
                    WHEN DataTypeEnum:CHARACTER THEN
                        hField:BUFFER-VALUE = GUID .
                    WHEN DataTypeEnum:INTEGER OR WHEN DataTypeEnum:INT64 OR WHEN DataTypeEnum:DECIMAL THEN
                        hField:BUFFER-VALUE = RANDOM (0, 2100000000) .
                    OTHERWISE
                        UNDO, THROW NEW AppError
                            (SUBSTITUTE ("Unable to assign default key value for field &1 of data-type &2.",
                                         hField:NAME,
                                         hField:DATA-TYPE), 0) .
                END CASE .
            END .

        END.

        THIS-OBJECT:BindingSource:Refresh () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handler for the CancelCreateRow event of the ProBindingSource.
                 A Row that has been created usind CreateRowHandler gets reverted.
        Notes:
        @param sender The object that raised the CancelCreateRow event (ProBindingSource)
        @param args The CancelCreateRowEventArgs instance of the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID CancelCreateRowHandler (sender AS System.Object,
                                                           args AS CancelCreateRowEventArgs):

        DEFINE VARIABLE hBuffer AS HANDLE                            NO-UNDO.
        DEFINE VARIABLE oBefore AS BeforeCancelCreateRecordEventArgs NO-UNDO .


        /* Remove the new row from the EntityTable */
        hBuffer = THIS-OBJECT:Dataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable) .

        oBefore = NEW BeforeCancelCreateRecordEventArgs (THIS-OBJECT:EntityName,
                                                         THIS-OBJECT:EntityTable,
                                                         hBuffer,
                                                         THIS-OBJECT:QueryHandle) .

        OnBeforeCancelCreateRecord (oBefore) .

        IF oBefore:Cancel THEN DO:
            IF oBefore:CancelMessage > "":U THEN
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException (oBefore:CancelMessage, 0) .
            ELSE
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException () .
        END.

        THIS-OBJECT:QueryHandle:DELETE-RESULT-LIST-ENTRY () .

        hBuffer:BUFFER-DELETE () .

        OnAfterCancelCreateRecord (NEW AfterCancelCreateRecordEventArgs (THIS-OBJECT:EntityName,
                                                                         THIS-OBJECT:EntityTable,
                                                                         hBuffer,
                                                                         THIS-OBJECT:QueryHandle)) .

        FINALLY:
            ASSIGN
                roCreateRowid         = ?
                iPositionBeforeCreate = ?.
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new record
        Notes:   Creation of rows and cancellation of rows in the
                 SmartBusinessEntityAdapter now no longer handled using the AddNew()
                 method of the BindingSource. We are now creating rows and removing
                 them on cancel. (Bug 1836)

                 The OnBeforeCreateRecord event handler are NOT allowed to contain
                 MESSAGE statements. This will cause the hBuffer of to no longer
                 point to the right record!
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID CreateRecord ():

        DEFINE VARIABLE hBuffer         AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE hCreateBuffer   AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE hChildBuffer    AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE hParentBuffer   AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE hRelation       AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE i               AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE j               AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE cValue          AS CHARACTER                   NO-UNDO.
        DEFINE VARIABLE cField          AS CHARACTER                   NO-UNDO.
        DEFINE VARIABLE hField          AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE iPosition       AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE iIndex          AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE cIndex          AS CHARACTER                   NO-UNDO.
        DEFINE VARIABLE roTmpRowid      AS ROWID                       NO-UNDO.

        DEFINE VARIABLE iReleaseBuffer  AS INTEGER                     NO-UNDO .
        DEFINE VARIABLE hReleaseBuffer  AS HANDLE                      NO-UNDO .
        DEFINE VARIABLE roReleaseRowid  AS ROWID EXTENT                NO-UNDO . 

        DEFINE VARIABLE oBeforeCreating AS CancelableEventArgs          NO-UNDO .
        DEFINE VARIABLE oBefore         AS BeforeCreateRecordEventArgs  NO-UNDO .
        DEFINE VARIABLE oAssignEvent    AS AssignForeignFieldsEventArgs NO-UNDO .

        /* If no valid BindingSource exist a new one is created right here */
        IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource) THEN
            CreateBindingSource ().

        /* Mike Fechner, Consultingwerk Ltd. 02.01.2012
           Create Query if not yet created - typically when CreateRecord is
           called with no previous call to RetrieveData */
        IF NOT VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN DO:
            THIS-OBJECT:PrepareDataBinding () .
            THIS-OBJECT:AttachQueryToBindingSource () .
            THIS-OBJECT:CloseQuery () .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 11.05.2014
           Raise the BeforeCreatingRecord event */
        oBeforeCreating = NEW CancelableEventArgs () . 
        
        THIS-OBJECT:OnBeforeCreatingRecord (oBeforeCreating) .

        IF oBeforeCreating:Cancel THEN 
            UNDO, THROW NEW Consultingwerk.Exceptions.CancelException () .
        
        /* Keep current position after cancel */
        ASSIGN iPositionBeforeCreate = THIS-OBJECT:BindingSource:Position .

        QueryHelper:GetCurrentRowids (THIS-OBJECT:QueryHandle, OUTPUT roCurrentRowids) .

        /* Create a new row in the EntityTable */
        hBuffer = THIS-OBJECT:Dataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable) .
        hBuffer:TABLE-HANDLE:TRACKING-CHANGES = TRUE .

        /* use an alternative buffer for creating */
        CREATE BUFFER hCreateBuffer FOR TABLE hBuffer .

        hCreateBuffer:BUFFER-CREATE () .

        EXTENT (roReleaseRowid) = THIS-OBJECT:QueryHandle:NUM-BUFFERS .

        DO iReleaseBuffer = 1 TO THIS-OBJECT:QueryHandle:NUM-BUFFERS:
            ASSIGN roReleaseRowid = ? . 
            
            hReleaseBuffer = THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (iReleaseBuffer) .
            
            IF hReleaseBuffer:AVAILABLE 
                THEN roReleaseRowid = hReleaseBuffer:ROWID . 
            
            IF hReleaseBuffer <> hBuffer THEN 
                hReleaseBuffer:BUFFER-RELEASE () .
        END.  

        /* Mike Fechner, Consultingwerk Ltd. 17.01.2010
           BeforeCreateRecord event */
        oBefore = NEW BeforeCreateRecordEventArgs (THIS-OBJECT:EntityName,
                                                   THIS-OBJECT:EntityTable,
                                                   hCreateBuffer,
                                                   THIS-OBJECT:QueryHandle) .

        /* Mike Fechner, Consultingwerk Ltd. 19.01.2010
           Note, that the OnBeforeCreateRecord event handler are NOT allowed
           to contain MESSAGE statements. This will cause the hBuffer of
           to no longer point to the right record! */
        OnBeforeCreateRecord (oBefore) .

        IF oBefore:Cancel THEN DO:
            IF hCreateBuffer:AVAILABLE THEN
                hCreateBuffer:BUFFER-DELETE () .

            DO iReleaseBuffer = 1 TO THIS-OBJECT:QueryHandle:NUM-BUFFERS:
                hReleaseBuffer = THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (iReleaseBuffer) .

                IF hReleaseBuffer <> hBuffer AND roReleaseRowid [iReleaseBuffer] <> ? THEN
                    hReleaseBuffer:FIND-BY-ROWID (roReleaseRowid) . 
            END.

            IF oBefore:CancelMessage > "":U THEN
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException (oBefore:CancelMessage, 0) .
            ELSE
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException () .
        END.

        IF oBefore:SkipUniqueRecordValidation = TRUE THEN .
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

        /* Assign ForeignFields */
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN DO:

            /* Mike Fechner, Consultingwerk Ltd. 23.01.2015
               SCL-625: Ensure data-source is located on right record. */
            IF TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter) AND 
               VALID-HANDLE (CAST (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter):QueryHandle) THEN DO:

                CAST (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter):QueryHandle:REPOSITION-TO-ROW
                        (CAST (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter):BindingSource:Position + 1) .
            END.  
            
            ASSIGN oAssignEvent = NEW AssignForeignFieldsEventArgs (hCreateBuffer,
                                                                    THIS-OBJECT:ForeignFields,
                                                                    THIS-OBJECT:SmartDataSource) . 
            
            THIS-OBJECT:OnAssignForeignFields (oAssignEvent) .

            IF oAssignEvent:Handled = FALSE THEN  
            DO:
                DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:ForeignFields) BY 2:
                    ASSIGN
                        cField = ENTRY(i + 1, THIS-OBJECT:ForeignFields)
                        hField = hCreateBuffer:BUFFER-FIELD (ENTRY(NUM-ENTRIES(cField, ".":U), cField, ".":U))
                        NO-ERROR .
                    IF VALID-HANDLE (hField) THEN DO:
                        ASSIGN cValue              = oSmartDataSource:GetFieldValues (ENTRY(i, THIS-OBJECT:ForeignFields))
                               hField:BUFFER-VALUE = cValue .
                    END.
                END.
            END.
        END.

        hCreateBuffer:BUFFER-RELEASE () .

        /* Mike Fechner, Consultingwerk Ltd. 20.01.2010
           Add new record to query using default buffer */
        hBuffer:FIND-BY-ROWID (roCreateRowid) .
                
        THIS-OBJECT:QueryHandle:CREATE-RESULT-LIST-ENTRY () .

        ASSIGN iPosition = THIS-OBJECT:QueryHandle:CURRENT-RESULT-ROW .

        THIS-OBJECT:BindingSource:RefreshAll () .

        /* 1 based query current-result-row */
        IF iPosition > 0 THEN
            THIS-OBJECT:BindingSource:Position = iPosition - 1.

        /* Mike Fechner, Consultingwerk Ltd. 01.01.2010
           After creating a new record, update the roCurrentRowids.
           This is required so that a new record is actually available after
           adding it. */
        QueryHelper:GetCurrentRowids (THIS-OBJECT:QueryHandle, OUTPUT roCurrentRowids) .

        OnAfterQueryPositionChanged (System.EventArgs:Empty) .

        OnAfterCreateRecord (NEW AfterCreateRecordEventArgs (THIS-OBJECT:EntityName,
                                                             THIS-OBJECT:EntityTable,
                                                             hBuffer,
                                                             THIS-OBJECT:QueryHandle)) .

        CATCH e AS Progress.Lang.Error :
            IF VALID-HANDLE (hCreateBuffer) AND hCreateBuffer:AVAILABLE
                THEN hCreateBuffer:BUFFER-DELETE () .

            UNDO, THROW e.
        END CATCH.

        FINALLY:
            IF VALID-OBJECT (oBefore) THEN
                DELETE OBJECT oBefore .

            IF VALID-HANDLE (hCreateBuffer) THEN
                DELETE OBJECT hCreateBuffer .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CreateRow event of the ProBindingSource.
        Notes:   A new Row gets created.
                 TRACKING-CHANGES is switched on and a new Buffer is created. Inside
                 the Query a new Result-List-Entry is created.
        @param sender The reference to the object that raised the event
        @param args The CreateRowEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID CreateRowHandler (sender AS System.Object,
                                                     args AS CreateRowEventArgs):

        DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO.
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cValue  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField  AS HANDLE    NO-UNDO.

        /* Create a new row in the EntityTalbe */
        hBuffer = THIS-OBJECT:Dataset:GET-BUFFER-HANDLE (THIS-OBJECT:EntityTable) .

        hBuffer:TABLE-HANDLE:TRACKING-CHANGES = TRUE .
        hBuffer:BUFFER-CREATE() .

        ASSIGN roCreateRowid = hBuffer:ROWID .

        /* Assign ForeignFields */
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN DO:
            DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:ForeignFields) BY 2:
                ASSIGN
                    cField = ENTRY(i + 1, THIS-OBJECT:ForeignFields)
                    hField = hBuffer:BUFFER-FIELD (ENTRY(NUM-ENTRIES(cField, ".":U), cField, ".":U))
                    cValue = oSmartDataSource:GetFieldValues (ENTRY(i, THIS-OBJECT:ForeignFields))
                    NO-ERROR .

            IF VALID-HANDLE (hField) THEN
                hField:BUFFER-VALUE = cValue .

            END.
        END.

        /* Release buffers of other tables */
        DO i = 1 TO THIS-OBJECT:QueryHandle:NUM-BUFFERS:
            IF THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE(i):NAME <> THIS-OBJECT:EntityTable
                THEN THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE(i):BUFFER-RELEASE () .
        END.

        THIS-OBJECT:QueryHandle:CREATE-RESULT-LIST-ENTRY() .

        args:Created = TRUE .

        RETURN.

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
        DEFINE VARIABLE cQueryString          AS CHARACTER                   NO-UNDO.

        ASSIGN iPositionBeforeDelete = THIS-OBJECT:BindingSource:Position.

        ASSIGN hBuffer = THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1) .

        IF NOT hBuffer:AVAILABLE THEN
            UNDO, THROW NEW AppError (SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_DeleteNoRecordError, 0).

        SetTrackingChanges (TRUE) .

        QueryHelper:GetCurrentRowids (THIS-OBJECT:QueryHandle, OUTPUT rRowids) .

        /* Mike Fechner, Consultingwerk Ltd. 17.01.2010
           BeforeDeleteRecord event */
        eBefore = NEW BeforeDeleteRecordEventArgs (THIS-OBJECT:EntityName,
                                                   THIS-OBJECT:EntityTable,
                                                   hBuffer,
                                                   THIS-OBJECT:QueryHandle) .

        OnBeforeDeleteRecord (eBefore) .

        IF eBefore:Cancel THEN DO:
            IF eBefore:CancelMessage > "":U THEN
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException (eBefore:CancelMessage, 0) .
            ELSE
                UNDO, THROW NEW Consultingwerk.Exceptions.CancelException () .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 29.07.2010
           During the deletion the record is removed from the AFTER-TABLE. When
           there is an (validation) error from the backend, the row needs to
           be recreated. This causes the ROWID to change. We need to find the
           record using the primary unique key values. When those don't exist,
           we build a querystring containing all field values. */
        cQueryString = Consultingwerk.Util.BufferHelper:UniqueFindPredicate (hBuffer) .
        IF cQueryString > "":U THEN .
        ELSE cQueryString = Consultingwerk.Util.BufferHelper:AllFieldsFindPredicate (hBuffer) .

        hBuffer:BUFFER-DELETE () .

        DO ON ERROR UNDO, THROW:
            SubmitChanges () .

            CATCH submiterr AS Progress.Lang.Error :
                hBuffer:FIND-FIRST (cQueryString) .

                THIS-OBJECT:QueryHandle:QUERY-OPEN () .
                THIS-OBJECT:BindingSource:RefreshAll() .
                THIS-OBJECT:QueryHandle:REPOSITION-TO-ROWID (hBuffer:ROWID) .

                IF iPositionBeforeDelete < THIS-OBJECT:BindingSource:Count THEN DO:
                    THIS-OBJECT:BindingSource:Position = iPositionBeforeDelete .

                    OnAfterQueryPositionChanged (System.EventArgs:Empty) .
                END.
                ELSE DO:
                    THIS-OBJECT:BindingSource:Position = THIS-OBJECT:BindingSource:Count - 1 .

                    OnAfterQueryPositionChanged (System.EventArgs:Empty) .
                END.

                UNDO, THROW submiterr .
            END CATCH.
        END.

        THIS-OBJECT:QueryHandle:QUERY-OPEN () .
        THIS-OBJECT:BindingSource:RefreshAll() .

        IF iPositionBeforeDelete < THIS-OBJECT:BindingSource:Count THEN DO:
            THIS-OBJECT:BindingSource:Position = iPositionBeforeDelete .

            OnAfterQueryPositionChanged (System.EventArgs:Empty) .
        END.
        ELSE DO:
            THIS-OBJECT:BindingSource:Position = THIS-OBJECT:BindingSource:Count - 1 .

            OnAfterQueryPositionChanged (System.EventArgs:Empty) .
        END.

        OnAfterDeleteRecord (NEW AfterDeleteRecordEventArgs (THIS-OBJECT:EntityName,
                                                             THIS-OBJECT:EntityTable,
                                                             hBuffer,
                                                             THIS-OBJECT:QueryHandle)) .

        /* Mike Fechner, Consultingwerk Ltd. 12.08.2011
           When the position was 0 before the delete and is 0 after the
           delete we will have to invoke the ParentPositionChanged event in
           the linked SmartDataAdapter classes by using the InvokeParentPositionChanged
           method */
        IF THIS-OBJECT:BindingSource:Position = 0 AND
           iPositionBeforeDelete = 0 THEN
           THIS-OBJECT:InvokeParentPositionChanged (THIS-OBJECT,
                                                    System.EventArgs:Empty) .

        FINALLY:
            /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
               Turn on Tracking Changes even in case of error */
            SetTrackingChanges (TRUE) .
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deregister a SmartGroupCreateTarget with the SmartGroupCreateSource

        Notes:   Enforced by Interface ISmartGroupCreateSource.
        @param poSmartGroupCreateTarget The ISmartGroupCreateTarget to deregister
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartGroupCreateTarget (poSmartGroupCreateTarget AS ISmartGroupCreateTarget):

        THIS-OBJECT:SmartGroupCreateTargets:Remove (poSmartGroupCreateTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Evaluates the QueryString based on the Parent Filter Fields
        Notes:   Used during the ParentPositionChanged event handler but also at
                 the end of the UpdateRow method to handle situations in a
                 GroupCreate link when the parent record has been created and the
                 final primary key values in the parent table are changed during
                 saving the record to the database
        @return The parent filter query string
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER EvaluateParentQuery ():

        DEFINE VARIABLE hDataSource        AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hBuffer            AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hService           AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cValue             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cForeignFieldQuery AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFetchQueryString  AS CHARACTER NO-UNDO INIT ? .
        DEFINE VARIABLE i                  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cList              AS CHARACTER NO-UNDO.

        IF NOT VALID-OBJECT (oSmartDataSource) THEN
            UNDO, THROW NEW EvaluateParentQueryException ("Unable to evaluate parent query string. There is no SmartDataSource."{&TRAN}, 0) .

        /* Marko Rüterbories, Consultingwerk Ltd. 26.04.2010
           Extended validation code to make sure that the oSmartDataSource has a
           valid BindingSource object to prevent error messages. */
        IF NOT VALID-OBJECT (oSmartDataSource:BindingSource) THEN
            UNDO, THROW NEW EvaluateParentQueryException ("Unable to evaluate parent query string. The SmartDataSource does not have a BindingSource."{&TRAN}, 0) .

        ASSIGN hDataSource = oSmartDataSource:BindingSource:Handle .

        IF NOT VALID-HANDLE(hDataSource) THEN
            UNDO, THROW NEW EvaluateParentQueryException ("Unable to evaluate parent query string. The SmartDataSource's handle is not attached to an ABL data object."{&TRAN}, 0) .

        /* Mike Fechner, Consultingwerk Ltd. 20.06.2013
           Support added for a DataSource with a BindingSource 
           that is working with a Buffer, not a Query or ProDataset 
           as the Handle */
        IF hDataSource:TYPE = WidgetTypeEnum:Buffer THEN
            hBuffer = hDataSource .
        ELSE
            hBuffer = hDataSource:GET-BUFFER-HANDLE(1) .

        /* Mike Fechner, Consultingwerk Ltd. 28.03.2014
           SCL-211: Validation of the ForeignField settings */
        IF NOT lHasValidatedForeignFields THEN 
            THIS-OBJECT:ValidateForeignFields () .
        
        IF hBuffer:AVAILABLE THEN DO:
            /* Request Foreign Field values,
               note ForeignFields stores the fields in the order
               parentfield1,childfield1,parentfield2,childfield2,.... */
            DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:ForeignFields) BY 2:
                ASSIGN cValue = oSmartDataSource:GetFieldValues (ENTRY(i, THIS-OBJECT:ForeignFields)) NO-ERROR .

                cForeignFieldQuery = cForeignFieldQuery + (IF i > 1 THEN " AND ":U ELSE " ":U) +
                                     THIS-OBJECT:EntityTable + ".":U + ENTRY(i + 1, THIS-OBJECT:ForeignFields) + " = ":U + QUOTER(cValue) .
            END.

            ASSIGN cFetchQueryString = ENTRY (1, THIS-OBJECT:QueryString, CHR(1)) .

            IF cFetchQueryString > "":U THEN DO:
                IF cFetchQueryString MATCHES "* WHERE *":U THEN
                    cFetchQueryString = cFetchQueryString + " AND ":U + cForeignFieldQuery .
                ELSE
                    cFetchQueryString = cFetchQueryString + " WHERE ":U + cForeignFieldQuery .
            END.
            ELSE cFetchQueryString = SUBSTITUTE ("PRESELECT EACH &1 WHERE &2":U,
                                                 THIS-OBJECT:EntityTable, cForeignFieldQuery) .

            IF NUM-ENTRIES (THIS-OBJECT:FetchQueryString, CHR(1)) > 1 THEN
                /* Mike Fechner, Consultingwerk Ltd. 08.05.2010
                   Bug 2281: OpenEdge 10.2B Service Pack 01, workaround for new
                   compiler message: "The target of the ENTRY statement must be
                   a field or an unqualified reference to a data member or
                   variable"
                   Using a temporary variable to replace the first entry of the list */
                ASSIGN cList                        = THIS-OBJECT:FetchQueryString
                       ENTRY(1, cList, CHR(1))      = cFetchQueryString
                       THIS-OBJECT:FetchQueryString = cList .
            ELSE
                THIS-OBJECT:FetchQueryString = cFetchQueryString .
        END.
        ELSE 
            UNDO, THROW NEW NoParentRecordAvailableException ("No parent record is available."{&TRAN}, 0) .

        RETURN cFetchQueryString .

        CATCH err AS Progress.Lang.SysError:
        	UNDO, THROW NEW EvaluateParentQueryException (err, "Unable to evaluate parent query string. "{&TRAN}, 0) . 	
        END CATCH.

    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Searches for a linked SmartDatasetAdpater with the given entity table
	    Notes:   Also returns THIS-OBJECT, when THIS-OBJECT is using the given EntityTable
	    @param pcEntityTable The Entity Table to locate
	    @return The reference to the lined SmartDatasetAdapter for the given entity table
	------------------------------------------------------------------------------*/
	METHOD PUBLIC SmartDatasetAdapter FindLinkedDatasetAdapterForEntityTable (pcEntityTable AS CHARACTER):
		
		DEFINE VARIABLE oAdapter AS SmartDatasetAdapter NO-UNDO . 
		
		{Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcEntityTable "'Entity Table':U"} .
		
		IF THIS-OBJECT:EntityTable = pcEntityTable THEN 
		    RETURN THIS-OBJECT . 

        {Consultingwerk/foreachABL.i ISmartDataTarget oSmartDataTarget in THIS-OBJECT:SmartDataTargets}
        
            IF TYPE-OF (oSmartDataTarget, SmartDatasetAdapter) THEN DO:
                
                oAdapter = CAST (oSmartDataTarget, SmartDatasetAdapter) . 
                
                IF oAdapter:EntityTable = pcEntityTable THEN 
                    RETURN oAdapter . 
                
                oAdapter = oAdapter:FindLinkedDatasetAdapterForEntityTable (pcEntityTable) .
                
                IF VALID-OBJECT (oAdapter) THEN 
                    RETURN oAdapter . 
            END . 
        END.
        
		RETURN ? .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Finds a row in the EntityTable and locates the BindingSource
                 to that row
        Notes:   The record is actually located using a dynamic query instead of a
                 FIND so that sorting is also supported (typically sorting should
                 be passed in with the same value as the QuerySort property of
                 this SmartBusinessEntityAdapter instance
                 Returns the FIRST record that matches the criteria
        @param pcFindString The find criteria, should not contain a BY phrase, as the Sort order of the Adapter will be used.
        @return Logical value indicating if a record was found
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FindRowWhere (pcFindString AS CHARACTER):

        DEFINE VARIABLE hFindBuffer AS HANDLE            NO-UNDO .
        DEFINE VARIABLE hQuery      AS HANDLE            NO-UNDO .
        DEFINE VARIABLE lResult     AS LOGICAL           NO-UNDO .
        DEFINE VARIABLE oTokenizer  AS Tokenizer         NO-UNDO .
        DEFINE VARIABLE oList       AS CharacterList     NO-UNDO .
        DEFINE VARIABLE oBrowser    AS ISmartDataBrowser NO-UNDO . 

        IF NOT VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN
            UNDO, THROW NEW AppError ("Unable to use FindRowWhere before the Query is valid.", 0) .

        IF NOT VALID-HANDLE (THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1)) THEN
            UNDO, THROW NEW AppError ("Unable to use FindRowWhere when the Query buffer is not valid.", 0) .

        /* Mike Fechner, Consultingwerk Ltd. 02.05.2012
           The Find String should not contain a BY phrase, as the Sort order of the
           Adapter will be used */
        oTokenizer = NEW Consultingwerk.Tokenizer () .
        oList = oTokenizer:Tokenize (pcFindString, TRUE) .

        IF oList:ContainsValue ("BY":U) THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcFindString":U,
                                                            pcFindString,
                                                            "Consultingwerk.SmartComponents.Base.SmartDatasetAdapter":U) .

        CREATE BUFFER hFindBuffer FOR TABLE (THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1)) .

        DO ON ERROR UNDO, THROW:
            ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hFindBuffer,
                                                             SUBSTITUTE ("FOR EACH &1 &2 &3":U,
                                                                         hFindBuffer:NAME,
                                                                         pcFindString,
                                                                         THIS-OBJECT:QuerySort)) .

            ASSIGN lResult = hFindBuffer:AVAILABLE .
        END.

        IF lResult = FALSE THEN
            RETURN FALSE .

        THIS-OBJECT:QueryHandle:REPOSITION-TO-ROWID (hFindBuffer:ROWID) .
        
        /* Mike Fechner, Consultingwerk Ltd. 03.06.2014
           GET-NEXT only required when not yet at the repositioned record */
        IF NOT THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1):AVAILABLE OR 
           THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1):ROWID <> hFindBuffer:ROWID THEN 
            THIS-OBJECT:QueryHandle:GET-NEXT () .
        
        oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .
        
        IF VALID-OBJECT (oBrowser) THEN
            oBrowser:ScrollToCurrentRow (FALSE, FALSE) .

        RETURN TRUE .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
            GarbageCollectorHelper:DeleteObject (hFindBuffer) .
            GarbageCollectorHelper:DeleteObject (oTokenizer) .
            GarbageCollectorHelper:DeleteObject (oList) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Finds the last a row in the EntityTable and locates the BindingSource
                 to that row
        Notes:   The record is actually located using a dynamic query instead of a
                 FIND so that sorting is also supported (typically sorting should
                 be passed in with the same value as the QuerySort property of
                 this SmartBusinessEntityAdapter instance
                 Returns the LAST record that matches the criteria
        @param pcFindString The find criteria, should not contain a BY phrase, as the Sort order of the Adapter will be used.
        @return Logical value indicating if a record was found
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FindLastRowWhere (pcFindString AS CHARACTER):

        DEFINE VARIABLE hFindBuffer AS HANDLE            NO-UNDO .
        DEFINE VARIABLE hQuery      AS HANDLE            NO-UNDO .
        DEFINE VARIABLE lResult     AS LOGICAL           NO-UNDO .
        DEFINE VARIABLE oTokenizer  AS Tokenizer         NO-UNDO .
        DEFINE VARIABLE oList       AS CharacterList     NO-UNDO .
        DEFINE VARIABLE oBrowser    AS ISmartDataBrowser NO-UNDO . 

        IF NOT VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN
            UNDO, THROW NEW AppError ("Unable to use FindRowWhere before the Query is valid.", 0) .

        IF NOT VALID-HANDLE (THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1)) THEN
            UNDO, THROW NEW AppError ("Unable to use FindRowWhere when the Query buffer is not valid.", 0) .

        /* Mike Fechner, Consultingwerk Ltd. 02.05.2012
           The Find String should not contain a BY phrase, as the Sort order of the
           Adapter will be used */
        oTokenizer = NEW Consultingwerk.Tokenizer () .
        oList = oTokenizer:Tokenize (pcFindString, TRUE) .

        IF oList:ContainsValue ("BY":U) THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcFindString":U,
                                                            pcFindString,
                                                            "Consultingwerk.SmartComponents.Base.SmartDatasetAdapter":U) .

        CREATE BUFFER hFindBuffer FOR TABLE (THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1)) .

        DO ON ERROR UNDO, THROW:
            ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hFindBuffer,
                                                             SUBSTITUTE ("FOR EACH &1 &2 &3":U,
                                                                         hFindBuffer:NAME,
                                                                         pcFindString,
                                                                         THIS-OBJECT:QuerySort)) .

            hQuery:GET-LAST () .

            ASSIGN lResult = hFindBuffer:AVAILABLE .
        END.

        IF lResult = FALSE THEN
            RETURN FALSE .

        THIS-OBJECT:QueryHandle:REPOSITION-TO-ROWID (hFindBuffer:ROWID) .
        
        /* Mike Fechner, Consultingwerk Ltd. 03.06.2014
           GET-NEXT only required when not yet at the repositioned record */
        IF NOT THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1):AVAILABLE OR 
           THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1):ROWID <> hFindBuffer:ROWID THEN 
            THIS-OBJECT:QueryHandle:GET-NEXT () .
        
        oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .
        
        IF VALID-OBJECT (oBrowser) THEN 
            oBrowser:ScrollToCurrentRow (FALSE) .

        RETURN TRUE .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
            GarbageCollectorHelper:DeleteObject (hFindBuffer) .
            GarbageCollectorHelper:DeleteObject (oTokenizer) .
            GarbageCollectorHelper:DeleteObject (oList) .
        END FINALLY.

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a key identifying the current record 
        Notes:   Returns either the SmartRecordKey value if populated or the result of the 
                 BufferHelper:UniqueFindPredicate function
        @return The single character key that represents the current record
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetCurrentRecordKey ():
		
		DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
		
		{Consultingwerk/Assertion/HandleAssert/ValidHandle.i THIS-OBJECT:QueryHandle """Query Handle"":U"} .
		
        ASSIGN hBuffer = THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hBuffer """Query Buffer"":U"} .
        
        IF BufferHelper:HasField (hBuffer, "SmartRecordKey":U) THEN 
            RETURN hBuffer::SmartRecordKey .
		
		RETURN BufferHelper:UniqueFindPredicate (hBuffer) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Current Rowids
        Notes:   Neeeds to be a Method instead of a Property Get to return an
                 undimensioned Array
        @return The Array of the current query ROWID's
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ROWID EXTENT GetCurrentRowid ():

        RETURN roCurrentRowids.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CHR(1) delimited list of field values from the
                 DataSource
        Notes:   The fields parameter is a CHR(1) delimited list of field names. 
                 Unqualified field names (CustNum) must exist in the first buffer 
                 of the data source (e.g. Entity Table), qualified field names are 
                 expected in the named buffer (eCustomer.CustNum)
                 Override to GetFieldValues method. Make sure the Query is properly
                 positioned before calling SUPER method
        @param pcFields The comma delimited list of fields to return
        @return The CHR(1) delimited list of field values
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER GetFieldValues (INPUT pcFields AS CHARACTER):

        RepositionCurrentRow () .

        RETURN SUPER:GetFieldValues (pcFields).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the first table of the Adapter provides a field 
                 SmartCopiedFrom 
        Notes:   Used by the SmartViewerControl and SmartUpdatableBrowser during copy
        @return Logical value indicating if the first table of the Adapter provides a field SmartCopiedFrom 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL HasCopiedFromField ():
		
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i THIS-OBJECT:QueryHandle """Query Handle"":U"} .
        
        ASSIGN hBuffer = THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hBuffer """Query Buffer"":U"} .
        
        RETURN BufferHelper:HasField (hBuffer, "SmartCopiedFrom":U) . 

	END METHOD .

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
        Purpose: Raises the AssignForeignFields
        Notes:   
        @param e The AssignForeignFieldsEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAssignForeignFields (e AS AssignForeignFieldsEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "AssignForeignFields":U) .
     
        THIS-OBJECT:AssignForeignFields:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the AssignKeyFieldError
        Notes:   
        @param e The AssignKeyFieldErrorEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAssignKeyFieldError (e AS AssignKeyFieldErrorEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "AssignKeyFieldError":U) .            
    
        THIS-OBJECT:AssignKeyFieldError:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the AssignKeyFields event
        Notes:
        @param e The AssignKeyFieldsEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAssignKeyFields (e AS AssignKeyFieldsEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "AssignKeyFields":U) .

        THIS-OBJECT:AssignKeyFields:PUBLISH (THIS-OBJECT, e) .

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
        Purpose: Raises the BeforeCreatingRecord
        Notes:   
        @param e The Consultingwerk.CancelableEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeCreatingRecord (e AS Consultingwerk.CancelableEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeCreatingRecord":U) .
     
        THIS-OBJECT:BeforeCreatingRecord:Publish (THIS-OBJECT, e) .
    
    END METHOD .

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
        Purpose: Raises the DatasetControllerChanged event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnDatasetControllerChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:DatasetControllerChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the NonUpdatableColumnsChanged event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnNonUpdatableColumnsChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:NonUpdatableColumnsChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: This Method is invoked by the ParentPositionChangedBase Event Handler
                 of the SmartDataAdapters base class when the PositionChanged Event of
                 the SmartDataSources BindingSource is fired.
        Notes:   Refresh Data when a new line is selected inside the Parent
                 SmartDataSource.
        @param sender The reference to the Object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID ParentPositionChanged (sender AS System.Object,
                                                       e      AS System.EventArgs)  :

        DEFINE VARIABLE cFetchQueryString AS CHARACTER NO-UNDO INIT ? .

        DO ON ERROR UNDO, THROW:
            cFetchQueryString = THIS-OBJECT:EvaluateParentQuery() .
            
            CATCH evaerr AS EvaluateParentQueryException:
            	ASSIGN cFetchQueryString = ? . 	
            END CATCH.
        END.
        
        IF cFetchQueryString > "":U THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 14.04.2011
               When the current (new) FetchQueryString equals to the last used
               FetchQueryString, we would be reading the same data again -
               which is of no use to the user and would only cause additional
               traffic.
               This might have happened on the grand-child DataAdapter of a
               SmartBusinessEntityAdapter changing the current record. */
            IF cPreviousFetchQueryString <> cFetchQueryString THEN
            DO ON ERROR UNDO, THROW:

                THIS-OBJECT:RetrieveData() .

                FINALLY:
                    ASSIGN cPreviousFetchQueryString = cFetchQueryString .
                END FINALLY.
            END.
        END.
        ELSE 
            CloseQuery () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessageBox (err) .
        END CATCH.

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

        IF VALID-HANDLE (THIS-OBJECT:QueryHandle) AND NOT THIS-OBJECT:QueryHandle:QUERY-OFF-END THEN
            QueryHelper:GetCurrentRowids (THIS-OBJECT:QueryHandle, OUTPUT roCurrentRowids) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Abstract method used to prepare the BindingSource:Handle object
                 (either a QUERY or a ProDataset).
        Notes:   Implemented in the SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 Called from RetrieveData or CreateRecord (when called before RetrieveData)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ABSTRACT VOID PrepareDataBinding ().

    /*------------------------------------------------------------------------------
        Purpose: Raises the error when the transaction connect be commited                                                                        
        Notes:
        @param poTarget The ISmartTableIOTarget instance that caused the error                                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RaiseUnableToCommitError (poTarget AS ISmartTableIOTarget):
        
        DEFINE VARIABLE cTargetName AS CHARACTER NO-UNDO.
        
        IF VALID-OBJECT (poTarget) THEN 
            ASSIGN cTargetName = poTarget:GetClass():TypeName .
        
        UNDO, THROW NEW UnableToCommitTransactionException (cTargetName, poTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the error when the transaction connect be rejected                                                                        
        Notes:                                                                     
        @param poTarget The ISmartTableIOTarget instance that caused the error   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RaiseUnableToRejectError (poTarget AS ISmartTableIOTarget):

        DEFINE VARIABLE cTargetName AS CHARACTER NO-UNDO.
        
        IF VALID-OBJECT (poTarget) THEN 
            ASSIGN cTargetName = poTarget:GetClass():TypeName .
        
        UNDO, THROW NEW UnableToRejectTransactionException (cTargetName, poTarget) .

    END METHOD.
    
	/*------------------------------------------------------------------------------
	    Purpose: Refreshes the SmartDatasetAdapter
	    Notes:   Reopens the client side query (does not retrieve data from the backend)
	             and optionally repositions to the record that is current before 
	             the refrsh
	    @param plRepositionToCurrent Logical value indicating if we the method should reposition to the row that was current before re-opening the query after the query has been reopened
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Refresh (plRepositionToCurrent AS LOGICAL):
		
        DEFINE VARIABLE roRowid AS ROWID  NO-UNDO . 
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO .
		
		ASSIGN roRowid = ? . 
		
		IF plRepositionToCurrent THEN DO: 
		    ASSIGN hBuffer = THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1) .
		
		    IF hBuffer:AVAILABLE THEN  
		        roRowid = hBuffer:ROWID . 
		END.

        THIS-OBJECT:QueryHandle:QUERY-OPEN () .
        THIS-OBJECT:BindingSource:RefreshAll () .

        IF plRepositionToCurrent AND 
           roRowid <> ? THEN DO ON ERROR UNDO, THROW:
            THIS-OBJECT:RepositionToRowid (roRowid) .
            
            CATCH err AS Progress.Lang.Error:
                THIS-OBJECT:BindingSource:Position = 0 .
            END CATCH.
        END.        

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Reopens the query and refreshes the BindingSource after a transaction
	             has been rejected
	    Notes:
	    @param proDatasetRowids The ROWID[] with all ROWID's of the ProDataset to reposition to 
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RefreshAfterRejectTranscation (proDatasetRowids AS ROWID EXTENT):
		
		DEFINE VARIABLE iBufferIndex AS INTEGER NO-UNDO.
		
		ASSIGN iBufferIndex = DatasetHelper:GetBufferIndex (THIS-OBJECT:DataSet, 
		                                                    THIS-OBJECT:EntityTable) .
		
        THIS-OBJECT:QueryHandle:QUERY-OPEN () .
        THIS-OBJECT:BindingSource:RefreshAll () .

        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:RepositionToRowid (proDatasetRowids[iBufferIndex]) .
            
            CATCH err AS Progress.Lang.Error:
                THIS-OBJECT:BindingSource:Position = 0 .
            END CATCH.
        END.        
        
        /* Iterate linked SmartDatasetChildAdapters as they are also
           affected by the RejectTransction */
        {Consultingwerk/foreachABL.i ISmartDataTarget oDataTarget in this-object:SmartDataTargets}
            IF TYPE-OF (oDataTarget, SmartDatasetChildAdapter) THEN 
                CAST (oDataTarget, SmartDatasetChildAdapter):RefreshAfterRejectTranscation (proDatasetRowids) .
        END.    

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Register a SmartGroupCreateTarget with the SmartGroupCreateSource
                 This Method is executed every time a new SmartGroupCreateSource is
                 registered to the corresponding Property of a Class implementing the
                 Interface ISmartGroupCreateSource.
        Notes:   Enforced by Interface ISmartGroupCreateSource.
        @param poSmartGroupCreateTarget The ISmartGroupCreateTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartGroupCreateTarget (poSmartGroupCreateTarget AS ISmartGroupCreateTarget):

        THIS-OBJECT:SmartGroupCreateTargets:Add (poSmartGroupCreateTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Repositions to the updated record after update
        Notes:   
        @param proRowid The rowid of the record to position to
        @param plCreated Logical value indicating if the current record was created
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID RepositionAfterUpdateRow (proRowid AS ROWID,
	                                                plCreated AS LOGICAL):
		
        DEFINE VARIABLE eUnableToRepositionEventArgs AS UnableToRepositionAfterUpdateRowEventArgs NO-UNDO . 
		
		THIS-OBJECT:QueryHandle:REPOSITION-TO-ROWID (proRowid) .

        CATCH err AS Progress.Lang.Error:
            
            /* Mike Fechner, Consultingwerk Ltd. 01.11.2014
               SCL-500: Event raised to allow custom behavior when we are no longer able to 
                        reposition to the updated record */
            eUnableToRepositionEventArgs = NEW UnableToRepositionAfterUpdateRowEventArgs (THIS-OBJECT:QueryHandle, 
                                                                                          proRowid) .
            
            THIS-OBJECT:OnUnableToRepositionAfterUpdateRow (eUnableToRepositionEventArgs) .
            
            IF VALID-OBJECT (eUnableToRepositionEventArgs) AND eUnableToRepositionEventArgs:Handled = FALSE THEN 
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                     (SUBSTITUTE ("Error during repositioning to updated record.&1&2"{&TRAN},
                                  Consultingwerk.Environment:NewLine,
                                  err:GetMessage (1)),
                      "Error during update row"{&TRAN},
                      SUBSTITUTE ("Current query string: &1&2Reposition ROWID: &3"{&TRAN},
                                  THIS-OBJECT:QueryHandle:PREPARE-STRING,
                                  Consultingwerk.Environment:NewLine,
                                  proRowid),
                      Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Repositions the Adapters Query to the current row
        Notes:   Based on a previously stored row as some ProDataset methods invalidate
                 the current record buffers
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RepositionCurrentRow ():

        DEFINE VARIABLE oError AS Progress.Lang.AppError NO-UNDO . 

        IF NOT VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN DO:
            oError = NEW Consultingwerk.Exceptions.InvalidHandleException ("Query":U) .
            oError:AddMessage ("Unable to reposition to current row before the Query has been created."{&TRAN}, 0) .
            UNDO, THROW oError .
        END . 

        /* Mike Fechner, Consultingwerk Ltd. 11.06.2015
           SCL-849 - cannot reposition when Query is not open. */
        IF NOT THIS-OBJECT:QueryHandle:IS-OPEN THEN 
            RETURN .

        IF NOT THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE(1):AVAILABLE THEN DO ON ERROR UNDO, THROW:
            QueryHelper:RepositionToRowidArray (THIS-OBJECT:QueryHandle, roCurrentRowids) .
            THIS-OBJECT:QueryHandle:GET-NEXT () .

            /* Mike Fechner, Consultingwerk Ltd. 02.10.2014
               Error 7331 may indicate that rowids to no longer match the query. In
               case of a joined query, if is fair to assume, that we can attempt to 
               reposition only the first query buffer (the other buffers typically
               joined n:1 from the first buffer) */
            CATCH syserr AS Progress.Lang.SysError:
                IF syserr:GetMessageNum (1) = 7331 THEN DO:
                    THIS-OBJECT:QueryHandle::REPOSITION-TO-ROWID (roCurrentRowids[1])  .
                    THIS-OBJECT:QueryHandle:GET-NEXT () .
                END.
                ELSE
                    UNDO, THROW syserr.
            END CATCH.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2014
           Provide ROWID Information with Error */
        CATCH err AS Progress.Lang.Error :
            IF err:GetMessageNum(1) = 7316 OR err:GetMessageNum(1) = 7331 THEN DO:

               oError = NEW Progress.Lang.AppError (err:GetMessage(1), err:GetMessageNum(1)) .

               IF EXTENT (roCurrentRowids) > 0 THEN DO:
                   DEFINE VARIABLE i AS INTEGER NO-UNDO.
                   DO i = 1 TO EXTENT (roCurrentRowids):
                       oError:AddMessage (SUBSTITUTE ("Rowid[&1]: &2":U, i, roCurrentRowids[i]), 0) .
                   END.
               END.
               ELSE
                   oError:AddMessage ("Invalid rowid[]"{&TRAN}, 0) .

                UNDO, THROW oError .
            END.
            ELSE 
                UNDO, THROW err . 

        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Repositions to a Rowid Array
        Notes:
        @param prRowid The Array of ROWID's to reposition to
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID RepositionToRowid (prRowid AS ROWID EXTENT):

        SUPER:RepositionToRowid (prRowid).

        Consultingwerk.Util.QueryHelper:GetCurrentRowids (THIS-OBJECT:QueryHandle,
                                                          OUTPUT roCurrentRowids) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the data in this ISmartGroupCreateSource related updating
                 ISmartDataTarget so that before saving a new child record, that
                 parent record is created
        Notes:
        @param poSmartGroupCreateTarget The ISmartGroupCreateTarget that invoked this Save operation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveFromGroupCreateTarget (poSmartGroupCreateTarget AS ISmartGroupCreateTarget):

        DEFINE VARIABLE oTableIOTarget     AS ISmartTableIOTarget     NO-UNDO .
        DEFINE VARIABLE oGroupCreateSource AS ISmartGroupCreateSource NO-UNDO .
        DEFINE VARIABLE oDataTarget        AS ISmartDataTarget        NO-UNDO .
        DEFINE VARIABLE cForeignFields     AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE cKeyFields         AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER                 NO-UNDO .

        tableIOTargetLoop: DO i = 1 TO THIS-OBJECT:SmartDataTargets:Count:
            oDataTarget = THIS-OBJECT:SmartDataTargets:GetItem (i) .

            IF TYPE-OF (oDataTarget, ISmartTableIOTarget) THEN DO:

                ASSIGN oTableIOTarget = CAST (oDataTarget, ISmartTableIOTarget) .

                IF VALID-OBJECT (oTableIOTarget:SmartTableIOSource) THEN
                    LEAVE tableIOTargetLoop .
                ELSE
                    ASSIGN oTableIOTarget = ? .
            END.
        END.

        IF NOT VALID-OBJECT (oTableIOTarget) THEN
            UNDO, THROW NEW AppError ("Unable to save parent record when there is no active TableIOTarget", 0) .

        oTableIOTarget:SaveChanges () .

    END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Recursively invokes SaveChanges in all DataTargets that are currently 
	             updating
	    Notes:   Throws an Exception, when of the DataTargets remains updating
	             Used during CommitChanges
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SaveInDataTargets ():
		
        DEFINE VARIABLE i                    AS INTEGER                               NO-UNDO.
        DEFINE VARIABLE oTarget              AS Progress.Lang.Object                  NO-UNDO .
        DEFINE VARIABLE oTableIOTarget       AS ISmartTableIOTarget                   NO-UNDO .  
		
        DO i = 1 TO THIS-OBJECT:SmartDataTargets:Count:

            ASSIGN oTarget = THIS-OBJECT:SmartDataTargets:GetItem (i) .
            
            IF VALID-OBJECT (oTarget) AND TYPE-OF (oTarget, ISmartTableIOTarget) THEN DO:
            
                oTableIOTarget = CAST (oTarget, ISmartTableIOTarget) .
                
                IF oTableIOTarget:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
                    oTableIOTarget:SaveChanges () . 
             
                IF oTableIOTarget:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
                    THIS-OBJECT:RaiseUnableToCommitError (oTableIOTarget) .                                    
            END.
            ELSE IF VALID-OBJECT (oTarget) AND TYPE-OF (oTarget, SmartDatasetAdapter) THEN 
                CAST (oTarget, SmartDatasetAdapter):SaveInDataTargets () .  
        END.     
        
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Overridable Setter for QuerySort property
        Notes:
        @param arg The new value for the QuerySort property
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ABSTRACT VOID SetQuerySort (arg AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Assigns if the SmartDatasetAdapter is currently saving data
        Notes:
        @param plSaving Logical value indicating if the SmartDatasetAdapter is currently saving data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ABSTRACT VOID SetSavingData (plSaving AS LOGICAL).

    /*------------------------------------------------------------------------------
        Purpose: Sets the tracking changes property of all temp-tables
        Notes:   Tables without a valid BEFORE-BUFFER will not be handled. For static
                 temp-tables this is typically understood as read-only temp-tables
                 (and the attempt to enable TRACKING-CHANGES would cause a runtime
                 error). Dynamic temp-tables that have never been set to
                 TRACKING-CHANGES = TRUE will never have a valid BEFORE-BUFFER. As
                 such it's expected that dynamic temp-tables that should be updatable
                 will be set to TRACKING-CHANGES briefly in their factory method.
        @param plTrackingChanges LOGICAL value indicating if tracking changes should be turned on or off
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetTrackingChanges (plTrackingChanges AS LOGICAL):

        DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer       AS HANDLE    NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
           Don't execute before Dataset is valid (might be before first call to RetrieveData) */
        IF NOT VALID-HANDLE (THIS-OBJECT:Dataset) THEN RETURN .

        DatasetHelper:SetTrackingChanges (THIS-OBJECT:Dataset, plTrackingChanges) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:  Stores the source record key information with the new record (copy)
        Notes:   
        @param pcSourceRecordKey The record key of the source record to store with the copied record
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID StoreCopiedFrom (pcSourceRecordKey AS CHARACTER):
		
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i THIS-OBJECT:QueryHandle """Query Handle"":U"} .
        
        ASSIGN hBuffer = THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE (1) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hBuffer """Query Buffer"":U"} .
    
        IF BufferHelper:HasField (hBuffer, "SmartCopiedFrom":U) THEN 
            ASSIGN hBuffer::SmartCopiedFrom = pcSourceRecordKey . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves the Dataset changes to the backend using the parent
                 SmartDataAdapter
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SubmitChanges ():

        IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) OR
           NOT TYPE-OF (THIS-OBJECT:SmartDataSource, ISupportsSubmitChanges) THEN

           UNDO, THROW NEW AppError ("This SmartDataAdapter requries a SmartDataSource that support SubmitChanges to be able to submit changes!", 0) .

        CAST (THIS-OBJECT:SmartDataSource, ISupportsSubmitChanges):SubmitChanges () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a String representation of this object instance
        Notes:   Userful for logging purposes, contains class name, entity name and entity table
        @return The String representation of the object instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE CHARACTER ToString ():

        RETURN SUBSTITUTE ("&1 &2 &3":U,
                           THIS-OBJECT:EntityName,
                           THIS-OBJECT:EntityTable,
                           THIS-OBJECT:EntityView).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: UpdateRow submits the changes to a buffer into the Database.
                 Called by SmartViewerControl

                 Enforced by Interface ISmartDataSource.
        Notes:   Calls UpdateRow (plAssignFromBindingSource = TRUE) .
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID UpdateRow ():
        THIS-OBJECT:UpdateRow (YES) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: UpdateRow submits the changes to a buffer into the Database.
                 Called by SmartViewerControl, SmartUpdatableBrowser
                 Data is updated into the Dataset from the BindingSource. After this
                 the Datasetchanges are submitted to the OERA Backend, merged into
                 the Dataset and the BindingSource gets refreshed.
        Notes:   Do NOT call SUPER:UpdateRow() in the overriding method.
        @param plAssignFromBindingSource Logical flag indicating of the value from the attached BindingSource should be assigned to the current record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateRow (plAssignFromBindingSource AS LOGICAL):

        DEFINE VARIABLE hChangesDataset      AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hBuffer              AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hUndoDataset         AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hUndoBuffer          AS HANDLE                       NO-UNDO .

        DEFINE VARIABLE cMessages            AS CHARACTER                    NO-UNDO.

        DEFINE VARIABLE cContext             AS CHARACTER                    NO-UNDO.

        DEFINE VARIABLE rRowids              AS ROWID                        NO-UNDO EXTENT .
        DEFINE VARIABLE i                    AS INTEGER                      NO-UNDO.
        DEFINE VARIABLE j                    AS INTEGER                      NO-UNDO.
        DEFINE VARIABLE rRowid               AS ROWID                        NO-UNDO.
        DEFINE VARIABLE eBeforeUpdate        AS BeforeUpdateRecordEventArgs  NO-UNDO.
        DEFINE VARIABLE eBeforeAssign        AS BeforeAssignRecordEventArgs  NO-UNDO.

        DEFINE VARIABLE iExtent              AS INTEGER                      NO-UNDO.
        DEFINE VARIABLE cKeyFields           AS CHARACTER                    NO-UNDO.

        DEFINE VARIABLE cFetchQueryString    AS CHARACTER                    NO-UNDO INIT ? .
        DEFINE VARIABLE cPreviousQueryString AS CHARACTER                    NO-UNDO .
    
        DEFINE VARIABLE oUpdatingControl     AS System.Windows.Forms.Control NO-UNDO . 
        
        DEFINE VARIABLE oBrowser             AS ISmartDataBrowser            NO-UNDO .  
    
        DEFINE VARIABLE lCreated             AS LOGICAL                      NO-UNDO INIT FALSE .
        
        DEFINE VARIABLE oList                AS CharacterList                NO-UNDO .
        DEFINE VARIABLE iEntry               AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE ex                   AS RecordAlreadyExistsException NO-UNDO .
        
        SetSavingData (TRUE) .
        SetTrackingChanges (TRUE) .

        /* Mike Fechner, Consultingwerk Ltd. 04.07.2009
           GET-CHANGES and REJECT-CHANGES might have invalidated Query position */
        RepositionCurrentRow () .

        /* Update Dataset from BindingSource */
        /* Mike Fechner, Consultingwerk Ltd. 13.07.2009
           Only assign the fields of the EntityTable,
           Assign will assign all fields from all tables. */
        ASSIGN hBuffer = THIS-OBJECT:DATASET:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable) .

        /* Mike Fechner, Consultingwerk Ltd. 29.04.2014
           Fix cases, where (for whatever reason?) the RepositionCurrentRow did
           not reposition to the correct row */
        IF NOT hBuffer:AVAILABLE AND EXTENT (roCurrentRowids) > 0 AND roCurrentRowids[1] <> ? THEN
            hBuffer:FIND-BY-ROWID (roCurrentRowids[1]) .

        eBeforeUpdate = NEW BeforeUpdateRecordEventArgs (THIS-OBJECT:EntityName,
                                                         THIS-OBJECT:EntityTable,
                                                         hBuffer,
                                                         THIS-OBJECT:QueryHandle) .

        OnBeforeUpdateRecord (eBeforeUpdate) .

        IF VALID-OBJECT (eBeforeUpdate) AND eBeforeUpdate:Cancel THEN
            UNDO, THROW NEW CancelException () .

        eBeforeAssign = NEW BeforeAssignRecordEventArgs (THIS-OBJECT:EntityName,
                                                         THIS-OBJECT:EntityTable,
                                                         hBuffer,
                                                         THIS-OBJECT:QueryHandle) .

        OnBeforeAssignRecord (eBeforeAssign) .

        IF VALID-OBJECT (eBeforeAssign) AND eBeforeAssign:Cancel THEN
            UNDO, THROW NEW CancelException () .

        IF plAssignFromBindingSource = TRUE THEN DO ON ERROR UNDO, THROW:
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
                END. /* DO i = 1 TO NUM-ENTRIES (cKeyFields) */  
            END. /* IF hBuffer:ROW-STATE = ROW-CREATED THEN  */            
            
            /* Mike Fechner, Consultingwerk Ltd. 20.08.2015
               SCL-961: We need to keep our own "before image" for this update operation
               in case we need to revert back on on index violation error in the CATCH for 
               ple 132 below */
            ASSIGN hUndoDataset = DatasetHelper:GetDatasetWithCurrentRecord (hBuffer) .
            
            fieldLoop: DO i = 1 TO hBuffer:NUM-FIELDS ON ERROR UNDO, THROW:
                
                /* Mike Fechner, Consultingwerk Ltd. 16.12.2013
                   Don't update the SmartFields in the list */
                IF ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (i):NAME,
                                             "SmartRecordKey,SmartAttachments,SmartComments,SmartCopiedFrom":U) THEN 
                    NEXT .
                
                /* Ability to mark certain Columns as non updatable */
                IF THIS-OBJECT:NonUpdatableColumns > "":U AND
                   ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (i):NAME, THIS-OBJECT:NonUpdatableColumns) THEN
                    NEXT .

                /* When using Exclude Fields, skip those fields */
                IF THIS-OBJECT:BindingSourceIncludeFields = "*":U AND THIS-OBJECT:BindingSourceExcludeFields > "":U THEN
                    IF ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (i):NAME, THIS-OBJECT:BindingSourceExcludeFields) OR
                       ListHelper:EntryIsInList (hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD (i):NAME, THIS-OBJECT:BindingSourceExcludeFields)
                    THEN NEXT.

                /* When using Include Fields, skip those fields */
                IF THIS-OBJECT:BindingSourceIncludeFields <> "*":U AND THIS-OBJECT:BindingSourceIncludeFields > "":U THEN
                    IF NOT ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (i):NAME, THIS-OBJECT:BindingSourceIncludeFields) AND
                       NOT ListHelper:EntryIsInList (hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD (i):NAME, THIS-OBJECT:BindingSourceIncludeFields)
                    THEN NEXT.

                /* Mike Fechner, Consultingwerk Ltd. 15.12.2011
                   Bug 2749: Assigning the BUFFER-VALUE from InputValue
                   for Byte[] fields does crash as AVM */
                IF hBuffer:BUFFER-FIELD(i):DATA-TYPE <> "BLOB":U THEN
                DO ON ERROR UNDO, THROW:
                    IF hBuffer:BUFFER-FIELD(i):EXTENT > 1 THEN DO ON ERROR UNDO, THROW:
                        DO j = 1 TO hBuffer:BUFFER-FIELD(i):EXTENT ON ERROR UNDO, THROW:
                            ASSIGN hBuffer:BUFFER-FIELD(i):BUFFER-VALUE[j] = UNBOX (THIS-OBJECT:BindingSource:InputValue [hBuffer:BUFFER-FIELD(i):NAME + "[":U + STRING(j) + "]":U]) .
                        END. /* EXTENT */
                    END. /* IF EXTENT > 1 */
                    ELSE
                        ASSIGN hBuffer:BUFFER-FIELD(i):BUFFER-VALUE = UNBOX (THIS-OBJECT:BindingSource:InputValue [hBuffer:BUFFER-FIELD(i):Name]) .
                END . /* DATA-TYPE <> BLOB */

                CATCH e AS System.IndexOutOfRangeException:
                    /* Ignore: Invalid fldName used with InputValue property: ... */
                    /* Mike Fechner, Consultingwerk Ltd. 18.08.2014
                       Questionable if it makes sense to show error to the developer here ... */
/*                    IF FrameworkSettings:DebugMode THEN   */
/*                        ErrorHelper:ShowErrorMessage (e) .*/

                    NEXT fieldLoop .
                END CATCH.

                CATCH ple AS Progress.Lang.Error :
                    IF ple:GetMessageNum(1) = 132 THEN DO:
                        ASSIGN hUndoBuffer = hUndoDataset:GET-BUFFER-HANDLE (hBuffer:NAME) .
                        
                        /* Mike Fechner, Consultingwerk Ltd. 31.12.2009
                           When there have been unique key issues on the temp-table (error 132),
                           reset buffer to default-value, aka back out what has been assigned here */
                        resetloop: DO j = 1 TO hBuffer:NUM-FIELDS:
                            IF THIS-OBJECT:BindingSourceIncludeFields = "*":U AND THIS-OBJECT:BindingSourceExcludeFields > "":U THEN
                                IF ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (j):NAME, THIS-OBJECT:BindingSourceExcludeFields) OR
                                   ListHelper:EntryIsInList (hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD (j):NAME, THIS-OBJECT:BindingSourceExcludeFields)
                                THEN NEXT resetloop.

                            /* When using Exclude Fields, skip those fields */
                            IF THIS-OBJECT:BindingSourceIncludeFields <> "*":U AND THIS-OBJECT:BindingSourceIncludeFields > "":U THEN
                                IF NOT ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD (j):NAME, THIS-OBJECT:BindingSourceIncludeFields) AND
                                   NOT ListHelper:EntryIsInList (hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD (j):NAME, THIS-OBJECT:BindingSourceExcludeFields)
                                THEN NEXT resetloop.

                            /* Mike Fechner, Consultingwerk Ltd. 11.03.2012
                               Initialize array fields  */
                            IF hBuffer:BUFFER-FIELD(j):EXTENT > 1 THEN DO:
                               DO iExtent = 1 TO hBuffer:BUFFER-FIELD(j):EXTENT:
                                  ASSIGN hBuffer:BUFFER-FIELD(j):BUFFER-VALUE[iExtent] = hUndoBuffer:BUFFER-FIELD(j):BUFFER-VALUE[iExtent] .
                               END.
                            END.
                            ELSE
                               ASSIGN hBuffer:BUFFER-FIELD(j):BUFFER-VALUE = hUndoBuffer:BUFFER-FIELD(j):BUFFER-VALUE .
                        END. /* resetloop */

                        ex = RecordAlreadyExistsException:FromSysError (CAST (ple, SysError)) .

                        OnAssignKeyFieldError (NEW AssignKeyFieldErrorEventArgs (ex,
                                                                                 hBuffer:NAME,
                                                                                 hBuffer:BUFFER-FIELD(i):NAME)) . 
                        UNDO, THROW ex .
                    END. /* error 132 */
                    ELSE 
                        UNDO, THROW ple . 
                END CATCH.
                
            END. /* fieldloop */

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hUndoDataset) .        
            END FINALLY.

        END. /* IF plAssignFromBindingSource = TRUE  */

        OnAfterAssignRecord (NEW AfterAssignRecordEventArgs (THIS-OBJECT:EntityName,
                                                             THIS-OBJECT:EntityTable,
                                                             hBuffer,
                                                             THIS-OBJECT:QueryHandle)) .

        QueryHelper:GetCurrentRowids (THIS-OBJECT:QueryHandle, OUTPUT rRowids) .

        ASSIGN lCreated = (hBuffer:ROW-STATE = ROW-CREATED) .

        DO ON ERROR UNDO, THROW:
            SubmitChanges () .

            /* Mike Fechner, Consultingwerk Ltd. 11.12.2013
               After successful submitting changes, we are no longer 
               creating a record. */
            ASSIGN roCreateRowid = ? .

            CATCH submiterr AS Progress.Lang.Error :
                QueryHelper:RepositionToRowidArray (THIS-OBJECT:QueryHandle, rRowids) .

                IF NOT THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE(1):AVAILABLE THEN
                    THIS-OBJECT:QueryHandle:GET-NEXT () .

                UNDO, THROW submiterr .
            END CATCH.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 28.11.2014
           Lock window update to avoid first row appearing briefly between
           QUERY-OPEN and REPOSITION below */
        IF VALID-OBJECT (THIS-OBJECT:UpdatingSmartDataTarget) AND 
           TYPE-OF (CAST (THIS-OBJECT:UpdatingSmartDataTarget, Progress.Lang.Object), System.Windows.Forms.Control) THEN 
            
            oUpdatingControl = CAST (CAST (THIS-OBJECT:UpdatingSmartDataTarget, Progress.Lang.Object), System.Windows.Forms.Control) . 
        
        IF VALID-OBJECT (oUpdatingControl) THEN
            Win32:LockWindowUpdate(oUpdatingControl:FindForm()) .

        ASSIGN rRowid = rRowids[1] .

        /* Mike Fechner, Consultingwerk Ltd. 11.03.2012
           Re-Evaluate the parent filter query based on eventually updated
           parent filter fields when in a GroupCreate situation the parent's
           key fields are changed on the server */
        DO ON ERROR UNDO, THROW:
            /* Mike Fechner, Consultingwerk Ltd. 15.07.2015
               SCL-907: EvaluateParentQuery now throws an error, when 
                        there is no Data-Source, previously did return ? */
            ASSIGN cFetchQueryString = THIS-OBJECT:EvaluateParentQuery() .
        
            CATCH evaerr AS EvaluateParentQueryException:
            	ASSIGN cFetchQueryString = ? .	
            END CATCH.        
        END.

        IF cFetchQueryString > "":U AND cFetchQueryString <> cPreviousFetchQueryString THEN DO ON ERROR UNDO, THROW:
            ASSIGN cPreviousQueryString = THIS-OBJECT:QueryHandle:PREPARE-STRING
                
                   oList                = StringHelper:QuoteAndBracketSafeEntriesToList (cPreviousQueryString) 
                   cPreviousQueryString = cFetchQueryString         
                   .
        
            DO iEntry = 2 TO oList:Count:
                ASSIGN cPreviousQueryString = SUBSTITUTE ("&1,&2", 
                                                          cPreviousQueryString, 
                                                          oList:GetValue (iEntry)) . 
            END . 
           
            cFetchQueryString = cPreviousQueryString .

            THIS-OBJECT:QueryHandle:QUERY-PREPARE (cFetchQueryString) .
            ASSIGN cPreviousFetchQueryString = cFetchQueryString .
            
            CATCH querypreperr AS Progress.Lang.Error:
                IF VALID-OBJECT (oUpdatingControl) THEN 
                    Win32:LockWindowUpdate(0) . 

            	UNDO, THROW Exception:FromErrorMessageAndError (SUBSTITUTE ("Error opening Query after update: &1"{&TRAN}, cFetchQueryString), 
            	                                                0,
            	                                                querypreperr) .  	
            END CATCH.
        END.

        ASSIGN oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser() .

        IF VALID-OBJECT (oBrowser) THEN 
            oBrowser:SuspendRowSynchronizationForReposition() .
    
        DO ON ERROR UNDO, THROW:

            THIS-OBJECT:QueryHandle:QUERY-OPEN () .
            THIS-OBJECT:BindingSource:RefreshAll() .
    
            IF VALID-OBJECT (oUpdatingControl) THEN
                Win32:LockWindowUpdate(0) .
    
            /* Mike Fechner, Consultingwerk Ltd. 11.11.2015
               SCL-1061 - allow to Fetch new batch after adding record */
            THIS-OBJECT:RepositionAfterUpdateRow (rRowid, lCreated) .
            
            IF NOT THIS-OBJECT:QueryHandle:GET-BUFFER-HANDLE(1):AVAILABLE THEN
                THIS-OBJECT:QueryHandle:GET-NEXT () .

            FINALLY:
                IF VALID-OBJECT (oBrowser) THEN 
                    oBrowser:ResumeRowSynchronizationAfterReposition() .                		
            END FINALLY.
        END.


        OnAfterUpdateRecord (NEW AfterUpdateRecordEventArgs (THIS-OBJECT:EntityName,
                                                             THIS-OBJECT:EntityTable,
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

            /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
               Turn on Tracking Changes even in case of error */
            SetTrackingChanges (TRUE) .
            SetSavingData (FALSE) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates the value of the ForeignFields property when during the 
                 first ParentPositionChanged event  
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ValidateForeignFields ():
		
		DEFINE VARIABLE cParentFieldName AS CHARACTER NO-UNDO .
		DEFINE VARIABLE cChildFieldName  AS CHARACTER NO-UNDO .
		DEFINE VARIABLE i                AS INTEGER   NO-UNDO .
		
		DEFINE VARIABLE cMessageTitle    AS CHARACTER NO-UNDO .
		DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO .
		
		IF NOT FrameworkSettings:DebugMode THEN  
		    RETURN . 
		
		IF NUM-ENTRIES (THIS-OBJECT:ForeignFields) MODULO 2 = 1 THEN DO: 
		    ASSIGN cMessage = "ForeignFields property requires an even number of entries."{&TRAN} .
		    RETURN .
		END.
		
        DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:ForeignFields) BY 2:
            ASSIGN cParentFieldName = ENTRY (i, THIS-OBJECT:ForeignFields) 
                   cChildFieldName  = ENTRY (i + 1, THIS-OBJECT:ForeignFields) . 

            IF NOT THIS-OBJECT:ValidateForeignField (cParentFieldName, THIS-OBJECT:SmartDataSource) THEN DO: 
                ASSIGN cMessage = SUBSTITUTE ("Parent Field &1 does not exist."{&TRAN}, cParentFieldName) .  
                RETURN.
            END.
		END.

        CATCH err AS Progress.Lang.Error:
           ErrorHelper:ShowErrorMessage (err) .     
        END CATCH.
		
		FINALLY:
            ASSIGN lHasValidatedForeignFields = TRUE .
            
            IF cMessage > "":U THEN DO:
                ASSIGN cMessageTitle = SUBSTITUTE ("Error validating ForeignFields for &1"{&TRAN},
                                                   ClassHelper:ShortClassName (THIS-OBJECT:GetClass ())) 
                       cMessage      = SUBSTITUTE ("&1&2Entity Table: &3&2Entity View: &4"{&TRAN}, 
                                                   cMessage,
                                                   Consultingwerk.Environment:NewLine,
                                                   THIS-OBJECT:EntityTable,
                                                   THIS-OBJECT:EntityView) . 
                  
                MessageFormHelper:ShowMessage (cMessage, 
                                               cMessageTitle, 
                                               Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning) .               
            END.     		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Validates that a single field name is available in the SmartDataSource 
                 or this Adapter instance  
        Notes:   
        @param pcFieldName The current field name entry
        @param poSmartDataSource The reference to the SmartDatasetAdapter instance that needs to contain the field
        @return Logical value indicating that the field can be found
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL ValidateForeignField (pcFieldName AS CHARACTER, 
                                                   poSmartDataSource AS ISmartDataSource):

        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hDataSource AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cFieldName  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iExtent     AS INTEGER   NO-UNDO.
    
        IF NOT VALID-OBJECT (poSmartDataSource:BindingSource) THEN 
            RETURN FALSE . 

        ASSIGN hDataSource = poSmartDataSource:BindingSource:Handle .

        IF NUM-ENTRIES(pcFieldName, ".":U) = 1 THEN DO:
            ASSIGN hBuffer = hDataSource:GET-BUFFER-HANDLE (1) .

            Consultingwerk.Util.BufferHelper:GetFieldNameAndExtent (pcFieldName, OUTPUT cFieldName, OUTPUT iExtent).

            RETURN BufferHelper:HasField (hBuffer, cFieldName) .
        END.
        ELSE DO:
            ASSIGN hBuffer = hDataSource:GET-BUFFER-HANDLE (ENTRY(1, pcFieldName, ".":U)) .    

            Consultingwerk.Util.BufferHelper:GetFieldNameAndExtent (ENTRY(2, pcFieldName, ".":U), OUTPUT cFieldName, OUTPUT iExtent).

            RETURN BufferHelper:HasField (hBuffer, cFieldName) .
        END.

        RETURN FALSE .             

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the SmartDatasetAdapter class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartDatasetAdapter ( ):

        IF VALID-HANDLE (THIS-OBJECT:QueryHandle) AND THIS-OBJECT:BindTo = "Query":U THEN
            DELETE OBJECT THIS-OBJECT:QueryHandle .

    END DESTRUCTOR.

END CLASS.
