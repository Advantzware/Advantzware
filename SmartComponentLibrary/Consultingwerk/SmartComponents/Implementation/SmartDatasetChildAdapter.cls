/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartDatasetChildTableAdapter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 23 21:28:56 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*                        FROM PROPATH . 
USING Consultingwerk.Framework.*                         FROM PROPATH .
USING Consultingwerk.Framework.Collections.*             FROM PROPATH . 
USING Consultingwerk.SmartComponents.Base.*              FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*              FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*    FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* . /* FROM PROPATH is not possible here, then the .Interfaces.Design would not be accessible */ 
USING Consultingwerk.SmartComponents.Interfaces.Design.* FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Resources.*         FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*           FROM PROPATH .
USING Consultingwerk.Util.*                              FROM PROPATH .
USING Progress.Data.*                                    FROM ASSEMBLY .
USING Progress.Lang.*                                    FROM ASSEMBLY .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter 
    INHERITS SmartDatasetAdapter 
    IMPLEMENTS ISmartGroupCreateTarget, ISmartBusinessEntityAdapter
    USE-WIDGET-POOL: 

    DEFINE PRIVATE VARIABLE lRegisteredDatasetController AS LOGICAL                 NO-UNDO INIT FALSE . 
    DEFINE PRIVATE VARIABLE oSmartGroupCreateSource      AS ISmartGroupCreateSource NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Event raised whenever the SmartGroupCreateSource property is changed 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SmartGroupCreateSourceChanged  DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Binding the BusinessEntityBindingSource to a Query or a Dataset.
                 This Property is set by the Developer during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY BindTo AS CHARACTER NO-UNDO INIT "QUERY":U 
    GET.
    
     /*------------------------------------------------------------------------------
        Purpose: Returns the handle to the ProDataset instance used (referenced) by the 
                 SmartDatasetChildTableAdapter 
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC OVERRIDE PROPERTY DataSet AS HANDLE NO-UNDO 
    GET():
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN 
            RETURN CAST(THIS-OBJECT:SmartDataSource, ISmartDataSourceWithDataset):Dataset .
        ELSE 
            RETURN ? .          
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: A Reference to the DatasetController that is is owning the Dataset 
                 on which this SmartDatasetChildAdapter is operating.
        Notes:   This property references the DatasetController of the SmartDataSource
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC OVERRIDE PROPERTY DatasetController AS Consultingwerk.SmartComponents.Interfaces.IDatasetController NO-UNDO 
    GET:
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND TYPE-OF (THIS-OBJECT:SmartDataSource, 
                                                                   IDatasetControllerConsumer) THEN 
                                                                   
            RETURN CAST (THIS-OBJECT:SmartDataSource, 
                         IDatasetControllerConsumer):DatasetController .      
                                                                          
    END GET . 

    /*------------------------------------------------------------------------------
        Purpose: A List in the same order as EntityView which defines Join (YES or NO)
                 for each Table of pcEntityView.
                 This Property is set by the Developer during DesignTime.
        Notes:   Not exposed publicly for SmartDatasetChildAdapter 
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED OVERRIDE PROPERTY EntityJoin AS CHARACTER NO-UNDO INIT "":U
    GET:
        RETURN TRIM(FILL ("YES,":U, NUM-ENTRIES (THIS-OBJECT:EntityView)), ",":U) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: RootTable for this Instance of the SmartDatasetChildTableAdapter.
                 This Property is set by the Developer during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY EntityName AS CHARACTER NO-UNDO INIT "":U
    GET:
        IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN 
            RETURN "":U .
        
        IF TYPE-OF (THIS-OBJECT:SmartDataSource, ISmartBusinessEntityAdapter) THEN 
            RETURN CAST (THIS-OBJECT:SmartDataSource, ISmartBusinessEntityAdapter):EntityName .
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: RootTable for this Instance of the SmartDatasetChildTableAdapter.
                 This Property is set by the Developer during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY EntityTable AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
        DEFINE VARIABLE hBuffer  AS HANDLE NO-UNDO.
        
        IF THIS-OBJECT:DesignTime AND NOT THIS-OBJECT:DesingerHostLoading THEN 
        DO ON ERROR UNDO, THROW:
            IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN DO:
                /* Mike Fechner, Consultingwerk Ltd. 22.04.2010
                   In case of the SmartDatasetChildAdapter we will accept any entered table name
                   when not yet linked to another DataAdapter */
                THIS-OBJECT:EntityTable = arg . 
                RETURN .  
            END.
            
            ASSIGN hDataset = THIS-OBJECT:DataSet . 
            
            IF VALID-HANDLE (hDataset) THEN DO:
                ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (arg) NO-ERROR .
                
                IF NOT VALID-HANDLE (hBuffer) THEN  
                    UNDO, THROW NEW AppError ("The entered table name is not valid.", 0) .
            END.
            ELSE 
                UNDO, THROW NEW AppError ("Unable to set EntityTable because Dataset is not valid.", 0) .
        END.
        
        THIS-OBJECT:EntityTable = arg . 
    END SET .   

    /*------------------------------------------------------------------------------
        Purpose: List of Tables to read with the SmartDatasetChildTableAdapter.
                 This Property is set by the Developer during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY EntityView AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE hDataset AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBuffer  AS HANDLE  NO-UNDO.
        DEFINE VARIABLE cBuffer  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i        AS INTEGER NO-UNDO.
        
        IF THIS-OBJECT:DesignTime AND NOT THIS-OBJECT:DesingerHostLoading THEN 
        DO ON ERROR UNDO, THROW:
            IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN DO:
                /* Mike Fechner, Consultingwerk Ltd. 22.04.2010
                   In case of the SmartDatasetChildAdapter we will accept any entered table name
                   when not yet linked to another DataAdapter */
                THIS-OBJECT:EntityTable = arg . 
                RETURN .  
            END.
            
            ASSIGN hDataset = THIS-OBJECT:DataSet . 
            
            IF VALID-HANDLE (hDataset) THEN DO i = 1 TO NUM-ENTRIES (arg):
                ASSIGN cBuffer = ENTRY (i, arg)
                       hBuffer = hDataset:GET-BUFFER-HANDLE (cBuffer) NO-ERROR .
                
                IF NOT VALID-HANDLE (hBuffer) THEN  
                    UNDO, THROW NEW AppError (SUBSTITUTE ("The entered table &1 name is not valid.", cBuffer) , 0) .
            END.
            ELSE 
                UNDO, THROW NEW AppError ("Unable to set EntityView because Dataset is not valid.", 0) .
        END.
        
        THIS-OBJECT:EntityView = arg . 
    END SET .   

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartGroupCreateSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
                SCL-725 - code moved from LinkGroupCreateSource.i directly into class file
   ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkGroupCreateSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupCreateSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartGroupCreateSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupCreateSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartGroupCreateSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupCreateSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupCreateSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartGroupCreateSource) THEN 
            ASSIGN THIS-OBJECT:SmartGroupCreateSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartGroupCreateSource) .         
    END.  

    /*------------------------------------------------------------------------------
        Purpose: This is a base query string.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY QueryString AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.       

    /*------------------------------------------------------------------------------
        Purpose: This property allows to subspend the call to RetrieveData in
                 the setter of QuerySort (SetQuerySort method)
        Notes:   Defaults to TRUE
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RetrieveDataOnChangeSortOrder AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the SmartGroupCreateSource
        Notes:      
    ------------------------------------------------------------------------------*/     
    DEFINE PUBLIC PROPERTY SmartGroupCreateSource AS ISmartGroupCreateSource NO-UNDO 
    GET:
        RETURN oSmartGroupCreateSource .
    END GET . 
    SET (arg AS ISmartGroupCreateSource):
        
            DEFINE VARIABLE oTempSource AS ISmartGroupCreateSource NO-UNDO.
            DEFINE VARIABLE lChanged    AS LOGICAL                 NO-UNDO INIT FALSE .
        
            IF arg <> THIS-OBJECT:SmartGroupCreateSource THEN 
                ASSIGN lChanged = TRUE . 
        
            IF VALID-OBJECT (arg) THEN 
            DO:
                /* A Viewer can not be it's own GroupCreateSource */
                IF arg = THIS-OBJECT THEN 
                    UNDO, THROW NEW AppError ("A SmartDatasetChildAdapter can not be it's own SmartGroupCreateSource!", 0).
                
                ASSIGN 
                    oSmartGroupCreateSource = arg . 
            
                arg:RegisterSmartGroupCreateTarget(THIS-OBJECT) .
            END.
            ELSE 
            DO:
                IF VALID-OBJECT (oSmartGroupCreateSource) THEN DO: 
                    oSmartGroupCreateSource:DeregisterSmartGroupCreateTarget (THIS-OBJECT) .
                END.
                             
                oSmartGroupCreateSource = ? .
            END.
            
            IF lChanged THEN 
                OnSmartGroupCreateSourceChanged (System.EventArgs:Empty) .        
        
    END SET.  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDatasetChildAdapter class
        Notes:   
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC SmartDatasetChildAdapter ():
        SUPER ().
    
        THIS-OBJECT:SetDesignerProperties () .
            
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDatasetChildAdapter class                                                                      
        Notes:                                                                        
        @param poContainer The System.ComponentModel.IContainer used as the Container for this component
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC SmartDatasetChildAdapter (poContainer AS System.ComponentModel.IContainer):
        
        SUPER (INPUT poContainer).
    
        THIS-OBJECT:SetDesignerProperties () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the Query to the BindingSource                                                                       
        Notes:                                                                               
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachQueryToBindingSource ():
        
        THIS-OBJECT:BindingSource:AutoSync = TRUE .
        THIS-OBJECT:BindingSource:Batching = FALSE .
        THIS-OBJECT:BindingSource:Handle = THIS-OBJECT:QueryHandle .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Begins a transaction when starting update
        Notes:   Allows SmartDatasetChildAdapter to initiate TransactionState in a 
                 parent SmartBusinessEntityAdapter
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID BeginTransactionState ():

        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND TYPE-OF (SmartDataSource, SmartDatasetAdapter) THEN 
            CAST (SmartDataSource, SmartDatasetAdapter):BeginTransactionState () .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Cancels the creation of a new record in the ISmartGroupCreateTarget 
        Notes:   Typically invoked when the ISmartGroupCreateSource also cancels the 
                 creation of a new record
        @param poSource The ISmartgroupCreateSource that caused cancelling the creation of a new row
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CancelCreateByGroupCreateSource (poSource AS ISmartGroupCreateSource):
    
        DEFINE VARIABLE oDataTarget    AS ISmartDataTarget    NO-UNDO .
        DEFINE VARIABLE oTableIOTarget AS ISmartTableIOTarget NO-UNDO .
        DEFINE VARIABLE i              AS INTEGER             NO-UNDO .
        
        IF THIS-OBJECT:CreatingRecord THEN DO:
            DO i = 1 TO THIS-OBJECT:SmartDataTargets:Count:

                ASSIGN oDataTarget = THIS-OBJECT:SmartDataTargets:GetItem (i) .

                IF TYPE-OF (oDataTarget, ISmartTableIOTarget) THEN DO:
                    ASSIGN oTableIOTarget = CAST (oDataTarget, ISmartTableIOTarget) .
            
                    IF oTableIOTarget:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
                        oTableIOTarget:CancelUpdate () .
                END.                 
            END.
        END.
        
    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Reopen Query so that it does not return any data.
        Notes:   SUPER:CloseQuery does not have to be invoked because it is an 
                 abstract METHOD.
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID CloseQuery ():
        DEFINE VARIABLE cPrepareQueryString AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oList               AS CharacterList NO-UNDO .
        DEFINE VARIABLE iEntry              AS INTEGER       NO-UNDO . 
        
        IF VALID-HANDLE (THIS-OBJECT:QueryHandle) THEN DO:
            ASSIGN cPrepareQueryString = THIS-OBJECT:QueryHandle:PREPARE-STRING .            

            ASSIGN oList               = StringHelper:QuoteAndBracketSafeEntriesToList (cPrepareQueryString) 
                   cPrepareQueryString = SUBSTITUTE ("PRESELECT EACH &1 WHERE ROWID (&1) = TO-ROWID ('0x0')":U, THIS-OBJECT:EntityTable).
            
            DO iEntry = 2 TO oList:Count:
                ASSIGN cPrepareQueryString = SUBSTITUTE ("&1,&2", 
                                                         cPrepareQueryString, 
                                                         oList:GetValue (iEntry)) . 
            END . 
            
            DO ON ERROR UNDO, THROW:            
                THIS-OBJECT:QueryHandle:QUERY-PREPARE (cPrepareQueryString) .     
    
                CATCH querypreperr AS Progress.Lang.Error:
                    UNDO, THROW Exception:FromErrorMessageAndError (SUBSTITUTE ("Error opening Query: &1"{&TRAN}, cPrepareQueryString), 
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
            
            /* Mike Fechner, Consultingwerk Ltd. 04.02.2012
               Bug 2797 RefreshAll () may cause client to crash when the Handle property
                        of the BindingSource is not yet set. */
            IF VALID-OBJECT (THIS-OBJECT:BindingSource) AND VALID-HANDLE (THIS-OBJECT:BindingSource:Handle) THEN 
                THIS-OBJECT:BindingSource:RefreshAll() .
            
        END.
        
        SUPER:CloseQuery () .
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Creates a new record                                                                        
        Notes:   When using a SmartGroupCreateSource ensure that a record in the 
                 SmartGroupCreateSource is created before creating a new record 
                 here.                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID CreateRecord ():
        
        DEFINE VARIABLE cForeignFields AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cKeyFields     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i              AS INTEGER   NO-UNDO .
        
        IF VALID-OBJECT (THIS-OBJECT:SmartGroupCreateSource) THEN DO:

            IF THIS-OBJECT:SmartGroupCreateSource:RecordPosition = RecordPositionEnum:NoRecordAvailable THEN DO:

                ASSIGN cForeignFields = THIS-OBJECT:ForeignFields . 
    
                DO i = 1 TO NUM-ENTRIES (cForeignFields) - 1:
                    ASSIGN cKeyFields = cKeyFields + SUBSTITUTE ("&1,":U, ENTRY (i, cForeignFields)).
                END.
                
                THIS-OBJECT:SmartGroupCreateSource:CreateParentRecord (TRIM (cKeyFields, ",":U)) .
            END. 
        END.
        
        SUPER:CreateRecord().

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the DatasetControllerChanged event of the 
                 SmartDataSource                                                                        
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DataSourceDatasetControllerChangedHandler (sender AS System.Object, 
                                                                     e AS System.EventArgs):
        
        THIS-OBJECT:RegisterWithDatasetController () . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deregisters from the Dataset Controller                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED FINAL VOID DeregisterWithDatasetController ():
        
        IF THIS-OBJECT:lRegisteredDatasetController AND VALID-OBJECT (THIS-OBJECT:DatasetController) THEN 
            THIS-OBJECT:DatasetController:DeregisterConsumer (THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Signals the object that initialization is complete.                                                                       
        Notes:   Makes sure that the BindingSource of the SmartDatasetChildAdapter 
                 is always valid.                                                                      
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID EndInit ():
        
        SUPER:EndInit().

        IF         VALID-OBJECT (THIS-OBJECT:SmartDataSource)  
           AND NOT VALID-OBJECT (THIS-OBJECT:BindingSource) AND 
               NOT THIS-OBJECT:DesignTime THEN 
           
           THIS-OBJECT:CreateBindingSource () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Ensures that the provided Query String contains a criteria for each
                 query buffer 
        Notes:   
        @param pcQueryString The base query string
        @return The extended query string
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER EnsureCompleteQueryString (pcQueryString AS CHARACTER):
		
        DEFINE VARIABLE cEntries AS CHARACTER NO-UNDO EXTENT .
        DEFINE VARIABLE cTables  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO .
        
        ASSIGN cTables  = TRIM (THIS-OBJECT:EntityTable + ",":U + THIS-OBJECT:EntityView, ",":U) 
               cEntries = QueryHelper:QueryStringEntries (pcQueryString) .

        ASSIGN pcQueryString = cEntries [1] .

        DO i = 2 TO NUM-ENTRIES (cTables):
        
            IF EXTENT (cEntries) >= i THEN 
                ASSIGN pcQueryString = SUBSTITUTE ("&1, &2":U, pcQueryString, cEntries[i]) .
            ELSE 
                ASSIGN pcQueryString = SUBSTITUTE ("&1, &2 OUTER-JOIN":U,
                                                   pcQueryString, 
                                                   DatasetHelper:ChildRelationQueryString (THIS-OBJECT:DataSet:GET-BUFFER-HANDLE (ENTRY (i, cTables)))).
        END.

        RETURN pcQueryString . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the SmartBusinessEntityAdpater
        Notes:   Useful to initalize the BindingSource and Query before calling 
                 RetrieveData
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID InitializeAdapter ():
        
        DEFINE VARIABLE lBindingSourceCreated AS LOGICAL NO-UNDO INIT FALSE .

        IF NOT THIS-OBJECT:FirstTime THEN 
            RETURN  . 

        /* If no valid BindingSource exist a new one is created right here */
        IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource) THEN DO:                     
            CreateBindingSource ().                 
            ASSIGN lBindingSourceCreated = TRUE .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 07.07.2009
           TRACKING-CHANGES needs to be turned of during EMPTY-DATASET */        
        SetTrackingChanges (FALSE) . 

        IF NOT lBindingSourceCreated THEN 
            THIS-OBJECT:PrepareDataBinding () .

        ASSIGN THIS-OBJECT:FirstTime = FALSE .

        THIS-OBJECT:AttachQueryToBindingSource () .

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Merges two Query Strings                                                                      
        Notes:                           
        @param pcFetchQueryString The first QueryString
        @param poQueryExpressions The List of QueryExpressions by Table to be merged with the first QueryString
        @return The resulting Query String          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE CHARACTER MergeQueryStrings (pcFetchQueryString AS CHARACTER, 
                                                           poQueryExpressions AS Consultingwerk.ListQueryExpressionByTable):
        
        DEFINE VARIABLE cQueryStrings    AS CHARACTER                          NO-UNDO EXTENT .
        DEFINE VARIABLE cTables          AS CHARACTER                          NO-UNDO .
        DEFINE VARIABLE cTable           AS CHARACTER                          NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER                            NO-UNDO .
        DEFINE VARIABLE oQueryExpression AS Consultingwerk.ListQueryExpression NO-UNDO .
        DEFINE VARIABLE cReturn          AS CHARACTER                          NO-UNDO .
        DEFINE VARIABLE cBaseQuery       AS CHARACTER                          NO-UNDO .
        
        ASSIGN cQueryStrings = QueryHelper:QueryStringEntries (pcFetchQueryString) .
        
        ASSIGN cTables = TRIM (THIS-OBJECT:EntityTable + ",":U + THIS-OBJECT:EntityView, ",":U) .

        DO i = 1 TO NUM-ENTRIES (cTables):
            ASSIGN cTable           = ENTRY (i, cTables)
                   oQueryExpression = poQueryExpressions:GetItem (cTable) .
        
            IF oQueryExpression:IsEmpty THEN 
                ASSIGN cReturn = cReturn + (IF i > 1 THEN ",":U ELSE "":U) + cQueryStrings[i] .
            ELSE DO:
                ASSIGN cBaseQuery = QueryHelper:InsertExpression (cQueryStrings[i], 
                                                                  oQueryExpression:GetExpression(),
                                                                  "AND":U) .

                ASSIGN cBaseQuery = REPLACE (cBaseQuery, " OUTER-JOIN":U, " ":U) .

                ASSIGN cReturn = cReturn + (IF i > 1 THEN ",":U ELSE "":U) + cBaseQuery .
            END.        
        END.

        RETURN cReturn . 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the SmartGroupCreateSourceChanged event                                                                        
        Notes:                                                  
        @param e .NET Default Event Argument                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSmartGroupCreateSourceChanged (e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty .
            
        THIS-OBJECT:SmartGroupCreateSourceChanged:PUBLISH (THIS-OBJECT, e) .                  

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs                                                                       
        Notes:  
        @param pcVerbText The text indicating the verb that was clicked                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID OnVerbClicked (pcVerbText AS CHARACTER):

        IF NOT THIS-OBJECT:DesignTime THEN 
            RETURN . 
    
        CASE pcVerbText:
            WHEN "Select Tables":U THEN 
                ShowTablePickerDialog() .
            WHEN "Create BindingSource":U THEN 
                BusinessEntityDesignerSupport:CreateBindingSourceFromAdapter (THIS-OBJECT,
                                                                              THIS-OBJECT:GetDesignTimeDataSourceClassName()) .            
        END .
        
        SUPER:OnVerbClicked (pcVerbText) .            

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
    END.                                          

    /*------------------------------------------------------------------------------
        Purpose: Method used to prepare the BindingSource:Handle object (QUERY).                                                                        
        Notes:   Implemented in the SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 Called from RetrieveData or CreateRecord (when called before RetrieveData)                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID PrepareDataBinding ():
        
        DEFINE VARIABLE cEntityJoin AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cExceptList AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hDataset    AS HANDLE    NO-UNDO.
        
        ASSIGN hDataset = CAST (THIS-OBJECT:SmartDataSource, ISmartDataSourceWithDataset):Dataset .
        
        THIS-OBJECT:FirstTime = FALSE .

        ASSIGN cEntityJoin = FILL ("YES,":U, NUM-ENTRIES (THIS-OBJECT:EntityView, ",":U)) . 

        THIS-OBJECT:QueryHandle = SmartBusinessEntityQuerySupport:PrepareBindingQuery (hDataset,
                                                           THIS-OBJECT:EntityTable,
                                                           THIS-OBJECT:EntityView,
                                                           cEntityJoin,
                                                           TRUE,
                                                           OUTPUT cExceptList) .

        IF cExceptList > "":U THEN
            IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource:TableSchema) THEN
                THIS-OBJECT:BindingSource:SetFields("*":U, cExceptList, "":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED FINAL VOID RegisterWithDatasetController ():
        
        DEFINE VARIABLE oController AS IDatasetController NO-UNDO . 
        
        IF lRegisteredDatasetController THEN RETURN . 
        
        oController = THIS-OBJECT:DatasetController .

        IF VALID-OBJECT (oController) THEN DO:
            oController:RegisterConsumer (THIS-OBJECT) .

            ASSIGN lRegisteredDatasetController = TRUE . 
            
            OnDatasetControllerChanged (System.EventArgs:Empty) .
        END.
    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: RetrieveData reads Data from a Datasource and links the DataHandle
                 with a BindingSource inside the Instance of SmartBusinessEntityAdapter.
        Notes:   If there is no valid instance of a BindigSource a new one is created.
                 After that a Dataset is cleared and then filled with the requested 
                 data from the Database. Now a Query is build or reopened and the 
                 Handle to this Query is assigned to the BindingSource:Handle.
                 Do NOT call SUPER:RetrieveData() in the overriding method.
                 Remember to test if a BindingSource exists. If not call 
                 CreateBindingSource from the base class.
        @return Logical value indicating the success of the method                 
    ------------------------------------------------------------------------------*/        
    METHOD PUBLIC OVERRIDE LOGICAL RetrieveData ():
    
        DEFINE VARIABLE cPrepareQueryString   AS CHARACTER                                 NO-UNDO.
        DEFINE VARIABLE lPositionWasZero      AS LOGICAL                                   NO-UNDO INIT FALSE .
        DEFINE VARIABLE lBindingSourceCreated AS LOGICAL                                   NO-UNDO INIT FALSE .
        DEFINE VARIABLE hDataset              AS HANDLE                                    NO-UNDO.
        DEFINE VARIABLE eCancel               AS System.ComponentModel.CancelEventArgs     NO-UNDO .
        DEFINE VARIABLE oList                 AS CharacterList                             NO-UNDO . 
        DEFINE VARIABLE iEntry                AS INTEGER                                   NO-UNDO .
        DEFINE VARIABLE oListQueryExpressions AS Consultingwerk.ListQueryExpressionByTable NO-UNDO . 
        
        eCancel = NEW System.ComponentModel.CancelEventArgs () .
        
        OnBeforeRetrieveData (eCancel) .        
                
        IF VALID-OBJECT (eCancel) AND eCancel:Cancel = TRUE THEN 
            RETURN FALSE .                
        
        THIS-OBJECT:PreviousPosition = THIS-OBJECT:BindingSource:Position NO-ERROR .         
        
        IF THIS-OBJECT:DesignTime THEN 
            RETURN FALSE . 
        
        IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN 
            UNDO, THROW NEW AppError ("The SmartDatasetChildTableAdapter require's a SmartDataSource with a Dataset.", 0).     
        IF NOT TYPE-OF (THIS-OBJECT:SmartDataSource, ISmartDataSourceWithDataset) THEN 
            UNDO, THROW NEW AppError ("The SmartDatasetChildTableAdapter require's a SmartDataSource with a Dataset.", 0).     
        
        ASSIGN hDataset = CAST (THIS-OBJECT:SmartDataSource, ISmartDataSourceWithDataset):Dataset .
    
        IF NOT lRegisteredDatasetController THEN 
            THIS-OBJECT:RegisterWithDatasetController () .
            
        /* If no valid BindingSource exist a new one is created right here */
        IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource) THEN DO:                        
            CreateBindingSource ().                    
            ASSIGN lBindingSourceCreated = TRUE .
        END.
        
        IF THIS-OBJECT:BindingSource:Position > 0 THEN 
            lPositionWasZero = FALSE .
        ELSE 
            lPositionWasZero = TRUE .          

        IF THIS-OBJECT:FirstTime THEN DO:
            THIS-OBJECT:PrepareDataBinding () .
            THIS-OBJECT:AttachQueryToBindingSource () . 

            /* Mike Fechner, Consultingwerk Ltd. 18.01.2013
               Bug 2845 setting FirstTime to FALSE does no longer happen in the FINALLY block
               so that in case of an error finally is not reset */
            THIS-OBJECT:FirstTime = FALSE . 
        END.
        
        IF THIS-OBJECT:QueryHandle:PREPARE-STRING > "":U THEN . 
            ELSE THIS-OBJECT:PrepareDataBinding () .  

        DO ON ERROR UNDO, THROW:
            cPrepareQueryString = THIS-OBJECT:EvaluateParentQuery () .

            /* Mike Fechner, Consultingwerk Ltd. 14.07.2015
               SCL-907 - when an error is thrown from EvaluateParentQuery (), we 
               should close our query */
            CATCH evaerr AS EvaluateParentQueryException:
                THIS-OBJECT:CloseQuery () . 
                RETURN FALSE .  
            END CATCH.                
        END.

        /* Mike Fechner, Consultingwerk Ltd. 22.09.2015
           Ensure that QueryString contains phrase for all query buffers */
        IF THIS-OBJECT:EntityView > "":U THEN 
            ASSIGN cPrepareQueryString = THIS-OBJECT:EnsureCompleteQueryString (cPrepareQueryString) . 
        
        /* Mike Fechner, Consultingwerk Ltd. 14.05.2015
           SCL-727 - Support for FilterRow etc. on SmartDatasetChildAdapter */
        ASSIGN oListQueryExpressions         = GetNewFilterValues ()
               THIS-OBJECT:FetchFilterValues = oListQueryExpressions .

        IF VALID-OBJECT (oListQueryExpressions) AND NOT oListQueryExpressions:IsEmpty THEN
            ASSIGN cPrepareQueryString = THIS-OBJECT:MergeQueryStrings (cPrepareQueryString,
                                                                        oListQueryExpressions) .

        /* Mike Fechner, Consultingwerk Ltd. 30.06.2010
           Bug 2357: Added a whitespace between cQueryString and cQuerySort to prevent queries from 
           beeing composed like "PRESELECT EACH fooBY foo.field" */
        cPrepareQueryString = SUBSTITUTE ("&1 &2":U, cPrepareQueryString, cQuerySort) .  

        DO ON ERROR UNDO, THROW:            
            THIS-OBJECT:QueryHandle:QUERY-PREPARE (cPrepareQueryString)  .     

            CATCH querypreperr AS Progress.Lang.Error:
                UNDO, THROW Exception:FromErrorMessageAndError (SUBSTITUTE ("Error opening Query: &1"{&TRAN}, cPrepareQueryString), 
                                                                0,
                                                                querypreperr) .     
            END CATCH.
        END. 

        /* Mike Fechner, Consultingwerk Ltd. 07.01.2012
           ISmartGroupCreateTarget implementation. When this instance
           itself is creating a record, don't actually reopen the query, stop after 
           preparing it. */
        IF THIS-OBJECT:CreatingRecord THEN  
            RETURN TRUE . 

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

            /* Mike Fechner, Consultingwerk Ltd. 04.02.2012
               Bug 2797 RefreshAll () may cause client to crash when the Handle property
                        of the BindingSource is not yet set. */
        IF NOT VALID-HANDLE (THIS-OBJECT:BindingSource:Handle) THEN 
            THIS-OBJECT:BindingSource:Handle = THIS-OBJECT:QueryHandle . 

        THIS-OBJECT:BindingSource:RefreshAll () .

        /* Initialize the state of the NavigationButtons */
        IF VALID-OBJECT (oSmartNavigationSource) THEN
            OnSmartNavigationTargetPositionChanged(NEW SmartNavigationTargetPositionChangedEventArgs(THIS-OBJECT:RecordPosition)) .

        /* Tell subscribers, that we got (new) data */
        OnAfterRetrieveData (System.EventArgs:Empty) .

        /* Mike Fechner, Consultingwerk Ltd. 24.06.2010
           When BindingSource's postition has been zero and is now zero 
           the PositionChanged event of the BindingSource has not been
           raised, so tell others */
        IF (THIS-OBJECT:BindingSource:Position = 0 AND lPositionWasZero OR lBindingSourceCreated) THEN 
            InvokeParentPositionChanged (THIS-OBJECT, System.EventArgs:Empty) .

        IF THIS-OBJECT:BindingSource:Position = 0 AND lPositionWasZero THEN 
            THIS-OBJECT:OnCurrentChanged (System.EventArgs:Empty) .

        RETURN TRUE .
        
        CATCH err AS Progress.Lang.Error :
            /* Variable re-throw */
            IF THIS-OBJECT:ThrowErrorsFromRetrieveData THEN 
                UNDO, THROW err .
            
            IF StartupParameterHelper:IOEverywhere = 1 THEN
                ErrorHelper:ShowErrorMessage (err, SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_RetrieveDataError) .
            ELSE  
                ErrorHelper:ShowErrorMessageBox (err, SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_RetrieveDataError) .             
        END CATCH.    
        
        FINALLY:
            THIS-OBJECT:PreviousPosition = THIS-OBJECT:BindingSource:Position NO-ERROR .
        END FINALLY.
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Sets Visual Designer related properties                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():
        
        THIS-OBJECT:DesignerVerbs = "Select Tables,Create BindingSource":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Overridable Setter for QuerySort property                                                                        
        Notes:
        @param arg The new value for the QuerySort property                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED OVERRIDE VOID SetQuerySort (arg AS CHARACTER):
        
        DEFINE VARIABLE oBrowser AS ISmartDataBrowser NO-UNDO . 
    
        /* Mike Fechner, Consultingwerk Ltd. 01.10.2013
           Bug 2836: Setting sort indicator on SmartDataBrowser */
        oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .

        IF VALID-OBJECT (oBrowser) THEN 
            oBrowser:UpdateSortIndicator (arg) .        

        IF TRIM (arg) <> TRIM (cQuerySort) THEN DO:        
            ASSIGN cQuerySort   = arg .
            
            IF THIS-OBJECT:RetrieveDataOnChangeSortOrder THEN 
                THIS-OBJECT:RetrieveData () . 
        END.                         

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the internal flag that data is being saved                                                                      
        Notes:   Enforced by ISupportsSubmitChanges                                                   
        @param plSaving The new value for the flag that data is being saved                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID SetSavingData (plSaving AS LOGICAL):
        
        IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) OR 
           NOT TYPE-OF (THIS-OBJECT:SmartDataSource, ISupportsSubmitChanges) THEN 
           
           UNDO, THROW NEW AppError ("This SmartDataAdapter requries a SmartDataSource that support SetSavingData to be able to submit changes!", 0) .

        CAST (THIS-OBJECT:SmartDataSource, ISupportsSubmitChanges):SetSavingData (plSaving) .

    END METHOD.

      /*------------------------------------------------------------------------------
        Purpose: SetSmartDataSource sets the SmartDataSource of the current instance
                 of the SmartDataAdapter. 
        Notes:   This Method is called from the Property Setter of SmartDataSource in
                 the Base Class.
                 Gets the ForeignFields from the hDataset which if not read before is
                 fetched from the Backend. Afterwards the backward handshake with the 
                 SmartDataSource is completed and the Handler for the PositionChanged
                 Event is subscribed to be able to recognize that the Parent Data has
                 changed.
                 You do not need to call SetSmartDataSource () in the overriding method.
        @param arg The SmartDataSource for this instance
    ------------------------------------------------------------------------------*/        
     METHOD PROTECTED OVERRIDE VOID SetSmartDataSource (arg AS ISmartDataSource):

        DEFINE VARIABLE i          AS INTEGER                NO-UNDO .
        DEFINE VARIABLE err        AS Progress.Lang.AppError NO-UNDO . 
        DEFINE VARIABLE hDataset   AS HANDLE                 NO-UNDO .

        IF VALID-OBJECT(arg) THEN DO:
            IF NOT TYPE-OF (arg, ISmartDataSourceWithDataset) THEN 
                UNDO, THROW NEW AppError ("The SmartDatasetChildTableAdapter require's a SmartDataSource with a Dataset - 2.", 0).     
            
            /* Build ForeignFields */
            IF THIS-OBJECT:ForeignFields > "":U THEN .
            ELSE DO ON ERROR UNDO, THROW:
                /* If I haven't read my dataset before, I need to read the schema */
                ASSIGN hDataset = CAST(arg, ISmartDataSourceWithDataset):DATASET .

                IF NOT VALID-HANDLE (hDataset) THEN 
                    UNDO, THROW NEW AppError ("SmartDataSource does not have a valid Dataset.", 0) .
                    
                ELSE DO:
                    /* Mike Fechner, Consultingwerk Ltd. 20.02.2010
                       Check for VALID EntityTable buffer first */
                    IF NOT VALID-HANDLE (hDataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable)) THEN DO:
                        ASSIGN err = NEW Progress.Lang.AppError ("Unable to build ForeignFields from DataSet.", 0) .     
                        err:AddMessage (SUBSTITUTE("The entity table '&1' is not valid.", THIS-OBJECT:EntityTable), 0)  .
                        UNDO, THROW err. 
                    END.
                    
                    IF NOT VALID-HANDLE (hDataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable):PARENT-RELATION) THEN DO:
                        ASSIGN err = NEW Progress.Lang.AppError ("Unable to build ForeignFields from DataSet.", 0) .     
                        err:AddMessage (SUBSTITUTE("The entity table &1 has no parent relation.", THIS-OBJECT:EntityTable), 0)  .
                        UNDO, THROW err. 
                    END.   
                    
                    THIS-OBJECT:ForeignFields = hDataset:GET-BUFFER-HANDLE(THIS-OBJECT:EntityTable):PARENT-RELATION:RELATION-FIELDS .
                END.
                    
                CATCH appe AS Progress.Lang.AppError:
                    /* Mike Fechner, Consultingwerk Ltd. 29.01.2010
                       Ignore AppErrors at DesignTime */
                    IF THIS-OBJECT:DesignTime THEN .
                    ELSE DO:
                        ASSIGN err = NEW Progress.Lang.AppError ("Unable to build ForeignFields from DataSet.", 0) .  
                            
                        DO i = 1 TO appe:NumMessages:
                            err:AddMessage (appe:GetMessage (i), appe:GetMessageNum (i)) . 
                        END.    
                            
                        UNDO, THROW err.
                    END.                                        
                END CATCH.            
                            
                CATCH e AS Progress.Lang.SysError:
                    ASSIGN err = NEW Progress.Lang.AppError ("Unable to build ForeignFields from DataSet.", 0) .     
                        
                    DO i = 1 TO e:NumMessages:
                        err:AddMessage (e:GetMessage (i), e:GetMessageNum (i)) . 
                    END.    
                        
                    UNDO, THROW err.
                END CATCH.
            END.
        END.
            
        IF NOT lRegisteredDatasetController THEN 
            THIS-OBJECT:RegisterWithDatasetController () .
            
        IF THIS-OBJECT:DesignTime = FALSE AND TYPE-OF (arg, IDatasetControllerConsumer) THEN 
            CAST (arg, IDatasetControllerConsumer):DatasetControllerChanged:Subscribe (DataSourceDatasetControllerChangedHandler).             
            
        SUPER:SetSmartDataSource (arg) . 
           
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Invokes the Table Picker Dialog and sets properties of the 
                 SmartBusinessEntityAdapter during Design Time                                                                        
        Notes:   Only intended for DesignTime                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowTablePickerDialog ():

        DEFINE VARIABLE hDataset   AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lcSchema   AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE cTables    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOldTables AS CHARACTER NO-UNDO.
        
        DEFINE VARIABLE cJoin      AS CHARACTER NO-UNDO INIT "?":U .
        
        DEFINE VARIABLE i        AS INTEGER NO-UNDO.
        
        IF NOT THIS-OBJECT:DesignTime THEN 
            RETURN .        

        ASSIGN hDataset = THIS-OBJECT:Dataset . 

        IF NOT VALID-HANDLE (hDataset) THEN DO:
            MESSAGE "Unable to start table picker dialog without a valid dataset."
                VIEW-AS ALERT-BOX WARNING .
            
            RETURN . 
        END.

        RUN Consultingwerk/SmartComponents/Support/SmartBusinessEntityAdapterDesigner_ShowBusinessEntityTblPic.p
                (hDataset,
                 TRIM (SUBSTITUTE ("&1,&2":U, THIS-OBJECT:EntityTable, THIS-OBJECT:EntityView), ",":U), 
                 OUTPUT cTables,
                 INPUT-OUTPUT cJoin) .

        IF cTables > "":U THEN DO:
            
            ASSIGN cOldTables              = THIS-OBJECT:EntityTable 
                   THIS-OBJECT:EntityTable = ENTRY (1, cTables) . 
            
            IF NUM-ENTRIES (cTables) > 1 THEN DO:
                ASSIGN 
                    ENTRY (1, cTables)     = "":U .
                    THIS-OBJECT:EntityView = TRIM (cTables, ",":U) .
            END.
            ELSE 
                THIS-OBJECT:EntityView = "":U .
            
        END.

        Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT, 
                                                                  "EntityTable":U,
                                                                  BOX (cOldTables), 
                                                                  BOX (THIS-OBJECT:EntityTable)) .   

        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN 
                DELETE OBJECT hDataset . 
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: UpdateRow submits the changes to a buffer into the Database.
                 Called by SmartViewerControl, SmartUpdatableBrowser
                 Data is updated into the Dataset from the BindingSource. After this
                 the Datasetchanges are submitted to the OERA Backend, merged into 
                 the Dataset and the BindingSource gets refreshed.
        Notes:   Used for ISmartGroupCreateTarget's to make sure that pending changes
                 in the SmartGroupCreateSource are save first.
        @param plAssignFromBindingSource Logical flag indicating of the value from the attached BindingSource should be assigned to the current record
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID UpdateRow (plAssignFromBindingSource AS LOGICAL):
        
        IF     VALID-OBJECT (THIS-OBJECT:SmartGroupCreateSource) 
           AND THIS-OBJECT:SmartGroupCreateSource:CreatingRecord THEN DO:
          
          
            THIS-OBJECT:SmartGroupCreateSource:SaveFromGroupCreateTarget (THIS-OBJECT) .
          
            THIS-OBJECT:ParentPositionChanged (?, ?) .        
        END.
        
        SUPER:UpdateRow (plAssignFromBindingSource).

        THIS-OBJECT:InvokeParentPositionChanged (?, ?) .

    END METHOD.
       
    /*------------------------------------------------------------------------------
        Purpose: Clean up Query and ProDataset from memory.
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartDatasetChildAdapter ( ):
    
        IF lRegisteredDatasetController THEN 
            DeregisterWithDatasetController () .
    
    END DESTRUCTOR.
        
END CLASS.
