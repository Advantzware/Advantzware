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
    File        : SmartDataAdapter
    Purpose     : Abstract base class for all Data Adapter implementations 
                  in the SmartComponente Library
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 21 11:42:57 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                         FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*              FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*              FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* .   /* FROM PROPATH is not possible here, then the .Interfaces.Design would not be accessible */ 
USING Consultingwerk.SmartComponents.Interfaces.Design.* FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Implementation.*    FROM PROPATH .
USING Consultingwerk.Util.*                              FROM PROPATH .
USING Progress.Lang.*                                    FROM ASSEMBLY .
USING System.ComponentModel.Component                    FROM ASSEMBLY .
USING Consultingwerk.* FROM PROPATH.

CLASS Consultingwerk.SmartComponents.Base.SmartDataAdapter  
    INHERITS SmartComponent
    IMPLEMENTS ISmartNavigationTarget, ISmartDataSource, ISmartDataTarget,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataTarget,
               System.ComponentModel.ISupportInitialize
    USE-WIDGET-POOL ABSTRACT: 

    DEFINE PROTECTED VARIABLE oSmartNavigationSource AS ISmartNavigationSource      NO-UNDO .
    
    /* Mike Fechner, Consultingwerk Ltd. 24.04.2009
       Bug 1754: Access Level to BindingSource is private!
                 Inheriting classes need to use BindingSource property as well */
    DEFINE PRIVATE   VARIABLE oBindingSource         AS Progress.Data.BindingSource NO-UNDO .      
    DEFINE PRIVATE   VARIABLE cSmartDataSourceState  AS CHARACTER                   NO-UNDO .         
    DEFINE PROTECTED VARIABLE oSmartDataSource       AS ISmartDataSource            NO-UNDO .
    DEFINE PRIVATE   VARIABLE lSmartDataSourceActive AS LOGICAL                     NO-UNDO INIT TRUE .
    
    DEFINE VARIABLE cBindingSourceExcludeFields AS CHARACTER NO-UNDO INIT "":U.
    DEFINE VARIABLE cBindingSourceIncludeFields AS CHARACTER NO-UNDO INIT "*":U.    

    /* Mike Fechner, Consultingwerk Ltd. 03.09.2013
       Static only on 10.2B to workaround too-many-temp-table issue
       Not static on 11.x to avoid issues with the combination of static
       and hybrids on 11.2 and 11.3, OE defect OE00240707  
       http://knowledgebase.progress.com/articles/Article/000042229?popup=true */
    DEFINE PRIVATE 
    &IF PROVERSION BEGINS "10.2":U &THEN 
    STATIC 
    &ENDIF TEMP-TABLE ttSmartDataTargetCollection NO-UNDO
        FIELD RecordOwner     AS Progress.Lang.Object
        FIELD SmartDataTarget AS Progress.Lang.Object
        INDEX RecordOwner RecordOwner SmartDataTarget
        .

    /* Naming the Temp-Table ttAfterRetrieveDataSubscriberCollection would cause
       the temp-table name to be > 32 chars which is not allowed */
    /* Mike Fechner, Consultingwerk Ltd. 03.09.2013
       Static only on 10.2B to workaround too-many-temp-table issue
       Not static on 11.x to avoid issues with the combination of static
       and hybrids on 11.2 and 11.3, OE defect OE00240707  
       http://knowledgebase.progress.com/articles/Article/000042229?popup=true */
    DEFINE PRIVATE 
    &IF PROVERSION BEGINS "10.2":U &THEN 
    STATIC 
    &ENDIF TEMP-TABLE ttAfterRetrieveDataSubscriberCol NO-UNDO
        FIELD RecordOwner     AS Progress.Lang.Object
        FIELD Subscriber      AS Progress.Lang.Object
        INDEX RecordOwner RecordOwner Subscriber
        .
    
    DEFINE PRIVATE VARIABLE oViewersWithDisabledEventHandler AS "System.Collections.Generic.List<System.Windows.Forms.Control>":U NO-UNDO .
    
    /*------------------------------------------------------------------------------
        Purpose: Raised when the CloseQuery method is called
        Notes:   E.g. when the parent SmartDataAdapter of the current instance does 
                 not hold any record anymore
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT QueryClosed DELEGATE System.EventHandler .
    
    /*------------------------------------------------------------------------------
        Purpose: The Constructor initializes a new SmartDataAdapter. The Default 
                 Value for cSmartDataSourceState is set to Navigatable.
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataAdapter ():
        SUPER ().

        /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
           Only subscribe to Disposed event when not in Visual Designer 
           to avoid conflicts when re-opening layouts. */        
        IF NOT THIS-OBJECT:DesignTime THEN 
            THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .        
        
        ASSIGN cSmartDataSourceState = DataSourceStateEnum:Navigatable . 
        
        oViewersWithDisabledEventHandler = NEW "System.Collections.Generic.List<System.Windows.Forms.Control>" () . 
    
        THIS-OBJECT:SmartDataTargets = NEW ListISmartDataTarget () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: The Constructor initializes a new SmartDataAdapter. The Default 
                 Value for cSmartDataSourceState is set to Navigatable.
        Notes:   
        @param poContainer The IContainer to add the current SmartDataAdapter to.                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataAdapter (poContainer AS System.ComponentModel.IContainer):
        
        SUPER (poContainer).
        
        /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
           Only subscribe to Disposed event when not in Visual Designer 
           to avoid conflicts when re-opening layouts. */        
        IF NOT THIS-OBJECT:DesignTime THEN 
            THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .        
        
        ASSIGN cSmartDataSourceState = DataSourceStateEnum:Navigatable . 
        
        oViewersWithDisabledEventHandler = NEW "System.Collections.Generic.List<System.Windows.Forms.Control>" () .
        
        THIS-OBJECT:SmartDataTargets = NEW ListISmartDataTarget () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event fired whenever the QueryPosition has changed (i.e. Add or 
                 Delete and Grids etc. need to update the current position)
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterQueryPositionChanged  DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired whenever the SmartDataAdapter has opened a new query
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterRetrieveData DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired before the SmartDataAdapter is about to open a new query
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeRetrieveData DELEGATE System.ComponentModel.CancelEventHandler .

    /*------------------------------------------------------------------------------
        Purpose: Event raised when a ISmartDataTarget of this SmartDataAdapter instance
                 begins the update state                                                                         
        Notes:                          
        @param sender The reference to the object that raised the event
        @param e The DataSourceUpdateStateEventArgs instance with the data for this event                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeginUpdateState SIGNATURE VOID (sender AS System.Object, 
                                                         e AS DataSourceUpdateStateEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired whenever the SmartDataAdapters BindingSource property 
                 changes
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The BindingSourceChangedEventArgs instance with the data for this event                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BindingSourceChanged SIGNATURE VOID (sender AS System.Object,
                                                             e AS BindingSourceChangedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised whenever the Data Adapter is positioned on a new row. 
        Notes:   The event is raised, when the BindingSource PositionChanged event 
                 is raised, when the ParentPosition has changed and the Query is closed 
                 etc.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT CurrentChanged DELEGATE System.EventHandler . 
    
    /*------------------------------------------------------------------------------
        Purpose: Event raised when a ISmartDataTarget of this SmartDataAdapter instance
                 ends the update state                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The DataSourceUpdateStateEventArgs instance with the data for this event                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT EndUpdateState SIGNATURE VOID (INPUT sender AS System.Object, 
                                                       INPUT e AS DataSourceUpdateStateEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired whenever the RecordPositon properties has changed
        Notes:
        @param sender The reference to the object that raised the event 
        @param e The SmartNavigationTargetPositionChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SmartNavigationTargetPositionChanged SIGNATURE VOID (sender AS System.Object, 
                                                                             e AS SmartNavigationTargetPositionChangedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired whenever the SmartDataSourceState property has changed
        Notes:   
        @param sender The reference to the object that raised the event 
        @param e The SmartNavigationTargetStateChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SmartNavigationTargetStateChanged SIGNATURE VOID (sender AS System.Object, 
                                                                          e AS SmartNavigationTargetStateChangedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the name of the AppServer Partition to be used
                 by this SmartDataAdapter instance
        Notes:   The property is defined in the abstract SmartDataAdapter base
                 class - even when some implementations may not require an AppServer
                 connection
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns if the DataSource currently has a record available 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Available AS LOGICAL NO-UNDO 
    GET:
        
        DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hDataSource AS HANDLE    NO-UNDO.
    
        IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource) THEN 
            RETURN ? . 

        ASSIGN hDataSource = THIS-OBJECT:BindingSource:Handle .

        IF NOT VALID-HANDLE (hDataSource) THEN 
            RETURN ? .
            
        IF hDataSource:TYPE = WidgetTypeEnum:BUFFER THEN 
            RETURN hDataSource:AVAILABLE . 
                        
        /* Works on QUERY and DATASET */
        IF hDataSource:NUM-BUFFERS > 0 THEN                         
            ASSIGN hBuffer = hDataSource:GET-BUFFER-HANDLE (1) .                        
        ELSE 
            RETURN ? .                         
                        
        IF VALID-HANDLE (hBuffer) THEN 
            RETURN hBuffer:AVAILABLE . 
        ELSE 
            RETURN ? .

    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Get/Set the BindingSource of the current Class. The BindingSource 
                 can be set to any class derived from Progress.Data.BindingSource.
                 This Property is set internaly if the SmartDataAdapter is attached
                 to a Browser where a DesignTime BindingSource is attached or if
                 the developer calls RetrieveData (). 
                 
                 If a valid BindingSource exists inside the SmartDataAdapter the 
                 EventHandlers get unsubscribe from that instance and it is replaced 
                 with the new one. After that the Handlers for the Events of the 
                 BindingSource are subscribed to the new instance.
        Notes:   Enforced by Interface ISmartDataSource.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSource AS Progress.Data.BindingSource NO-UNDO
    GET:
        IF THIS-OBJECT:DesignTime THEN 
            RETURN ? . 
        
        RETURN oBindingSource .
    END.
    PROTECTED SET (arg AS Progress.Data.BindingSource):
        DEFINE VARIABLE oOldBindingSource AS Progress.Data.BindingSource NO-UNDO . 
        
        ASSIGN oOldBindingSource = oBindingSource . 
        
        IF VALID-OBJECT(oBindingSource) THEN DO:
            oBindingSource:PositionChanged:Unsubscribe (PositionChangedHandler) NO-ERROR .
            oBindingSource:CancelCreateRow:Unsubscribe (CancelCreateRowHandler) NO-ERROR . 
            oBindingSource:CreateRow:Unsubscribe (CreateRowHandler) NO-ERROR .
            oBindingSource:OffEnd:Unsubscribe (OffEndHandler) NO-ERROR . 
        END. 
        
        oBindingSource = arg .
        
        IF VALID-OBJECT (arg) THEN DO:
            oBindingSource:PositionChanged:Subscribe (PositionChangedHandler) .        
            oBindingSource:CancelCreateRow:Subscribe (CancelCreateRowHandler) . 
            oBindingSource:CreateRow:Subscribe (CreateRowHandler) . 
            oBindingSource:OffEnd:Subscribe (OffEndHandler) .
        END.
        
        OnBindingSourceChanged(NEW BindingSourceChangedEventArgs (oOldBindingSource, arg)) .
    END.

     /*------------------------------------------------------------------------------
        Purpose: Get/Set the Exclude Fields for the BindingSource created by the 
                 CreateBindingSource method.                  
        Notes:   As optional parameters, the constructors of the BindingSource can accept 
                 comma-separated lists of data object fields, in display order, to make 
                 available to or exclude from the UI. If you want to make only a few fields 
                 available, list them in the included field list. If you want to make most 
                 fields available, set the available fields to be all fields and list 
                 the remaining fields in the excluded field list. When the included field 
                 list is an asterisk (*), all fields in the data object are made available 
                 to any bound UI control.
                 
                 The excluded field list is only meaningful when the included field list is 
                 an asterisk (*). If the included field list contains specific fields, 
                 the excluded field list is ignored. However, when you are not specifying 
                 any excluded fields, you still must specify a blank value ("") to match the
                 constructor's signature.                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSourceExcludeFields AS CHARACTER NO-UNDO INIT "":U     
    GET:
        RETURN cBindingSourceExcludeFields .
    END GET.
    SET (arg AS CHARACTER):
        IF VALID-OBJECT(THIS-OBJECT:BindingSource) AND (arg <> cBindingSourceExcludeFields)THEN 
            UNDO, THROW NEW AppError ("BindingSourceExcludeFields can only be set before BindingSource is created.", 0) .
            
        ASSIGN cBindingSourceExcludeFields = arg .                    
    END SET. 
    
     /*------------------------------------------------------------------------------
        Purpose: Get/Set the Include Fields for the BindingSource created by the 
                 CreateBindingSource method.                   
        Notes:   See BindingSourceExcludeFields for further comments    
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSourceIncludeFields AS CHARACTER NO-UNDO INIT "*":U
    GET:
        RETURN cBindingSourceIncludeFields .
    END GET.
    SET (arg AS CHARACTER):
        IF VALID-OBJECT(THIS-OBJECT:BindingSource) AND (arg <> cBindingSourceIncludeFields) THEN 
            UNDO, THROW NEW AppError ("BindingSourceIncludeFields can only be set before BindingSource is created.", 0) .
                    
        ASSIGN cBindingSourceIncludeFields = arg .                    
    END. 
    

    /*------------------------------------------------------------------------------
       Purpose: Returns a reference to the BindingSource of the SmartDataSource
    ------------------------------------------------------------------------------*/      
    DEFINE PUBLIC PROPERTY DataSourceBindingSource AS Progress.Data.BindingSource NO-UNDO 
    GET.
    PRIVATE SET.
    
    /*------------------------------------------------------------------------------
       Purpose: Indicates that the SmartDataAdapter is currrently being 
                Initialized
       Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Initializing AS LOGICAL NO-UNDO INIT FALSE  
    GET.
    PRIVATE SET.
    
    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartDataSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
                SCL-725 - code moved from LinkDataSource.i directly into class file
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkDataSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) THEN 
            ASSIGN THIS-OBJECT:SmartDataSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) .         
    END.

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartNavigationSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
                SCL-725 - code moved from LinkNavigationSource.i directly into class file
   ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkNavigationSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartNavigationSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartNavigationSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartNavigationSource) THEN 
            ASSIGN THIS-OBJECT:SmartNavigationSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartNavigationSource) .         
    END.
                
    /*------------------------------------------------------------------------------
        Purpose: Get/Set if the Position of this Adapters BindingSource's position
                 prio to the last RetrieveData call. 
        Notes:   The value of this property is used to invoke the ParentPosition
                 Changed event of Child-DataAdapters after this DataAdapter
                 has refetched data   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY PreviousPosition AS INTEGER NO-UNDO INIT 0
    GET.
    SET.    

      /*------------------------------------------------------------------------------
        Purpose: Get/Set if this instance of the SmartDataAdapter is subscribed  
                 to the PositionChanged event of the SmartDataSource's BindingSource 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSourceActive AS LOGICAL NO-UNDO INIT TRUE 
    GET:
        RETURN lSmartDataSourceActive .
    END GET.
    SET (INPUT arg AS LOGICAL):
        IF THIS-OBJECT:DesignTime THEN 
           RETURN . 
        
        CASE arg:
            WHEN ? THEN 
                UNDO, THROW NEW AppError ("SmartDataSourceActive needs to be yes or no.", 0) .
            WHEN TRUE THEN DO:
                ASSIGN lSmartDataSourceActive = arg .
                
                IF VALID-OBJECT (oSmartDataSource) AND VALID-OBJECT(oSmartDataSource:BindingSource) THEN DO: 
                    oSmartDataSource:BindingSource:PositionChanged:Subscribe(ParentPositionChangedBase) NO-ERROR .        
            
                    ParentPositionChangedBase (?, ?) .
                END.                               
            END.
            WHEN FALSE THEN DO:
                ASSIGN lSmartDataSourceActive = arg .
                
                IF VALID-OBJECT (oSmartDataSource) AND VALID-OBJECT(oSmartDataSource:BindingSource) THEN
                    oSmartDataSource:BindingSource:PositionChanged:Unsubscribe(ParentPositionChangedBase) NO-ERROR .        
            END.
        END CASE .                      
    END SET.
    
     /*------------------------------------------------------------------------------
        Purpose: Get/Set the SmartDataSourceState of the current Class. 
                 This Property is set to Updating if the Method BeginUpdateState () of the
                 SmartDataAdapter instance is called and it is set to Navigatable
                 during initialization and if the Property SmartTableIOState of a
                 Viewer is set to anything but ModifyingData. 
        Notes:   Possible Values (DataSourceStateEnum) are Navigatable, Updating
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSourceState AS CHARACTER NO-UNDO INIT "Navigatable":U
    GET :
        RETURN cSmartDataSourceState .         
    END GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE cPrevious AS CHARACTER NO-UNDO.
        
        IF NOT DataSourceStateEnum:IsValid (arg) THEN 
            UNDO, THROW NEW Progress.Lang.AppError 
                ("Possible values for SmartDataSourceState are: Navigatable, Updating", 0) .
        
        ASSIGN cPrevious             = cSmartDataSourceState 
               cSmartDataSourceState = arg .
                
        CASE cPrevious:
            WHEN DataSourceStateEnum:Updating THEN DO:
                IF arg = DataSourceStateEnum:Navigatable THEN DO:
                   EndUpdateState (THIS-OBJECT:UpdatingSmartDataTarget) .
                END.
            END.
        END CASE.
        
        /* Mike Fechner, Consultingwerk Ltd. 22.08.2009
           Raise events */        
        OnSmartNavigationTargetStateChanged (NEW SmartNavigationTargetStateChangedEventArgs (cSmartDataSourceState)) .

    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of SmartDataTargets as a generic list
        Notes:      
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY SmartDataTargets AS ListISmartDataTarget NO-UNDO 
    GET .
    PRIVATE SET . 

    /*------------------------------------------------------------------------------
        Purpose: Get and Set the current SmartNavigationSource of the SmartDataAdapter.
                 Every SmartNavigationTarget is registered with handshake and kept in a
                 collection.
                 This Property is set by the developer when initializing a Control 
                 implementing the Interface ISmartDataSource.
                 Enforced by Interface ISmartDataSource.
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY SmartNavigationSource AS ISmartNavigationSource NO-UNDO 
    GET:
        RETURN oSmartNavigationSource . 
    END GET.
    SET (arg AS ISmartNavigationSource):
        DEFINE VARIABLE err AS AppError NO-UNDO . 
        
        /* Mike Fechner, Consultingwerk Ltd. 21.08.2009
           Only a single SmartNavigationSource can be set using the Property */        
        IF VALID-OBJECT (oSmartNavigationSource) AND VALID-OBJECT (arg) THEN DO:
            err = NEW AppError ("This SmartDataAdapter instance can only have a single SmartNavigationSource instance set using this property.", 0) .
            err:AddMessage ("Add the additional SmartNavigationSource using the method ~"AddSmartNavigationSource~".", 0).  
            UNDO, THROW err .
        END.
        
        IF VALID-OBJECT (arg) THEN DO:
            ASSIGN oSmartNavigationSource = arg . 
            
            AddSmartNavigationSource (arg) .
        END.
        ELSE DO:
            IF VALID-OBJECT (oSmartNavigationSource) THEN 
                THIS-OBJECT:RemoveSmartNavigationSource (oSmartNavigationSource) .                
            
            ASSIGN oSmartNavigationSource = ? .
        END.
    END.    
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a Value of RecordPositionEnum representing the current position
                 The Property is read in the Method PositionChangedHandler () of a
                 Class implementing the Interface ISmartNavigationTarget.
                 Enforced by Interface ISmartNavigationTarget.
        Notes:   Possible Values (RecordPositionEnum):
                 NoRecordAvailable, OnlyRecord, FirstRecord, LastRecord, NotFirstOrLast
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecordPosition AS CHARACTER NO-UNDO INIT "":U
    GET():
        RETURN GetRecordPosition () .
    END GET.
     
    /*------------------------------------------------------------------------------
        Purpose: Activates SmartViewer TextChanged/ChechedChanged Event Handlers
                 after a navigation (FetchFirst/FetchNext/FetchPrev/FetchLast)                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ActivateSmartViewerEventHandler ():
        
        {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in oViewersWithDisabledEventHandler}
        
            CAST (oControl, SmartViewerControl):ActivateEventHandler () .
        
        END.

        oViewersWithDisabledEventHandler:Clear () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a(nother) SmartNavigationSource to the SmartDataAdapter 
                 instance                                                                        
        Notes:   Enabled multiple SmartNavigationSources to the SmartDataAdapter.
        @param poSmartNavigationSource The ISmartNavigationSource to add to the list of NavigationSources                              
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID AddSmartNavigationSource (poSmartNavigationSource AS ISmartNavigationSource):
        
        /* Mike Fechner, Consultingwerk Ltd. 22.08.2009
           Register as first SmartNavigationSource */        
        IF NOT VALID-OBJECT (THIS-OBJECT:SmartNavigationSource) THEN DO:
            THIS-OBJECT:SmartNavigationSource = poSmartNavigationSource .
            RETURN .           
        END.
        
        poSmartNavigationSource:RegisterSmartNavigationTarget (THIS-OBJECT) .
        
        THIS-OBJECT:SmartNavigationTargetPositionChanged:Subscribe (poSmartNavigationSource:UpdateSmartNavigationTargetPosition) .
        THIS-OBJECT:SmartNavigationTargetStateChanged:Subscribe (poSmartNavigationSource:UpdateSmartNavigationTargetState) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the Data Schema to the BindingSource                                                                        
        Notes:   Do not call this method from an overloaded method when the 
                 overloaded method does set the Except fields          
        @param poBindingSource The BindingSource to attach the Schema to                                                              
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID AttachSchemaToBindingSource (poBindingSource AS Progress.Data.BindingSource):
        
        THIS-OBJECT:BindingSource:SetFields (THIS-OBJECT:BindingSourceIncludeFields,
                                             THIS-OBJECT:BindingSourceExcludeFields, "":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked at the beginning of a Containers InitializeComponents 
                 method                                                       
        Notes:   Enforced by System.ComponentModel.ISupportInitialize           
                 http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.begininit(VS.80).aspx                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BeginInit ():
        
        ASSIGN THIS-OBJECT:Initializing = TRUE  .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeginUpdateState event of the SmartDataSource                                                                       
        Notes:                   
        @param sender The reference to the System.Object that raised the event
        @param e The DataSourceUpdateStateEventArgs with the data for this event                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BeginUpdateStateHandler (sender AS System.Object,
                                                   e AS DataSourceUpdateStateEventArgs):
        
        BeginUpdateState (e:UpdatingDataTarget) .

    END METHOD.

      /*------------------------------------------------------------------------------
        Purpose: Invoked from the SmartDataTarget to cancel the creation of a new 
                 row
        Notes:   
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID CancelCreateRecord ():
        
        THIS-OBJECT:BindingSource:CancelEdit () .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Event Handler for the CancelCreateRow event of the ProBindingSource
        Notes:   This method is abstract and needs to be overridden by the actual 
                 implementation of the BindingSource.
        @param sender The reference to the System.Object that raised the event
        @param args The CancelCreateRowEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED ABSTRACT VOID CancelCreateRowHandler (sender AS System.Object, 
                                                           args AS Progress.Data.CancelCreateRowEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Empties the Resultset of this SmartDataAdapter. Used to clear grids
        Notes:   This method used to be abstract, but contains implementation now!
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseQuery ():
        
        FOR EACH ttSmartDataTargetCollection WHERE 
                 ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT NO-LOCK:

            IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) AND 
                TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget, SmartDataAdapter) THEN 
                
                CAST (ttSmartDataTargetCollection.SmartDataTarget, SmartDataAdapter):CloseQuery () .                
        END.        
        
        THIS-OBJECT:OnQueryClosed (System.EventArgs:Empty) .
        THIS-OBJECT:OnCurrentChanged (System.EventArgs:Empty) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the AppServer partition associated with 
                 this SmartDataAdapter                                                                        
        Notes:
        @return The handle of the AppServer partition used by this SmartDataAdapter                                                                         
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED HANDLE ConnectService ():

        RETURN FrameworkSettings:AppServerServiceManager:ConnectService(THIS-OBJECT:AppServerPartition) . 
                
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked from the SmartDataTarget to create a new row                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID CreateRecord ():
        
        THIS-OBJECT:BindingSource:AddNew () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handler for the CreateRow event of the ProBindingSource
        Notes:   This method is abstract and needs to be overridden by the actual 
                 implementation of the BindingSource.
        @param sender The reference to the System.Object that raised the event
        @param args The reference to the CreateRowEventArgs instance with the data for this event 
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED ABSTRACT VOID CreateRowHandler (sender AS System.Object, 
                                                     args AS Progress.Data.CreateRowEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Activates SmartViewer TextChanged/ChechedChanged Event Handlers
                 after a navigation (FetchFirst/FetchNext/FetchPrev/FetchLast)                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeactivateSmartViewerEventHandler ():
        
        FOR EACH ttSmartDataTargetCollection WHERE 
            ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT:
                
            IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) AND 
               TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget,
                        Consultingwerk.SmartComponents.Base.SmartViewerControl) AND
               CAST (ttSmartDataTargetCollection.SmartDataTarget,
                     Consultingwerk.SmartComponents.Base.SmartViewerControl):EventHandlerActive                         
                
            THEN DO:
                CAST (ttSmartDataTargetCollection.SmartDataTarget,
                     Consultingwerk.SmartComponents.Base.SmartViewerControl):DeactivateEventHandler () .                            
                                            
                oViewersWithDisabledEventHandler:Add (CAST (ttSmartDataTargetCollection.SmartDataTarget,
                                                            System.Windows.Forms.Control)) .                            
            END.
        END. 

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: This protected method can be used to remove the current row from a 
                 Query. This method calls DeleteQueryRow(FALSE).
        Notes:   
        @return Logical value indicating if the method was successful
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED LOGICAL DeleteQueryRow ():
        RETURN DeleteQueryRow (FALSE) .
    END METHOD .         

    /*------------------------------------------------------------------------------
        Purpose: This protected method can be used to remove the current row from a 
                 Query. The  parameter is logical and controls if only the first
                 table (FALSE) or all rows shall be removed.
        Notes:   Any error (like AppError from a Database trigger will not be catched
                 so that the caller cna
        @param plRemoveAll Logical value indicating if all buffers should be deleted (TRUE) or just the first query buffer (FALSE)
        @return Logical value indicating if the method was successful
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED LOGICAL DeleteQueryRow (plRemoveAll AS LOGICAL):
        
        DEFINE VARIABLE hQuery          AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBuffer         AS HANDLE  NO-UNDO.
        DEFINE VARIABLE iBuffer         AS INTEGER NO-UNDO.
        DEFINE VARIABLE iLastBuffer     AS INTEGER NO-UNDO.
        DEFINE VARIABLE iPosition       AS INTEGER NO-UNDO.
        
        IF NOT VALID-OBJECT(THIS-OBJECT:BindingSource) OR NOT VALID-HANDLE(THIS-OBJECT:BindingSource:HANDLE) THEN 
            RETURN FALSE .
            
        IF THIS-OBJECT:BindingSource:HANDLE:TYPE <> "QUERY":U THEN 
            RETURN FALSE .             

        ASSIGN hQuery = THIS-OBJECT:BindingSource:HANDLE . 
             
        
        IF NOT VALID-HANDLE(hQuery) THEN 
            RETURN FALSE .
        
        IF NOT hQuery:GET-BUFFER-HANDLE(1):AVAILABLE THEN 
            RETURN FALSE .
        
        IF plRemoveAll = FALSE THEN 
            iLastBuffer = 1 .
        ELSE 
            iLastBuffer = hQuery:NUM-BUFFERS .
        
        /* Save last BingingSource position */
        iPosition = THIS-OBJECT:BindingSource:Position .

        /* Remove the query records */
        DO TRANSACTION ON ERROR UNDO, THROW:
            DO iBuffer = iLastBuffer TO 1 BY -1:
                hQuery:GET-BUFFER-HANDLE(iBuffer):FIND-CURRENT (EXCLUSIVE-LOCK) .
                /* Delete the record from the query - without NO-ERROR.
                   Errors should be thrown to the called */
                hQuery:GET-BUFFER-HANDLE(iBuffer):BUFFER-DELETE() . 
            END.
        END.
                
        hQuery:DELETE-RESULT-LIST-ENTRY () .
        
        THIS-OBJECT:BindingSource:RefreshAll() .
        
        /* Reposition the Query */
        IF THIS-OBJECT:BindingSource:Position <> iPosition AND THIS-OBJECT:BindingSource:Count > 0 THEN DO ON ERROR UNDO, THROW:
            IF iPosition < THIS-OBJECT:BindingSource:Count THEN 
                THIS-OBJECT:BindingSource:POSITION = iPosition . 
            ELSE ASSIGN THIS-OBJECT:BindingSource:Position = THIS-OBJECT:BindingSource:Count - 1 .         
            
        END.        
        
    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: The DeleteRow is an abstract method - that means that it needs 
                 to be overridden in a class that inherits from this base class.
                 Enforced by Interface ISmartDataSource.
        Notes:   
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC ABSTRACT VOID DeleteRow () . 
    
     /*------------------------------------------------------------------------------
        Purpose: Deregister a SmartDataTarget with the SmartDataSource. Usually done 
                 when a SmartDataTarget is deleted and the SmartDataSource is still
                 running
        Notes:
        @param poSmartDataTarget The reference to the ISmartDataTarget to deregister   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartDataTarget (poSmartDataTarget AS ISmartDataTarget).
        
        FIND ttSmartDataTargetCollection 
            WHERE ttSmartDataTargetCollection.RecordOwner     = THIS-OBJECT 
              AND ttSmartDataTargetCollection.SmartDataTarget = poSmartDataTarget
            EXCLUSIVE-LOCK NO-ERROR . 
            
        IF AVAILABLE ttSmartDataTargetCollection THEN 
            DELETE ttSmartDataTargetCollection . 

        IF THIS-OBJECT:SmartDataTargets:Contains (poSmartDataTarget) THEN 
            THIS-OBJECT:SmartDataTargets:Remove (poSmartDataTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Disposed event of this instance                                                                      
        Notes:                                       
        @param sender The reference to the object that raised the Disposed event
        @param e The System.EventArgs with the data for this event                                  
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID DisposedHandler (sender AS System.Object, 
                                           e AS System.EventArgs  ):

        DELETE OBJECT THIS-OBJECT . 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Invoked at the end of a Containers InitializeComponents method                                                       
        Notes:   Enforced by System.ComponentModel.ISupportInitialize           
                 http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.endinit(VS.80).aspx                                                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EndInit ():
        
       ASSIGN THIS-OBJECT:Initializing = FALSE . 
    
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: The Message to for instance enable a Browser is send to all 
                 SmartDataTargets except the calling one.
        Notes:   This Method is called from the SmartViewerControl derived Class when
                 starting to Modify or Add data.
                 Enforced by Interface ISmartDataSource.
        @param poSmartDataTarget The reference to the ISmartDataTarget that ends the UpdateState    
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID EndUpdateState (poSmartDataTarget AS ISmartDataTarget):       
    
        IF NOT VALID-OBJECT (THIS-OBJECT:UpdatingSmartDataTarget) OR 
           THIS-OBJECT:UpdatingSmartDataTarget <> poSmartDataTarget THEN 
           
           RETURN .
         
        THIS-OBJECT:SmartDataSourceState = DataSourceStateEnum:Navigatable .
    
        OnEndUpdateState (NEW DataSourceUpdateStateEventArgs (poSmartDataTarget)) .

        THIS-OBJECT:UpdatingSmartDataTarget = ? . 

        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN
            THIS-OBJECT:EndUpdateStateInDataSource (poSmartDataTarget) . 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EndUpdateState event of the SmartDataSource                                                                       
        Notes:                   
        @param sender The reference to the System.Object that raised the event
        @param e The DataSourceUpdateStateEventArgs with the data for this event                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EndUpdateStateHandler (sender AS System.Object,
                                                 e AS DataSourceUpdateStateEventArgs):
        
        EndUpdateState (e:UpdatingDataTarget) .

    END METHOD.
    
	/*------------------------------------------------------------------------------
	    Purpose: Invokes EndUpdateState in the SmartDataSource of this SmartDataAdapter
	    Notes:   Method is overridable, so that a child class of this class (e.g. 
	             SmartBusinessEntityAdapter) can override this and add further criterias
	             to the call 
        @param poSmartDataTarget The reference to the ISmartDataTarget that ends the UpdateState    
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID EndUpdateStateInDataSource (poSmartDataTarget AS ISmartDataTarget):
		
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN
            THIS-OBJECT:SmartDataSource:EndUpdateState (poSmartDataTarget) .

	END METHOD.

      /*------------------------------------------------------------------------------
        Purpose: This Method is enforced by the Interface ISmartDataTarget and is 
                 only required for visual DataTargets.
        Notes:   Required only for visual DataTagets (Viewer)
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID EvaluateTableIOState ():        
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: This Method moves to the first DataRow inside the BindingSource.
        Notes:   It is called by The Navigation Tools of the Toolbars.
                 Enforced by Interface ISmartNavigationTarget.
        @return Logical value, currently not used   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FetchFirst ():
        
        DEFINE VARIABLE oBrowser AS ISmartDataBrowser NO-UNDO . 
        
        DeactivateSmartViewerEventHandler() .    
        
        THIS-OBJECT:BindingSource:MoveFirst() .

        oBrowser = THIS-OBJECT:FindLinkedSmartDataBrowser () .

        IF VALID-OBJECT (oBrowser) THEN DO:
            oBrowser:ScrollToFirstRow() .
        END.

        FINALLY:
            ActivateSmartViewerEventHandler() .        
        END FINALLY.

    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose: This Method moves to the last DataRow inside the BindingSource.
        Notes:   It is called by The Navigation Tools of the Toolbars.
                 Enforced by Interface ISmartNavigationTarget.
        @return Logical value, currently not used   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FetchLast ():
        
        DeactivateSmartViewerEventHandler() .   
        
        THIS-OBJECT:BindingSource:MoveLast() .

        FINALLY:
            ActivateSmartViewerEventHandler() .     
        END FINALLY.

    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose: This Method moves to the next DataRow inside the BindingSource.
        Notes:   It is called by The Navigation Tools of the Toolbars.
                 Enforced by Interface ISmartNavigationTarget.
        @return Logical value, currently not used   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FetchNext ():

        DeactivateSmartViewerEventHandler() .   
        
        THIS-OBJECT:BindingSource:MoveNext() .             

        FINALLY:
            ActivateSmartViewerEventHandler() .     
        END FINALLY.

    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose: This Method moves to the previous DataRow inside the BindingSource.
        Notes:   It is called by The Navigation Tools of the Toolbars.
                 Enforced by Interface ISmartNavigationTarget.   
        @return Logical value, currently not used   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FetchPrev ():
        
        DeactivateSmartViewerEventHandler() .   
        
        THIS-OBJECT:BindingSource:MovePrevious() . 

        FINALLY:
            ActivateSmartViewerEventHandler() .     
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the linked SmartDataBrowser, if any                                                                        
        Notes:   Returns ? when no browser is linked
        @return The reference to the linked ISmartDataBrowser or ? when no browser is linked                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ISmartDataBrowser FindLinkedSmartDataBrowser ():
        
        FOR EACH ttSmartDataTargetCollection WHERE ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT:
            IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) AND 
               TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget, ISmartDataBrowser) THEN 
               
                RETURN CAST (ttSmartDataTargetCollection.SmartDataTarget, ISmartDataBrowser) .
        END.

        RETURN ?.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the first linked ISmartDataTarget which is 
                 also an ISmartTableIOTarget, that has a valid SmartTableIOSource assigned                                                                        
        Notes:   Returns ? when no updating ISmartDataTarget is linked
        @return The reference to the linked updating ISmartDataTarget as a ISmartTableIOTarget or ? when no updating ISmartDataTarget is linked                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ISmartTableIOTarget FindLinkedUpdatingSmartDataTarget ():
        
        DEFINE VARIABLE oTarget AS ISmartTableIOTarget NO-UNDO . 
        
        FOR EACH ttSmartDataTargetCollection WHERE ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT:
            
            IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) AND 
               TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget, ISmartTableIOTarget) THEN DO:
               
               ASSIGN oTarget = CAST (ttSmartDataTargetCollection.SmartDataTarget, ISmartTableIOTarget) .
               
               IF VALID-OBJECT (oTarget:SmartTableIOSource) THEN 
                   RETURN oTarget . 
            END.
        END.

        RETURN ?.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CHR(1) delimited list of field values from the 
                 DataSource                                                                        
        Notes:   The fields parameter is a CHR(1) delimited list of field names. 
                 Unqualified field names (CustNum) must exist in the first buffer 
                 of the data source (e.g. Entity Table), qualified field names are 
                 expected in the named buffer (eCustomer.CustNum) 
        @param pcFields The comma delimited list of fields to return
        @return The CHR(1) delimited list of field values                                                                    
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC CHARACTER GetFieldValues (pcFields AS CHARACTER):
        
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cValue      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cReturn     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cField      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hDataSource AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cFieldName  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iExtent     AS INTEGER   NO-UNDO.
    
        IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource) THEN 
            RETURN "?":U . 

        ASSIGN hDataSource = THIS-OBJECT:BindingSource:Handle .

        DO i = 1 TO NUM-ENTRIES (pcFields):
            ASSIGN cField = ENTRY(i, pcFields) .
            
            IF NUM-ENTRIES(cField, ".":U) = 1 THEN DO:
                ASSIGN hBuffer = hDataSource:GET-BUFFER-HANDLE (1) .

                Consultingwerk.Util.BufferHelper:GetFieldNameAndExtent (cField, OUTPUT cFieldName, OUTPUT iExtent).

                IF hBuffer:AVAILABLE THEN DO:
                    IF iExtent = 0 THEN 
                        ASSIGN cValue = STRING(hBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE) .
                    ELSE
                        ASSIGN cValue = STRING(hBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE[iExtent]) . 
                END.
                ELSE 
                    ASSIGN cValue = "?":U .  
            END.
            ELSE DO:
                ASSIGN hBuffer = hDataSource:GET-BUFFER-HANDLE (ENTRY(1, cField, ".":U)) .    

                Consultingwerk.Util.BufferHelper:GetFieldNameAndExtent (ENTRY(2, cField, ".":U), OUTPUT cFieldName, OUTPUT iExtent).

                IF hBuffer:AVAILABLE THEN DO: 
                    IF iExtent = 0 THEN 
                        ASSIGN cValue = STRING(hBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE) .
                    ELSE 
                        ASSIGN cValue = STRING(hBuffer:BUFFER-FIELD(cFieldName):BUFFER-VALUE[iExtent]) .
                END.
                ELSE 
                    ASSIGN cValue = "?":U .                  
            END.
            
            IF cValue = ? THEN 
                ASSIGN cValue = "?":U .
                
            ASSIGN cReturn = cReturn + (IF cReturn > "":U THEN CHR(1) ELSE "":U) + cValue .
        END.

        RETURN cReturn .
    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Evaluates the current record position property value                                                                        
        Notes:   Protected method so that child classes may have a different 
                 implementation    
        @return The current record position as a CHARACTER representing an RecordPositionEnum value                                                                  
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED CHARACTER GetRecordPosition ():    
            
        IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource) OR THIS-OBJECT:BindingSource:Count = 0 THEN 
            RETURN RecordPositionEnum:NoRecordAvailable .
            
        IF THIS-OBJECT:BindingSource:COUNT = 1 THEN 
            RETURN RecordPositionEnum:OnlyRecord .
            
        IF THIS-OBJECT:BindingSource:POSITION = 0 THEN 
            RETURN RecordPositionEnum:FirstRecord . 
            
        IF THIS-OBJECT:BindingSource:POSITION >= THIS-OBJECT:BindingSource:COUNT - 1 THEN  
            RETURN RecordPositionEnum:LastRecord .                                                            
                        
        RETURN RecordPositionEnum:NotFirstOrLast . 
        
    END METHOD.

	/*------------------------------------------------------------------------------
	   Purpose: Returns a logical value indicating if the SmartDataAdapter can provide
	            the given fields
	   Notes:   Useful to validate if the GetFieldValues methos can return vslues for 
	            the given fields
	   @param pcFields Comma delimited list of field names
	   @return Logical value indicating if the given fields are available in the Data Adapter instance
	------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL HasFields (pcFields AS CHARACTER):
		
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cValue      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cReturn     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cField      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hDataSource AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cFieldName  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iExtent     AS INTEGER   NO-UNDO.
    
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:BindingSource """BindingSource"":U"} .

        ASSIGN hDataSource = THIS-OBJECT:BindingSource:Handle .

        DO i = 1 TO NUM-ENTRIES (pcFields):
            ASSIGN cField = ENTRY(i, pcFields) .
            
            IF NUM-ENTRIES(cField, ".":U) = 1 THEN DO:
                ASSIGN hBuffer = hDataSource:GET-BUFFER-HANDLE (1) .

                Consultingwerk.Util.BufferHelper:GetFieldNameAndExtent (cField, OUTPUT cFieldName, OUTPUT iExtent).

                IF NOT BufferHelper:HasField (hBuffer, cField) THEN 
                    RETURN FALSE . 
            END.
            ELSE DO:
                ASSIGN hBuffer = hDataSource:GET-BUFFER-HANDLE (ENTRY(1, cField, ".":U)) .    

                Consultingwerk.Util.BufferHelper:GetFieldNameAndExtent (ENTRY(2, cField, ".":U), OUTPUT cFieldName, OUTPUT iExtent).

                IF NOT BufferHelper:HasField (hBuffer, cField) THEN 
                    RETURN FALSE . 
            END.
        END.

        RETURN TRUE .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the ParentPositionChanged event handler in all connected 
                 SmartDataAdapter instances                                                                        
        Notes:  
        @param sender The reference to the sender associated with the event
        @param e The reference to the System.EventArgs with the data for the event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InvokeParentPositionChanged (sender AS System.Object, 
                                                       e AS System.EventArgs):
        
        FOR EACH ttSmartDataTargetCollection WHERE ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT NO-LOCK:
            IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) AND 
               TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget, SmartDataAdapter) THEN
                CAST (ttSmartDataTargetCollection.SmartDataTarget, SmartDataAdapter):ParentPositionChangedBase (sender, e).
        END.

    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Returns if if this Adapter or any of it's Data-Target Adapters are 
	             updating (a Data-Target with SmartTableIOState:ModifyingData)
	    Notes:
	    @param plIndirectOnly Logical value indicating if only TableIOChilds of linked data adapters are tested
	    @return Logical value indicating if this Adapter or any of it's Data-Target Adapters are updating 
	------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL IsUpdating (plIndirectOnly AS LOGICAL):
		
		{Consultingwerk/foreachABL.i ISmartDataTarget oTarget in THIS-OBJECT:SmartDataTargets}

            IF NOT plIndirectOnly THEN 
                IF TYPE-OF (oTarget, ISmartTableIOTarget) THEN 
                    IF CAST (CAST (oTarget, Progress.Lang.Object), ISmartTableIOTarget):SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
                        RETURN TRUE .  

            IF TYPE-OF (oTarget, SmartDataAdapter) AND CAST (oTarget, SmartDataAdapter):IsUpdating (FALSE) = TRUE THEN 
                RETURN TRUE .  
		END.

		RETURN FALSE .

	END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Event handler for the OffEnd event of the ProBindingSource object                                                                        
        Notes:   Used as abstract method / place holder here. Batching needs to be 
                 supported by the actual DataAdapter class. 
        @param sender The reference to the object that raised the event
        @param e The OffEndEventArgs with the data for the event                                                                       
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID OffEndHandler (sender AS System.Object, 
                                         e AS Progress.Data.OffEndEventArgs):
        
        /* NOOP */
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterQueryPositionChanged event                                                              
        Notes:                                                                        
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterQueryPositionChanged (e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty . 
        
        THIS-OBJECT:AfterQueryPositionChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterRetrieveData event                                                                        
        Notes:   Invokes the AfterRetrieveDataCallack method in all subscribers                                                                        
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID OnAfterRetrieveData (e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
          ASSIGN e = System.EventArgs:Empty .
        
        /* Mike Fechner, Consultingwerk Ltd. 21.08.2009
           Raise AfterRetrieveData event */
        THIS-OBJECT:AfterRetrieveData:Publish (THIS-OBJECT, e) .
        
        FOR EACH ttAfterRetrieveDataSubscriberCol WHERE 
                 ttAfterRetrieveDataSubscriberCol.RecordOwner = THIS-OBJECT NO-LOCK:

            IF VALID-OBJECT (ttAfterRetrieveDataSubscriberCol.Subscriber) THEN 
                CAST (ttAfterRetrieveDataSubscriberCol.Subscriber, IAfterRetrieveDataSubscriber):AfterRetrieveDataCallback (THIS-OBJECT) .                             
        END.
                
        /* Let SmartDataTagets evaluate their TableIOState */
        FOR EACH ttSmartDataTargetCollection WHERE 
                 ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT NO-LOCK:
            
            IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) THEN 
                CAST (ttSmartDataTargetCollection.SmartDataTarget, ISmartDataTarget):EvaluateTableIOState() .                     
        END.                                      
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeRetrieveData event                                                                       
        Notes:                   
        @param e The CancelEventArgs with the data for the event
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID OnBeforeRetrieveData (e AS System.ComponentModel.CancelEventArgs):
        
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeRetrieveData":U) .
        
        THIS-OBJECT:BeforeRetrieveData:Publish (THIS-OBJECT, e) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeginUpdateState event                                                                        
        Notes: 
        @param e The DataSourceUpdateStateEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeginUpdateState (e AS DataSourceUpdateStateEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The BeginUpdateState event requires a valid event arg!", 0) .
        
        THIS-OBJECT:BeginUpdateState:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BindingSourceChanged event                                                                       
        Notes:                                                                        
        @param e The BindingSourceChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID OnBindingSourceChanged (e AS BindingSourceChangedEventArgs):

        IF NOT VALID-OBJECT (e) THEN 
          ASSIGN e = NEW BindingSourceChangedEventArgs (?, ?) .
        
        THIS-OBJECT:BindingSourceChanged:Publish (THIS-OBJECT, e).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the CurrentChanged event
        Notes:   
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnCurrentChanged (e AS System.EventArgs):
            
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty . 
    
        THIS-OBJECT:CurrentChanged:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the EndUpdateState event                                                                        
        Notes:                 
        @param e The DataSourceUpdateStateEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnEndUpdateState (e AS DataSourceUpdateStateEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The EndUpdateState event requires a valid event arg!", 0) .
        
        THIS-OBJECT:EndUpdateState:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the QueryClosed
        Notes:   
        @param e The eventargs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnQueryClosed (e AS System.EventArgs):
            
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty . 
    
        THIS-OBJECT:QueryClosed:Publish (THIS-OBJECT, e) .
    
    END METHOD .            
            
    /*------------------------------------------------------------------------------
        Purpose: Raises the SmartNavigationTargetPositionChanged event                                                                          
        Notes:                                                                        
        @param e The SmartNavigationTargetPositionChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID OnSmartNavigationTargetPositionChanged (e AS SmartNavigationTargetPositionChangedEventArgs):
        
        THIS-OBJECT:SmartNavigationTargetPositionChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Raises the SmartNavigationTargetStateChanged event                                                                          
        Notes:                                                                        
        @param e The SmartNavigationTargetStateChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID OnSmartNavigationTargetStateChanged (e AS SmartNavigationTargetStateChangedEventArgs):
        
        THIS-OBJECT:SmartNavigationTargetStateChanged:Publish (THIS-OBJECT, 
                                                               e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the BindingSourceChanged event of the 
                 SmartDataSoruce                                                                       
        Notes:              
        @param sender The reference to the object that raised the event
        @param e The BindingSourceChangedEventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ParentBindingSourceChangedHandler (sender AS Progress.Lang.Object,
                                                          e AS BindingSourceChangedEventArgs):
        
        IF VALID-OBJECT (e:Old) THEN 
          e:Old:PositionChanged:Unsubscribe (ParentPositionChangedBase) NO-ERROR . 
       
        THIS-OBJECT:AttachBindingSource (e:New) .                
        THIS-OBJECT:DataSourceBindingSource = e:New .
        
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
    METHOD PUBLIC VOID ParentPositionChanged (sender AS System.Object, 
                                              e AS System.EventArgs) :
    
        /* NO-OP */
        
    END.

    /*------------------------------------------------------------------------------
        Purpose: Handler for the PositionChanged Event of the SmartDataSources 
                 BindingSource.
        Notes:   Calls the ParentPositionChanged Method which is implemented in the
                 specialized classes to refresh data.
                 The PositionChanged or CloseQuery Method is invoked
                 in all existing ChildSmartDataAdapter to refresh the Dataset inside
                 of them.
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event  
    ------------------------------------------------------------------------------*/        
    METHOD PUBLIC FINAL VOID ParentPositionChangedBase (sender AS System.Object, 
                                                        e AS System.EventArgs) :
    
        DEFINE VARIABLE lWasPositionZero       AS LOGICAL                     NO-UNDO.
    
        IF THIS-OBJECT:Initializing THEN 
            RETURN .
    
        /* Marko Rüterbories, Consultingwerk Ltd. 15.07.2009
           When the ParentPositionChangedBase is invoked it might be that there is not yet 
           a valid BindingSource (during initialization) in this instance of the SmartDataAdapter. 
           That will change in the ParentPositionChanged Handler because there we make sure 
           that a new BindingSource is created when there is no valid one. */
        IF VALID-OBJECT (THIS-OBJECT:BindingSource) THEN 
            lWasPositionZero = (THIS-OBJECT:BindingSource:Position = 0).
        ELSE 
        /* Mike Fechner, Consultingwerk Ltd. 16.07.2009
           lWasPositionZero should be set to FALSE as after creating the BindingSource in 
           RetrieveData (called by ParentPositionChanged) is done using the CreateBindingSource
           method. So basically all dependent DataAdapters will receive the ParentPositionChanged
           event when this BindingSource:Handle property is set to an opened Query; in this case 
           there is no need to simulate the THIS-OBJECT:BindingSource:PositionChanged event by 
           calling into the dependent DataAdapters ParentPositonChangedBase method. */        
            lWasPositionZero = FALSE .    
    
        /* Call the actual ParentPositionChanged handler. This method
           is meant to reopen the query of this SmartDataAdapter instance
           based on the current record in the SmartDataSource data adapter */
        ParentPositionChanged (sender, e).
                
        /* Mike Fechner, Consultingwerk Ltd. 29.06.2010
           Due to changes for Bug 2353 and changes in 13804, 13754 and 13753 
           it's no longer required / useful to raise an error here. Just silently 
           quit*/
        
        IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource) THEN 
            RETURN . 
        
        /* When this SmartDataAdapter query does not return any record,
           close child SmartDataAdapters queries */
        IF THIS-OBJECT:BindingSource:Count = 0 THEN 
            FOR EACH ttSmartDataTargetCollection WHERE ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT NO-LOCK:
                IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) AND 
                   TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget, SmartDataAdapter) THEN
                    CAST (ttSmartDataTargetCollection.SmartDataTarget, SmartDataAdapter):CloseQuery ().
            END.
          
        /* When we had been positioned in Row 0, reopening this Query does not 
           raise the PositionChanged event. */
        ELSE IF lWasPositionZero THEN DO:
            InvokeParentPositionChanged (sender, e) .

            THIS-OBJECT:OnCurrentChanged (System.EventArgs:Empty) .
        END.                    
                    
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessageBox (err) .        
        END CATCH.            
    END.

     /*------------------------------------------------------------------------------
        Purpose: Handler for the PositionChanged event of the BindingSource attached
                 to this SmartDataAdapter instance. 
        Notes:   The current RecordPosition is passed to the SmartNavigationSource.
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event  
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID PositionChangedHandler (sender AS System.Object, e AS System.EventArgs):

        DEFINE VARIABLE oObject AS Progress.Lang.Object NO-UNDO . 

        DEFINE BUFFER ttSmartDataTargetCollection FOR ttSmartDataTargetCollection . 
        
        OnSmartNavigationTargetPositionChanged (NEW SmartNavigationTargetPositionChangedEventArgs (THIS-OBJECT:RecordPosition)) .        

        ASSIGN oObject = THIS-OBJECT . 

        THIS-OBJECT:OnCurrentChanged (System.EventArgs:Empty) .
        
        /* Tell Viewers that/if we got a record */
        FOR EACH ttSmartDataTargetCollection WHERE ttSmartDataTargetCollection.RecordOwner = oObject /*THIS-OBJECT*/ NO-LOCK:
            IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) THEN        
                CAST(ttSmartDataTargetCollection.SmartDataTarget, ISmartDataTarget):EvaluateTableIOState () .
        END.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .        
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: The Message to for instance Disable a Browser is send to all 
                 SmartDataTargets except the calling one
        Notes:   This Method is called from the SmartViewerControl derived Class when
                 starting to Modify or Add data.
                 Enforced by Interface ISmartDataSource.
        @param poSmartDataTarget The reference to the ISmartDataTarget which is in UpdateState now   
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID BeginUpdateState (poSmartDataTarget AS ISmartDataTarget):       
    
        IF VALID-OBJECT (THIS-OBJECT:UpdatingSmartDataTarget) THEN 
            RETURN .
         
        THIS-OBJECT:SmartDataSourceState = DataSourceStateEnum:Updating .
        THIS-OBJECT:UpdatingSmartDataTarget = poSmartDataTarget .
    
        OnBeginUpdateState (NEW DataSourceUpdateStateEventArgs (poSmartDataTarget)) .
    
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN 
            THIS-OBJECT:SmartDataSource:BeginUpdateState (poSmartDataTarget) .
    
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Register a SmartDataTarget with the SmartDataSource.
                 This Method is called by Classes implementing the ISmartDataTarget
                 Interface when a new SmartDataSource is set.
        Notes:   After a new SmartDataTargetCollection Row is created this routine 
                 tests if there is a valid BindingSource inside the new SmartDataTarget.
                 If there is one, the BindingSource of the SmartDataAdapter is disposed
                 and replaced by the one of the SmartDataTarget. A new BindingSource is
                 then populated to all SmartDataTargets of this SmartDataAdapter.
                 Enforced by Interface ISmartDataSource.
        @param poSmartDataTarget The reference to the ISmartDataTarget to add to this SmartDataAdapter                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartDataTarget (poSmartDataTarget AS ISmartDataTarget):

        CREATE ttSmartDataTargetCollection.

        ASSIGN
            ttSmartDataTargetCollection.RecordOwner     = THIS-OBJECT 
            ttSmartDataTargetCollection.SmartDataTarget = poSmartDataTarget
            .
        
        RELEASE ttSmartDataTargetCollection.

        THIS-OBJECT:SmartDataTargets:Add (poSmartDataTarget) .

        /* Mike Fechner, Consultingwerk Ltd. 27.12.2009
           Workaround for the following issue (very likely OpenEdge bug):
           
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL -- SYSTEM ERROR when invoking ABL method EndInit from .NET 
 Exception code: C0000005 ACCESS_VIOLATION 
 Fault address:  10069DA2 01:00068DA2 C:\Progress\OpenEdge102B\bin\prow32.dll
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL -- ** ABL Debug-Alert Stack Trace **
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL -- --> RegisterSmartDataTarget Consultingwerk.SmartComponents.Base.SmartDataAdapter (C:\Work\SmartComponents4NET\Trunk\ABL\Consultingwerk\SmartComponents\Base\SmartDataAdapter.r) at line 939
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL --     propSet_SmartDataSource Consultingwerk.SmartComponents.Base.SmartViewerControl (C:\Work\SmartComponents4NET\Trunk\ABL\Consultingwerk\SmartComponents\Base\SmartViewerControl.r) at line 274
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL --     EndInit Consultingwerk.SmartComponents.Base.SmartViewerControl (C:\Work\SmartComponents4NET\Trunk\ABL\Consultingwerk\SmartComponents\Base\SmartViewerControl.r) at line 813
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL --     InitializeComponent Test.Demo.DemoWindowForm (C:\Work\SmartComponents4NET\Trunk\ABL\Test\Demo\DemoWindowForm.r) at line 639
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL --     DemoWindowForm Test.Demo.DemoWindowForm (C:\Work\SmartComponents4NET\Trunk\ABL\Test\Demo\DemoWindowForm.r) at line 44
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL --     C:\Work\SmartComponents4NET\Workspace\.metadata\.plugins\com.openedge.pdt.project\temp.dir\RunClass-DemoWindowForm-54967.p (C:\Work\SmartComponents4NET\Workspace\.metadata\.plugins\com.openedge.pdt.project\temp.dir\RunClass-DemoWindowForm-54967.p) at line 8
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL --     adecomm/_runcode.p (adecomm/_runcode.r) at line 665
[09/12/27@01:20:19.285+0100] P-005460 T-005496 1 4GL --     C:\Progress\OpenEdge102B\oeide\eclipse\plugins\com.openedge.pdt.debug.core_10.2.1.00\abl\_debuglauncher.p (C:\Progress\OpenEdge102B\oeide\eclipse\plugins\com.openedge.pdt.debug.core_10.2.1.00\abl\_debuglauncher.p) at line 100
           
               When the SmartViewerControl delayed the call to RegisterSmartDataTarget during 
               the Forms initialization (ISupportInitialize) this error was caused by calling 
               TYPE-OF (poSmartDataTarget, ISmartDataTargetWithBindingSource). For this reason
               we are exiting here when a SmartViewerControl is received. */        

        IF TYPE-OF (poSmartDataTarget, SmartViewerControl) OR TYPE-OF (poSmartDataTarget, SmartDataAdapter) THEN 
            RETURN .

        IF TYPE-OF (poSmartDataTarget, ISmartDataTargetWithBindingSource) AND
            /* Check if SmartDataBrowser is using a design time BindingSource */
           VALID-OBJECT(CAST(poSmartDataTarget, ISmartDataTargetWithBindingSource):BindingSource) THEN DO:
               
            IF VALID-OBJECT(THIS-OBJECT:BindingSource) THEN
                THIS-OBJECT:BindingSource:Dispose().
                        
            THIS-OBJECT:BindingSource = CAST(poSmartDataTarget, ISmartDataTargetWithBindingSource):BindingSource.            

            FOR EACH ttSmartDataTargetCollection WHERE ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT NO-LOCK
                ON ERROR UNDO, THROW:
                    
                IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) AND 
                   TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget, ISmartDataTarget) THEN
                    IF NOT TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget, SmartDataAdapter) THEN
                        CAST(ttSmartDataTargetCollection.SmartDataTarget, ISmartDataTarget):AttachBindingSource (THIS-OBJECT:BindingSource).
            END.
        END.
               
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Removes a SmartNavigationSource from the SmartDataAdapter 
                 instance                                                                     
        Notes:   Enabled multiple SmartNavigationSources to the SmartDataAdapter.
        @param poSmartNavigationSource The reference to the ISmartNavigationSource to be removed                                                                  
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID RemoveSmartNavigationSource (poSmartNavigationSource AS ISmartNavigationSource):
        
       IF VALID-OBJECT (poSmartNavigationSource) THEN 
           poSmartNavigationSource:DeregisterSmartNavigationTarget (THIS-OBJECT) .

        THIS-OBJECT:SmartNavigationTargetPositionChanged:Unsubscribe (poSmartNavigationSource:UpdateSmartNavigationTargetPosition) 
            NO-ERROR .
        THIS-OBJECT:SmartNavigationTargetStateChanged:Unsubscribe (poSmartNavigationSource:UpdateSmartNavigationTargetState) 
            NO-ERROR .

    END METHOD.

      /*------------------------------------------------------------------------------
        Purpose: The UpdateRow is an abstract method - that means that it needs 
                 to be overridden in a class that inherits from this base class.
                 Enforced by Interface ISmartDataSource.
        Notes:   Do NOT call SUPER:UpdateRow() in the overriding method.
    ------------------------------------------------------------------------------*/        
    METHOD PUBLIC ABSTRACT VOID UpdateRow ().

    /*------------------------------------------------------------------------------
        Purpose: Repositions to a single Rowid                                                                        
        Notes:  
        @param prRowid The ROWID to reposition to                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RepositionToRowid (prRowid AS ROWID):

        DEFINE VARIABLE rRowids AS ROWID EXTENT 1 NO-UNDO .
        
        rRowids[1] = prRowid .

        THIS-OBJECT:RepositiontoRowid (rRowids) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Repositions to a Rowid list from a character                                                                         
        Notes:
        @param pcRowids A comma delimited list of ROWIDs                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RepositionToRowid (pcRowids AS CHARACTER):

        DEFINE VARIABLE rRowids AS ROWID EXTENT NO-UNDO.
        DEFINE VARIABLE i       AS INTEGER      NO-UNDO.

        EXTENT (rRowids) = NUM-ENTRIES (pcRowids) .

        DO i = 1 TO NUM-ENTRIES(pcRowids):
            rRowids[i] = TO-ROWID (ENTRY(i, pcRowids)) . 
        END.
        
        THIS-OBJECT:RepositionToRowid (rRowids) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Repositions to a Rowid Array                                                                      
        Notes:        
        @param prRowid An Array of ROWIDs                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RepositionToRowid (prRowid AS ROWID EXTENT):

        DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        
        IF NOT TYPE-OF (THIS-OBJECT, ISupportsReposition) THEN 
            UNDO, THROW NEW AppError ("RepositionToRowid is only valid for SmartDataAdapters that implement the ISupportReposition interface", 0) .
        
        ASSIGN hQuery = CAST(THIS-OBJECT, ISupportsReposition):QueryHandle .
        
        ASSIGN lResult = Consultingwerk.Util.QueryHelper:RepositionToRowidArray (hQuery, prRowid) .
        
        IF lResult AND hQuery:GET-BUFFER-HANDLE (1):AVAILABLE = FALSE THEN 
            hQuery:GET-NEXT () .
        
        THIS-OBJECT:BindingSource:Position = hQuery:CURRENT-RESULT-ROW - 1. 

        OnAfterQueryPositionChanged (System.EventArgs:Empty) . 
                
    END METHOD .     
  
     /*------------------------------------------------------------------------------
        Purpose: RetrieveData is an abstract method - that means that it needs 
                 to be overridden in a class that inherits from this base class.
                 Enforced by Interface ISmartDataSource.
        Notes:   Do NOT call SUPER:RetrieveData() in the overriding method.
                 Remember to test if a BindingSource exists. If not call 
                 CreateBindingSource from this base class.
        @return Logical value indicating the success of the method, not always used
    ------------------------------------------------------------------------------*/        
    METHOD PUBLIC ABSTRACT LOGICAL RetrieveData ().
    
    /*------------------------------------------------------------------------------
        Purpose: CreateBindingSource is to be called inside the RetrieveData Method
                 if no valid BindingSource is present inside the SmartDataAdapter.
                 The newly created BindingSource is then registered to all 
                 SmartDataTargets of this SmartDataAdapter.
        Notes:   This method is not indended to be executed during Design time
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateBindingSource ():

        IF THIS-OBJECT:DesignTime THEN 
            RETURN . 

        THIS-OBJECT:BindingSource = NEW Progress.Data.BindingSource () .

        /* Mike Fechner, Consultingwerk Ltd. 07.02.2011
           Bug 2581: When possible (starting 10.2B03) set the MapNullToEmptyString 
                     property to FALSE  */
        Consultingwerk.Util.ReflectionHelper:SetPropertyValue (THIS-OBJECT:BindingSource,
                                                               "MapNullToEmptyString":U,
                                                               BOX (FALSE)) .                

        /* Mike Fechner, Consultingwerk Ltd. 07.09.2009
           Build schema information in DataAdapter */
        THIS-OBJECT:AttachSchemaToBindingSource (THIS-OBJECT:BindingSource) .

        /* Register the current DataAdapters BindingSource in all DataTargets */
        FOR EACH ttSmartDataTargetCollection WHERE ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT NO-LOCK:
            IF VALID-OBJECT (ttSmartDataTargetCollection.SmartDataTarget) AND 
               TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget, ISmartDataTarget) THEN
                IF NOT TYPE-OF (ttSmartDataTargetCollection.SmartDataTarget, SmartDataAdapter) THEN
                    CAST(ttSmartDataTargetCollection.SmartDataTarget, ISmartDataTarget):AttachBindingSource (THIS-OBJECT:BindingSource).
        END.
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Destructor of SmartDataAdapter.
                 Every owned SmartDataTargetCollection Row gets deleted.
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartDataAdapter ( ):

        DEFINE VARIABLE oDeleteBindingSource AS Progress.Data.BindingSource NO-UNDO . 

        IF VALID-OBJECT (oViewersWithDisabledEventHandler) THEN 
            oViewersWithDisabledEventHandler:Clear () . 

        IF VALID-OBJECT (THIS-OBJECT:DataSourceBindingSource) THEN DO:
            THIS-OBJECT:DataSourceBindingSource:PositionChanged:Unsubscribe(ParentPositionChangedBase) .
        
            THIS-OBJECT:DataSourceBindingSource = ? .
        END.

        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN 
            THIS-OBJECT:SmartDataSource:BindingSourceChanged:Unsubscribe (ParentBindingSourceChangedHandler) .

        IF VALID-OBJECT (THIS-OBJECT:BindingSource) THEN DO:
            oDeleteBindingSource = THIS-OBJECT:BindingSource .
            
            THIS-OBJECT:BindingSource = ? . 

            /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
               Only Dispose BindingSource when not in Visual Designer 
               to avoid conflicts when re-opening layouts. */
            /* Mike Fechner, Consultingwerk Ltd. 23.04.2014
               Ignore errors from within the Dispose() 
               System.NullReferenceException: Der Objektverweis 
               wurde nicht auf eine Objektinstanz festgelegt.
                .NET StackTrace:
                -->   bei Progress.Data.DataSource.a(a A_0, Boolean A_1)
                   bei Progress.Data.BindingSource.Dispose(Boolean disposing)
                   bei Consultingwerk.SmartComponents.Implementation.SmartBindingSource.Dispose(Boolean )
                   bei Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource.Dispose(Boolean )
                   bei Progress.Data.BindingSource.Dispose() */
                    
            IF NOT THIS-OBJECT:DesignTime THEN DO ON ERROR UNDO, THROW:
                oDeleteBindingSource:Dispose () .
    
                CATCH nullex AS System.NullReferenceException:
                	/* noop */	
                END CATCH.

                /* Mike Fechner, Consultingwerk Ltd. 07.08.2014
                   SCL-380: One customer had here the infamous 
                   System.InvalidOperationException: Attempt to use an object 
                   that has been explicitly deleted (15580)  */
                CATCH invalidoperationex AS System.InvalidOperationException:
                   /* noop */
                END CATCH.
            END.

            IF VALID-OBJECT (oDeleteBindingSource) THEN             
                DELETE OBJECT oDeleteBindingSource . 
        END.

        FOR EACH ttSmartDataTargetCollection WHERE ttSmartDataTargetCollection.RecordOwner = THIS-OBJECT:
            DELETE ttSmartDataTargetCollection.
        END.
        
        FOR EACH ttAfterRetrieveDataSubscriberCol WHERE ttAfterRetrieveDataSubscriberCol.RecordOwner = THIS-OBJECT:
            DELETE ttAfterRetrieveDataSubscriberCol.
        END.         
        
        THIS-OBJECT:SmartDataTargets:Clear() .
        
    END DESTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: This Method is used to attach a new BindingSource to a SmartDataTarget.
                 Inside a SmartDataAdapter which can be a SmartDataTarget a new 
                 BindingSource is created  or borrowed from a Browser.
        Notes:   Enforced by Interface ISmartDataTarget
        @param poBindingSource The BindingSource to be attached to the Grids DataSource Property
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AttachBindingSource (poBindingSource AS Progress.Data.BindingSource):
        
        IF VALID-OBJECT(oSmartDataSource:BindingSource) THEN 
            oSmartDataSource:BindingSource:PositionChanged:Subscribe (ParentPositionChangedBase) .        
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Get/Set the SmartDataSource of the current Class. The SmartDataSource 
                 can be set to any class implementing the Interface ISmartDataSource.
                 This Property is set by the developer when initializing a Control 
                 implementing the Interface ISmartDataTarget.
                 Enforced by Interface ISmartDataTarget.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSource AS ISmartDataSource NO-UNDO 
    GET():
        RETURN oSmartDataSource . 
    END GET.
    SET(INPUT arg AS ISmartDataSource):
        /* A SmartDataAdapter can not be it's own GroupAssignSource */
        IF arg = THIS-OBJECT THEN 
            UNDO, THROW NEW AppError ("A SmartDataAdapter can not be it's own SmartDataSource!", 0).
                
        SetSmartDataSource (arg).
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the SmartDataTarget which is currently in 
                 update mode
        Notes:   This might be a SmartDataTarget actually linked to a different 
                 SmartDataAdapter. This SmartDataTarget is supposed to end the 
                 UpdateState before this SmartDataAdapter becomes navigatable again
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UpdatingSmartDataTarget AS ISmartDataTarget NO-UNDO 
    GET.
    PROTECTED SET.
    
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
     METHOD PROTECTED VOID SetSmartDataSource (arg AS ISmartDataSource):         
    
        /* When changing the SmartDataSource, remove subscription */        
        IF VALID-OBJECT (oSmartDataSource) AND VALID-OBJECT(oSmartDataSource:BindingSource) THEN 
            oSmartDataSource:BindingSource:PositionChanged:Unsubscribe (ParentPositionChangedBase) NO-ERROR . 
    
        IF VALID-OBJECT (oSmartDataSource) THEN DO:
            oSmartDataSource:BindingSourceChanged:Unsubscribe (ParentBindingSourceChangedHandler) NO-ERROR .
            oSmartDataSource:BeginUpdateState:Unsubscribe (BeginUpdateStateHandler) NO-ERROR .
            oSmartDataSource:EndUpdateState:Unsubscribe (EndUpdateStateHandler) NO-ERROR .
        END.
    
        IF VALID-OBJECT(arg) THEN DO:
            oSmartDataSource = arg .
            
            /* Mike Fechner, Consultingwerk Ltd. 27.12.2009
               No need to Register during design time */
            
            IF NOT THIS-OBJECT:DesignTime THEN DO:            
                oSmartDataSource:RegisterSmartDataTarget(THIS-OBJECT).

                IF VALID-OBJECT (oSmartDataSource) THEN  DO:
                    oSmartDataSource:BindingSourceChanged:Subscribe (ParentBindingSourceChangedHandler) .
                    oSmartDataSource:BeginUpdateState:Subscribe (BeginUpdateStateHandler) .
                    oSmartDataSource:EndUpdateState:Subscribe (EndUpdateStateHandler) .        
                END.
        
                IF THIS-OBJECT:SmartDataSourceActive AND VALID-OBJECT(oSmartDataSource:BindingSource) THEN DO:    
                    THIS-OBJECT:DataSourceBindingSource = oSmartDataSource:BindingSource .
                                
                    oSmartDataSource:BindingSource:PositionChanged:Subscribe(ParentPositionChangedBase) .        
                
                    ParentPositionChangedBase (?, ?) .                               
                END.
            END.
        END.
        ELSE DO:
            IF VALID-OBJECT (oSmartDataSource) THEN 
                oSmartDataSource:DeregisterSmartDataTarget (THIS-OBJECT) .
            
            oSmartDataSource = ? .
        END. 
           
    END METHOD.        
        
    /*------------------------------------------------------------------------------
        Purpose: Subscribes an instance of an object to the AfterRetrieveData
                 callback.                                                                        
        Notes:   The object needs to implement the IAfterRetrieveDataSubscriber interface 
        @param poSubscriber The reference to the IAfterRetrieveDataSubscribe to add to the list of subscribers                                                                         
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID SubscribeAfterRetrieveData (poSubscriber AS IAfterRetrieveDataSubscriber):
        
        IF NOT VALID-OBJECT (poSubscriber) THEN
            RETURN .  

        CREATE ttAfterRetrieveDataSubscriberCol.
        ASSIGN ttAfterRetrieveDataSubscriberCol.RecordOwner = THIS-OBJECT 
               ttAfterRetrieveDataSubscriberCol.Subscriber  = poSubscriber .

        RELEASE ttAfterRetrieveDataSubscriberCol .

    END METHOD.
    
END CLASS.
