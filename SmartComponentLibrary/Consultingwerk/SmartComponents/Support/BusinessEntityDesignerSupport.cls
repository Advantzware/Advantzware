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
    File        : BusinessEntityDesignerSupport
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 14 14:31:13 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Consultingwerk.Framework.*                      FROM PROPATH . 
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.ComponentModel.Design.*                  FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.Support.BusinessEntityDesignerSupport: 

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE STATIC"}

    /*------------------------------------------------------------------------------
        Purpose: Controls if this class disconnects from the AppServer after 
                 each request 
        Notes:   With connection bound AppServers that QUIT on disconnect (PASOE) so
                 that a) there's no permanent connection form the Visual Designer
                 to the PASOE instance and b) the AppServer session always uses up to
                 date business entities
                 SCL-887 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DisconnectAppServerAfterEachRequest AS LOGICAL NO-UNDO
       INIT FALSE  
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Controls if Business Entities are searched on the AppServer 
        Notes:   Useful when the Business Entities are only accessible via an 
                 AppServer call (e.g. UNIX AppServer, different Progress Release) 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SearchBusinessEntititiesOnAppServer AS LOGICAL NO-UNDO 
       INIT FALSE
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityDesignerSupport class
        Notes:   Disallow instance creation   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE BusinessEntityDesignerSupport ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates a SmartBusinessEntityBindingSource from a SmartBusinessEntityAdapter
                 or SmartDatasetChildAdapter instance on the design surface 
        Notes:   
        @param poSourceComponent The source SmartBusinessEntiyAdapter or SmartDatasetChildAdapter as a component 
        @param pcBindingSourceClassName The name of the ABL class to create the Binding Source of 
        @return The reference to the created SmartBusinessEntityBindingSource instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC SmartBusinessEntityBindingSource CreateBindingSourceFromAdapter (poSourceComponent AS System.ComponentModel.Component,
                                                                                          pcBindingSourceClassName AS CHARACTER):
        
        DEFINE VARIABLE oType                  AS System.Type                      NO-UNDO . 
        DEFINE VARIABLE oInstance              AS Progress.Lang.Object             NO-UNDO .
        DEFINE VARIABLE oComponent             AS System.ComponentModel.IComponent NO-UNDO .
        DEFINE VARIABLE oBindingSource         AS SmartBusinessEntityBindingSource NO-UNDO . 
        DEFINE VARIABLE oBusinessEntityAdapter AS SmartBusinessEntityAdapter       NO-UNDO . 
        DEFINE VARIABLE oDatasetsChildAdapter  AS SmartDatasetChildAdapter         NO-UNDO .
        
        ObjectAssert:IsValid (poSourceComponent, "poSourceComponent":U) .
        Assert:NotNullOrEmpty (pcBindingSourceClassName, "pcBindingSourceClassName":U) .
        
        oType = Progress.Util.TypeHelper:GetType (pcBindingSourceClassName) .
        
        IF NOT VALID-OBJECT (oType) THEN DO:
            
            oInstance = DYNAMIC-NEW (pcBindingSourceClassName) () . 
            
            oType = Progress.Util.TypeHelper:GetType (pcBindingSourceClassName) .
    
            DELETE OBJECT oInstance .         
        END.
        
        IF NOT VALID-OBJECT (oType) THEN  
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to create component type: &1"{&TRAN}, 
                                                  pcBindingSourceClassName), 0) . 
        
        oComponent = CAST(poSourceComponent:Container,
                          IDesignerHost):CreateComponent (oType)  .        
        
        ASSIGN oBindingSource = CAST (oComponent, SmartBusinessEntityBindingSource) .
        
        IF TYPE-OF (poSourceComponent, 
                    SmartBusinessEntityAdapter) THEN DO:
                        
            ASSIGN oBusinessEntityAdapter = CAST (poSourceComponent, 
                                                  SmartBusinessEntityAdapter) 
                
                   oBindingSource:EntityName  = oBusinessEntityAdapter:EntityName 
                   oBindingSource:EntityTable = oBusinessEntityAdapter:EntityTable
                   oBindingSource:EntityView  = oBusinessEntityAdapter:EntityView 
                   oBindingSource:EntityJoin  = oBusinessEntityAdapter:EntityJoin 
                   .                                                                         
        END.
        ELSE DO:
            ASSIGN oDatasetsChildAdapter = CAST (poSourceComponent, 
                                                 SmartDatasetChildAdapter)  
                
                   oBindingSource:EntityName  = oDatasetsChildAdapter:EntityName 
                   oBindingSource:EntityTable = oDatasetsChildAdapter:EntityTable
                   oBindingSource:EntityView  = oDatasetsChildAdapter:EntityView 
                   .

            IF oBindingSource:EntityView > "":U THEN         
                oBindingSource:EntityJoin  = TRIM (FILL ("yes,":U, NUM-ENTRIES (oDatasetsChildAdapter:EntityView)), ",":U) .
        END.

        /* Finally import the schema */        
        IF oBindingSource:EntityName > "":U AND
           oBindingSource:EntityTable > "":U THEN

            oBindingSource:DesignerImportSchema (FALSE) .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Refresh Button of the Refresh Button of the 
                 Business Entity Picker form  
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC VOID RefreshButtonClickHandler (sender AS System.Object, 
                                                          e AS System.EventArgs):
        DEFINE VARIABLE oCache        AS IClassCacheProvider      NO-UNDO .  
        DEFINE VARIABLE oForm         AS BusinessEntityPickerForm NO-UNDO .
        
        oForm = CAST (sender, BusinessEntityPickerForm) .
        
        oCache = {Consultingwerk/get-service.i Consultingwerk.SmartComponents.Support.IClassCacheProvider
                                               "NEW Consultingwerk.SmartComponents.Support.ClassCacheProvider()"} . 
        
        oCache:Refresh ("Consultingwerk.OERA.BusinessEntity":U) .
        oCache:GetClassNames ("Consultingwerk.OERA.BusinessEntity":U,
                              OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Refresh Button of the Refresh Button of the 
                 Business Entity Picker form  
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC VOID RefreshButton2ClickHandler (sender AS System.Object, 
                                                           e AS System.EventArgs):
        DEFINE VARIABLE oCache        AS IClassCacheProvider      NO-UNDO .  
        DEFINE VARIABLE oForm         AS BusinessEntityPickerForm NO-UNDO .
        
        oForm = CAST (sender, BusinessEntityPickerForm) .
        
        oCache = {Consultingwerk/get-service.i Consultingwerk.SmartComponents.Support.IClassCacheProvider
                                               "NEW Consultingwerk.SmartComponents.Support.ClassCacheProvider()"} . 
        
        oCache:Refresh ("Consultingwerk.SmartComponents.Interfaces.IDatasetController":U) .
        oCache:GetClassNames ("Consultingwerk.SmartComponents.Interfaces.IDatasetController":U,
                              OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Restarts the Business Entities                                                                         
        Notes:   Useful when addding fields or tables to the BusinessEntity schema
                 Allows loading the schema of those field without restarting the 
                 OpenEdge Architect or the prokject AVM                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RestartBusinessEntities ():
        
        Consultingwerk.OERA.ServiceManager:StopAllBusinessServices
           (Progress.Lang.Class:GetClass ("Consultingwerk.OERA.IBusinessEntity":U)) .

        Consultingwerk.Util.MessageFormHelper:ShowMessage 
            ("Shut down all locally running Business Entities", 
             "SmartComponent Library") .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Selects a Business Entity                                                                       
        Notes:   Method is VOID because of WAIT-FOR
        @param pcClassName OUTPUT The selected business entity name                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SelectBusinessEntityName (OUTPUT pcClassName AS CHARACTER):

        DEFINE VARIABLE oForm         AS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                               NO-UNDO .

        DEFINE VARIABLE oInfo         AS Infragistics.Win.Misc.UltraDesktopAlertWindowInfo               NO-UNDO . 

        DEFINE VARIABLE hServer       AS HANDLE                                                          NO-UNDO .
        
        DEFINE VARIABLE oCache        AS IClassCacheProvider                                             NO-UNDO .  

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show ("SmartComponent Library",
                                                                                "Scanning Project Folder(s) for Business Entities.") .

        oForm = NEW Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm () .

        IF BusinessEntityDesignerSupport:SearchBusinessEntititiesOnAppServer = TRUE THEN DO
            ON ERROR UNDO, THROW
            ON STOP UNDO, RETURN ERROR NEW Consultingwerk.Exceptions.StopConditionException ():
                
            ASSIGN hServer = FrameworkSettings:AppServerServiceManager:ConnectService ("":U) .            
            
            RUN Consultingwerk/Util/Support/getclassnamesonserver.p ON hServer 
                                                                     ("Consultingwerk.OERA.BusinessEntity":U, 
                                                                      FALSE, 
                                                                      OUTPUT TABLE ttClassNames) .

            IF BusinessEntityDesignerSupport:DisconnectAppServerAfterEachRequest THEN 
                FrameworkSettings:AppServerServiceManager:DisconnectService ("":U) .                                                                      
        END.
        ELSE DO:  
            oCache = {Consultingwerk/get-service.i Consultingwerk.SmartComponents.Support.IClassCacheProvider
                                                   "NEW Consultingwerk.SmartComponents.Support.ClassCacheProvider()"} . 
            
            oCache:GetClassNames ("Consultingwerk.OERA.BusinessEntity":U,
                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .
            
            oForm:RefreshButtonClick:Subscribe (RefreshButtonClickHandler) .
            oForm:RefreshButtonVisible = TRUE . 
        END.

        oInfo:Close () .
        
        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .
        
/*        WAIT-FOR oForm:ShowDialog () SET oDialogResult .*/

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2010
           WAIT-FOR Workaround */
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog (oForm) .

        oForm:RefreshButtonClick:Unsubscribe (RefreshButtonClickHandler) NO-ERROR .
        
        IF Progress.Util.EnumHelper:AreEqual(oDialogResult, 
                                             System.Windows.Forms.DialogResult:Ok) THEN DO:
                                    
            ASSIGN pcClassName = oForm:GetSelectedBusinessEntityName () .
        END.
        ELSE 
            ASSIGN pcClassName = ? .                                                

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Selects a Dataset Controller                                                                       
        Notes:   Method is VOID because of WAIT-FOR
        @param pcClassName OUTPUT The selected dataset controller name                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SelectDatasetControllerType (OUTPUT pcClassName AS CHARACTER):

        DEFINE VARIABLE oForm         AS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                               NO-UNDO .

        DEFINE VARIABLE oInfo         AS Infragistics.Win.Misc.UltraDesktopAlertWindowInfo               NO-UNDO . 

        DEFINE VARIABLE oCache        AS IClassCacheProvider                                             NO-UNDO . 

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show ("SmartComponent Library",
                                                                                "Scanning Project Folder(s) for Dataset Controller.") .

        oForm = NEW Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm () .

        oCache = {Consultingwerk/get-service.i Consultingwerk.SmartComponents.Support.IClassCacheProvider
                                               "NEW Consultingwerk.SmartComponents.Support.ClassCacheProvider()"} . 
        
        oCache:GetClassNames ("Consultingwerk.SmartComponents.Interfaces.IDatasetController":U,
                              OUTPUT TABLE ttClassNames BY-REFERENCE) .
        
        oForm:RefreshButtonClick:Subscribe (RefreshButton2ClickHandler) .
        oForm:RefreshButtonVisible = TRUE . 

        oInfo:Close () .

        oForm:Text = "Select DatasetController class" .
        
        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .
        
/*        WAIT-FOR oForm:ShowDialog () SET oDialogResult .*/

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2010
           WAIT-FOR Workaround */
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog(oForm) .
        
        IF Progress.Util.EnumHelper:AreEqual(oDialogResult, 
                                             System.Windows.Forms.DialogResult:Ok) THEN DO:
                                    
            ASSIGN pcClassName = oForm:GetSelectedBusinessEntityName () .
        END.
        ELSE 
            ASSIGN pcClassName = ? .                                                

    END METHOD.

END CLASS.
