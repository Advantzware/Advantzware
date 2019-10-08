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
    File        : BaseRadRibbonForm
    Purpose     : Base Form extending the Telerik RadRibbonForm
    Syntax      : 
    Description : Extended base form based on the Telerik RadRibbonForm supporting
                  Telerik Styling
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 19 17:10:12 CET 2015
    Notes       : Currently not yet inheriting from Progress.Windows.Form
                  as Telerik Styling requires to inherit from their Forms.
                  It is expected that Progress will provide specific base
                  form classes combining the Telerik behaviour and the 
                  Progress.Windows.Form ABL window interop
                  https://community.progress.com/community_groups/openedge_development/f/19/p/16706/59825.aspx?Redirected=true#59825
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Forms.*                              FROM PROPATH .
USING Consultingwerk.Forms.Telerik.*                      FROM PROPATH .  
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.Framework.Authorization.*            FROM PROPATH .
USING Consultingwerk.Framework.Authorization.Exceptions.* FROM PROPATH .
&IF DEFINED (SmartComponentLibrary) NE 0 &THEN 
USING Consultingwerk.SmartComponents.*                    FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Support.*            FROM PROPATH  .
&ENDIF
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Telerik.WinControls.UI.*                            FROM ASSEMBLY .
USING System.ComponentModel.*                             FROM ASSEMBLY .
USING System.Windows.Forms.*                              FROM ASSEMBLY .
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Forms.Telerik.BaseRadRibbonForm 
&IF DEFINED (TelerikRadControls) EQ 0 &THEN 
    :
&ELSE
    INHERITS RadRibbonForm
    IMPLEMENTS IBaseForm  
&IF DEFINED (SmartComponentLibrary) NE 0 &THEN     
/*    , ICustomTypeDescriptor, IHasDesignerVerbs*/
&IF "{&CustomBaseFormImplements}":U NE "":U &THEN
    , {&CustomBaseFormImplements}
&ENDIF     
    
&ELSE    
&IF "{&CustomBaseFormImplements}":U NE "":U &THEN
    , {&CustomBaseFormImplements}
&ENDIF     
&ENDIF
    : 
    
/*    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN               */
/*    /* Mike Fechner, Consultingwerk Ltd. 19.05.2010              */
/*       Core implementation of ICustomTypeDescriptor */           */
/*    {Consultingwerk/SmartComponents/Base/ICustomTypeDescriptor.i}*/
/*    {Consultingwerk/SmartComponents/Base/CreateVerbs.i}          */
/*    &ENDIF                                                       */
            
    DEFINE PRIVATE VARIABLE cWindowPositionRegistryKey AS CHARACTER NO-UNDO INIT ? .             

    /* Mike Fechner, Consultingwerk Ltd. 13.03.2014
       SCL-204: Ability to delay the deletion of the Form instance OnFormClosed */    
    DEFINE VARIABLE oCloseDeleteTimer AS System.Windows.Forms.Timer NO-UNDO . 
    
    /*------------------------------------------------------------------------------
        Purpose: Raised when a Form is destroyed (in the Destructor)                                                                
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PUBLIC EVENT FormDestroyed DELEGATE System.EventHandler.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets if the BaseForm instance should automatically delete 
                 itself in the OnFormClosed method (defaults to True). Setting this
                 to FALSE will allow to let the BaseForm instance in memory (when no
                 other reference exists), i.e. to access properties of the Form 
                 class when the Form has been closed.   
        Notes:   See Progress K-Base Entry ID: P179041
                 Title: "User Controls that have events attached to them cause 
                 parent forms to not get garbage collected"
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY DeleteOnFormClosed AS LOGICAL INITIAL TRUE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: A list of Forms that are dependent from this Form.
        Notes:   Dependent Forms are similar to ChildForms with the exception that
                 DependentForms are not automatically kept on top of the Owner.
                 But when the owner is closed, ths DependentForms will be closed as 
                 well.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DependentForms AS "System.Collections.Generic.List<Progress.Windows.Form>":U NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to generate Designer Verbs (actions for the 
                 Visual Designer)                                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PROTECTED PROPERTY DesignerVerbs AS CHARACTER NO-UNDO 
    GET. 
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.                                                                        
        Notes:   Set during constructor using a check at the 
                 System.ComponentModel.LicenseManager                                                                     
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY DesignTime AS LOGICAL NO-UNDO 
    GET.
    PRIVATE SET.          

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Forms has been closed.
        Notes:   A Form may be closed but not yet deleted                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY IsClosed AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Controls if this Form instance should store and restore the Form
                 position and size.
        Notes:   Allows alternative Form positions (like CenterParent etc.)                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PROTECTED PROPERTY StoreFormPosition AS LOGICAL INITIAL TRUE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: When set to True the OnLoad method will apply the current form (startup
                 size) to the MinimumSize. This will be done before loading the stored
                 Form size from the user profile (e.g. Windows Registry)
        Notes:   Defaulted by static property UseStartupSizeAsMinSizeDefault in 
                 Consultingwerk.Framework.FrameworkSettings. This property used to 
                 be in the class itself, but was moved to the FrameworkSettings class 
                 to avoid issues with the combination of static and hybrids on 11.2 
                 and 11.3, OE defect OE00240707  
                 http://knowledgebase.progress.com/articles/Article/000042229?popup=true                                                                          
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY UseStartupSizeAsMinSize AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Registry Key used for storing this Forms
                 window position                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WindowPositionRegistryKey AS CHARACTER NO-UNDO 
    GET:
        IF cWindowPositionRegistryKey > "":U THEN 
            RETURN cWindowPositionRegistryKey .
        ELSE IF THIS-OBJECT:NAME > "":U THEN 
            RETURN THIS-OBJECT:NAME . 
        ELSE 
            RETURN THIS-OBJECT:GetClass():TypeName . 
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN cWindowPositionRegistryKey = arg . 
    END SET.

/* Mike Fechner, Consultingwerk Ltd. 12.06.2012
   Ability to customize Consultingwerk.Forms.BaseForm */
&IF "{&CustomBaseFormIncludeFile}":U NE "":U &THEN
{{&CustomBaseFormIncludeFile}}
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BaseRadRibbonForm class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BaseRadRibbonForm ():
        
        SUPER ().
    
        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        BaseFormHelper:CheckFormAuthorizationProvider (THIS-OBJECT) .
        
        THIS-OBJECT:DependentForms = NEW "System.Collections.Generic.List<Progress.Windows.Form>":U () .
         
        /* Mike Fechner, Consultingwerk Ltd. 14.05.2012
           Apply default from static property */ 
        ASSIGN THIS-OBJECT:UseStartupSizeAsMinSize = FrameworkSettings:UseStartupSizeAsMinSizeDefault .
         
/* Mike Fechner, Consultingwerk Ltd. 12.06.2012
   Ability to customize Consultingwerk.Forms.BaseForm */
&IF "{&CustomBaseFormConstructorInclude}":U NE "":U &THEN
{{&CustomBaseFormConstructorInclude}}
&ENDIF
         
&IF DEFINED (SmartComponentLibrary) NE 0 &THEN
        IF THIS-OBJECT:DesignTime THEN DO:
/*            THIS-OBJECT:SetDesignerProperties () .*/
            THIS-OBJECT:DeleteOnFormClosed = FALSE .
        END.         
&ENDIF
 
    END CONSTRUCTOR.

&IF DEFINED (DelayFormDeletionOnFormClosed) NE 0 &THEN
    /*------------------------------------------------------------------------------
       Purpose: Event handle for the Tick event of the oCloseDeleteTimer component 
       Notes:   See SCL-204 / OnFormClosed method for details
       @param sender The reference to the object instance that raised this event
       @param e The reference to the System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID CloseDeleteTimerTickHandler (sender AS System.Object, 
                                                     e AS System.EventArgs):
        
        oCloseDeleteTimer:Enabled = FALSE . 
        oCloseDeleteTimer:Tick:Unsubscribe (CloseDeleteTimerTickHandler). 
        
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oCloseDeleteTimer) .
        
        DELETE OBJECT THIS-OBJECT . 

        CATCH err AS Progress.Lang.Error:
            IF err:GetMessageNum (1) = 15580 OR err:GetMessage(1) MATCHES "* (15580)*":U THEN
               /* ignore */ .
            ELSE
                UNDO, THROW err .
        END CATCH.

    END METHOD.
&ENDIF

/*&IF DEFINED (Infragistics) NE 0 &THEN                                                                       */
/*    /*------------------------------------------------------------------------------                        */
/*        Purpose: Customizes the UltraToolbarController associated with this BaseForm                        */
/*        Notes:   Uses the IFormToolbarCustomizer Service if present                                         */
/*    ------------------------------------------------------------------------------*/                        */
/*    METHOD PROTECTED VOID CustomizeToolbar ():                                                              */
/*                                                                                                            */
/*        DEFINE VARIABLE oToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO .*/
/*        DEFINE VARIABLE oCustomizer      AS Consultingwerk.Windows.IFormToolbarCustomizer          NO-UNDO .*/
/*                                                                                                            */
/*        oToolbarsManager = Infragistics.Win.UltraWinToolbars.UltraToolbarsManager:FromForm (THIS-OBJECT) .  */
/*                                                                                                            */
/*        IF NOT VALID-OBJECT (oToolbarsManager) THEN                                                         */
/*            RETURN .                                                                                        */
/*                                                                                                            */
/*        oCustomizer = {Consultingwerk/get-service.i Consultingwerk.Windows.IFormToolbarCustomizer} .        */
/*                                                                                                            */
/*        IF VALID-OBJECT (oCustomizer) THEN                                                                  */
/*            oCustomizer:CustomizeToolbar (oToolbarsManager, THIS-OBJECT) .                                  */
/*    END METHOD .                                                                                            */
/*&ENDIF                                                                                                      */

    /*------------------------------------------------------------------------------
        Purpose: Returns if the component can be found in the ComponentsCollection
        Notes:   Available from OpenEdge 11.1 on
        @param poComponent The component to locate
        @return Logical value indicating if the component can be found in the ComponentsCollection
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL CanFindInComponentsCollection (poComponent AS System.Object):

        /* Commented out until 11.6 introduces the Progress.Windows.Form methods 
           for the Telerik base forms */ 
        
/*        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN                                 */
/*        {Consultingwerk/foreach.i System.ComponentModel.IContainer oContainer in THIS-OBJECT:ComponentsCollection}*/
/*                                                                                                                  */
/*            {Consultingwerk/foreach.i System.Object oComponent in oContainer:Components}                          */
/*                                                                                                                  */
/*                IF poComponent = oComponent THEN                                                                  */
/*                    RETURN TRUE .                                                                                 */
/*            END.                                                                                                  */
/*        END.                                                                                                      */
/*        &ENDIF                                                                                                    */
        RETURN FALSE . 

    END METHOD .

&IF DEFINED (SmartComponentLibrary) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Inserts a user control instance at design time.                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DesignerInsertUserControl ():
        
        IF NOT THIS-OBJECT:DesignTime THEN 
            RETURN . 

        SmartWindowFormDesignerSupport:InsertUserControl (THIS-OBJECT). 

    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all Controls in the Form that are of the given type
        Notes:   
        @param poType The reference to the System.Type 
        @return The List of all Controls of the given type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC "System.Collections.Generic.List<System.Windows.Forms.Control>":U FindAllControlsOfType (poType AS System.Type):
            
        RETURN ControlHelper:FindAllControlsOfType (THIS-OBJECT, poType) . 

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all Components in the Form 
        Notes:   Supported on OpenEdge 11 only, due to dependency to the ComponentsCollection
                 list not available on 10.2B
        @return The List of Components in the Form 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC "System.Collections.Generic.List<System.ComponentModel.Component>" GetAllComponents ():
        
        DEFINE VARIABLE oList AS "System.Collections.Generic.List<System.ComponentModel.Component>" NO-UNDO.

        oList = NEW "System.Collections.Generic.List<System.ComponentModel.Component>" () .

        /* Commented out until 11.6 introduces the Progress.Windows.Form methods 
           for the Telerik base forms */ 

/*        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN                                 */
/*        {Consultingwerk/foreach.i System.ComponentModel.IContainer oContainer in THIS-OBJECT:ComponentsCollection}*/
/*                                                                                                                  */
/*                                                            */
/*            IF NOT VALID-OBJECT (oContainer) OR NOT VALID-OBJECT (oContainer:Components) THEN*/
/*                NEXT .                                      */
/*                                                            */
/*            {Consultingwerk/foreach.i System.Object oComponent in oContainer:Components}                          */
/*                                                                                                                  */
/*                IF TYPE-OF (oComponent, Component) THEN                                                           */
/*                    oList:Add (CAST (oComponent, Component)) .                                                    */
/*            END.                                                                                                  */
/*        END.                                                                                                      */
/*        &ENDIF                                                                                                    */

        RETURN oList.

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose:  Restores the Quick Access Toolbar items from the Registry                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LoadRibbonQuickAccessToolbar ():
        
/*        &IF DEFINED (Infragistics) NE 0 &THEN                                                               */
/*        DEFINE VARIABLE oToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO .*/
/*        DEFINE VARIABLE cTools           AS CHARACTER                                              NO-UNDO .*/
/*        DEFINE VARIABLE cMinimized       AS CHARACTER                                              NO-UNDO .*/
/*        DEFINE VARIABLE lMinimized       AS LOGICAL                                                NO-UNDO .*/
/*                                                                                                            */
/*        oToolbarsManager = Infragistics.Win.UltraWinToolbars.UltraToolbarsManager:FromForm (THIS-OBJECT) .  */
/*                                                                                                            */
/*        IF VALID-OBJECT (oToolbarsManager) THEN DO:                                                         */
/*            ASSIGN cTools = THIS-OBJECT:LoadSettings ("QuickAccessTools":U) .                               */
/*                                                                                                            */
/*            IF cTools > "":U THEN                                                                           */
/*                Consultingwerk.Util.UltraToolbarsHelper:RestoreQuickAccessToolbarTools (oToolbarsManager,   */
/*                                                                                        cTools) .           */
/*                                                                                                            */
/*            IF oToolbarsManager:Ribbon:Visible THEN DO:                                                     */
/*                cMinimized = THIS-OBJECT:LoadSettings("RibbonIsMinimized":U) .                              */
/*                                                                                                            */
/*                IF cMinimized > "":U THEN DO:                                                               */
/*                    lMinimized = DataTypeHelper:ToLogical (cMinimized) .                                    */
/*                                                                                                            */
/*                    IF lMinimized <> ? THEN                                                                 */
/*                        oToolbarsManager:Ribbon:IsMinimized = lMinimized .                                  */
/*                END.                                                                                        */
/*            END.                                                                                            */
/*        END.                                                                                                */
/*        &ENDIF                                                                                              */
/*                                                                                                            */
/*        CATCH ex AS System.Exception:                                                                       */
/*            /* NOOP */                                                                                      */
/*        END CATCH.                                                                                          */

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Loads the stored window settings (position etc.) from the registry                                                                     
        Notes:                                                                          
        @return The string containing the stored window settings 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER LoadSettings ():
    
        RETURN THIS-OBJECT:LoadSettings ("Position":U) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads  stored window settings from the registry                                                                     
        Notes:     
        @param pcSettingsKey The key under which the settings for this Form are stored (e.g. Position)
        @return The string containing the stored window settings 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER LoadSettings (pcSettingsKey AS CHARACTER):
    
        RETURN BaseFormHelper:LoadSettings (THIS-OBJECT, 
                                            pcSettingsKey) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosed event                                                                     
        Notes:   Deletes the Form Instance when it has been closed.
                 See Progress K-Base Entry ID: P179041
                 Title: "User Controls that have events attached to them cause 
                 parent forms to not get garbage collected"
                 Dialog Forms will not get deleted. Typically Dialog Forms will need
                 to return values to the calling Program using properties. This 
                 requires that the dialog form is valid after the WAIT-FOR 
                 ShowDialog statement. 
                 (Dialog Forms are detected using the Modal property)
        @param e A FormClosedEventArgs that contains the event data. 
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnFormClosed (INPUT e AS System.Windows.Forms.FormClosedEventArgs):
    
        /* Mike Fechner, Consultingwerk Ltd. 11.05.2014
           Now moved to just actively hiding an MDI Child while closing. This will cause
           that the MDI Container's UltraToolbarsController removes the Ribbon Tools of 
           the MDI Child earlier. Seems to solve remaining issues with dying Ribbons in 
           the MDI Container */
        /* Mike Fechner, Consultingwerk Ltd. 24.01.2014
           Commented out the following block of code. Setting the MDI Parent property 
           seems to cause the .NET Framework to Create to Form on the Windows desktop 
           a) it seems no longer required as a workaround to issues in ther OnMdiChildActivate 
              event method override of an MDI Container
           b) causes flickering issues when closing WinKit Forms that are not MDI Childs,
              e.g. an MS Office application behind the MDI Container Form popping up when 
              closing an non MDI Child Window in the application */
/*        /* Mike Fechner, Consultingwerk Ltd. 08.11.2012                 */
/*           Unparent Mdi Childs during closing to avoid System.Exceptions*/
/*           from the Mdi Containers OnMdiChildActivate (?) method */     */
        IF NOT THIS-OBJECT:DesignTime AND VALID-OBJECT (THIS-OBJECT:MdiParent) THEN DO:
/*            THIS-OBJECT:MdiParent = ? .  */
            THIS-OBJECT:Visible = FALSE .
        END.
        
        THIS-OBJECT:IsClosed = TRUE . 
        
        DO ON ERROR UNDO, THROW:
            SUPER:OnFormClosed (INPUT e).
        
            /* Mike Fechner, Consultingwerk Ltd. 02.08.2011
               When a Control subscribing to the FormClosed event of the containing Form
               (or grand-parent Form) is already disposed it looks like in 10.2B04 the 
               AVM is causing a NullReferenceException when the .NET side of the bridge 
               is trying to invoke the ABL event handler. It is considered save to ignore 
               those errors during OnFormClosed in the Form base class 
               (Progress.Windows.Form) as there is little we can do from the ABL to 
               avoid these errors. */
            CATCH nullex AS System.NullReferenceException:
                /* do nothing */    
            END CATCH.
        END.
    
/*        &IF DEFINED (Infragistics) NE 0 AND DEFINED (ClearToolsCollectionOnFormClosed) NE 0 &THEN               */
/*        IF NOT THIS-OBJECT:DesignTime THEN DO:                                                                  */
/*            /* Mike Fechner, Consultingwerk Ltd. 12.03.2014                                                     */
/*               Clear the Tools Collection of a ToolbarsManager, to workaround error                             */
/*               Attempt to use an object that has been explicitly deleted (15580)                                */
/*               when closing a Form that is using a ControlContainerTool hosting an                              */
/*               UltraNumericEditor */                                                                            */
/*            DEFINE VARIABLE oToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO .*/
/*                                                                                                                */
/*            oToolbarsManager = Infragistics.Win.UltraWinToolbars.UltraToolbarsManager:FromForm (THIS-OBJECT) .  */
/*                                                                                                                */
/*            IF VALID-OBJECT (oToolbarsManager) THEN DO:                                                         */
/*                oToolbarsManager:Tools:Clear () .                                                               */
/*            END.                                                                                                */
/*        END .                                                                                                   */
/*        &ENDIF                                                                                                  */

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.    

        FINALLY:
            IF NOT THIS-OBJECT:DesignTime THEN DO:
                /* Mike Fechner, Consultingwerk Ltd. 14.04.2011
                   In 10.2B04 querying the Modal property in OnFormClosed caused a
                   System.NullReferenceException. Probably the .NET side is already
                   disposed due to a fix to the issue described in the Progress
                   K-Base solution P179041 - we are querying Modal in OnLoad now
                   and set the DeleteOnFormClosed to false property when Modal is
                   true */
                IF THIS-OBJECT:DeleteOnFormClosed /*AND NOT THIS-OBJECT:Modal*/ THEN 
                &IF DEFINED (DelayFormDeletionOnFormClosed) EQ 0 &THEN
                    DELETE OBJECT THIS-OBJECT .
                &ELSE
                /* Mike Fechner, Consultingwerk Ltd. 13.03.2014
                   SCL-204: Ability to delay the deletion of the BaseForm instance during
                   OnFormClosed by using a Timer event. Resolved crashes while closing some
                   SmartComponent Library Forms at Interbos/Dynamics4.NET on Windows 8/Windows
                   Server 2012 only */
                DO:
                    oCloseDeleteTimer = NEW System.Windows.Forms.Timer () . 
                    oCloseDeleteTimer:Interval = 10 .
                    oCloseDeleteTimer:Tick:Subscribe (CloseDeleteTimerTickHandler) .
                    oCloseDeleteTimer:Enabled = TRUE . 
                END. 
                &ENDIF
            END.
        END FINALLY .
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event                                                                        
        Notes:   Invoked when the Form is about to be closed   
        @param e A FormClosingEventArgs that contains the event data.                                                                   
    ------------------------------------------------------------------------------*/    
    METHOD OVERRIDE PROTECTED VOID OnFormClosing (e AS System.Windows.Forms.FormClosingEventArgs):
        
        IF THIS-OBJECT:DesignTime = FALSE THEN DO:
        
            IF THIS-OBJECT:StoreFormPosition AND 
               FrameworkSettings:StoreWindowPosition AND 
               NOT VALID-OBJECT (THIS-OBJECT:MdiParent) THEN 
                StoreWindowPosition () .
            
            IF FrameworkSettings:StoreRibbonQuickAccessToolbar THEN
                StoreRibbonQuickAccessToolbar () .

            IF FrameworkSettings:StoreUltraSplitterPosition THEN
                THIS-OBJECT:StoreRadSplitterPosition () .

        END.
        
        SUPER:OnFormClosing(e).

        BaseFormHelper:CloseDependentForms (THIS-OBJECT, e) .

    END METHOD.
        
    /*------------------------------------------------------------------------------
        Purpose: Raises the FormDestroyed event                                                                       
        Notes:                                 
        @param e The System.EventArgs with the data for this event                                       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFormDestroyed (e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
          e = System.EventArgs:Empty.
          
        THIS-OBJECT:FormDestroyed:Publish (THIS-OBJECT, e) .
 
        DELETE OBJECT e . 
 
    END METHOD.
        
     /*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                  
        Notes:  Invoked when a Form is displayed for the first time
        @param e An EventArgs that contains the event data.                                                                       
    ------------------------------------------------------------------------------*/    
    METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs):
    
        IF NOT THIS-OBJECT:DesignTime THEN DO:
        
            IF THIS-OBJECT:UseStartupSizeAsMinSize THEN 
                THIS-OBJECT:MinimumSize = THIS-OBJECT:Size . 
            
            IF THIS-OBJECT:StoreFormPosition AND 
               FrameworkSettings:StoreWindowPosition AND 
               NOT VALID-OBJECT (THIS-OBJECT:MdiParent) THEN 
                RestoreWindowPosition () .
    
            /* Mike Fechner, Consultingwerk Ltd. 14.04.2011
               In 10.2B04 querying the Modal property in OnFormClosed caused a 
               System.NullReferenceException. Probably the .NET side is already 
               disposed due to a fix to the issue described in the Progress 
               K-Base solution P179041 - we are querying Modal in OnLoad now 
               and set the DeleteOnFormClosed to false property when Modal is
               true */
            IF THIS-OBJECT:Modal THEN
                ASSIGN THIS-OBJECT:DeleteOnFormClosed = FALSE .  
    
/*            &IF DEFINED (Infragistics) NE 0 &THEN                         */
/*            /* Mike Fechner, Consultingwerk Ltd. 16.08.2013               */
/*               Allow customization on the UltraToolbarsManager associatied*/
/*               with this BaseForm instance */                             */
/*            THIS-OBJECT:CustomizeToolbar () .                             */
/*            &ENDIF                                                        */
    
            IF FrameworkSettings:StoreUltraSplitterPosition THEN
                THIS-OBJECT:RestoreRadSplitterPosition () .
        END.
        
        SUPER:OnLoad(e).

        IF NOT THIS-OBJECT:DesignTime AND FrameworkSettings:StoreRibbonQuickAccessToolbar THEN
            THIS-OBJECT:LoadRibbonQuickAccessToolbar () .

    END METHOD.        
    
&IF DEFINED (SmartComponentLibrary) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs                                                                       
        Notes:
        @param pcVerbText The label of the verb the developer clicked on in the Visual Designer                                                                           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):
    
        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE . 
        
        CASE pcVerbText:
            WHEN "Insert UserControl":U THEN 
                DesignerInsertUserControl () .
                
        END CASE.

        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE . 
        
    END METHOD.
&ENDIF
    
    /*------------------------------------------------------------------------------
       Purpose: Restores the position of the Rad Splitter Controls
       Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RestoreRadSplitterPosition ():
        
/*        DEFINE VARIABLE oList        AS "System.Collections.Generic.List<System.Windows.Forms.Control>":U NO-UNDO .                            */
/*        DEFINE VARIABLE oReference   AS Control                                                           NO-UNDO .                            */
/*        DEFINE VARIABLE iIndex       AS INTEGER                                                           NO-UNDO .                            */
/*        DEFINE VARIABLE cSettings    AS CHARACTER                                                         NO-UNDO .                            */
/*        DEFINE VARIABLE cSettingsKey AS CHARACTER                                                         NO-UNDO .                            */
/*                                                                                                                                               */
/*        oList = ControlHelper:FindAllControlsOfType (THIS-OBJECT, Progress.Util.TypeHelper:GetType ("Infragistics.Win.Misc.UltraSplitter":U)) .*/
/*                                                                                                                                               */
/*        {Consultingwerk/foreach.i Control oControl in oList}                                                                                   */
/*                                                                                                                                               */
/*            IF NOT VALID-OBJECT (oControl:Dock) THEN NEXT .                                                                                    */
/*                                                                                                                                               */
/*            ASSIGN oReference = ?                                                                                                              */
/*                   iIndex     = oControl:Parent:Controls:GetChildIndex (oControl).                                                             */
/*                                                                                                                                               */
/*            IF Progress.Util.EnumHelper:AreEqual (oControl:Dock, DockStyle:Left) THEN                                                          */
/*                oReference = ControlHelper:GetControlByChildIndex (oControl:Parent:Controls, iIndex + 1) .                                     */
/*            ELSE IF Progress.Util.EnumHelper:AreEqual (oControl:Dock, DockStyle:Right) THEN                                                    */
/*                oReference = ControlHelper:GetControlByChildIndex (oControl:Parent:Controls, iIndex - 1) .                                     */
/*            ELSE IF Progress.Util.EnumHelper:AreEqual (oControl:Dock, DockStyle:Top) THEN                                                      */
/*                oReference = ControlHelper:GetControlByChildIndex (oControl:Parent:Controls, iIndex - 1) .                                     */
/*            ELSE IF Progress.Util.EnumHelper:AreEqual (oControl:Dock, DockStyle:Bottom) THEN                                                   */
/*                oReference = ControlHelper:GetControlByChildIndex (oControl:Parent:Controls, iIndex + 1) .                                     */
/*                                                                                                                                               */
/*            IF NOT VALID-OBJECT (oReference) THEN                                                                                              */
/*                NEXT .                                                                                                                         */
/*                                                                                                                                               */
/*            ASSIGN cSettingsKey = SUBSTITUTE ("splitter:&1:&2":U,                                                                              */
/*                                              oControl:Name,                                                                                   */
/*                                              oReference:Name) .                                                                               */
/*                                                                                                                                               */
/*            ASSIGN cSettings = THIS-OBJECT:LoadSettings (cSettingsKey) .                                                                       */
/*                                                                                                                                               */
/*            IF cSettings > "":U AND NUM-ENTRIES (cSettings) > 1 THEN DO ON ERROR UNDO, THROW:                                                  */
/*                                                                                                                                               */
/*                oReference:Size = NEW System.Drawing.Size (INTEGER (ENTRY (1, cSettings)),                                                     */
/*                                                           INTEGER (ENTRY (2, cSettings))) .                                                   */
/*                                                                                                                                               */
/*                CATCH ex AS System.Exception:                                                                                                  */
/*                    /* ignore */                                                                                                               */
/*                END CATCH.                                                                                                                     */
/*            END.                                                                                                                               */
/*        END.                                                                                                                                   */
        
    END METHOD.
        
    /*------------------------------------------------------------------------------
        Purpose: Restores the stored window position                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/        
    METHOD PROTECTED VOID RestoreWindowPosition ():
        
        BaseFormHelper:RestoreWindowPosition (THIS-OBJECT) .
                           
    END METHOD.

/*    /*------------------------------------------------------------------------------*/
/*        Purpose: Sets Properties to customize Visual Designer behaviour             */
/*        Notes:                                                                      */
/*    ------------------------------------------------------------------------------*/*/
/*    METHOD PRIVATE VOID SetDesignerProperties ():                                   */
/*                                                                                    */
/*        THIS-OBJECT:DesignerVerbs = "Insert UserControl":U .                        */
/*                                                                                    */
/*    END METHOD.                                                                     */

    /*------------------------------------------------------------------------------
        Purpose: Stores the current Quick Access Toolbar items in the Registry
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreRibbonQuickAccessToolbar ():

/*        DEFINE VARIABLE oToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO .       */
/*        DEFINE VARIABLE cTools AS CHARACTER NO-UNDO.                                                               */
/*                                                                                                                   */
/*        oToolbarsManager = Infragistics.Win.UltraWinToolbars.UltraToolbarsManager:FromForm (THIS-OBJECT) .         */
/*                                                                                                                   */
/*        IF VALID-OBJECT (oToolbarsManager) THEN DO:                                                                */
/*            ASSIGN cTools = Consultingwerk.Util.UltraToolbarsHelper:GetQuickAccessToolbarTools (oToolbarsManager) .*/
/*                                                                                                                   */
/*            THIS-OBJECT:StoreSettings (cTools, "QuickAccessTools":U) .                                             */
/*                                                                                                                   */
/*            IF oToolbarsManager:Ribbon:Visible THEN                                                                */
/*                THIS-OBJECT:StoreSettings (STRING (oToolbarsManager:Ribbon:IsMinimized), "RibbonIsMinimized":U) .  */
/*        END.                                                                                                       */

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the current window settings (position etc.) to the registry                                                                        
        Notes:         
        @param pcSettings The string containing the window settings to be stored
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreSettings (pcSettings AS CHARACTER):
    
        THIS-OBJECT:StoreSettings (pcSettings, "Position":U) .
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores window settings to the registry                                                                       
        Notes:       
        @param pcSettings The string containing the window settings to be stored
        @param pcSettingsKey The key under which the settings for this Form are stored (e.g. Position)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreSettings (pcSettings AS CHARACTER,
                                         pcSettingsKey AS CHARACTER):
    
        BaseFormHelper:StoreSettings (THIS-OBJECT,
                                      pcSettings,
                                      pcSettingsKey) .
                                           
    END METHOD.
    
    /*------------------------------------------------------------------------------
       Purpose: Stores the position of the RadSplitter Controls in the Registry
       Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreRadSplitterPosition ():
        
/*        DEFINE VARIABLE oList        AS "System.Collections.Generic.List<System.Windows.Forms.Control>":U NO-UNDO .                            */
/*        DEFINE VARIABLE oReference   AS Control                                                           NO-UNDO .                            */
/*        DEFINE VARIABLE iIndex       AS INTEGER                                                           NO-UNDO .                            */
/*        DEFINE VARIABLE cSettings    AS CHARACTER                                                         NO-UNDO .                            */
/*        DEFINE VARIABLE cSettingsKey AS CHARACTER                                                         NO-UNDO .                            */
/*                                                                                                                                               */
/*        oList = ControlHelper:FindAllControlsOfType (THIS-OBJECT, Progress.Util.TypeHelper:GetType ("Infragistics.Win.Misc.UltraSplitter":U)) .*/
/*                                                                                                                                               */
/*        {Consultingwerk/foreach.i Control oControl in oList}                                                                                   */
/*                                                                                                                                               */
/*            IF NOT VALID-OBJECT (oControl:Dock) THEN NEXT .                                                                                    */
/*                                                                                                                                               */
/*            ASSIGN oReference = ?                                                                                                              */
/*                   iIndex     = oControl:Parent:Controls:GetChildIndex (oControl).                                                             */
/*                                                                                                                                               */
/*            IF Progress.Util.EnumHelper:AreEqual (oControl:Dock, DockStyle:Left) THEN                                                          */
/*                oReference = ControlHelper:GetControlByChildIndex (oControl:Parent:Controls, iIndex + 1) .                                     */
/*            ELSE IF Progress.Util.EnumHelper:AreEqual (oControl:Dock, DockStyle:Right) THEN                                                    */
/*                oReference = ControlHelper:GetControlByChildIndex (oControl:Parent:Controls, iIndex - 1) .                                     */
/*            ELSE IF Progress.Util.EnumHelper:AreEqual (oControl:Dock, DockStyle:Top) THEN                                                      */
/*                oReference = ControlHelper:GetControlByChildIndex (oControl:Parent:Controls, iIndex - 1) .                                     */
/*            ELSE IF Progress.Util.EnumHelper:AreEqual (oControl:Dock, DockStyle:Bottom) THEN                                                   */
/*                oReference = ControlHelper:GetControlByChildIndex (oControl:Parent:Controls, iIndex + 1) .                                     */
/*                                                                                                                                               */
/*            IF NOT VALID-OBJECT (oReference) THEN                                                                                              */
/*                NEXT .                                                                                                                         */
/*                                                                                                                                               */
/*            ASSIGN cSettingsKey = SUBSTITUTE ("splitter:&1:&2":U,                                                                              */
/*                                              oControl:Name,                                                                                   */
/*                                              oReference:Name)                                                                                 */
/*                   cSettings    = SUBSTITUTE ("&1,&2":U,                                                                                       */
/*                                              oReference:Width,                                                                                */
/*                                              oReference:Height) .                                                                             */
/*                                                                                                                                               */
/*            THIS-OBJECT:StoreSettings (cSettings, cSettingsKey) .                                                                              */
/*        END.                                                                                                                                   */

    END METHOD.
            
    /*------------------------------------------------------------------------------
        Purpose: Stores the window position to the resgistry                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID StoreWindowPosition ():
        
        BaseFormHelper:StoreWindowPosition (THIS-OBJECT) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the BaseRadRibbonForm class                                                                        
        Notes:   Publishes the FormDestroyed event so that SmartComponents can clean up.                                                                      
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BaseRadRibbonForm  ():

        OnFormDestroyed (System.EventArgs:Empty) .

/* Mike Fechner, Consultingwerk Ltd. 12.06.2012
   Ability to customize Consultingwerk.Forms.BaseForm */
&IF "{&CustomBaseFormDestructorInclude}":U NE "":U &THEN
{{&CustomBaseFormDestructorInclude}}
&ENDIF

    END DESTRUCTOR.
&ENDIF
END CLASS.
