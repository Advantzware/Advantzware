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
    File        : DataObjectDefViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.11.2013 18:18:53
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.Windows.Forms.*                          FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Rollbase.DataObjectDefViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE DataObjectDef_objDefName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE DataObjectDef_SingularName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE DataObjectDef_SingularName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE DataObjectDef_PluralName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE DataObjectDef_PluralName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE DataObjectDef_id_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE DataObjectDef_id AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE DataObjectDef_origId_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE DataObjectDef_origId AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DataObjectDefViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DataObjectDefViewerControl ():
                
        SUPER().
        InitializeComponent().
        
        THIS-OBJECT:SetControlEnabled (smartBusinessEntityLookup1,
                                       ultraButton1,
                                       Consultingwerk.SmartComponents.Enum.ControlEnabledEnum:Always) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Handles the FormClosed event of the containing Form                                                                        
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The FormClosingEventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ViewerFormClosingHandler (sender AS System.Object, 
                                                  e AS FormClosingEventArgs):

        IF VALID-OBJECT (sender) AND TYPE-OF (sender, System.Windows.Forms.Form) THEN 
            CAST (sender, System.Windows.Forms.Form):FormClosing:Unsubscribe (ViewerFormClosingHandler) .
            
        CAST (THIS-OBJECT:SmartDataSource, SmartBusinessEntityAdapter):NewFetchDataRequest:Unsubscribe (NewFetchDataRequestHandler) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs):
        
        SUPER:OnLoad (e).

        CAST (THIS-OBJECT:SmartDataSource, SmartBusinessEntityAdapter):NewFetchDataRequest:Subscribe (NewFetchDataRequestHandler) .
        THIS-OBJECT:FindForm():FormClosing:Subscribe (ViewerFormClosingHandler) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Filters the DataSource 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Filter ():
        
        CAST (THIS-OBJECT:SmartDataSource, SmartBusinessEntityAdapter):RetrieveData () .  

    END METHOD .

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("DataObjectDef":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Rollbase.DataObjectDefViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:DataObjectDef_objDefName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:DataObjectDef_SingularName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:DataObjectDef_SingularName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:DataObjectDef_PluralName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:DataObjectDef_PluralName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:DataObjectDef_id_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:DataObjectDef_id = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:DataObjectDef_origId_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:DataObjectDef_origId = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:DataObjectDef_SingularName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:DataObjectDef_PluralName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:DataObjectDef_id, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:DataObjectDef_origId, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.RollbaseAdapter.MetaData.ObjectDefBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "DataObjectDef":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("id":U, "id":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("origId":U, "origId":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("objDefName":U, "objDefName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("SingularName":U, "SingularName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("PluralName":U, "PluralName":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* DataObjectDef_objDefName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:DataObjectDef_objDefName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:DataObjectDef_objDefName_Label, "DataObjectDef_objDefName_Label":U).
        THIS-OBJECT:DataObjectDef_objDefName_Label:Name = "DataObjectDef_objDefName_Label":U.
        /*  */
        /* DataObjectDef_SingularName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:DataObjectDef_SingularName_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:DataObjectDef_SingularName_Label, "DataObjectDef_SingularName_Label":U).
        THIS-OBJECT:DataObjectDef_SingularName_Label:Name = "DataObjectDef_SingularName_Label":U.
        /*  */
        /* DataObjectDef_SingularName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:DataObjectDef_SingularName, "DataObjectDef_SingularName":U).
        THIS-OBJECT:DataObjectDef_SingularName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SingularName":U, TRUE)).
        THIS-OBJECT:DataObjectDef_SingularName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:DataObjectDef_SingularName:Name = "DataObjectDef_SingularName":U.
        /*  */
        /* DataObjectDef_PluralName_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:DataObjectDef_PluralName_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:DataObjectDef_PluralName_Label, "DataObjectDef_PluralName_Label":U).
        THIS-OBJECT:DataObjectDef_PluralName_Label:Name = "DataObjectDef_PluralName_Label":U.
        /*  */
        /* DataObjectDef_PluralName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:DataObjectDef_PluralName, "DataObjectDef_PluralName":U).
        THIS-OBJECT:DataObjectDef_PluralName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "PluralName":U, TRUE)).
        THIS-OBJECT:DataObjectDef_PluralName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:DataObjectDef_PluralName:Name = "DataObjectDef_PluralName":U.
        /*  */
        /* DataObjectDef_id_Label */
        /*  */
        resources:ApplyResources(THIS-OBJECT:DataObjectDef_id_Label, "DataObjectDef_id_Label":U).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:DataObjectDef_id_Label:Appearance = appearance4.
        THIS-OBJECT:DataObjectDef_id_Label:Name = "DataObjectDef_id_Label":U.
        /*  */
        /* DataObjectDef_id */
        /*  */
        resources:ApplyResources(THIS-OBJECT:DataObjectDef_id, "DataObjectDef_id":U).
        THIS-OBJECT:DataObjectDef_id:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "id":U, TRUE)).
        THIS-OBJECT:DataObjectDef_id:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:DataObjectDef_id:Name = "DataObjectDef_id":U.
        /*  */
        /* DataObjectDef_origId_Label */
        /*  */
        resources:ApplyResources(THIS-OBJECT:DataObjectDef_origId_Label, "DataObjectDef_origId_Label":U).
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:DataObjectDef_origId_Label:Appearance = appearance5.
        THIS-OBJECT:DataObjectDef_origId_Label:Name = "DataObjectDef_origId_Label":U.
        /*  */
        /* DataObjectDef_origId */
        /*  */
        resources:ApplyResources(THIS-OBJECT:DataObjectDef_origId, "DataObjectDef_origId":U).
        THIS-OBJECT:DataObjectDef_origId:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "origId":U, TRUE)).
        THIS-OBJECT:DataObjectDef_origId:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:DataObjectDef_origId:Name = "DataObjectDef_origId":U.
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup1, "smartBusinessEntityLookup1":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "objDefName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupAdditionalFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "ObjectDefName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupCascade = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupControls = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogClassName = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "ObjectDefName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.RollbaseAdapter.MetaData.ObjectDefNamesBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eObjectDefName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityView = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "ObjectDefName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValue = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "FOR EACH eObjectDefName WHERE eObjectDefName.ObjectDefName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDelay = 750.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        THIS-OBJECT:smartBusinessEntityLookup1:KeyDown:Subscribe(THIS-OBJECT:smartBusinessEntityLookup1_KeyDown).
        /*  */
        /* ultraButton1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButton1, "ultraButton1":U).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* DataObjectDefViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DataObjectDef_objDefName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DataObjectDef_SingularName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DataObjectDef_SingularName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DataObjectDef_PluralName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DataObjectDef_PluralName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DataObjectDef_id_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DataObjectDef_id).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DataObjectDef_origId_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:DataObjectDef_origId).
        THIS-OBJECT:Name = "DataObjectDefViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:DataObjectDef_SingularName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:DataObjectDef_PluralName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:DataObjectDef_id, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:DataObjectDef_origId, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the NewFetchDataRequest event of the data source 
        Notes:   
        @param sender The object that raised the NewFetchDataRequest event
        @param e The NewFetchDataRequestEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID NewFetchDataRequestHandler (sender AS Progress.Lang.Object,
                                                      e AS NewFetchDataRequestEventArgs):
        
        IF CharacterType:IsNullOrEmpty (smartBusinessEntityLookup1:Text) THEN 
            UNDO, THROW NEW AppError ("Object Name needs to be selected first."{&TRAN}, 0) .
             
        e:FetchDataRequest:CustomParameter = NEW CharacterHolder (smartBusinessEntityLookup1:Text).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the smartBusinessEntityLookup1
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityLookup1_KeyDown (sender AS System.Object,
                                                            e AS System.Windows.Forms.KeyEventArgs):
        
        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Enter) OR  
           Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Return) THEN  
          
            THIS-OBJECT:Filter () . 

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) . 
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object, 
                                            e AS System.EventArgs):
        
        THIS-OBJECT:Filter () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) . 
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DataObjectDefViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DataObjectDefViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
