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
    File        : BindingSourceFieldsForm
    Purpose     : Provides fields from a BindingSource as an IToolboxService
                  implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 09 23:46:44 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Design.*             FROM PROPATH .
USING Consultingwerk.Forms.*              FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH .
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY .
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Windows.*                  FROM ASSEMBLY .

CLASS Consultingwerk.Design.SmartComponents.BindingSourceFieldsForm 
    INHERITS Form 
    IMPLEMENTS System.Drawing.Design.IToolboxService: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.

    DEFINE PRIVATE VARIABLE oToolboxItem AS System.Drawing.Design.ToolboxItem NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the BindingSource                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSource AS Progress.Data.BindingSource NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the names of all the tool categories currently on the toolbox.                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CategoryNames AS System.Drawing.Design.CategoryNameCollection 
    GET.
     
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Reference to the associated Designer Control instance                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DesignerControl AS Consultingwerk.Design.SmartComponents.IDesignerControl NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Reference to the associated IDesignerHost instance                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DesignerHost AS System.ComponentModel.Design.IDesignerHost NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets or sets the name of the currently selected tool category from the toolbox.                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedCategory AS CHARACTER  
    GET.
    SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BindingSourceFieldsForm class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BindingSourceFieldsForm ():
        
        SUPER().
        InitializeComponent().
        
        oToolboxItem = NEW System.Drawing.Design.ToolboxItem (Progress.Util.TypeHelper:GetType ("System.Windows.Forms.TextBox":U)) .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds a new toolbox item creator for a specified data format.                                                                       
        Notes:   Not implemented       
        @param creator A ToolboxItemCreatorCallback that can create a component when the toolbox item is invoked.
        @param format0 The data format that the creator handles.            
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddCreator (creator AS System.Drawing.Design.ToolboxItemCreatorCallback, 
                                   format0 AS CHARACTER):
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a new toolbox item creator for a specified data format and designer host.                                                                       
        Notes:   Not implemented   
        @param creator A ToolboxItemCreatorCallback that can create a component when the toolbox item is invoked.
        @param format0 The data format that the creator handles.                                                           
        @param host The IDesignerHost that represents the designer host to associate with the creator.                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddCreator (creator AS System.Drawing.Design.ToolboxItemCreatorCallback,
                                   format0 AS CHARACTER, 
                                   host AS System.ComponentModel.Design.IDesignerHost):
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds the specified project-linked toolbox item to the toolbox.                                                                        
        Notes:   Not implemented                                                                   
        @param toolboxItem The linked ToolboxItem to add to the toolbox.
        @param host The reference to the IDesignerHost implementation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddLinkedToolboxItem (toolboxItem AS System.Drawing.Design.ToolboxItem, 
                                             host AS System.ComponentModel.Design.IDesignerHost):
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds the specified project-linked toolbox item to the toolbox in the specified category.                                                                       
        Notes:   Not implemented    
        @param toolboxItem The linked ToolboxItem to add to the toolbox.
        @param category The toolbox item category to add the toolbox item to.
        @param host The IDesignerHost for the current design document.                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddLinkedToolboxItem (toolboxItem AS System.Drawing.Design.ToolboxItem, 
                                             category AS CHARACTER, 
                                             host AS System.ComponentModel.Design.IDesignerHost):
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds the specified toolbox item to the toolbox.                                                                       
        Notes:   Not implemented                                                                     
        @param toolboxItem The ToolboxItem to add to the toolbox.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddToolboxItem (toolboxItem AS System.Drawing.Design.ToolboxItem):
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds the specified toolbox item to the toolbox in the specified category.                                                                       
        Notes:   Not implemented
        @param toolboxItem The ToolboxItem to add to the toolbox.
        @param category The toolbox item category to add the ToolboxItem to.                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddToolboxItem (toolboxItem AS System.Drawing.Design.ToolboxItem, 
                                       category AS CHARACTER):
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a toolbox item from the specified object that represents a 
                 toolbox item in serialized form.                                                                       
        Notes:                                                                        
        @param serializedObject The object that contains the ToolboxItem to retrieve.
        @return The ToolboxItem created from deserialization.                                                                                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Design.ToolboxItem DeserializeToolboxItem (serializedObject AS System.Object):
    
       RETURN THIS-OBJECT:DeserializeToolboxItem (serializedObject, THIS-OBJECT:DesignerHost) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a toolbox item from the specified object that represents a 
                 toolbox item in serialized form, using the specified designer host.                                                                       
        Notes:  
        @param serializedObject The object that contains the ToolboxItem to retrieve.
        @param host The IDesignerHost to associate with this ToolboxItem.
        @return The ToolboxItem created from deserialization.                                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Design.ToolboxItem DeserializeToolboxItem (serializedObject AS System.Object, 
                                                                            host AS System.ComponentModel.Design.IDesignerHost):
        
        RETURN CAST (CAST (serializedObject, System.Windows.Forms.DataObject):GetData (Progress.Util.TypeHelper:GetType ("System.Drawing.Design.ToolboxItem":U)),
                           System.Drawing.Design.ToolboxItem).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets the currently selected toolbox item                                                                       
        Notes:   Not implemented                                                                  
        @return The ToolboxItem that is currently selected, or a null reference (Nothing in Visual Basic) if no toolbox item is currently selected.                                                                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Design.ToolboxItem GetSelectedToolboxItem ():
        
        RETURN ? /*oToolboxItem*/ . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets the currently selected toolbox item if it is available to all 
                 designers, or if it supports the specified designer.                                                                       
        Notes:   Not implemented
        @param host The IDesignerHost that the selected tool must be associated with for it to be returned.
        @return The ToolboxItem that is currently selected, or a null reference (Nothing in Visual Basic) if no toolbox item is currently selected.                                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Design.ToolboxItem GetSelectedToolboxItem (host AS System.ComponentModel.Design.IDesignerHost):
        
        RETURN ? /*oToolboxItem*/ .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets the entire collection of toolbox items from the toolbox.                                                                       
        Notes:   Not implemented                                                                     
        @return A ToolboxItemCollection that contains the current toolbox items that are associated with the specified category and designer host.                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Design.ToolboxItemCollection GetToolboxItems ():
        
        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets the collection of toolbox items that are associated with 
                 the specified designer host from the toolbox.                                                                       
        Notes:   Not implemented                                                                   
        @param host The IDesignerHost that is associated with the toolbox items to retrieve.
        @return A ToolboxItemCollection that contains the current toolbox items that are associated with the specified category and designer host.                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Design.ToolboxItemCollection GetToolboxItems (host AS System.ComponentModel.Design.IDesignerHost):
        
        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a collection of toolbox items from the toolbox that match 
                 the specified category.                                                                       
        Notes:   Not implemented                                                              
        @param category The toolbox item category to retrieve the toolbox items from.
        @return A ToolboxItemCollection that contains the current toolbox items that are associated with the specified category and designer host.                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Design.ToolboxItemCollection GetToolboxItems (category AS CHARACTER):
        
        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets the collection of toolbox items that are associated with the 
                 specified designer host and category from the toolbox.                                                                        
        Notes:   Not implemented
        @param category The toolbox item category to retrieve the toolbox items from.
        @param host The IDesignerHost that is associated with the toolbox items to retrieve.
        @return A ToolboxItemCollection that contains the current toolbox items that are associated with the specified category and designer host.                                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Design.ToolboxItemCollection GetToolboxItems (category AS CHARACTER, 
                                                                               host AS System.ComponentModel.Design.IDesignerHost):
        
        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds the fiels of the BindingSource schema as objects to the Toolbox                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeBindingSourceFields ():
        
        DEFINE VARIABLE oColumns AS "Progress.Data.ColumnPropDesc[]" NO-UNDO . 
        DEFINE VARIABLE oItem    AS UltraListViewItem                NO-UNDO . 
        
        
        /* Mike Fechner, Consultingwerk Ltd. 12.09.2011
           Make usable for the OpenEdge Architect Visual Designer */
        IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource) THEN 
            RETURN . 
        
        ASSIGN oColumns = THIS-OBJECT:BindingSource:TableSchema:Columns .

        {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in oColumns}
            
            ASSIGN oItem = ultraListView1:Items:Add (oColumn:ColumnName, oColumn:ColumnLabel) 
                   oItem:Tag = oColumn
                   oItem:Appearance:Image = DesignerFieldsHelper:DataTypeImage (oColumn:ColumnType) . 
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Design.SmartComponents.BindingSourceFieldsForm":U).
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraListView1 */
        /*  */
        THIS-OBJECT:ultraListView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraListView1:ItemSettings:AllowEdit = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraListView1:ItemSettings:DefaultImage = CAST(resources:GetObject("ultraListView1.ItemSettings.DefaultImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraListView1:ItemSettings:SelectionType = Infragistics.Win.UltraWinListView.SelectionType:Single.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(224, 500).
        THIS-OBJECT:ultraListView1:TabIndex = 0.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView1:ViewSettingsList:MultiColumn = FALSE.
        THIS-OBJECT:ultraListView1:MouseDown:Subscribe(THIS-OBJECT:ultraListView1_MouseDown).
        /*  */
        /* BindingSourceFieldsForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(224, 500).
        THIS-OBJECT:ControlBox = FALSE.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:SizableToolWindow.
        THIS-OBJECT:Name = "BindingSourceFieldsForm":U.
        THIS-OBJECT:Text = "Fields":U.
        THIS-OBJECT:TopMost = TRUE.
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a value indicating whether the specified object which 
                 represents a serialized toolbox item can be used by the specified designer host.                                                                     
        Notes:          
        @param serializedObject The object that contains the ToolboxItem to retrieve.
        @param host The IDesignerHost to test for support for the ToolboxItem.
        @return true if the specified object is compatible with the specified designer host; otherwise, false.                                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsSupported (serializedObject AS System.Object, 
                                       host AS System.ComponentModel.Design.IDesignerHost):
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a value indicating whether the specified object which 
                 represents a serialized toolbox item matches the specified attributes.                                                                       
        Notes:  
        @param serializedObject The object that contains the ToolboxItem to retrieve.
        @param filterAttributes An ICollection that contains the attributes to test the serialized object for.
        @return true if the object matches the specified attributes; otherwise, false.                                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsSupported (serializedObject AS System.Object, 
                                       filterAttributes AS System.Collections.ICollection):
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a value indicating whether the specified object is a serialized 
                 toolbox item.                                                                       
        Notes:                                                                        
        @param serializedObject The object to inspect.
        @return true if the object contains a toolbox item object; otherwise, false.                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsToolboxItem (serializedObject AS System.Object):
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a value indicating whether the specified object is a serialized 
                 toolbox item, using the specified designer host.                                                                       
        Notes:              
        @param serializedObject The object to inspect.
        @param host The IDesignerHost that is making this request.
        @return true if the object contains a toolbox item object; otherwise, false.                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsToolboxItem (serializedObject AS System.Object, 
                                         host AS System.ComponentModel.Design.IDesignerHost):
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                        
        Notes:   Invokes InitializeBindingSourceFields                           
        @param e The default .NET System.EventArgs instance of the Load event                                              
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        SUPER:OnLoad (e) .
        
        InitializeBindingSourceFields () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes a previously added toolbox item creator of the specified 
                 data format.                                                                        
        Notes:                                                                        
        @param format0 The data format of the creator to remove.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveCreator (format0 AS CHARACTER):
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes a previously added toolbox creator that is associated with 
                 the specified data format and the specified designer host.                                                                        
        Notes:      
        @param format0 The data format of the creator to remove.
        @param host The IDesignerHost that is associated with the creator to remove.                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveCreator (format0 AS CHARACTER, 
                                      host AS System.ComponentModel.Design.IDesignerHost):
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes the specified toolbox item from the toolbox.                                                                       
        Notes: 
        @param toolboxItem The ToolboxItem to remove from the toolbox.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveToolboxItem  (toolboxItem AS System.Drawing.Design.ToolboxItem):
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes the specified toolbox item from the toolbox.                                                                       
        Notes:                                                                        
        @param toolboxItem The ToolboxItem to remove from the toolbox.
        @param category The toolbox item category to remove the ToolboxItem from.                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveToolboxItem (toolboxItem AS System.Drawing.Design.ToolboxItem, 
                                          category AS CHARACTER ):
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Notifies the toolbox service that the selected tool has been used.                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SelectedToolboxItemUsed ():
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets a serializable object that represents the specified toolbox item.                                                                       
        Notes:                                                                      
        @param toolboxItem The ToolboxItem to serialize.
        @return An object that represents the specified ToolboxItem.  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Object SerializeToolboxItem (toolboxItem AS System.Drawing.Design.ToolboxItem):
    
        RETURN NEW System.Windows.Forms.DataObject (toolboxItem).
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the current application's cursor to a cursor that represents the 
                 currently selected tool.                                                                        
        Notes:      
        @return true if the cursor is set by the currently selected tool, false if there is no tool selected and the cursor is set to the standard windows cursor.                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SetCursor ():
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Selects the specified toolbox item.                                                                       
        Notes:  
        @param toolboxItem The ToolboxItem to select.                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetSelectedToolboxItem (toolboxItem AS System.Drawing.Design.ToolboxItem):
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseDown event of the ultraUltraView1                                                                       
        Notes:                                                              
        @param sender The object that raised the event
        @param e The MouseEventArgs instance that contains the parameters of the event          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_MouseDown (sender AS System.Object, 
                                                  e AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oItem   AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        DEFINE VARIABLE oColumn AS Progress.Data.ColumnPropDesc                        NO-UNDO . 

        IF THIS-OBJECT:ultraListView1:SelectedItems:Count > 0 THEN DO:  

            oItem = THIS-OBJECT:ultraListView1:SelectedItems[0] .
            oColumn = CAST (oItem:Tag, Progress.Data.ColumnPropDesc) NO-ERROR .

            IF VALID-OBJECT (oColumn) AND VALID-OBJECT (THIS-OBJECT:DesignerControl) THEN 
                THIS-OBJECT:DesignerControl:PrepareAddingControl (oColumn:ColumnName,
                                                                  oColumn:ColumnType:ToString(),
                                                                  oColumn:ColumnLabel) .            

            THIS-OBJECT:DoDragDrop (SerializeToolboxItem(oToolboxItem),
                                    System.Windows.Forms.DragDropEffects:Copy) .
        END.
        
        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the BindingSourceFieldsForm class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BindingSourceFieldsForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.