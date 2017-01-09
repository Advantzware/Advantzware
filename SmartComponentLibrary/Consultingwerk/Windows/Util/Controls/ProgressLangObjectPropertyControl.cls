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
    File        : ProgressLangObjectPropertyControl
    Purpose     : Visualizes properties on a Progress.Lang.Object instance
                  in a GUI for .NET Control
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Dec 05 08:21:09 CET 2015
    Notes       : Relies on reflection, so requires OpenEdge 11.6 or above
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*     FROM PROPATH .
USING Consultingwerk.Framework.Base.* FROM PROPATH.
USING Consultingwerk.OERA.*           FROM PROPATH .
USING Consultingwerk.Util.*           FROM PROPATH .
USING Consultingwerk.Windows.Util.*   FROM PROPATH .
USING Infragistics.Win.UltraWinTree.* FROM ASSEMBLY.
USING Progress.Lang.*                 FROM PROPATH .
USING Progress.Windows.*              FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Util.Controls.ProgressLangObjectPropertyControl 
    INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTree1 AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO.
		
    DEFINE PRIVATE VARIABLE oUltraTreeColumnResizeHelper AS UltraTreeColumnResizeHelper NO-UNDO.
		
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
        Purpose: Constructor for the ProgressLangObjectPropertyControl class  
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ProgressLangObjectPropertyControl ():
		
        SUPER().

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .          

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        Consultingwerk.Util.GarbageCollectorHelper:ClearUltraTreeNodes (THIS-OBJECT:ultraTree1) .

        THIS-OBJECT:ultraTree1:Nodes:Clear () .

        /* Initialize resizing for NodeColumnSets */
        oUltraTreeColumnResizeHelper = NEW UltraTreeColumnResizeHelper (THIS-OBJECT:ultraTree1,
                                                                        175,
                                                                        "PropertyLabel":U).

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Displays the properties of the passed in object in the property grid
	    Notes:
	    @param poObject The object to visualize
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID DisplayObjectProperties (poObject AS Progress.Lang.Object):
		
&IF DEFINED (AblReflection) NE 0 &THEN
		
		DEFINE VARIABLE oNode AS UltraTreeNode NO-UNDO .
		
        GarbageCollectorHelper:ClearUltraTreeNodes (ultraTree1) .

        oNode = ultraTree1:Nodes:Add ("root":U) .

        IF VALID-OBJECT (poObject) THEN DO: 
            oNode:Cells ["PropertyLabel":U]:Value = BOX (poObject:GetClass():TypeName) . 
            oNode:Cells ["PropertyValue":U]:Value = BOX (THIS-OBJECT:GetToStringValue (poObject)) . 
            
            THIS-OBJECT:DisplayObjectProperties (poObject, oNode:Nodes) .
        END.
        ELSE DO:
            oNode:Cells ["PropertyLabel":U]:Value = "<invalue object reference>":U . 
        END.
        
        oNode:Expanded = TRUE .
&ENDIF 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Displays the properties of the passed in object in the property grid
        Notes:
        @param poObject The object to visualize
        @param poTreeNodes The parent nodes collection to add the object properties to
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisplayObjectProperties (poObject AS Progress.Lang.Object,
                                                   poTreeNodes AS TreeNodesCollection):

&IF DEFINED (AblReflection) NE 0 &THEN

        DEFINE VARIABLE oProperty   AS Progress.Reflect.Property NO-UNDO . 
        DEFINE VARIABLE oProperties AS Progress.Reflect.Property NO-UNDO EXTENT .
        DEFINE VARIABLE i           AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE cType       AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cValue      AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cName       AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE oValue      AS Progress.Lang.Object      NO-UNDO .
        DEFINE VARIABLE oNode       AS UltraTreeNode             NO-UNDO .  
        DEFINE VARIABLE iIndex      AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE oItemNode   AS UltraTreeNode             NO-UNDO .     

        IF VALID-OBJECT (poTreeNodes:ParentNode) THEN 
            oUltraTreeColumnResizeHelper:InitializeColumnResizing (poTreeNodes:ParentNode:Level + 1).
        
        IF NOT VALID-OBJECT (poObject:GetClass()) THEN DO:
            oNode = poTreeNodes:Add (GUID) .
    
            oNode:Cells ["PropertyLabel":U]:Value = "?":U . 
            oNode:Cells ["PropertyValue":U]:Value = "Unable to reflect .NET object.":U .                
                
            RETURN . 
        END.
        
        oProperties = poObject:GetClass ():GetProperties(Progress.Reflect.Flags:Public:SetFlag (Progress.Reflect.Flags:Instance)) .
        
        DO i = 1 TO EXTENT (oProperties):
            ASSIGN oProperty = oProperties [i] 
                   cValue    = ? 
                   cName     = oProperty:Name
                   oValue    = ?.
            
            /* Skip Next-Sibling and Prev-Sibling */
            IF oProperty:Name = "Next-Sibling":U OR oProperty:Name = "Prev-Sibling":U THEN  
                NEXT .              
            
            IF VALID-OBJECT (oProperty:DataType) AND oProperty:DataType <> Progress.Reflect.DataType:Object THEN 
                ASSIGN cType = STRING (oProperty:DataType) .
            ELSE 
                ASSIGN cType = oProperty:DataTypeName .

            CASE oProperty:DataType:
                
                WHEN Progress.Reflect.DataType:Object THEN DO ON ERROR UNDO, THROW:
                    oValue = oProperty:Get (poObject) .
                    
                    IF VALID-OBJECT (oValue) THEN DO:
                        IF TYPE-OF (oValue, System.Object) THEN 
                            ASSIGN cName  = SUBSTITUTE ("&1 : &2":U, cName, CAST (oValue, System.Object):GetType():FullName)
                                   cValue = oValue:ToString () .                         
                        ELSE 
                            ASSIGN cName  = SUBSTITUTE ("&1 : &2":U, cName, oValue:GetClass():TypeName)
                                   cValue = oValue:ToString () .
                    END.                               
                    ELSE 
                        cValue = "<invalid object reference>":U . 
                    
                    CATCH err AS Progress.Lang.Error :
                        cValue = SUBSTITUTE ("Error: &1":U, err:GetMessage (1)) .   
                    END CATCH.
                END .
                OTHERWISE DO ON ERROR UNDO, THROW:
                    cValue = STRING (oProperty:Get (poObject)) .
                    
                    CATCH err AS Progress.Lang.Error :
                        cValue = SUBSTITUTE ("Error: &1":U, err:GetMessage (1)) .   
                    END CATCH.
                END.
                
            END CASE .
            
            oNode = poTreeNodes:Add (GUID) .
    
            oNode:Cells ["PropertyLabel":U]:Value = cName . 
            oNode:Cells ["PropertyValue":U]:Value = cValue . 
                    
            IF oProperty:DataTypeName > "":U AND VALID-OBJECT (oValue) THEN 
                THIS-OBJECT:DisplayObjectProperties (oValue, oNode:Nodes) . 
             
            /* Iterate IEnumerable, but not TableModel as that would dump all records */ 
            IF VALID-OBJECT (oValue) AND TYPE-OF (oValue, IEnumerable) AND NOT TYPE-OF (oValue, TableModel) THEN DO:
                ASSIGN iIndex = 1 .

                {Consultingwerk/foreachABL.i Progress.Lang.Object oItem in oValue}

                    oItemNode = oNode:Nodes:Add (GUID) .
    
                    IF NOT VALID-OBJECT (oItem) THEN DO:
                        oItemNode:Cells ["PropertyLabel":U]:Value = STRING (iIndex) . 
                        oItemNode:Cells ["PropertyValue":U]:Value = "<invalid object reference>":U . 
                    END.
                    ELSE DO:
                        DO ON ERROR UNDO, THROW:
                            ASSIGN cValue = oItem:ToString () . 
                            
                            CATCH err AS Progress.Lang.Error :
                            	cValue = err:GetMessage (1) .	
                            END CATCH.
                        END.

                        oUltraTreeColumnResizeHelper:InitializeColumnResizing (oNode:Level + 1).

                        oItemNode:Cells ["PropertyLabel":U]:Value = SUBSTITUTE ("&1 : &2":U, iIndex, oItem:GetClass():TypeName) . 
                        oItemNode:Cells ["PropertyValue":U]:Value = cValue . 

                        /* Avoid infinite recursion caused by objects referencing themselves (circular references) */
                        IF oNode:Level < 25 THEN 
                            THIS-OBJECT:DisplayObjectProperties (oItem, oItemNode:Nodes) .
                    END.
                
                    ASSIGN iIndex = iIndex + 1 . 
                END.
            END.
        END.
&ENDIF        

    END METHOD . 

	/*------------------------------------------------------------------------------
	    Purpose: Returns the string representation of an object instance
	    Notes:
	    @param poObject The reference to the object to return the string representation for
	    @return The character value representing the object
	------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER GetToStringValue (poObject AS Progress.Lang.Object):
		
		RETURN poObject:ToString () .

        CATCH err AS Progress.Lang.Error:
        	RETURN err:GetMessage (1) .	
        END CATCH.
    
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeColumnSet1 AS Infragistics.Win.UltraWinTree.UltraTreeColumnSet NO-UNDO.
        ultraTreeColumnSet1 = NEW Infragistics.Win.UltraWinTree.UltraTreeColumnSet().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn1 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn1 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNodeColumn2 AS Infragistics.Win.UltraWinTree.UltraTreeNodeColumn NO-UNDO.
        ultraTreeNodeColumn2 = NEW Infragistics.Win.UltraWinTree.UltraTreeNodeColumn().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE m_override1 AS Infragistics.Win.UltraWinTree.Override NO-UNDO.
        m_override1 = NEW Infragistics.Win.UltraWinTree.Override().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode1 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode1 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode2 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode2 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode3 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode3 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode4 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode4 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode5 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode5 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTreeNode6 AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
        ultraTreeNode6 = NEW Infragistics.Win.UltraWinTree.UltraTreeNode().
        THIS-OBJECT:ultraTree1 = NEW Infragistics.Win.UltraWinTree.UltraTree().
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTree1 */
        /*  */
        THIS-OBJECT:ultraTree1:ColumnSettings:AllowCellEdit = Infragistics.Win.UltraWinTree.AllowCellEdit:ReadOnly.
        THIS-OBJECT:ultraTree1:ColumnSettings:AllowCellSizing = Infragistics.Win.UltraWinTree.LayoutSizing:Both.
        THIS-OBJECT:ultraTree1:ColumnSettings:AutoGenerateColumnSets = FALSE.
        THIS-OBJECT:ultraTree1:ColumnSettings:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:ultraTree1:ColumnSettings:BorderStyleColumnHeader = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraTree1:ColumnSettings:ColumnAutoSizeMode = Infragistics.Win.UltraWinTree.ColumnAutoSizeMode:VisibleNodes.
        THIS-OBJECT:ultraTree1:ColumnSettings:ColumnHeaderWrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraTreeNodeColumn1:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn1:Key = "PropertyLabel":U.
        ultraTreeNodeColumn1:LayoutInfo:PreferredCellSize = NEW System.Drawing.Size(153, 18).
        ultraTreeNodeColumn1:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size(153, 0).
        ultraTreeNodeColumn1:LayoutInfo:SpanX = 2.
        ultraTreeNodeColumn1:LayoutInfo:SpanY = 2.
        ultraTreeNodeColumn2:ButtonDisplayStyle = Infragistics.Win.UltraWinTree.ButtonDisplayStyle:Always.
        ultraTreeNodeColumn2:Key = "PropertyValue":U.
        ultraTreeNodeColumn2:LayoutInfo:PreferredCellSize = NEW System.Drawing.Size(221, 18).
        ultraTreeNodeColumn2:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size(221, 0).
        ultraTreeNodeColumn2:LayoutInfo:SpanX = 2.
        ultraTreeNodeColumn2:LayoutInfo:SpanY = 2.
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn1).
        ultraTreeColumnSet1:Columns:Add(ultraTreeNodeColumn2).
        ultraTreeColumnSet1:Key = "Level-0":U.
        ultraTreeColumnSet1:TipStyleCell = Infragistics.Win.UltraWinTree.TipStyleCell:Show.
        THIS-OBJECT:ultraTree1:ColumnSettings:ColumnSets:Add(ultraTreeColumnSet1).
        THIS-OBJECT:ultraTree1:ColumnSettings:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:ColumnSettings:LabelPosition = Infragistics.Win.UltraWinTree.NodeLayoutLabelPosition:None.
        THIS-OBJECT:ultraTree1:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        THIS-OBJECT:ultraTree1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTree1:FullRowSelect = TRUE.
        THIS-OBJECT:ultraTree1:ImageTransparentColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraTree1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTree1:Name = "ultraTree1":U.
        THIS-OBJECT:ultraTree1:NodeConnectorColor = System.Drawing.SystemColors:ControlDark.
        m_override1:ColumnSetIndex = 0.
        THIS-OBJECT:ultraTree1:NodeLevelOverrides:Add(m_override1).
        ultraTreeNode2:Text = "":U.
        ultraTreeNode3:Text = "":U.
        ultraTreeNode4:Text = "":U.
        ultraTreeNode5:Text = "":U.
        ultraTreeNode6:Text = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 5 NO-UNDO.
        arrayvar0[1] = ultraTreeNode2.
        arrayvar0[2] = ultraTreeNode3.
        arrayvar0[3] = ultraTreeNode4.
        arrayvar0[4] = ultraTreeNode5.
        arrayvar0[5] = ultraTreeNode6.
        ultraTreeNode1:Nodes:AddRange(arrayvar0).
        ultraTreeNode1:Text = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinTree.UltraTreeNode EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraTreeNode1.
        THIS-OBJECT:ultraTree1:Nodes:AddRange(arrayvar1).
        THIS-OBJECT:ultraTree1:Size = NEW System.Drawing.Size(470, 327).
        THIS-OBJECT:ultraTree1:TabIndex = 0.
        THIS-OBJECT:ultraTree1:UseAppStyling = FALSE.
        THIS-OBJECT:ultraTree1:ViewStyle = Infragistics.Win.UltraWinTree.ViewStyle:Grid.
        /*  */
        /* ProgressLangObjectPropertyControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTree1).
        THIS-OBJECT:Name = "ProgressLangObjectPropertyControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(470, 327).
        CAST(THIS-OBJECT:ultraTree1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ProgressLangObjectPropertyControl class  
        Notes:   
    ------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC ProgressLangObjectPropertyControl ():

	END DESTRUCTOR.

END CLASS.
