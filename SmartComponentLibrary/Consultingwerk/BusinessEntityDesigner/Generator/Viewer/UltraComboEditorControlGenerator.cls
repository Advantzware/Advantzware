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
    File        : UltraComboEditorControlGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 08:12:34 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.*                    FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*          FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.*   FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Generator.Web2View.* FROM PROPATH.
USING Consultingwerk.Util.*                                      FROM PROPATH . 
USING Progress.Lang.*                                            FROM PROPATH .
USING Progress.Util.*                                            FROM ASSEMBLY .
USING Infragistics.Win.*                                         FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.UltraComboEditorControlGenerator
    INHERITS ControlGenerator
    IMPLEMENTS IControlGenerator, IHasWidthAndHeight, IHasValidatingEvent, IHasValueChangedEvent, ISupportsWeb2ViewGeneration:

    DEFINE VARIABLE oValueListItems AS Consultingwerk.Framework.Collections.CharacterList NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Container Control 
        Notes:   Leave Empty if the Control is not part of a Container such as a 
                 GroupBox
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContainerName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the TypeName for to be used for this UltraEditorControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ControlTypeName AS CHARACTER NO-UNDO INIT "Infragistics.Win.UltraWinEditors.UltraComboEditor":U
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: The DataBindigns for the generated Control
        Notes:   The ControlGenerator implementation determines the actual properties
                 that we'll bind to (Text, Value, ...). So this propery contains the 
                 names of the BindingSource Fields. Multiple field names are supported,
                 in that case we'll pass in a comma delimited list of property names
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataBindings AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the collection of the ValueList Items
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Items AS "System.Collections.Generic.List<Infragistics.Win.ValueListItem>" NO-UNDO  
    GET. 
    PRIVATE SET . 

    /*------------------------------------------------------------------------------
        Purpose: The Height of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Height AS INTEGER NO-UNDO INIT 21
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The Position of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Left AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Control
        Notes:   Removes Designer unsafe characters in the Name
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO 
    GET.
    SET (arg AS CHARACTER):
        THIS-OBJECT:Name = DesignerHelper:DesignerSafeName (arg) . 
    END .  

    /*------------------------------------------------------------------------------
        Purpose: Logical value indicating if the Control supports the ISupportInitialize 
                 interface
        Notes:   Used for generating the BeginInit and EndInit method calls
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SupportInitialize AS LOGICAL NO-UNDO INIT FALSE 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The TabOrder property of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TabOrder AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The Tag property of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tag AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template File to be used by this class
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TemplateFileName AS CHARACTER NO-UNDO INIT "UI/ultracomboeditor.template":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template File to be used by this class
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TemplateFileNameUnbound AS CHARACTER NO-UNDO INIT "UI/ultracomboeditor_unbound.template":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Code block for the Validating (LEAVE) event
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ValidatingTriggerBlock AS LONGCHAR NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Code block for the ValueChanged (VALUE-CHANGED) event
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ValueChangedTriggerBlock AS LONGCHAR NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: The Width of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Width AS INTEGER NO-UNDO INIT 80 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The Position of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Top AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraComboEditorControlGenerator class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraComboEditorControlGenerator ():
        SUPER ().
    
        THIS-OBJECT:Items = NEW "System.Collections.Generic.List<Infragistics.Win.ValueListItem>" () .  
        oValueListItems = NEW Consultingwerk.Framework.Collections.CharacterList () . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Generats the source code that assigns the property values for this Control
        Notes:   
        @return The source code for the code block that assigns the property values for this Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateAssignPropertiesBlock ():
        
        DEFINE VARIABLE lcCode      AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cProperties AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE iIndex      AS INTEGER   NO-UNDO INIT 0.
        DEFINE VARIABLE cValue      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cVarName    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cArrayName  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .

        IF THIS-OBJECT:DataBindings > "":U THEN 
            FILE-INFO:FILE-NAME = TemplateHelper:Find (UltraComboEditorControlGenerator:TemplateFileName) .
        ELSE 
            FILE-INFO:FILE-NAME = TemplateHelper:Find (UltraComboEditorControlGenerator:TemplateFileNameUnbound) .

        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO lcCode . 
        
        {Consultingwerk/foreach.i Infragistics.Win.ValueListItem oItem in THIS-OBJECT:Items}
        
            ASSIGN iIndex   = iIndex + 1
                   cVarName = oValueListItems:GetValue (iIndex) . 
        
            IF TYPE-OF (oItem:DataValue, System.String) THEN 
                ASSIGN cValue = SUBSTITUTE ("~"&1~":U", UNBOX (oItem:DataValue)) .
            ELSE 
                ASSIGN cValue = STRING (UNBOX (oItem:DataValue)) .
        
            ASSIGN cProperties = cProperties +
                                 SUBSTITUTE ("        &1:DataValue = &2.~n" + 
                                             "        &1:DisplayText = ~"&3~":U.~n",
                                             cVarName,
                                             cValue,
                                             oItem:DisplayText) .  
        END.

        ASSIGN cArrayName 
        &IF NOT PROVERSION BEGINS "10.2" &THEN
                 = THIS-OBJECT:ContainerControlGenerator:GenerateObjectName ("arrayvar":U). 
        &ELSE        
                 = CAST (THIS-OBJECT:ContainerControlGenerator, IContainerControlGenerator):GenerateObjectName ("arrayvar":U). 
        &ENDIF     

        IF THIS-OBJECT:Items:Count > 0 THEN DO:  
            ASSIGN cProperties = cProperties + 
                                  SUBSTITUTE ("        @VisualDesigner.FormMember (NeedsInitialize=~"false~":U, InitializeArray=~"true~":U).~n":U +
                                              BusinessEntityGenerator:CaseKeywords ("        DEFINE VARIABLE &1 AS Infragistics.Win.ValueListItem EXTENT &2 NO-UNDO.~n":U, " Infragistics.Win.ValueListItem ":U),
                                              cArrayName,
                                              oValueListItems:Count) . 
    
            DO iIndex = 1 TO oValueListItems:Count:
        
                ASSIGN cProperties = cProperties + 
                                     SUBSTITUTE ("        &1[&2] = &3.~n",
                                                 cArrayName, iIndex, oValueListItems:GetValue (iIndex)) .
            END.
    
            ASSIGN cProperties = cProperties +
                                 SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:&1:Items:AddRange(&2).~n", ":Items:AddRange(":U),
                                             THIS-OBJECT:Name,
                                             cArrayName) . 
        END.
                
        ASSIGN lcCode = SUBSTITUTE (lcCode, 
                                    THIS-OBJECT:Name,
                                    THIS-OBJECT:DataBindings,
                                    "":U /* no appearance object */,
                                    THIS-OBJECT:Left,
                                    THIS-OBJECT:Top,
                                    THIS-OBJECT:TabOrder,
                                    cProperties) + "~n":U + 
                                    (IF THIS-OBJECT:Tag > "":U THEN SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:&1:Tag = &2:U.~n":U, ":Tag ":U),
                                                                                THIS-OBJECT:Name, 
                                                                                QUOTER(THIS-OBJECT:Tag)) ELSE "":U)
                                    . 

        ASSIGN lcCode = REPLACE (REPLACE (lcCode, 
                                          "@Width@":U, 
                                          STRING (THIS-OBJECT:Width)), 
                                 "@Height@":U,
                                 STRING (THIS-OBJECT:Height)) .        

        IF THIS-OBJECT:ValidatingTriggerBlock > "":U THEN DO:
            ASSIGN lcCode = lcCode + SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN   
                                                    "        THIS-OBJECT:&1:Validating:Subscribe(&1_Validating).~n":U
                                                 ELSE 
                                                    "        this-object:&1:Validating:Subscribe(&1_Validating).~n":U,
                                                 THIS-OBJECT:Name) .
                                                 
            THIS-OBJECT:InsertEventHandler (SUBSTITUTE ("&1_Validating":U, THIS-OBJECT:Name), 
                                            "Validating":U,
                                            THIS-OBJECT:Name,
                                            "System.ComponentModel.CancelEventArgs":U, 
                                            THIS-OBJECT:ValidatingTriggerBlock) .                                                 
        END.

        IF THIS-OBJECT:ValueChangedTriggerBlock > "":U THEN DO:
            ASSIGN lcCode = lcCode + SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                    "        THIS-OBJECT:&1:ValueChanged:Subscribe(&1_ValueChanged).~n":U
                                                 ELSE 
                                                    "        this-object:&1:ValueChanged:Subscribe(&1_ValueChanged).~n":U,
                                                 THIS-OBJECT:Name) .

            THIS-OBJECT:InsertEventHandler (SUBSTITUTE ("&1_ValueChanged":U, THIS-OBJECT:Name), 
                                            "ValueChanged":U,
                                            THIS-OBJECT:Name,
                                            "System.EventArgs":U, 
                                            THIS-OBJECT:ValueChangedTriggerBlock) .                                                 
        END.
        
        RETURN lcCode . 

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Generates the NEW statement used to create an intance of the Control
        Notes:   
        @return The source code for the NEW statement to create an instanc of the Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateControlNew ():
        
        RETURN SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:&1 = NEW &2().~n":U),
                           THIS-OBJECT:Name,
                           THIS-OBJECT:ControlTypeName) .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML for the current Control 
        Notes:   
        @return The HTML source for the current Control 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateHtml ():

        DEFINE VARIABLE oGenerator AS IHtmlControlGenerator NO-UNDO . 
        DEFINE VARIABLE cInner     AS CHARACTER             NO-UNDO .
        
        oGenerator = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Generator.Web2View.IHtmlControlGenerator
                                                   "NEW Consultingwerk.BusinessEntityDesigner.Generator.Web2View.HtmlControlGenerator()"} .
        
        {Consultingwerk/foreach.i ValueListItem oItem in THIS-OBJECT:Items}

            ASSIGN cInner = cInner + 
                            SUBSTITUTE ('<option value="&1">&2</option>':U,
                                        STRING (UNBOX (oItem:DataValue)),
                                        oItem:DisplayText) +
                            "~n":U . 
        END.        
        
        RETURN oGenerator:GenerateHtmlCode ("input":U,
                                            SUBSTITUTE ('type="select" size=1 id="&4" style="position:absolute; left:&1px; top:&2px; width: &3px;"':U,
                                                        ROUND (THIS-OBJECT:Left * HtmlControlGenerator:HorizontalScaling, 0), 
                                                        ROUND (THIS-OBJECT:Top * HtmlControlGenerator:VerticalScaling, 0),
                                                        ROUND (THIS-OBJECT:Width * HtmlControlGenerator:HorizontalScaling, 0),
                                                        THIS-OBJECT:Name),
                                            THIS-OBJECT:DataBindings,
                                            cInner) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the code required to build supporting objects, like 
                 Appearance objects, Editor-Buttons etc.
        Notes:   
        @return The source code for the supporting object definition and instantiation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateSupportingObjectDefinition ():

        DEFINE VARIABLE cItemName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcCode    AS LONGCHAR NO-UNDO.

        oValueListItems:Clear() .    

        {Consultingwerk/foreach.i Infragistics.Win.ValueListItem oItem in this-object:Items}

            ASSIGN cItemName  
&IF NOT PROVERSION BEGINS "10.2" &THEN
         = THIS-OBJECT:ContainerControlGenerator:GenerateObjectName ("valueListItem":U). 
&ELSE        
         = CAST (THIS-OBJECT:ContainerControlGenerator, IContainerControlGenerator):GenerateObjectName ("valueListItem":U). 
&ENDIF     

            IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                ASSIGN lcCode = lcCode + SUBSTITUTE (
                                "        @VisualDesigner.FormMember (NeedsInitialize=~"true~":U).~n" +
                                "        DEFINE VARIABLE &1 AS Infragistics.Win.ValueListItem NO-UNDO.~n" +
                                "        &1 = NEW Infragistics.Win.ValueListItem().~n", cItemName) 
                                .
            ELSE 
                ASSIGN lcCode = lcCode + SUBSTITUTE (
                                "        @VisualDesigner.FormMember (NeedsInitialize=~"true~":u).~n" +
                                "        define variable &1 as Infragistics.Win.ValueListItem no-undo.~n" +
                                "        &1 = new Infragistics.Win.ValueListItem().~n", cItemName) 
                                .
                            
            oValueListItems:Add (cItemName) .            
        END.

        RETURN lcCode . 
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the variable definition ABL statementfor the Control
        Notes:   
        @return The source code for the variable definition section
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateVariableDefinition ():
        
        RETURN SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("    DEFINE PRIVATE VARIABLE &1 AS &2 NO-UNDO.~n":U),
                           THIS-OBJECT:Name,
                           THIS-OBJECT:ControlTypeName) .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds the Items List based on LIST-ITEMS 
        Notes:   
        @param pcListItems The LIST-ITEMS of an ABL COMBO-BOX Widget CHR(1) delimited 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SetListItems (pcListItems AS CHARACTER):
		
		DEFINE VARIABLE iCount AS INTEGER       NO-UNDO .
		DEFINE VARIABLE i      AS INTEGER       NO-UNDO .
		DEFINE VARIABLE oItem  AS ValueListItem NO-UNDO . 
		
		ASSIGN iCount = NUM-ENTRIES (pcListItems, CHR(1)) .
		
		DO i = 1 TO iCount:
		    ASSIGN oItem = NEW ValueListItem (ENTRY (i, pcListItems, CHR (1)),
		                                      ENTRY (i, pcListItems, CHR (1))) . 
		    
		    THIS-OBJECT:Items:Add (oItem) .
		END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds the Items List based on LIST-ITEM-PAIRS 
        Notes:   
        @param pcListItems The LIST-ITEM-PAIRS of an ABL COMBO-BOX Widget CHR(1) delimited 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetListItemPairs (pcListItems AS CHARACTER):
        
        DEFINE VARIABLE iCount AS INTEGER       NO-UNDO .
        DEFINE VARIABLE i      AS INTEGER       NO-UNDO .
        DEFINE VARIABLE oItem  AS ValueListItem NO-UNDO . 
        
        ASSIGN iCount = NUM-ENTRIES (pcListItems, CHR(1)) .
        
        DO i = 1 TO iCount BY 2:
            ASSIGN oItem = NEW ValueListItem (ENTRY (i + 1, pcListItems, CHR (1)),
                                              ENTRY (i,     pcListItems, CHR (1))) . 
            
            THIS-OBJECT:Items:Add (oItem) .
        END.

    END METHOD .
    
END CLASS.
