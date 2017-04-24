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
    File        : UltraNumericEditorControlGenerator
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

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.UltraNumericEditorControlGenerator
    INHERITS ControlGenerator
    IMPLEMENTS IControlGenerator, IHasWidthAndHeight, IHasValidatingEvent, IHasValueChangedEvent, ISupportsWeb2ViewGeneration:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Appearance Variable used by this Label
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY AppearanceVariableName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Container Control 
        Notes:   Leave Empty if the Control is not part of a Container such as a 
                 GroupBox
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContainerName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Retusn the TypeName for to be used for this UltraEditorControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ControlTypeName AS CHARACTER NO-UNDO INITIAL "Infragistics.Win.UltraWinEditors.UltraNumericEditor":U
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
        Purpose: The Height of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Height AS INTEGER NO-UNDO INITIAL 21
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
        Purpose: Get and sets the NumericType for this Control 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NumericType AS Infragistics.Win.UltraWinEditors.NumericType NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Logical value indicating if the Control supports the ISupportInitialize 
                 interface
        Notes:   Used for generating the BeginInit and EndInit method calls
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SupportInitialize AS LOGICAL NO-UNDO INITIAL TRUE
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
    DEFINE PUBLIC STATIC PROPERTY TemplateFileName AS CHARACTER NO-UNDO INITIAL "UI/ultranumericeditor.template":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template File to be used by this class
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TemplateFileNameUnbound AS CHARACTER NO-UNDO INITIAL "UI/ultranumericeditor_unbound.template":U
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
    DEFINE PUBLIC PROPERTY Width AS INTEGER NO-UNDO INITIAL 80
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
        Purpose: Constructor for the UltraNumericEditorControlGenerator class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraNumericEditorControlGenerator ():
        SUPER ().
    
        THIS-OBJECT:NumericType = Infragistics.Win.UltraWinEditors.NumericType:Integer .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Generats the source code that assigns the property values for this Control
        Notes:   
        @return The source code for the code block that assigns the property values for this Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateAssignPropertiesBlock ():
        
        DEFINE VARIABLE lcCode      AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cProperties AS CHARACTER NO-UNDO .

        IF THIS-OBJECT:DataBindings > "":U THEN 
            FILE-INFORMATION:FILE-NAME = TemplateHelper:Find (UltraNumericEditorControlGenerator:TemplateFileName) .
        ELSE 
            FILE-INFORMATION:FILE-NAME = TemplateHelper:Find (UltraNumericEditorControlGenerator:TemplateFileNameUnbound) .

        COPY-LOB FROM FILE FILE-INFORMATION:FULL-PATHNAME TO lcCode . 
        
        IF EnumHelper:AreEqual (THIS-OBJECT:NumericType,
                                Infragistics.Win.UltraWinEditors.NumericType:Double) OR 
           EnumHelper:AreEqual (THIS-OBJECT:NumericType,
                                Infragistics.Win.UltraWinEditors.NumericType:Decimal) THEN 
                                
            ASSIGN cProperties = SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                 "~n        THIS-OBJECT:&1:TabNavigation = Infragistics.Win.UltraWinMaskedEdit.MaskedEditTabNavigation:NextControl.":U
                                             ELSE   
                                                 "~n        this-object:&1:TabNavigation = Infragistics.Win.UltraWinMaskedEdit.MaskedEditTabNavigation:NextControl.":U,
                                             THIS-OBJECT:Name) .                                
        
        ASSIGN lcCode = SUBSTITUTE (lcCode, 
                                    THIS-OBJECT:Name,
                                    THIS-OBJECT:DataBindings,
                                    "":U /* no appearance object */,
                                    THIS-OBJECT:Left,
                                    THIS-OBJECT:Top,
                                    THIS-OBJECT:TabOrder,
                                    THIS-OBJECT:NumericType:ToString(),
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
        
        oGenerator = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Generator.Web2View.IHtmlControlGenerator
                                                   "NEW Consultingwerk.BusinessEntityDesigner.Generator.Web2View.HtmlControlGenerator()"} .
        
        RETURN oGenerator:GenerateHtmlCode ("input":U,
                                            SUBSTITUTE ('type="text" id="&4" style="position:absolute; left:&1px; top:&2px; width: &3px;"':U,
                                                        ROUND (THIS-OBJECT:Left * HtmlControlGenerator:HorizontalScaling, 0), 
                                                        ROUND (THIS-OBJECT:Top * HtmlControlGenerator:VerticalScaling, 0),
                                                        ROUND (THIS-OBJECT:Width * HtmlControlGenerator:HorizontalScaling, 0),
                                                        THIS-OBJECT:Name),
                                            THIS-OBJECT:DataBindings) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the code required to build supporting objects, like 
                 Appearance objects, Editor-Buttons etc.
        Notes:   
        @return The source code for the supporting object definition and instantiation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateSupportingObjectDefinition ():

        RETURN "":U . 
        
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

END CLASS.
