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
    File        : SmartComboEditorControlGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 08:12:34 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.*                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*        FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .  
USING Consultingwerk.Util.*                                    FROM PROPATH . 
USING Progress.Lang.*                                          FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.SmartComboEditorControlGenerator
    INHERITS ControlGenerator
    IMPLEMENTS IControlGenerator, IHasWidthAndHeight, IHasValidatingEvent, IHasValueChangedEvent:

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
    DEFINE PUBLIC PROPERTY ControlTypeName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartComponents.Implementation.SmartComboEditor":U
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
    DEFINE PUBLIC PROPERTY SupportInitialize AS LOGICAL NO-UNDO INIT TRUE 
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
    DEFINE PUBLIC STATIC PROPERTY TemplateFileName AS CHARACTER NO-UNDO INIT "UI/smartcomboeditor.template":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template File to be used by this class
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TemplateFileNameUnbound AS CHARACTER NO-UNDO INIT "UI/smartcomboeditor_unbound.template":U
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
    DEFINE PUBLIC PROPERTY Width AS INTEGER NO-UNDO INIT 150 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Generats the source code that assigns the property values for this Control
        Notes:   
        @return The source code for the code block that assigns the property values for this Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateAssignPropertiesBlock ():
        
        DEFINE VARIABLE lcCode AS LONGCHAR NO-UNDO.

        IF THIS-OBJECT:DataBindings > "":U THEN 
            FILE-INFO:FILE-NAME = TemplateHelper:Find (SmartComboEditorControlGenerator:TemplateFileName) .
        ELSE 
            FILE-INFO:FILE-NAME = TemplateHelper:Find (SmartComboEditorControlGenerator:TemplateFileNameUnbound) .

        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO lcCode . 
        
        ASSIGN lcCode = SUBSTITUTE (lcCode, 
                                    THIS-OBJECT:Name,
                                    THIS-OBJECT:DataBindings,
                                    "":U /* no appearance object */,
                                    THIS-OBJECT:Left,
                                    THIS-OBJECT:Top,
                                    THIS-OBJECT:TabOrder) + "~n" + 
                                    (IF THIS-OBJECT:Tag > "":U THEN SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:&1:Tag = &2:U.~n":U, ":Tag":U),
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
