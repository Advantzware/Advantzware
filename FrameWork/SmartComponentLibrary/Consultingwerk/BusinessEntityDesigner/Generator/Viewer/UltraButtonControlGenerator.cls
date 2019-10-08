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
    File        : UltraButtonControlGenerator
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

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.UltraButtonControlGenerator
    INHERITS ControlGenerator
    IMPLEMENTS IControlGenerator, IHasWidthAndHeight, IHasClickEvent, ISupportsWeb2ViewGeneration:

    /*------------------------------------------------------------------------------
        Purpose: Code block for the Click (CHOOSE) event
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClickTriggerBlock AS LONGCHAR NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Container Control 
        Notes:   Leave Empty if the Control is not part of a Container such as a 
                 GroupBox
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContainerName AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Retusn the TypeName for to be used for this UltraEditorControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ControlTypeName AS CHARACTER NO-UNDO INITIAL "Infragistics.Win.Misc.UltraButton":U
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the default data bindings of the Control
        Notes:   
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
        Purpose: Logical value indicating if the Control supports the ISupportInitialize 
                 interface
        Notes:   Used for generating the BeginInit and EndInit method calls
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SupportInitialize AS LOGICAL NO-UNDO INITIAL FALSE
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
    DEFINE PUBLIC STATIC PROPERTY TemplateFileName AS CHARACTER NO-UNDO INITIAL "UI/ultrabutton.template":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Text of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Text AS CHARACTER NO-UNDO 
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
        Purpose: The Width of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Width AS INTEGER NO-UNDO INITIAL 150
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Generats the source code that assigns the property values for this Control
        Notes:   
        @return The source code for the code block that assigns the property values for this Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateAssignPropertiesBlock ():
        
        DEFINE VARIABLE lcCode AS LONGCHAR NO-UNDO.

        FILE-INFORMATION:FILE-NAME = TemplateHelper:Find (UltraButtonControlGenerator:TemplateFileName) .

        COPY-LOB FROM FILE FILE-INFORMATION:FULL-PATHNAME TO lcCode . 
        
        ASSIGN lcCode = SUBSTITUTE (lcCode, 
                                    THIS-OBJECT:Name,
                                    "":U /* no data bindings for button */,
                                    "":U /* no appearance object */,
                                    THIS-OBJECT:Left,
                                    THIS-OBJECT:Top,
                                    THIS-OBJECT:TabOrder,
                                    (IF THIS-OBJECT:Text > "":U THEN THIS-OBJECT:Text ELSE THIS-OBJECT:Name)) + "~n":U + 
                                    (IF THIS-OBJECT:Tag > "":U THEN SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:&1:Tag = &2:U.~n":U, ":Tag ":U),
                                                                                THIS-OBJECT:Name, 
                                                                                QUOTER(THIS-OBJECT:Tag)) ELSE "":U)
                                    . 

        ASSIGN lcCode = REPLACE (REPLACE (lcCode, 
                                          "@Width@":U, 
                                          STRING (THIS-OBJECT:Width)), 
                                 "@Height@":U,
                                 STRING (THIS-OBJECT:Height)) .        

        IF THIS-OBJECT:ClickTriggerBlock > "":U THEN DO:
            ASSIGN lcCode = lcCode + SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:&1:Click:Subscribe(&1_Click).~n":U, ":Click:":U + CHR(1) + "_Click)":U),
                                                 THIS-OBJECT:Name) .

            THIS-OBJECT:InsertEventHandler (SUBSTITUTE ("&1_Click":U, THIS-OBJECT:Name), 
                                            "Click":U,
                                            THIS-OBJECT:Name,
                                            "System.EventArgs":U, 
                                            THIS-OBJECT:ClickTriggerBlock) .                                                 
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
                                            SUBSTITUTE ('type="button" id="&4" style="position:absolute; left:&1px; top:&2px; width: &3px;"':U,
                                                        ROUND (THIS-OBJECT:Left * HtmlControlGenerator:HorizontalScaling, 0), 
                                                        ROUND (THIS-OBJECT:Top * HtmlControlGenerator:VerticalScaling, 0),
                                                        ROUND (THIS-OBJECT:Width * HtmlControlGenerator:HorizontalScaling, 0),
                                                        THIS-OBJECT:Name)) .

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
