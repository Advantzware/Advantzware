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
    File        : UltraLabelControlGenerator
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

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.UltraLabelControlGenerator
    IMPLEMENTS IControlGenerator, ISupportsWeb2ViewGeneration:


    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if a Colon should be appended to the Text property 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY AddColonToText AS LOGICAL INITIAL TRUE  
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Appearance Variable used by this Label
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY AppearanceVariableName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the SmartViewerGenerator used by the
                 IControlGenerator implementation
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContainerControlGenerator AS &IF PROVERSION BEGINS "10.2":U &THEN Progress.Lang.Object &ELSE IContainerControlGenerator &ENDIF NO-UNDO 
    GET.
    SET (arg AS &IF PROVERSION BEGINS "10.2":U &THEN Progress.Lang.Object &ELSE IContainerControlGenerator &ENDIF):
        THIS-OBJECT:ContainerControlGenerator = arg .
        
&IF NOT PROVERSION BEGINS "10.2" &THEN
        THIS-OBJECT:AppearanceVariableName = arg:GenerateObjectName ("appearance":U). 
&ELSE        
        IF TYPE-OF (arg, IContainerControlGenerator) THEN 
            THIS-OBJECT:AppearanceVariableName = CAST (arg, IContainerControlGenerator):GenerateObjectName ("appearance":U). 
&ENDIF        
END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Container Control 
        Notes:   Leave Empty if the Control is not part of a Container such as a 
                 GroupBox
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContainerName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Retusn the TypeName for to be used for this UltraLabel Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ControlTypeName AS CHARACTER NO-UNDO INIT "Infragistics.Win.Misc.UltraLabel":U
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
    DEFINE PUBLIC STATIC PROPERTY TemplateFileName AS CHARACTER NO-UNDO INIT "UI/ultralabel.template":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Text Property (Label) for this Control
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
        Purpose: Generats the source code that assigns the property values for this Control
        Notes:   
        @return The source code for the code block that assigns the property values for this Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateAssignPropertiesBlock ():
        
        DEFINE VARIABLE lcCode AS LONGCHAR NO-UNDO.

        FILE-INFO:FILE-NAME = TemplateHelper:Find (UltraLabelControlGenerator:TemplateFileName) .

        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO lcCode . 
        
        ASSIGN lcCode = SUBSTITUTE (lcCode, 
                                    THIS-OBJECT:Name,
                                    "":U /*THIS-OBJECT:DataBindings*/,
                                    THIS-OBJECT:AppearanceVariableName,
                                    THIS-OBJECT:Left,
                                    THIS-OBJECT:Top,
                                    THIS-OBJECT:TabOrder,
                                    THIS-OBJECT:Text + (IF THIS-OBJECT:AddColonToText THEN ":":U ELSE "":U)) + "~n" + 
                                    (IF THIS-OBJECT:Tag > "":U THEN SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:&1:Tag = &2:U.~n":U, ":Tag ":U),
                                                                                THIS-OBJECT:Name, 
                                                                                QUOTER(THIS-OBJECT:Tag)) ELSE "":U)
                                    . 

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
		
		RETURN oGenerator:GenerateHtmlCode ("label":U,
		                                    SUBSTITUTE ('style="position:absolute; left:&1px; top:&2px;"':U,
                                                        ROUND (THIS-OBJECT:Left * HtmlControlGenerator:HorizontalScaling, 0), 
                                                        ROUND (THIS-OBJECT:Top * HtmlControlGenerator:VerticalScaling, 0)),
		                                    "":U,
		                                    THIS-OBJECT:Text) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the code required to build supporting objects, like 
                 Appearance objects, Editor-Buttons etc.
        Notes:   
        @return The source code for the supporting object definition and instantiation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateSupportingObjectDefinition ():

        IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN 
            RETURN SUBSTITUTE ("        @VisualDesigner.FormMember (NeedsInitialize=~"true~":U).~n" +
                               "        DEFINE VARIABLE &1 AS Infragistics.Win.Appearance NO-UNDO.~n" +
                               "        &1 = NEW Infragistics.Win.Appearance().~n",
                               THIS-OBJECT:AppearanceVariableName) .
        ELSE 
            RETURN SUBSTITUTE ("        @VisualDesigner.FormMember (NeedsInitialize=~"true~":u).~n" +
                               "        define variable &1 as Infragistics.Win.Appearance no-undo.~n" +
                               "        &1 = new Infragistics.Win.Appearance().~n",
                               THIS-OBJECT:AppearanceVariableName) .
        
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
