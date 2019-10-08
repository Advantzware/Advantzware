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
    File        : HtmlControlGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike
    Created     : Sun Nov 01 13:01:57 GMT+01:00 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Generator.Web2View.* FROM PROPATH .  
USING Progress.Lang.*                                            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Web2View.HtmlControlGenerator 
    IMPLEMENTS IHtmlControlGenerator: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the horizontal scaling factor
        Notes:   Defaults to 1.2
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY HorizontalScaling AS DECIMAL INITIAL 1.3 NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the horizontal scaling factor
        Notes:   Defaults to 1.1
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY VerticalScaling AS DECIMAL INITIAL 1.2 NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML code for the Control
        Notes:   
        @param pcTag The HTML tag for the Control
        @param pcAttributes The attributes for the Control
        @param pcBinding The Control binding attribute
        @param pcInnerHtml The inner html for the control
        @return The HTML code for the Control 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateHtmlCode (pcTag AS CHARACTER, 
                                              pcAttributes AS CHARACTER, 
                                              pcBinding AS CHARACTER, 
                                              pcInnerHtml AS CHARACTER):
        
        IF pcBinding > "":U THEN 
            RETURN SUBSTITUTE ('        <&1 &2 ng-model="<%= get-field (~'scope~':U) %>.&3">&4</&1>':U,
                               pcTag, 
                               pcAttributes,
                               pcBinding,
                               pcInnerHtml) .
        ELSE 
            RETURN SUBSTITUTE ('        <&1 &2>&4</&1>':U,
                               pcTag, 
                               pcAttributes,
                               "":U,
                               pcInnerHtml) .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML code for the Control
        Notes:   
        @param pcTag The HTML tag for the Control
        @param pcAttributes The attributes for the Control
        @param pcBinding The Control binding attribute
        @return The HTML code for the Control 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateHtmlCode (pcTag AS CHARACTER, 
                                              pcAttributes AS CHARACTER, 
                                              pcBinding AS CHARACTER):
        
        RETURN THIS-OBJECT:GenerateHtmlCode (pcTag, pcAttributes, pcBinding, "":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML code for the Control
        Notes:   
        @param pcTag The HTML tag for the Control
        @param pcAttributes The attributes for the Control
        @return The HTML code for the Control 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateHtmlCode (pcTag AS CHARACTER, 
                                              pcAttributes AS CHARACTER):
        
        RETURN THIS-OBJECT:GenerateHtmlCode (pcTag, pcAttributes, "":U, "":U) .

    END METHOD .

END CLASS.
