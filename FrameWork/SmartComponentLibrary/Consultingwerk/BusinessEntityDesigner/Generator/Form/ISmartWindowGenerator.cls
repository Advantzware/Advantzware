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
    File        : ISmartWindowGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 08:28:51 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Collections.*                 FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.Generator.*      FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Generator.Form.* FROM PROPATH .  
USING Progress.Lang.*                                        FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.Form.ISmartWindowGenerator
&IF NOT PROVERSION BEGINS "10.2" &THEN
    INHERITS IContainerControlGenerator
&ENDIF        
    :  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Columns in the BindingSource Schema
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSourceColumns AS "System.Collections.Generic.List<Progress.Data.ColumnPropDesc>":U NO-UNDO 
    GET.  
  
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Class Name for the generated SmartViewerControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of IControlGenerators used by this SmartViewerGenerator
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ControlGenerators AS ListIControlGenerator NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Dataset Controller used
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetControllerName AS CHARACTER NO-UNDO 
    GET . 
    SET .  
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityJoin property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityJoin AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityName property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityTable property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityView property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityView AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Form Title (Text Property of the generated Form)
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FormTitle AS CHARACTER NO-UNDO 
    GET . 
    SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of Grid Columns
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY GridColumns AS CharacterList NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Class Name for the generated SmartViewerControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PackageName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template to be used for generation of 
                 the .resx file
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResxTemplateName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template to be used for generation  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TemplateName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the SmartViewer to be used in the SmartWindowForm
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ViewerName AS CHARACTER NO-UNDO 
    GET . 
    SET .
    
    /*------------------------------------------------------------------------------
        Purpose: Generates the source code for the SmartViewerControl 
        Notes:   
        @return The file name of the generated source 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Generate ().

END INTERFACE.
