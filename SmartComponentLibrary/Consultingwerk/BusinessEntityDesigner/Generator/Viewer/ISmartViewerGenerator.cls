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
    File        : ISmartViewerGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 08:28:51 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.*        FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .  
USING Progress.Lang.*                                          FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.Viewer.ISmartViewerGenerator
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
        Purpose: Gets and sets if the generated User Control should contain a 
                 Binding Source or not
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CreateBindingSource AS LOGICAL NO-UNDO  
    GET.
    SET. 

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
        Purpose: Gets and sets the Class Name for the generated SmartViewerControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PackageName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Generates the source code for the SmartViewerControl 
        Notes:   
        @return The file name of the generated source 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Generate ().

END INTERFACE.
