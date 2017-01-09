/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IControlGeneratorFactory
    Purpose     : Service Interface for Factory for ControlGenerator instances
                  used by the SmartViewerBuilder
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 04 22:57:43 CEST 2014
    Notes       : This is an alternative serivce interface to 
                  IControlGeneratorFactoryExt. Only one of these two interfaces
                  should be registered in the ServiceContainer (services.xml)
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.*        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .
USING Progress.Lang.*                                          FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.Viewer.IControlGeneratorFactory:  
  
	/*------------------------------------------------------------------------------
	    Purpose: Generates and returns a ControlGenerator instance for a binding source field
	    Notes:   The SmartViewerBuilder will assign the following properties to the 
	             ControlGenerator instance created from this factory: 
	             DataBindings, Left, Name, ContainerControlGenerator, TabOrder, Top 
	    @param poViewerBuilder The reference to the viewer builder instance
	    @param pcColumnName The name of the current BindingSource Column
	    @param pcFieldName The name (including buffer) of the current business entity field
	    @param pcCustomControlName The name of the custom control as specified in the field properties in the Business Entity Designer
	    @return The IControlGenerator instance for the current field or ? when the default control generator should be used
	------------------------------------------------------------------------------*/
	METHOD PUBLIC IControlGenerator GetControlGenerator (poViewerBuilder AS SmartViewerBuilder, 
                                                         pcColumnName AS CHARACTER, 
                                                         pcFieldName AS CHARACTER,
                                                         pcCustomControlName AS CHARACTER).

END INTERFACE.
