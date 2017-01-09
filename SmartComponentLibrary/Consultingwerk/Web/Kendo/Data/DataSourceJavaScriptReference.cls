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
    File        : DataSourceJavaScriptReference
    Purpose     : Represents a Kendo UI data source instance as a JavaScript 
                  reference
    Syntax      : 
    Description : The DataSource plays a central role in practically all 
                  web applications built with Kendo UI. It is an abstraction 
                  for using local data (arrays of JavaScript objects) or 
                  remote data (web services returning JSON, JSONP, OData or XML).
                  http://docs.telerik.com/kendo-ui/framework/datasource/overview
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 12 22:30:59 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.*      FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Data.DataSourceJavaScriptReference 
    INHERITS JavaScriptReference
    IMPLEMENTS IDataSource: 
        
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the JavaScript source code which is represented by
                 this node
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i source CHARACTER} .            
	
    /*------------------------------------------------------------------------------
        Purpose: Not used by this type
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i type CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Not used by this type
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i transport Consultingwerk.Web.Kendo.Data.ITransport} .

    /*------------------------------------------------------------------------------
        Purpose: Not used by this type
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i schema Consultingwerk.Web.Kendo.Data.Schema} .

    /*------------------------------------------------------------------------------
        Purpose: Not used by this type
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i error Consultingwerk.Web.Kendo.Data.ErrorJavaScriptReference} .    

    /*------------------------------------------------------------------------------
        Purpose: Not used by this type
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i pageSize INTEGER "INIT ?"} .

    /*------------------------------------------------------------------------------
        Purpose: Not used by this type
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i serverPaging LOGICAL "INIT ?"} .

    /*------------------------------------------------------------------------------
        Purpose: Not used by this type
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i serverFiltering LOGICAL "INIT ?"} .

    /*------------------------------------------------------------------------------
        Purpose: Not used by this type
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i serverSorting LOGICAL "INIT ?"} .    

	/*------------------------------------------------------------------------------
        Purpose: Constructor for the DataSourceJavaScriptReference class                                                                     
        Notes:                                                                        
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DataSourceJavaScriptReference ():
		SUPER ().
		
		THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DataSourceJavaScriptReference class                                                                     
        Notes:
        @param pcSource The source code reference                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DataSourceJavaScriptReference (pcSource AS CHARACTER):
        THIS-OBJECT ().

        THIS-OBJECT:source = pcSource .
        
    END CONSTRUCTOR .

END CLASS.
