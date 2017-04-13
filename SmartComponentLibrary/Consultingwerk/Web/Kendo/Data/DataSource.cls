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
    File        : DataSource
    Purpose     : Represents a Kendo UI data source instance
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
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Data.DataSource 
    INHERITS JsonSerializable
    IMPLEMENTS IDataSource: 
    
    {Consultingwerk/JsonSerializableProperty11.i type CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i transport Consultingwerk.Web.Kendo.Data.ITransport} .
    {Consultingwerk/JsonSerializableProperty11.i schema Consultingwerk.Web.Kendo.Data.Schema} .
    {Consultingwerk/JsonSerializableProperty11.i error Consultingwerk.Web.Kendo.Data.ErrorJavaScriptReference} .    
    {Consultingwerk/JsonSerializableProperty11.i pageSize INTEGER "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i serverPaging LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i serverFiltering LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i serverSorting LOGICAL "INIT ?"} .    
    {Consultingwerk/JsonSerializableProperty11.i filter Consultingwerk.Web.Kendo.Data.Filter} .    
    {Consultingwerk/JsonSerializableProperty11.i filters Consultingwerk.Web.Kendo.Data.ListFilter} .    
    {Consultingwerk/JsonSerializableProperty11.i sort Consultingwerk.Web.Kendo.Data.Sort} .    

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DataSource class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DataSource ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:SerializeEmptyLists = FALSE . 
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        
        THIS-OBJECT:filters = NEW ListFilter () . 
        
    END CONSTRUCTOR.

END CLASS.
