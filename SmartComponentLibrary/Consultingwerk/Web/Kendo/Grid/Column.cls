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
    File        : Column
    Purpose     : Represents a column of the Kendo UI grid
    Syntax      : 
    Description : http://docs.telerik.com/kendo-ui/api/javascript/ui/grid#configuration-columns
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 12 22:30:59 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.Grid.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Grid.Column 
    INHERITS JsonSerializable: 

    {Consultingwerk/JsonSerializableProperty.i field CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i command "CHARACTER EXTENT"} .
    {Consultingwerk/JsonSerializableProperty.i title CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i type CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i filterable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty.i sortable LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty.i width CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i attributes Consultingwerk.Web.Kendo.Grid.Attributes} .
	{Consultingwerk/JsonSerializableProperty.i template CHARACTER "INIT ?"} .
	
	/*------------------------------------------------------------------------------
        Purpose: Constructor for the Column class                                                                     
        Notes:                                                                        
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC Column ():
		SUPER ().
		
		THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
		THIS-OBJECT:SerializeNullValues = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                		
	END CONSTRUCTOR.

END CLASS.
