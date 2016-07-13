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
    File        : Transport
    Purpose     : Represents the transport properties of the Kendo UI 
                  datasource
    Syntax      : 
    Description : The configuration used to load and save the data items. 
                  A data source is remote or local based on the way of it 
                  retrieves data items.
                  http://docs.telerik.com/kendo-ui/api/javascript/data/datasource#configuration-transport
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 12 22:30:59 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Data.Transport 
    INHERITS JsonSerializable
    IMPLEMENTS ITransport: 

    {Consultingwerk/JsonSerializableProperty.i read CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i dataType CHARACTER} .
	
	/*------------------------------------------------------------------------------
        Purpose: Constructor for the Transport class                                                                     
        Notes:                                                                        
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC Transport ():
		SUPER ().
		
		THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                		
	END CONSTRUCTOR.

END CLASS.
