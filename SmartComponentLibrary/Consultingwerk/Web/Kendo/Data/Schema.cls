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
    File        : Schema
    Purpose     : Represents the schema object of a Kendo UI datasource
    Syntax      : 
    Description : The configuration used to parse the remote service response.
                  http://docs.telerik.com/kendo-ui/api/javascript/data/datasource#configuration-schema
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 12 22:30:59 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Data.Schema 
    INHERITS JsonSerializable: 

    {Consultingwerk/JsonSerializableProperty11.i data CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i model Consultingwerk.Web.Kendo.Data.Model} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Schema class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Schema ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Schema class                                                                     
        Notes:                       
        @param pcData The value for the data property                                                 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Schema (pcData AS CHARACTER):
        THIS-OBJECT ().
        
        THIS-OBJECT:data = pcData .
                        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Schema class                                                                     
        Notes:                       
        @param poModel The reference to the Model                                                 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Schema (poModel AS Model):
        THIS-OBJECT ().
        
        THIS-OBJECT:model = poModel .
                        
    END CONSTRUCTOR.

END CLASS.
