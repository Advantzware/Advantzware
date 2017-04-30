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
    File        : Model
    Purpose     : Represents the Model object of a Kendo UI datasource schema
    Syntax      : 
    Description : The Model inherits from the ObservableObject and extends 
                  it with the ability to define schema - fields and methods. 
                  The DataSource contains instances of the Model when 
                  the schema.model setting is specified.
                  http://docs.telerik.com/kendo-ui/api/javascript/data/model
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 12 22:30:59 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Web.Kendo.Data.Model 
    INHERITS JsonSerializable: 

    {Consultingwerk/JsonSerializableProperty11.i id CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Model class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Model ():
        SUPER ().
        
        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
                        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Model class                                                                     
        Notes:              
        @param pcId The value of the Model's ID.                                                             
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Model (pcId AS CHARACTER):
        THIS-OBJECT ().
        
        THIS-OBJECT:id = pcId . 
                        
    END CONSTRUCTOR.

END CLASS.
