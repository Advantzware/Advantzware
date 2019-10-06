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
    File        : IJsonSerializable
    Purpose     : Specialization of the ISerializable Interface
                  Describes methods required for Json Serialization
                  The Interface is required for nesting Json Objects during 
                  the serialization (non-primitive properties)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 29 23:11:10 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.*            FROM PROPATH .
USING Progress.Json.ObjectModel.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

INTERFACE Consultingwerk.IJsonSerializable 
    INHERITS ISerializable:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns a JsonObject representation of the state of the class instance                                                                      
        Notes:   Returns the classname together with the Json serialization of the 
                 object
        @return The Progress.Json.ObjectModel.JsonObject representation of the ISerializable objects data                                                                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Json.ObjectModel.JsonObject ToJsonObject () .  
  
    /*------------------------------------------------------------------------------
        Purpose: Initializes the object instance based on the passed in JsonObject
                 representation of the object instance                                                                      
        Notes:               
        @param poJsonObject The stored JsonObject representation of the objects data                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeserializeFromJsonObject (poJsonObject AS JsonObject) .
    
END INTERFACE.
