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
    File        : IDeserializationFactory
    Purpose     : Interface to allow alternative implementations
                  of the static Consultingwerk.Serailizable:DeserializeInstance
                  method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Aug 25 00:01:22 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH.

INTERFACE Consultingwerk.IDeserializationFactory:  
  
    /*------------------------------------------------------------------------------
        Purpose: Deserializes a LONGCHAR representation of an object to a new 
                 instance                                                                      
        Notes:   Supports XML and JSON Serialized objects    
        @param plcSerializable The serialized representation to be deserialized    
        @return The LONGCHAR representation of the ISerializable objects data                                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ISerializable DeserializeInstance (plcSerializable AS LONGCHAR) .  
  
END INTERFACE.