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
    File        : NameValuePair
    Purpose     : A general purpose NameValuePair
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 10 19:06:29 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*  FROM PROPATH . 
USING Progress.Lang.*   FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.NameValuePair
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the NameValuePair                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Name CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Value of the NameValuePair                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Value CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NameValuePair class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NameValuePair ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NameValuePair class                                                                        
        Notes:                                          
        @param pcName The name value
        @param pcValue The value                               
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NameValuePair (pcName AS CHARACTER,
                                      pcValue AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:Name  = pcName
               THIS-OBJECT:Value = pcValue . 
        
    END CONSTRUCTOR.

END CLASS.
