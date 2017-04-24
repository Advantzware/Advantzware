/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Point
    Purpose     : Serializable holder class for a Point structure
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 17 21:38:02 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.Point 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    :

    {Consultingwerk/JsonSerializableProperty.i X INTEGER} .
    {Consultingwerk/JsonSerializableProperty.i Y INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Point class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Point ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Point class                                                                     
        Notes:                            
        @param piX The X Value
        @param piY The Y Value                                            
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Point (piX AS INTEGER, piY AS INTEGER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:X = piX
               THIS-OBJECT:Y = piY . 

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a string representation of the Point instance 
        Notes:   Useful for debugging purposes
        @return The string representation of the Point  
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER ToString ():
        
        RETURN SUBSTITUTE ("Point X: &1 - Y: &2":U,
                           THIS-OBJECT:X, 
                           THIS-OBJECT:Y).

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
    /*------------------------------------------------------------------------------
        Purpose: Returns a System.Drawing.Point with the same coordinates as the 
                 Consultingwerk.Point 
        Notes:   
        @return The System.Drawing.Point with the same coordinates 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Point ToSystemDrawingPoint ():
        
        RETURN NEW System.Drawing.Point (THIS-OBJECT:X, THIS-OBJECT:Y).

    END METHOD .
&ENDIF
END CLASS.
