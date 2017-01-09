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
    File        : GetValueListEntryByKeyParameter
    Purpose     : Parameter object for the EntryForKey method of the ValueListBusinessEntity
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 07 17:53:03 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                     FROM PROPATH .
USING Consultingwerk.SmartFramework.*      FROM PROPATH .  
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.GetValueListEntryByKeyParameter
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    :
    
    /*------------------------------------------------------------------------------
        Purpose: Returns if the Root Menu Entry should also te retrieved 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i ValueListKey CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Number of Levels deep the Menu is requested 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i ValueListEntryKey CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetValueListEntryByKeyParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC GetValueListEntryByKeyParameter ():
		SUPER ().
		
		THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetValueListEntryByKeyParameter class
        Notes:   
        @param pcValueListKey The value for the ValueListKey property
        @param pcValueListEntryKey The value for the ValueListEntryKey property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetValueListEntryByKeyParameter (pcValueListKey AS CHARACTER,
                                                        pcValueListEntryKey AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:ValueListKey      = pcValueListKey 
               THIS-OBJECT:ValueListEntryKey = pcValueListEntryKey .
        
    END CONSTRUCTOR.

END CLASS.
