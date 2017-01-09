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
    File        : RowStateEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 12 15:00:26 CEST 2011
    Notes       : ROW-UNMODIFIED  0  The row was not modified.  
                  ROW-DELETED  1  The row was deleted.  
                  ROW-MODIFIED  2  The row was modified.  
                  ROW-CREATED  3  The row was created.  
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Enum                    FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.OERA.Enum.*             FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.OERA.Enum.RowStateEnum 
    INHERITS Enum: 

    /*------------------------------------------------------------------------------
        Purpose: The row was not modified 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Unknown ? RowStateEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The row was not modified 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i RowUnmodified 0 RowStateEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The row was deleted 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i RowDeleted 1 RowStateEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The row was modified 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i RowModified 2 RowStateEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The row was created 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i RowCreated 3 RowStateEnum}  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RowStateEnum class                                                                      
        Notes:                                            
        @param piValue The integer enum value
        @param pcLabel The charcter enum label                            
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE RowStateEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Value = piValue 
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Returns the representation of the RowStateEnum value which is the
	             same as the ABL ROW-STATE compiler constants
	    Notes:
	    @return The representation of the RowStateEnum value which is the same as the ABL ROW-STATE compiler constants
	------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER ToAblConstant ():
		
        CASE THIS-OBJECT:Value:
            WHEN 0 THEN 
                RETURN "ROW-UNMODIFIED":U . 
            WHEN 1 THEN 
                RETURN "ROW-DELETED":U . 
            WHEN 2 THEN 
                RETURN "ROW-MODIFIED":U . 
            WHEN 3 THEN 
                RETURN "ROW-CREATED":U . 
            WHEN ? THEN 
                RETURN "UNKNOWN":U . 
            OTHERWISE 
                RETURN "UNDEFINDED":U .
        END CASE . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Integer value of the Enum member                                                                     
        Notes:
        @return The INTEGER Value matching the Enum member                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER ToInteger ():
        
        RETURN THIS-OBJECT:Value . 
    
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the RowStateEnum member matching the integer ROW-STATE value                                                                       
        Notes:                                                                       
        @param piValue The integer ROW-STATE value
        @return The mathing RowStateEnum member 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC RowStateEnum FromInteger (piValue AS INTEGER):
        
        CASE piValue:
            WHEN 0 THEN RETURN RowStateEnum:RowUnmodified .
            WHEN 1 THEN RETURN RowStateEnum:RowDeleted .
            WHEN 2 THEN RETURN RowStateEnum:RowModified .
            WHEN 3 THEN RETURN RowStateEnum:RowCreated .
            WHEN ? THEN RETURN RowStateEnum:Unknown . 
            
            OTHERWISE UNDO, THROW NEW Consultingwerk.Exceptions.InvalidValueException (STRING (piValue)) .
        END CASE .
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the names of the Enum members 
        Notes:   
        @return The CharacterList with the names of the Enum members
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetNames ():
        
        DEFINE VARIABLE oReturn AS CharacterList NO-UNDO . 
        
        RETURN NEW CharacterList ('{&EnumMembers}':U).

    END METHOD .

    {Consultingwerk/EnumFromString.i Consultingwerk.OERA.Enum.RowStateEnum}    

END CLASS.
