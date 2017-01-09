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
    File        : LoggingLevelEnum
    Purpose     : Enumeration with possible values for the logging level (LOG-MANAGER)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 22 13:39:07 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.*        FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.Framework.Enum.LoggingLevelEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    /*------------------------------------------------------------------------------
        Purpose: Log no entries. This is equivalent to turning logging off.                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i None 0 LoggingLevelEnum}     
    
    /*------------------------------------------------------------------------------
        Purpose:     Log ABL error messages. This includes all error messages and is 
                     unrelated to the entry types specified. Errors are logged at 
                     level 1 (Errors) and higher.
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Errors 1 LoggingLevelEnum}
       
    /*------------------------------------------------------------------------------
        Purpose: Log entry type determines the logged information.                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Basic 2 LoggingLevelEnum}

    /*------------------------------------------------------------------------------
        Purpose: Log entry type determines the logged information.                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Verbose 3 LoggingLevelEnum}

    /*------------------------------------------------------------------------------
        Purpose: Log entry type determines the logged information.                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Extended 4 LoggingLevelEnum}  

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LoggingLevelEnum class                                                                       
        Notes:          
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE LoggingLevelEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the names of the Enum members 
        Notes:   
        @return The CharacterList with the names of the Enum members
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetNames ():
        
        DEFINE VARIABLE oReturn AS CharacterList NO-UNDO . 
        
        RETURN NEW CharacterList ('{&EnumMembers}':U).

    END METHOD .
    
    {Consultingwerk/EnumFromString.i Consultingwerk.Framework.Enum.LoggingLevelEnum}    
    
&ENDIF
    
END {&CLASS} .
