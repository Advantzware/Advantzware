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
    File        : ServiceInterfaceLoggingLevelEnum
    Purpose     : Enumeration for the LoggingLevel property of the 
                  Consultingwerk.OERA.ServiceInterface class
    Syntax      : Enum class
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 14 13:32:15 CEST 2011
    Notes       : Currently only None and Request are supported. Possible
                  future values are: RequestDetail, Response, ResponseDetail
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
USING Consultingwerk.OERA.Enum.*             FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.OERA.Enum.ServiceInterfaceLoggingLevelEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    /*------------------------------------------------------------------------------
        Purpose: Do not log ServiceInterface requests 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i None 1 ServiceInterfaceLoggingLevelEnum}  

    /*------------------------------------------------------------------------------
        Purpose: Log ServiceInterface requests 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Request 2 ServiceInterfaceLoggingLevelEnum}  

    /*------------------------------------------------------------------------------
        Purpose: Log ServiceInterface requests including time it took to execute
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Runtime 3 ServiceInterfaceLoggingLevelEnum}  

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Private constructor of the ServiceInterfaceLoggingLevelEnum class                                                                      
        Notes:   Only accessible for static methods/properties of this class.    
        @param piValue The integer enum value
        @param pcLabel The charcter enum label                            
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ServiceInterfaceLoggingLevelEnum (piValue AS INTEGER, 
                                                          pcLabel AS CHARACTER):
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
       
    {Consultingwerk/EnumFromString.i Consultingwerk.OERA.Enum.ServiceInterfaceLoggingLevelEnum}    

&ENDIF
       
END {&CLASS} .
