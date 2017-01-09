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
    File        : FindRowWhereModeEnum
    Purpose     : Enumeration Describing the Modes of the FindRowWhere Method
                  in SmartBusinessEntityAdapter 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 16 00:04:59 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

{&CLASS} Consultingwerk.SmartComponents.Enum.FindRowWhereModeEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    /*------------------------------------------------------------------------------
        Purpose: Only perform FindRowWhere with the current client data                                                                   
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    {Consultingwerk/EnumMember.i FindOnClient 1 FindRowWhereModeEnum}  

    /*------------------------------------------------------------------------------
        Purpose: Perform FindRowWhere on client first and if now row has been found
                 perform FindRowWhere in the Business Entity/Data Access Object                    
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    {Consultingwerk/EnumMember.i FindOnServer 2 FindRowWhereModeEnum}  

    /*------------------------------------------------------------------------------
        Purpose: Directly perform FindRowWhere on the Server                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    {Consultingwerk/EnumMember.i FindOnServerOnly 3 FindRowWhereModeEnum}  

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FindRowWhereModeEnum class                                                                     
        Notes:  
        @param piValue The integer enum value
        @param pcLabel The charcter enum label                                                                                    
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE FindRowWhereModeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Value = piValue 
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

    {Consultingwerk/EnumFromString.i Consultingwerk.SmartComponents.Enum.FindRowWhereModeEnum}    

&ENDIF

END {&CLASS} .
