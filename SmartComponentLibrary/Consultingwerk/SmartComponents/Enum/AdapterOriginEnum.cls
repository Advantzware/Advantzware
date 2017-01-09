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
    File        : AdapterOriginEnum
    Purpose     : Enum with possible values for the origin of the 
                  SmartBusinessEntityAdapter used by the SmartBusinessEntityLookup
                  (used in the AfterInitializeBusinessEntityAdapter event) 
                 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 11 21:13:31 CEST 2011
    Notes       : Used for the AfterInitializeBusinessEntityAdapterEventArgs
                  class
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

{&CLASS} Consultingwerk.SmartComponents.Enum.AdapterOriginEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    /*------------------------------------------------------------------------------
        Purpose: The Adapter instance of the LookupControl is initialized                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i LookupControl 1 AdapterOriginEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The Adapter instance of the LookupDialog is initialized                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i LookupDialog 2 AdapterOriginEnum}  

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AdapterOriginEnum class                                                                      
        Notes:                                                       
        @param piValue The integer enum value
        @param pcLabel The charcter enum label                            
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE AdapterOriginEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Value = piValue 
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

    {Consultingwerk/EnumFromString.i Consultingwerk.SmartComponents.Enum.AdapterOriginEnum}    

&ENDIF

END {&CLASS} .
