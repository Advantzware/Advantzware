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
    File        : RibbonToolSize
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 05 21:12:08 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*             FROM PROPATH .
USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

{&CLASS} Consultingwerk.Web.Widgets.RibbonToolSize 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    
     
    /* Static members */  
    {Consultingwerk/EnumMember.i ImageOnly 1 RibbonToolSize}  
    {Consultingwerk/EnumMember.i Normal 2 RibbonToolSize}
    {Consultingwerk/EnumMember.i Large 2 RibbonToolSize}  

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN
     
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RibbonToolSize class                                                                      
        Notes:                                                       
        @param piValue The integer enum value
        @param pcLabel The charcter enum label                            
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE RibbonToolSize (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Value = piValue 
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

    {Consultingwerk/EnumFromString.i Consultingwerk.Web.Widgets.RibbonToolSize}    

&ENDIF 

END {&CLASS} .
