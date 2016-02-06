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
    File        : CompareStrengthEnum
    Purpose     : Enum with possible values for the strength parameter of 
                  the ABL Compare function. Use ToString() to pass as the 
                  required character parameter 
                 
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
USING Progress.Lang.*                       FROM PROPATH .

{&CLASS} Consultingwerk.CompareStrengthEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    /*------------------------------------------------------------------------------
        Purpose: The AVM compares the two strings using the numeric values in the 
                 current code page.                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Raw 1 CompareStrengthEnum}  
    
    /*------------------------------------------------------------------------------
        Purpose: The AVM performs a case-sensitive comparison of the two strings 
                 using the numeric values in either the collation table specified in 
                 collation, or the collation table of the client. If you specify this 
                 strength with an ICU collation, the AVM applies the ICU TERTIARY strength.                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Case-Sensitive 2 CompareStrengthEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The AVM performs a case-insensitive comparison of the two strings 
                 using the numeric values in either the collation table specified in 
                 collation, or the collation table of the client. If you specify this 
                 strength with an ICU collation, the AVM applies the ICU SECONDARY strength.                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Case-Insensitive 3 CompareStrengthEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The AVM converts any lowercase letters in the two strings to uppercase 
                 letters, based on the settings of the Internal Code Page (-cpinternal) 
                 and Case Table (-cpcase) startup parameters, and then performs a raw 
                 comparison of the resulting strings. When neither string contains a 
                 wildcard character, this option behaves the same as the MATCHES operator.                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Caps 4 CompareStrengthEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The AVM compares the base characters in the two strings.                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Primary 5 CompareStrengthEnum}  
    
    /*------------------------------------------------------------------------------
        Purpose: The AVM compares the base characters and any diacritical marks in 
                 the two strings.                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Secondary 6 CompareStrengthEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The AVM performs a case-sensitive comparison of the base characters 
                 and diacritical marks in the two strings.                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Tertiary 7 CompareStrengthEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The AVM performs a case-sensitive comparison of the base characters 
                 and any diacritical marks in the two strings, and distinguishes words 
                 with and without punctuation. ICU uses this strength to distinguish 
                 between Hiragana and Katakana when applied with the ICU-JA (Japanese) 
                 collation. Otherwise, it is the same as TERTIARY.                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Quarternary 8 CompareStrengthEnum}  

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CompareStrengthEnum class                                                                      
        Notes:                                                       
        @param piValue The integer enum value
        @param pcLabel The charcter enum label                            
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE CompareStrengthEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Value = piValue 
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

    {Consultingwerk/EnumFromString.i Consultingwerk.CompareStrengthEnum}    

&ENDIF

END {&CLASS} .
