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
    File        : TransactionStateEnum
    Purpose     : Enum with possible values for the SmartTransactionState
                  property of the ISmartCommitTarget interface
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 02 01:16:35 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.Enum                   FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .


{&CLASS} Consultingwerk.SmartComponents.Enum.TransactionStateEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    /*------------------------------------------------------------------------------
        Purpose: The commit target is setup up for automatical commit, so the 
                 SmartCommitTarget functionality is currently not supported 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i NoManualCommit 1 TransactionStateEnum}  

    /*------------------------------------------------------------------------------
        Purpose: The commit target has pending changes
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i PendingChanges 2 TransactionStateEnum}

    /*------------------------------------------------------------------------------
        Purpose: The commit target has no pending changes
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i NoPendingChanges 3 TransactionStateEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TransactionStateEnum class                                                                         
        Notes:                                              
        @param piValue The Enum member value
        @param pcLabel The Enum member label     
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE TransactionStateEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Value = piValue 
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

    {Consultingwerk/EnumFromString.i Consultingwerk.SmartComponents.Enum.TransactionStateEnum}    

&ENDIF 

END {&CLASS} .
