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
    File        : UnableToRejectTransactionException
    Purpose     : Exception thrown when the SmartBusinessEntityAdapter 
                  cannot reject the transaction because one TableIOTarget
                  remained in update mode after calling SaveChanges
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 28 05:22:30 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*       FROM PROPATH .  
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .  
USING Consultingwerk.SmartComponents.Resources.*  FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.UnableToRejectTransactionException 
    INHERITS Exception
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference of the TableIOTarget that caused the issue
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableIOTarget AS ISmartTableIOTarget NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the class name of the TableIOTarget that caused the issue
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableIOTargetName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRejectTransactionException class
        Notes:                          
        @param pcTableIOTargetName The name of the TableIOTarget that caused the issue
        @param poTableIOTarget The reference to the TableIOTarget that cause the issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnableToRejectTransactionException (pcTableIOTargetName AS CHARACTER,
                                                           poTableIOTarget AS ISmartTableIOTarget):
        SUPER (SUBSTITUTE ((IF pcTableIOTargetName > "":U THEN "&1~n&2":U ELSE "&1":U),
                           SmartComponentLibraryCustomizer:SmartBusinessEntityAdapter_UnableToRejectTransaction,
                           pcTableIOTargetName),
              0).
        
        ASSIGN THIS-OBJECT:TableIOTarget     = poTableIOTarget
               THIS-OBJECT:TableIOTargetName = pcTableIOTargetName . 
        
    END CONSTRUCTOR.

END CLASS.
