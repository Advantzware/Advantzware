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
    File        : NotImplementedException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwer Ltd.
    Created     : Sun Feb 17 09:48:56 CET 2008
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.NotImplementedException 
    INHERITS Exception 
    {&SERIALIZABLE}: 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the class name that caused this NotImplementedException 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the method name that caused this NotImplementedException 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MethodName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NotImplementedException class
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotImplementedException ():
        SUPER ("This method is not implemented yet."{&TRAN}, 0).
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NotImplementedException class
        Notes:                                                                        
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotImplementedException (poInnerException AS Error):
        
        SUPER (poInnerException,
               "This method is not implemented yet."{&TRAN}, 
               0).
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NotImplementedException class
        Notes:                                                                        
        @param pcMethodName The name of the method that is not implemented
        @param pcClassName The name of the class that caused this NotImplementedException
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotImplementedException (pcMethodName AS CHARACTER,
                                                pcClassName  AS CHARACTER):
        
        SUPER (SUBSTITUTE("The method &1 of class &2 is not implemente yet."{&TRAN}, 
                          pcMethodName, pcClassName), 0).

        ASSIGN THIS-OBJECT:ClassName  = pcClassName
               THIS-OBJECT:MethodName = pcMethodName .
           
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NotImplementedException class
        Notes:   Expects the method and class name as a single CHARACTER parameter, SPACE delimited as returned by PROGRAM-NAME(1)
        @param pcMethodClassName The single parameter combining Method and Class name as returned by PROGRAM-NAME(1) 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotImplementedException (pcMethodClassName AS CHARACTER):
        
        THIS-OBJECT (ENTRY (1, pcMethodClassName, " ":U),
                     (IF NUM-ENTRIES (pcMethodClassName, " ":U) > 1 THEN ENTRY (2, pcMethodClassName, " ":U) ELSE "":U)).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NotImplementedException class
        Notes:                                                                        
        @param poInnerException The reference to the original error
        @param pcMethodName The name of the method that is not implemented
        @param pcClassName The name of the class that caused this NotImplementedException
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotImplementedException (poInnerException AS Error,
                                                pcMethodName AS CHARACTER,
                                                pcClassName  AS CHARACTER):
        
        SUPER (poInnerException,
               SUBSTITUTE("The method &1 of class &2 is not implemente yet."{&TRAN}, 
                          pcMethodName, pcClassName), 
               0).

        ASSIGN THIS-OBJECT:ClassName  = pcClassName
               THIS-OBJECT:MethodName = pcMethodName .
           
    END CONSTRUCTOR.

END CLASS.
