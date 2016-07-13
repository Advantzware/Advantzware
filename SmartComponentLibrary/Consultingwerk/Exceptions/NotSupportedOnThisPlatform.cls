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
    File        : NotSupportedOnThisPlatform
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 23 09:55:20 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.NotSupportedOnThisPlatform
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the class name that caused this NotSupportedOnThisPlatform 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the method name that caused this NotSupportedOnThisPlatform 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MethodName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NotSupportedOnThisPlatformException class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotSupportedOnThisPlatform ():
        
        SUPER ("This method is not supported on this platform."{&TRAN}, 0).
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NotSupportedOnThisPlatformException class                                                                       
        Notes:
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotSupportedOnThisPlatform (poInnerException AS Progress.Lang.Error):

        SUPER (poInnerException,
               "This method is not supported on this platform."{&TRAN}, 
               0).
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NotSupportedOnThisPlatformException class                                                                     
        Notes:
        @param pcMethodName The name of the method that is not implemented
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotSupportedOnThisPlatform (pcMethodName AS CHARACTER):
        
        SUPER (SUBSTITUTE("The method &1 is not supported on this platform."{&TRAN}, 
                          pcMethodName), 0).

        ASSIGN THIS-OBJECT:MethodName = pcMethodName .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NotSupportedOnThisPlatformException class                                                                     
        Notes:
        @param poInnerException The reference to the original error
        @param pcMethodName The name of the method that is not implemented
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotSupportedOnThisPlatform (poInnerException AS Progress.Lang.Error, 
                                                   pcMethodName AS CHARACTER):
                                                       
        SUPER (poInnerException,
               SUBSTITUTE("The method &1 is not supported on this platform."{&TRAN}, 
                          pcMethodName), 
               0).

        ASSIGN THIS-OBJECT:MethodName = pcMethodName .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NotSupportedOnThisPlatformException class                                                                      
        Notes:                                                                        
        @param pcMethodName The name of the method that is not implemented
        @param pcClassName The name of the class that caused this NotSupportedOnThisPlatformException
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotSupportedOnThisPlatform (pcMethodName AS CHARACTER, 
                                                   pcClassName AS CHARACTER):
                                                       
        SUPER (SUBSTITUTE("The method &1 of class &2 is not supported on this platform."{&TRAN}, 
                          pcMethodName, pcClassName), 0).

        ASSIGN THIS-OBJECT:ClassName  = pcClassName
               THIS-OBJECT:MethodName = pcMethodName .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NotSupportedOnThisPlatformException class                                                                      
        Notes:                                                                        
        @param poInnerException The reference to the original error
        @param pcMethodName The name of the method that is not implemented
        @param pcClassName The name of the class that caused this NotSupportedOnThisPlatformException
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotSupportedOnThisPlatform (poInnerException AS Progress.Lang.Error, 
                                                   pcMethodName AS CHARACTER, 
                                                   pcClassName AS CHARACTER):
                                                                                                              
        SUPER (poInnerException,
               SUBSTITUTE("The method &1 of class &2 is not supported on this platform."{&TRAN}, 
                          pcMethodName, pcClassName), 
               0).

        ASSIGN THIS-OBJECT:ClassName  = pcClassName
               THIS-OBJECT:MethodName = pcMethodName .
        
    END CONSTRUCTOR.


END CLASS.
