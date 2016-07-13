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
    File        : LoginStateEnum
    Purpose     : Enum for the values of the Client-Principal LOGIN-STATE attribute
    Syntax      : 
    Description : Reduce risk of typos when verifying the LOGIN-STATE 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 15 06:53:29 CEST 2013
    Notes       : This Enum type is purposely implemented using CHARACTER 
                  static properties.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*.

CLASS Consultingwerk.LoginStateEnum: 

    /*------------------------------------------------------------------------------
        Purpose: From the creation of an unsealed client-principal object using the 
                 CREATE CLIENT-PRINCIPAL statement until the object is sealed, and 
                 after calling the INITIALIZE() method on a sealed or unsealed 
                 client-principal object in any state                                                                   
        Notes:   Default value                                                                     
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Initial AS CHARACTER NO-UNDO INIT "INITIAL":U 
    GET. 
    
    /*------------------------------------------------------------------------------
        Purpose: After the user identity represented by an unsealed client-principal 
                 object is successfully authenticated and the object is sealed using 
                 the SEAL() method, SECURITY-POLICY:SET-CLIENT() method, or the 
                 SET-DB-CLIENT function                                                                    
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Login AS CHARACTER NO-UNDO INIT "LOGIN":U 
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: After executing the LOGOUT() method on a sealed client-principal 
                 object in the LOGIN state                                                                      
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Logout AS CHARACTER NO-UNDO INIT "LOGOUT":U
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: After executing the SEAL() method, SET-CLIENT() method, SET-DB-CLIENT 
                 function, or IMPORT-PRINCIPAL() method, or VALIDATE-SEAL() method, 
                 and the date and time set for the LOGIN-EXPIRATION-TIMESTAMP attribute 
                 has expired                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Expired AS CHARACTER NO-UNDO INIT "EXPIRED":U 
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: After executing the AUTHENTICATION-FAILED() method on an unsealed 
                 client-principal object or after a user authentication operation 
                 fails on an unsealed client-principal object when calling the 
                 SET-CLIENT() method or SET-DB-CLIENT function.                                                                     
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Failed AS CHARACTER NO-UNDO INIT "FAILED":U
    GET. 

    /*------------------------------------------------------------------------------
        Purpose: Private constructor for the LoginStateEnum class                                                                    
        Notes:   Disallow instance creation                                                                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE LoginStateEnum ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the specified login-state value is valid                                                                     
        Notes:                                                  
        @param pcValue The value to verify
        @return Logical value indicating if the value is valid                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsValid (pcValue AS CHARACTER):
        
        IF INDEX ("INITIAL,LOGIN,LOGOUT,EXPIRED,FAILED":U, pcValue) > 0 THEN 
            RETURN TRUE . 
            
        RETURN FALSE .     

    END METHOD.

END CLASS.
