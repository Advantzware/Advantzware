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
    File        : UserCredentials
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 07 01:55:18 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.IUserCredentials.

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Framework.UserCredentials IMPLEMENTS IUserCredentials: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Company Key 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CompanyKey AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Company Name 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CompanyName AS CHARACTER NO-UNDO 
    GET.
    SET. 
      
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Password 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Password AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the UserName
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UserName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserCredentials class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserCredentials ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserCredentials class
        Notes:   
        @param pcUserName The UserName
        @param pcPassword The Password
        @param pcCompanyKey The Company Key
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserCredentials (pcUserName AS CHARACTER,
                                        pcPassword AS CHARACTER,
                                        pcCompanyKey AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:UserName   = pcUserName
               THIS-OBJECT:Password   = pcPassword
               THIS-OBJECT:CompanyKey = pcCompanyKey .
        
    END CONSTRUCTOR.

END CLASS.
