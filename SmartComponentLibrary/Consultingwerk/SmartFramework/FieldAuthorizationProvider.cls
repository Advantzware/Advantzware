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
    File        : FieldAuthorizationProvider
    Purpose     : Sample IFieldAuthorizationProvider implementation 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 09 00:17:05 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Authorization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*          FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.SmartFramework.FieldAuthorizationProvider 
    IMPLEMENTS IFieldAuthorizationProvider: 

    /*------------------------------------------------------------------------------
        Purpose: Provides authorization for fields 
        Notes:   
        @param poRequestor The reference to the requesting object
        @param pcFieldName The field name to secure
        @param pcSecurityKey The security key of the field's container (Viewer, Browser)
        @return The FieldRestrictionEnum value  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC FieldRestrictionEnum IsFieldRestricted (poRequestor AS Progress.Lang.Object,
                                                          pcFieldName AS CHARACTER,
                                                          pcSecurityKey AS CHARACTER):

        IF Consultingwerk.Framework.Session.SessionManager:UserName = "demo":U THEN DO:
            CASE pcFieldName:
                WHEN "eCustomer.Name":U THEN
                    RETURN FieldRestrictionEnum:Disabled .
                WHEN "eCustomer.Balance":U THEN
                    RETURN FieldRestrictionEnum:Hidden .
                WHEN "eCustomer.CreditLimit":U THEN
                    RETURN FieldRestrictionEnum:Hidden .
            END CASE .
        END.
                
        RETURN FieldRestrictionEnum:None .

	END METHOD .

END CLASS.
