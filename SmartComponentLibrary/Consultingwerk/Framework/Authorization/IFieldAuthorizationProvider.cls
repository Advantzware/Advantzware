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
    File        : IFieldAuthorizationProvider
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 09 00:11:19 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Authorization.* FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH .

INTERFACE Consultingwerk.Framework.Authorization.IFieldAuthorizationProvider:  
  
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
	                                                      pcSecurityKey AS CHARACTER).

END INTERFACE.