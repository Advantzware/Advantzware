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
    File        : RollbaseCredentials
    Purpose     : Returns Rollbase Username and Password from the Windows
                  Registry 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 03 08:56:13 CET 2013
    Notes       : Uses the Default Settings Service, reads values form 
                  Rollbase\UserName
                  Rollbase\Password
                  Values are read one, when the RollbaseCredentials service
                  is instantiated
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*       FROM PROPATH . 
USING Consultingwerk.RollbaseAdapter.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.RollbaseAdapter.RollbaseCredentials
    INHERITS Consultingwerk.JsonSerializable
    IMPLEMENTS IRollbaseCredentials: 

    {Consultingwerk/JsonSerializableProperty.i UserName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Password CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RollbaseCredentials class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RollbaseCredentials ():
        SUPER ().
        
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
        
        IF VALID-OBJECT (oSettingsService) THEN 
            ASSIGN THIS-OBJECT:UserName = oSettingsService:GetSetting ("Rollbase":U, "UserName":U)
                   THIS-OBJECT:Password = oSettingsService:GetSetting ("Rollbase":U, "Password":U).

    END CONSTRUCTOR.

END CLASS.
