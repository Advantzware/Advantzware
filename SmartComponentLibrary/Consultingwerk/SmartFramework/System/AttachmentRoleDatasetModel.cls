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
    File        : AttachmentRole
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:47:26,666+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.AttachmentRoleDatasetModel
    INHERITS AttachmentRoleDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttachmentRoleDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttachmentRoleDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttachmentRoleDatasetModel class
        Notes:   
        @param pAttachmentRoleGUID The filter value for the eSmartAttachmentRole.AttachmentRoleGUID field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttachmentRoleDatasetModel (pAttachmentRoleGUID AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartAttachmentRole:Filter:AttachmentRoleGUID:EQ (pAttachmentRoleGUID) .

        THIS-OBJECT:SmartAttachmentRole:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
