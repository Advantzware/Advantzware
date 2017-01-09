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
    File        : AttachmentRoleProvider
    Purpose     : SmartFramework dependent IAttachmentRoleProvider implementation
                  Retrieves attachment roles from the SmartDB
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 23:07:10 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                            FROM PROPATH .
USING Consultingwerk.OERA.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.IAttachmentRoleProvider FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.*             FROM PROPATH .
USING Progress.Lang.*                                       FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.AttachmentRoleProvider
    IMPLEMENTS IAttachmentRoleProvider:

    {Consultingwerk/SmartFramework/ttAttachmentRole.i &NO-BEFORE}
    {Consultingwerk/SmartFramework/System/dsAttachmentRole.i}

    DEFINE VARIABLE lReadFromBackend AS LOGICAL NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Returns the ttAttachmentRole Temp-Table with the list of available
                 attachment roles
        Notes:
        @param ttAttachmentRole OUTPUT Temp-Table with the list of available attachment roles
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetAttachmentRoles (OUTPUT TABLE ttAttachmentRole):

        IF lReadFromBackend THEN
            RETURN .

        EMPTY TEMP-TABLE ttAttachmentRole .

        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       "Consultingwerk.SmartFramework.System.AttachmentRoleBusinessEntity":U,
                                                       NEW FetchDataRequest ("eSmartAttachmentRole":U),
                                                       OUTPUT DATASET dsAttachmentRole) .

        ASSIGN lReadFromBackend = TRUE .

        FOR EACH eSmartAttachmentRole:

            CREATE ttAttachmentRole .
            BUFFER-COPY eSmartAttachmentRole TO ttAttachmentRole
                ASSIGN ttAttachmentRole.AttachmentRoleKey = eSmartAttachmentRole.AttachmentRoleGuid .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Count of Attachment Roles
        Notes:
        @return The count of attachment roles
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER GetCount ():

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF NOT lReadFromBackend THEN
            THIS-OBJECT:GetAttachmentRoles (OUTPUT TABLE ttAttachmentRole) .

        FOR EACH ttAttachmentRole:
            i = i + 1 .
        END.

        RETURN i .
    END METHOD .

END CLASS.
