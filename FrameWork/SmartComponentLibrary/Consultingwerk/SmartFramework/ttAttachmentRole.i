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
    File        : ttAttachmentRole.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 14.01.2013 22:18:47
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttAttachmentRole NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE ttAttachmentRoleBefore &ENDIF
    FIELD AttachmentRoleKey AS CHARACTER FORMAT "x(36)":U LABEL "AttachmentRoleKey":T SERIALIZE-NAME "AttachmentRoleKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD AttachmentRoleName AS CHARACTER FORMAT "x(80)":U LABEL "Attachment Role":T SERIALIZE-NAME "AttachmentRoleName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD AttachmentRoleDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T SERIALIZE-NAME "AttachmentRoleDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD AttachmentRoleImageSmall AS CHARACTER FORMAT "x(8)":U LABEL "AttachmentRoleImageSmall":T SERIALIZE-NAME "AttachmentRoleImageSmall":U XML-DATA-TYPE "base64Binary":U XML-NODE-TYPE "ELEMENT":U
    FIELD AttachmentRoleImageLarge AS CHARACTER FORMAT "x(8)":U LABEL "AttachmentRoleImageLarge":T SERIALIZE-NAME "AttachmentRoleImageLarge":U XML-DATA-TYPE "base64Binary":U XML-NODE-TYPE "ELEMENT":U

    INDEX AttachmentRoleKey AS UNIQUE AttachmentRoleKey ASCENDING
    INDEX AttachMentRoleName AS UNIQUE PRIMARY AttachmentRoleName ASCENDING

    .
