/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : eSmartAttachment.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 21.11.2015 20:55:52
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.AttachmentBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartAttachment{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartAttachmentBefore{&SUFFIX} &ENDIF
    FIELD AttachmentGUID AS CHARACTER FORMAT "x(36)":U LABEL "AttachmentGUID":T
    FIELD TableGUID AS CHARACTER FORMAT "x(36)":U LABEL "TableGUID":T
    FIELD RecordKeyValue AS CHARACTER FORMAT "x(80)":U LABEL "RecordKeyValue":T
    FIELD AttachmentRoleGUID AS CHARACTER FORMAT "x(36)":U LABEL "AttachmentRoleGUID":T
    FIELD CommentRoleGUID AS CHARACTER FORMAT "x(36)":U LABEL "CommentRoleGUID":T
    FIELD AttachmentName AS CHARACTER FORMAT "x(80)":U LABEL "Name":T
    FIELD AttachmentFolder AS CHARACTER FORMAT "x(80)":U LABEL "Folder":T
    FIELD LanguageGuid AS CHARACTER FORMAT "x(36)":U LABEL "LanguageGuid":T
    FIELD AttachmentData AS BLOB FORMAT "x(8)":U LABEL "AttachmentData":T
    FIELD AttachmentThumbnail AS BLOB FORMAT "x(8)":U LABEL "AttachmentThumbnail":T
    FIELD CommentData AS CLOB FORMAT "x(8)":U LABEL "CommentData":T
    FIELD AttachmentRoleName AS CHARACTER FORMAT "x(80)":U LABEL "Attachment Role":T
    FIELD AttachmentSize AS INT64 FORMAT "->,>>>,>>9":U LABEL "Attachment Size":T

    INDEX AttachmentName AS UNIQUE PRIMARY TableGUID ASCENDING RecordKeyValue ASCENDING AttachmentFolder ASCENDING AttachmentName ASCENDING
    INDEX AttachmentRole TableGUID ASCENDING RecordKeyValue ASCENDING AttachmentRoleGUID ASCENDING LanguageGuid ASCENDING
    INDEX AtttachmentGUID AS UNIQUE AttachmentGUID ASCENDING
    INDEX CommentGUID TableGUID ASCENDING RecordKeyValue ASCENDING CommentRoleGUID ASCENDING LanguageGuid ASCENDING

    .

    