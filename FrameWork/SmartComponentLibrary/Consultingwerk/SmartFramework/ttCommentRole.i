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
    File        : ttCommentRole.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 14.01.2013 22:21:57
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttCommentRole NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE ttCommentRoleBefore &ENDIF
    FIELD CommentRoleKey AS CHARACTER FORMAT "x(36)":U LABEL "CommentRoleKey":T SERIALIZE-NAME "CommentRoleKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD CommentRoleName AS CHARACTER FORMAT "x(80)":U LABEL "Comment Role":T SERIALIZE-NAME "CommentRoleName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD CommentRoleDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T SERIALIZE-NAME "CommentRoleDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD CommentRoleImageSmall AS CHARACTER FORMAT "x(8)":U LABEL "CommentRoleImageSmall":T SERIALIZE-NAME "CommentRoleImageSmall":U XML-DATA-TYPE "base64Binary":U XML-NODE-TYPE "ELEMENT":U
    FIELD CommentRoleImageLarge AS CHARACTER FORMAT "x(8)":U LABEL "CommentRoleImageLarge":T SERIALIZE-NAME "CommentRoleImageLarge":U XML-DATA-TYPE "base64Binary":U XML-NODE-TYPE "ELEMENT":U

    INDEX CommentRoleKey AS UNIQUE CommentRoleKey ASCENDING
    INDEX CommentRoleName AS UNIQUE PRIMARY CommentRoleName ASCENDING

    .
