/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : eSmartSupportedLink.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.05.2016 23:28:17
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.SupportedLinkBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartSupportedLink{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartSupportedLinkBefore{&SUFFIX} &ENDIF
    FIELD SupportedLinkGuid AS CHARACTER FORMAT "x(36)":U LABEL "SupportedLinkGuid":T
    FIELD ObjectTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectTypeGuid":T
    FIELD LinkTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "LinkTypeGuid":T
    FIELD LinkSource AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "Supported as source":T
    FIELD LinkTarget AS LOGICAL FORMAT "yes/no":U INIT "yes":U LABEL "Supported as target":T
    FIELD DeactivateOnHide AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "DeactivateLinkOnHide":T
    FIELD LinkName AS CHARACTER FORMAT "x(80)":U LABEL "Link Name":T

    INDEX LinkTypeObjectType AS UNIQUE LinkTypeGuid ASCENDING ObjectTypeGuid ASCENDING
    INDEX ObjectTypeLinkType AS UNIQUE ObjectTypeGuid ASCENDING LinkTypeGuid ASCENDING
    INDEX SupportedLinkGuid AS UNIQUE PRIMARY SupportedLinkGuid ASCENDING

    .

    