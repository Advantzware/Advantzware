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
    File        : dsAttachment.i
    Purpose     : Business Entity for Attachment

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 21.11.2015 20:55:52
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsAttachment

{ Consultingwerk/SmartFramework/System/eSmartAttachment.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.AttachmentBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsAttachment{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartAttachment{&SUFFIX} 

    .    
