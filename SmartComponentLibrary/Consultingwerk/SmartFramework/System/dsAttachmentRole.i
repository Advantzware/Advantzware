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
    File        : dsAttachmentRole.i
    Purpose     : Business Entity for AttachmentRole

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 14.01.2013 22:18:47
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsAttachmentRole

{ Consultingwerk/SmartFramework/System/eSmartAttachmentRole.i }


DEFINE {&ACCESS} DATASET dsAttachmentRole {&REFERENCE-ONLY} FOR eSmartAttachmentRole 

    .    
