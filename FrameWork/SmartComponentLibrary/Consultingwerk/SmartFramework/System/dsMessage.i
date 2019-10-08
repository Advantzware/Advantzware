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
    File        : dsMessage.i
    Purpose     : Business Entity for Message

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 09.01.2013 08:02:19
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsMessage

{ Consultingwerk/SmartFramework/System/eSmartMessage.i }


DEFINE {&ACCESS} DATASET dsMessage {&REFERENCE-ONLY} FOR eSmartMessage 

    .    
