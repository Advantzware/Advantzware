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
    File        : dsCommentRole.i
    Purpose     : Business Entity for CommentRole

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 14.01.2013 22:21:57
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsCommentRole

{ Consultingwerk/SmartFramework/System/eSmartCommentRole.i }


DEFINE {&ACCESS} DATASET dsCommentRole {&REFERENCE-ONLY} FOR eSmartCommentRole 

    .    
