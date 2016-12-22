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
    File        : eStringResources.i
    Purpose     : Used to view and update string resources from a .resx file

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 16.05.2013 22:47:02
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eStringResources NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eStringResourcesBefore &ENDIF
    FIELD ResourceKey AS CHARACTER FORMAT "X(40)":U LABEL "Key":T
    FIELD ResourceKeyPartial AS CHARACTER FORMAT "X(40)":U
    FIELD OriginalString AS CHARACTER FORMAT "X(40)":U LABEL "Original Text":T
    FIELD TranslatedString AS CHARACTER FORMAT "X(40)":U LABEL "Translation":T
    FIELD TranslationType AS CHARACTER FORMAT "X":U INIT "1":U LABEL "Type":T

    INDEX ResourceKey AS UNIQUE PRIMARY ResourceKey ASCENDING

    .
