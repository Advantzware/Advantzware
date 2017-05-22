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
    File        : ttEmbeddedWindows.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 11 15:10:18 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

 DEFINE {&ACCESS} TEMP-TABLE ttEmbeddedWindows NO-UNDO {&REFERENCE-ONLY}
    FIELD EmbeddedWindow AS Progress.Lang.Object
    FIELD WindowTitle   AS CHARACTER LABEL "Window Title"
    FIELD ClassName     AS CHARACTER LABEL "Class Name" 
    FIELD ProcedureName AS CHARACTER LABEL "Procedure Name" .