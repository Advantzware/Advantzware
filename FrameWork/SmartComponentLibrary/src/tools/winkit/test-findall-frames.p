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
    File        : test-findall-frames.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 24 11:18:01 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cFrames AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

CREATE ALIAS dictdb FOR DATABASE "sports2000" . 

RUN tools/winkit/find-all-define-frame-names.p
        ("src/demo/customerviewer.w", 
         FALSE,
         "c:\temp\test.log",
         OUTPUT cFrames) .
         
MESSAGE cFrames 
    VIEW-AS ALERT-BOX.         
