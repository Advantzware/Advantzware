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
    File        : download-rss.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Oct 03 17:57:21 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.Util.* FROM PROPATH.

DEFINE VARIABLE lcReturn  AS LONGCHAR  NO-UNDO .
DEFINE VARIABLE cTempFile AS CHARACTER NO-UNDO .

/* ***************************  Main Block  *************************** */

lcReturn = NetworkHelper:DownloadString 
    ("http://confluence.consultingwerkcloud.com/createrssfeed.action?types=page&spaces=SCL&title=Consultingwerk+Confluence+RSS-Feed&labelString=releasenote&excludedSpaceKeys%3D&sort=created&maxResults=10&timeSpan=50&confirm=RSS-Feed+erstellen&showContent=false":U) .

ASSIGN cTempFile = FileHelper:GetTempFileName() .

COPY-LOB FROM lcReturn TO FILE cTempFile .

PUT UNFORMATTED cTempFile SKIP (2) .    
