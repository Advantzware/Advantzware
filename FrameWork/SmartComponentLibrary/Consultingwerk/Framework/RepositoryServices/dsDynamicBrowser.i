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
    File        : dsDynamicBrowser.i
    Purpose     : Business Entity for DynamicBrowser

    Syntax      :

    Description : Dataset returned by the IDynamicBrowserRepository and 
                  used by IDynamicsBrowserRenderer 

    Author(s)   : Mike Fechner
    Created     : 15.07.2013 17:55:36
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsDynamicBrowser

{ Consultingwerk/Framework/RepositoryServices/ttBrowserProperties.i &NO-BEFORE=YES }
{ Consultingwerk/Framework/RepositoryServices/ttColumnProperties.i &NO-BEFORE=YES }


DEFINE {&ACCESS} DATASET dsDynamicBrowser {&REFERENCE-ONLY} FOR ttBrowserProperties, ttColumnProperties 
    DATA-RELATION ttBrowserPropertiesttColumnPrope FOR ttBrowserProperties, ttColumnProperties 
        RELATION-FIELDS (BrowserKey,BrowserKey)

    .    
