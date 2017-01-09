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
    File        : TranslationTypeEnum
    Purpose     : Enumeration of Translation Types
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 12 21:03:00 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Localization.* FROM PROPATH .  
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Localization.TranslationTypeEnum: 

    /*------------------------------------------------------------------------------
        Purpose: Local translation for this container only
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY LocalByKey AS CHARACTER NO-UNDO INIT "1":U
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Local translation for this container by partial key and original 
                 text
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LocalPartialKeyAndText AS CHARACTER NO-UNDO INIT "2":U
    GET.
         
    /*------------------------------------------------------------------------------
        Purpose: Local translation for this container by original text
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LocalByText AS CHARACTER NO-UNDO INIT "3":U
    GET.    	 

    /*------------------------------------------------------------------------------
        Purpose: Global translation by resource key
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY GlobalByKey AS CHARACTER NO-UNDO INIT "4":U
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Global translation by partial resource key (matches) and original
                 text
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY GlobalByPartialKeyAndText AS CHARACTER NO-UNDO INIT "5":U
    GET.
	 
    /*------------------------------------------------------------------------------
        Purpose: Global translation by original text only
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY GlobalByText AS CHARACTER NO-UNDO INIT "6":U
    GET.
	 
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TranslationTypeEnum class
        Notes:   Disallow instance creation   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE TranslationTypeEnum ():
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
