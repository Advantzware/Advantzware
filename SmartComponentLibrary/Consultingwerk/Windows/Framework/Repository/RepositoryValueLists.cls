/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : RepositoryValueLists
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 02 07:34:18 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*                         FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.* FROM PROPATH .
USING Infragistics.Win.*                            FROM ASSEMBLY .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.RepositoryValueLists:

    /**
     * Purpose: Protected constructor for the RepositoryValueLists class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED RepositoryValueLists ():

        /* NOOP */

    END CONSTRUCTOR.

    /**
     * Purpose: Returns a value list for the property or event column of
     *          the Attribute
     * Notes:
     * @return The reference to the value list instance
     */
    METHOD PUBLIC STATIC ValueList GetPropertyOrEventValueList():

        DEFINE VARIABLE oValueList AS ValueList NO-UNDO .

        oValueList = NEW ValueList () .
        oValueList:DisplayStyle = ValueListDisplayStyle:DisplayTextAndPicture .

        oValueList:ValueListItems:Add (BOX (TRUE),  "Property":U):Appearance:Image = ImageHelper:Load ("Consultingwerk/Windows/Framework/Repository/Images/property.PNG":U).
        oValueList:ValueListItems:Add (BOX (FALSE), "Event":U):Appearance:Image    = ImageHelper:Load ("Consultingwerk/Windows/Framework/Repository/Images/event.PNG":U).

        RETURN oValueList .

    END METHOD.

END CLASS.
