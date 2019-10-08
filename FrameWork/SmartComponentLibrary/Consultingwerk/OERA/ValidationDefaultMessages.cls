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
    File        : ValidationDefaultMessages
    Purpose     : Static class containing the default validation message
                  used by the Consultingwerk.OERA.Validate class
    Syntax      : Static property
    Description : Subscribes to the SessionManager LanguageChanged event
                  to support message translation
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 02 14:13:19 CEST 2016
    Notes       : Substitute patterns: &1 Field Value,
                  &2 reference value (optionally), &3: Field Label
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Session.* FROM PROPATH .
USING Consultingwerk.OERA.*              FROM PROPATH .
USING Consultingwerk.SmartFramework.*    FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.ValidationDefaultMessages:

    /**
     * Purpose: Gets and sets the default validation message
     *          for the CanFind validation
     * Notes:   VALMSG:101
     */
    DEFINE PUBLIC STATIC PROPERTY CanFind AS CHARACTER NO-UNDO INIT
        '&3 record for "&1" not found.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the EQ validation
     * Notes:   VALMSG:102
     */
    DEFINE PUBLIC STATIC PROPERTY EQ AS CHARACTER NO-UNDO INIT
        '&3 must be "&2".'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the GE validation
     * Notes:   VALMSG:103
     */
    DEFINE PUBLIC STATIC PROPERTY GE AS CHARACTER NO-UNDO INIT
        '&3 must be greater or equal to "&2".'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the GT validation
     * Notes:   VALMSG:104
     */
    DEFINE PUBLIC STATIC PROPERTY GT AS CHARACTER NO-UNDO INIT
        '&3 must be greater than "&2".'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsEmpty validation
     * Notes:   VALMSG:105
     */
    DEFINE PUBLIC STATIC PROPERTY IsEmpty AS CHARACTER NO-UNDO INIT
        '&3 must be empty.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsInList validation
     * Notes:   VALMSG:106
     */
    DEFINE PUBLIC STATIC PROPERTY IsInList AS CHARACTER NO-UNDO INIT
        '&3 must be a value of "&2".'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsInRange validation
     * Notes:   VALMSG:107
     */
    DEFINE PUBLIC STATIC PROPERTY IsInRange AS CHARACTER NO-UNDO INIT
        '&3 must be between &2 and &4.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsValidEmailAddress validation
     * Notes:   VALMSG:125
     */
    DEFINE PUBLIC STATIC PROPERTY IsInvalidEmailAddress AS CHARACTER NO-UNDO INIT
        '&1 is not a valid email address.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsNotNullOrEmpty validation
     * Notes:   VALMSG:108
     */
    DEFINE PUBLIC STATIC PROPERTY IsNotNullOrEmpty AS CHARACTER NO-UNDO INIT
        '&3 must not be empty or unknown.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsNotRestricted validation
     * Notes:   VALMSG:109
     */
    DEFINE PUBLIC STATIC PROPERTY IsNotRestricted AS CHARACTER NO-UNDO INIT
        '&3 is restricted.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsNotUnknown validation
     * Notes:   VALMSG:110
     */
    DEFINE PUBLIC STATIC PROPERTY IsNotUnknown AS CHARACTER NO-UNDO INIT
        '&3 must not be unknown.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsNotUnknownOrZero validation
     * Notes:   VALMSG:111
     */
    DEFINE PUBLIC STATIC PROPERTY IsNotUnknownOrZero AS CHARACTER NO-UNDO INIT
        '&3 must not be unknown or zero.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsTokenNotRestricted validation
     * Notes:   VALMSG:112
     */
    DEFINE PUBLIC STATIC PROPERTY IsTokenNotRestricted AS CHARACTER NO-UNDO INIT
        'The function is &2 restricted.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsUnknown validation
     * Notes:   VALMSG:113
     */
    DEFINE PUBLIC STATIC PROPERTY IsUnknown AS CHARACTER NO-UNDO INIT
        '&3 must be unknown.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsUnknownOrZero validation
     * Notes:   VALMSG:114
     */
    DEFINE PUBLIC STATIC PROPERTY IsUnknownOrZero AS CHARACTER NO-UNDO INIT
        '&3 must be unknown or zero.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsWeekday validation
     * Notes:   VALMSG:115
     */
    DEFINE PUBLIC STATIC PROPERTY IsWeekday AS CHARACTER NO-UNDO INIT
        '&3 must be a weekday.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsWeekday (WeekDayEnum) validation
     * Notes:   VALMSG:116
     */
    DEFINE PUBLIC STATIC PROPERTY IsSpecificWeekday AS CHARACTER NO-UNDO INIT
        '&3 must be a &2.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the IsZero validation
     * Notes:   VALMSG:117
     */
    DEFINE PUBLIC STATIC PROPERTY IsZero AS CHARACTER NO-UNDO INIT
        '&3 must be zero.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the LE validation
     * Notes:   VALMSG:118
     */
    DEFINE PUBLIC STATIC PROPERTY LE AS CHARACTER NO-UNDO INIT
        '&3 must be less or equal to "&2".'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the LT validation
     * Notes:   VALMSG:119
     */
    DEFINE PUBLIC STATIC PROPERTY LT AS CHARACTER NO-UNDO INIT
        '&3 must be less than "&2".'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the MaxLength validation
     * Notes:   VALMSG:120
     */
    DEFINE PUBLIC STATIC PROPERTY MaxLength AS CHARACTER NO-UNDO INIT
        'Length of &3 must more than &2 characters.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the MinLength validation
     * Notes:   VALMSG:121
     */
    DEFINE PUBLIC STATIC PROPERTY MinLength AS CHARACTER NO-UNDO INIT
        'Length of &3 must be at least &2 characters.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the NE validation
     * Notes:   VALMSG:122
     */
    DEFINE PUBLIC STATIC PROPERTY NE AS CHARACTER NO-UNDO INIT
        '&3 must not be equal to "&2".'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the NotCanFind validation
     * Notes:   VALMSG:123
     */
    DEFINE PUBLIC STATIC PROPERTY NotCanFind AS CHARACTER NO-UNDO INIT
        '&3 record for "&1" exists.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Gets and sets the default validation message
     *          for the ReadOnly validation
     * Notes:   VALMSG:124
     */
    DEFINE PUBLIC STATIC PROPERTY ReadOnly AS CHARACTER NO-UNDO INIT
        '&3 cannot be modified.'{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Protected default constructor for the ValidationDefaultMessages class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED ValidationDefaultMessages():

    END CONSTRUCTOR.

END CLASS.
