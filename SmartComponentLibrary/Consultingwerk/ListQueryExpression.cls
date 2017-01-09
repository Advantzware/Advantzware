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
    File        : ListQueryExpression
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 10 12:49:17 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Framework.Base.*           FROM PROPATH.
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.ListQueryExpression
    INHERITS GenericList
    IMPLEMENTS IQueryExpression:

    { Consultingwerk/Framework/Base/GenericList.i IQueryExpression }

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Operator of the QueryExpression
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Operator AS OperatorEnum NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression ():
        SUPER ().

        THIS-OBJECT:Operator = OperatorEnum:And .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItems The array of Items to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItems AS IQueryExpression EXTENT):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItems) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
        @param poItem12 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression, poItem12 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .
        THIS-OBJECT:Add (poItem12) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
        @param poItem12 The item to add to the list
        @param poItem13 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression, poItem12 AS IQueryExpression, poItem13 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .
        THIS-OBJECT:Add (poItem12) .
        THIS-OBJECT:Add (poItem13) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
        @param poItem12 The item to add to the list
        @param poItem13 The item to add to the list
        @param poItem14 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression, poItem12 AS IQueryExpression, poItem13 AS IQueryExpression, poItem14 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .
        THIS-OBJECT:Add (poItem12) .
        THIS-OBJECT:Add (poItem13) .
        THIS-OBJECT:Add (poItem14) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
        @param poItem12 The item to add to the list
        @param poItem13 The item to add to the list
        @param poItem14 The item to add to the list
        @param poItem15 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression, poItem12 AS IQueryExpression, poItem13 AS IQueryExpression, poItem14 AS IQueryExpression, poItem15 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .
        THIS-OBJECT:Add (poItem12) .
        THIS-OBJECT:Add (poItem13) .
        THIS-OBJECT:Add (poItem14) .
        THIS-OBJECT:Add (poItem15) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
        @param poItem12 The item to add to the list
        @param poItem13 The item to add to the list
        @param poItem14 The item to add to the list
        @param poItem15 The item to add to the list
        @param poItem16 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression, poItem12 AS IQueryExpression, poItem13 AS IQueryExpression, poItem14 AS IQueryExpression, poItem15 AS IQueryExpression,
                                            poItem16 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .
        THIS-OBJECT:Add (poItem12) .
        THIS-OBJECT:Add (poItem13) .
        THIS-OBJECT:Add (poItem14) .
        THIS-OBJECT:Add (poItem15) .
        THIS-OBJECT:Add (poItem16) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
        @param poItem12 The item to add to the list
        @param poItem13 The item to add to the list
        @param poItem14 The item to add to the list
        @param poItem15 The item to add to the list
        @param poItem16 The item to add to the list
        @param poItem17 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression, poItem12 AS IQueryExpression, poItem13 AS IQueryExpression, poItem14 AS IQueryExpression, poItem15 AS IQueryExpression,
                                            poItem16 AS IQueryExpression, poItem17 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .
        THIS-OBJECT:Add (poItem12) .
        THIS-OBJECT:Add (poItem13) .
        THIS-OBJECT:Add (poItem14) .
        THIS-OBJECT:Add (poItem15) .
        THIS-OBJECT:Add (poItem16) .
        THIS-OBJECT:Add (poItem17) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
        @param poItem12 The item to add to the list
        @param poItem13 The item to add to the list
        @param poItem14 The item to add to the list
        @param poItem15 The item to add to the list
        @param poItem16 The item to add to the list
        @param poItem17 The item to add to the list
        @param poItem18 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression, poItem12 AS IQueryExpression, poItem13 AS IQueryExpression, poItem14 AS IQueryExpression, poItem15 AS IQueryExpression,
                                            poItem16 AS IQueryExpression, poItem17 AS IQueryExpression, poItem18 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .
        THIS-OBJECT:Add (poItem12) .
        THIS-OBJECT:Add (poItem13) .
        THIS-OBJECT:Add (poItem14) .
        THIS-OBJECT:Add (poItem15) .
        THIS-OBJECT:Add (poItem16) .
        THIS-OBJECT:Add (poItem17) .
        THIS-OBJECT:Add (poItem18) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
        @param poItem12 The item to add to the list
        @param poItem13 The item to add to the list
        @param poItem14 The item to add to the list
        @param poItem15 The item to add to the list
        @param poItem16 The item to add to the list
        @param poItem17 The item to add to the list
        @param poItem18 The item to add to the list
        @param poItem19 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression, poItem12 AS IQueryExpression, poItem13 AS IQueryExpression, poItem14 AS IQueryExpression, poItem15 AS IQueryExpression,
                                            poItem16 AS IQueryExpression, poItem17 AS IQueryExpression, poItem18 AS IQueryExpression, poItem19 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .
        THIS-OBJECT:Add (poItem12) .
        THIS-OBJECT:Add (poItem13) .
        THIS-OBJECT:Add (poItem14) .
        THIS-OBJECT:Add (poItem15) .
        THIS-OBJECT:Add (poItem16) .
        THIS-OBJECT:Add (poItem17) .
        THIS-OBJECT:Add (poItem18) .
        THIS-OBJECT:Add (poItem19) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:
        @param poItem1 The item to add to the list
        @param poItem2 The item to add to the list
        @param poItem3 The item to add to the list
        @param poItem4 The item to add to the list
        @param poItem5 The item to add to the list
        @param poItem6 The item to add to the list
        @param poItem7 The item to add to the list
        @param poItem8 The item to add to the list
        @param poItem9 The item to add to the list
        @param poItem10 The item to add to the list
        @param poItem11 The item to add to the list
        @param poItem12 The item to add to the list
        @param poItem13 The item to add to the list
        @param poItem14 The item to add to the list
        @param poItem15 The item to add to the list
        @param poItem16 The item to add to the list
        @param poItem17 The item to add to the list
        @param poItem18 The item to add to the list
        @param poItem19 The item to add to the list
        @param poItem20 The item to add to the list
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression (poItem1 AS IQueryExpression, poItem2 AS IQueryExpression, poItem3 AS IQueryExpression, poItem4 AS IQueryExpression, poItem5 AS IQueryExpression,
                                            poItem6 AS IQueryExpression, poItem7 AS IQueryExpression, poItem8 AS IQueryExpression, poItem9 AS IQueryExpression, poItem10 AS IQueryExpression,
                                            poItem11 AS IQueryExpression, poItem12 AS IQueryExpression, poItem13 AS IQueryExpression, poItem14 AS IQueryExpression, poItem15 AS IQueryExpression,
                                            poItem16 AS IQueryExpression, poItem17 AS IQueryExpression, poItem18 AS IQueryExpression, poItem19 AS IQueryExpression, poItem20 AS IQueryExpression):
        THIS-OBJECT ().

        THIS-OBJECT:Add (poItem1) .
        THIS-OBJECT:Add (poItem2) .
        THIS-OBJECT:Add (poItem3) .
        THIS-OBJECT:Add (poItem4) .
        THIS-OBJECT:Add (poItem5) .
        THIS-OBJECT:Add (poItem6) .
        THIS-OBJECT:Add (poItem7) .
        THIS-OBJECT:Add (poItem8) .
        THIS-OBJECT:Add (poItem9) .
        THIS-OBJECT:Add (poItem10) .
        THIS-OBJECT:Add (poItem11) .
        THIS-OBJECT:Add (poItem12) .
        THIS-OBJECT:Add (poItem13) .
        THIS-OBJECT:Add (poItem14) .
        THIS-OBJECT:Add (poItem15) .
        THIS-OBJECT:Add (poItem16) .
        THIS-OBJECT:Add (poItem17) .
        THIS-OBJECT:Add (poItem18) .
        THIS-OBJECT:Add (poItem19) .
        THIS-OBJECT:Add (poItem20) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param poSecondExpression The right operand of the Query Expression
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        poSecondExpression AS IQueryExpression):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     poSecondExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator
        @param pcExpression A Character Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pcExpression AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pcExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator
        @param pdtExpression A Date Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATE):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator
        @param pdtExpression A DateTime Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator
        @param pdtExpression A DateTime Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME-TZ):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator
        @param pdeExpression A Decimal Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdeExpression AS DECIMAL):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pdeExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator
        @param piExpression An Integer Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INTEGER):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     piExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator
        @param piExpression An Int64 Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INT64):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     piExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator
        @param plExpression A Logical Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        plExpression AS LOGICAL):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     plExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pcExpression A Character Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pcExpression AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pcExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pdtExpression A Date Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATE):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pdtExpression A DateTime Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pdtExpression A DateTime Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME-TZ):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pdtExpression)) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pdeExpression A Decimal Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdeExpression AS DECIMAL):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pdeExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param piExpression An Integer Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INTEGER):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     piExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param piExpression An Int64 Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INT64):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     piExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param plExpression A Logical Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        plExpression AS LOGICAL):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     plExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Expression to be used in a Query String
        Notes:
        @return The CHARACTER Expression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetExpression ():

        DEFINE VARIABLE cExpression AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOperator   AS CHARACTER NO-UNDO.

        IF NOT VALID-OBJECT (THIS-OBJECT:Operator) THEN
            THIS-OBJECT:Operator = OperatorEnum:And .

        ASSIGN cOperator = THIS-OBJECT:Operator:ToString() .

        {Consultingwerk/foreachABL.i IQueryExpression oExpression in THIS-OBJECT}

            IF cExpression > "":U THEN
                cExpression = SUBSTITUTE ("&1 &3 &2":U,
                                          cExpression,
                                          THIS-OBJECT:GetExpressionForEntry (oExpression),
                                          cOperator) .
            ELSE
                cExpression = THIS-OBJECT:GetExpressionForEntry (oExpression) .
        END.

/*        IF THIS-OBJECT:Operator <> OperatorEnum:And THEN*/
/*            RETURN SUBSTITUTE ("(&1)":U, cExpression) . */

        RETURN cExpression .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the expression value for an entry in the List
        Notes:   Adds parenthesis for Lists that are entries in this List
        @param poExpression The expression to return the value for
        @return The value for the given expression
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetExpressionForEntry (poExpression AS IQueryExpression):

        IF TYPE-OF (poExpression, IEnumerable) THEN
            RETURN SUBSTITUTE ("(&1)":U, poExpression:GetExpression()) .
        ELSE
            RETURN poExpression:GetExpression() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the ListQueryExpression is empty
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IsEmpty AS LOGICAL NO-UNDO
    GET():

        IF THIS-OBJECT:Count > 0 THEN
            RETURN FALSE .
        ELSE
            RETURN TRUE .

    END GET.

END CLASS.
