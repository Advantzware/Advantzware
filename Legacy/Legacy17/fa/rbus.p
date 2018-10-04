/* fa/rbus.p - Property Statement Support Report */
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 1996 - 1997 Foresight Software, Inc.  All Rights       *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/

{fa/busprop.y}
if tby = "G":U and detsum = "D":U then run fa/busgl.p.
if tby = "G":U and detsum = "S":U then run fa/busgls.p.
if tby = "1":U and detsum = "D":U then run fa/busprop.p.
if tby = "1":U and detsum = "S":U then run fa/busprops.p.
if tby = "2":U and detsum = "D":U then run fa/busprop2.p.
if tby = "2":U and detsum = "S":U then run fa/buspr2s.p.
if tby = "L":U and detsum = "D":U then run fa/busloc.p.
if tby = "L":U and detsum = "S":U then run fa/buslocs.p.
if tby = "T":U and detsum = "D":U then run fa/bustag.p.
if tby = "T":U and detsum = "S":U then run fa/bustags.p.
