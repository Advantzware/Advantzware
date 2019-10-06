/* fa/rtags.p - Asset Labels Tags */
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 1996 - 1997 Foresight Software, Inc.  All Rights       *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/

define            variable wpage-number as integer.
define            variable msg as character format "x(25)".
define            stream   crt.

define variable i as integer format "9999999999". /* mod-rg 3/92 */
{fa/rtags.y}
{fa/std}

if terminal > "" then input stream crt from terminal.

MAIN-LOOP:
    FOR EACH FA-MAST WHERE
    fa-mast.fa-entity eq logincontrol and
    FA-MAST.ASSET-CODE GE BEG-CODE AND
    FA-MAST.ASSET-CODE LE END-CODE AND
    FA-MAST.entity-code GE BEG-ENTITY AND FA-MAST.entity-code LE END-ENTITY AND
    FA-MAST.GL-CODE GE BEG-GL AND FA-MAST.GL-CODE LE END-GL AND
    FA-MAST.LOCATION GE BEG-LOC AND FA-MAST.LOCATION LE END-LOC AND
    (IF SORT1 NE "" THEN CAN-DO(SORT1,SORT-CODE1) ELSE TRUE) AND
    (IF SORT2 NE "" THEN CAN-DO(SORT2,SORT-CODE2) ELSE TRUE) AND
    (INDEX(t-status,FA-MAST.ASSET-STATUS) NE 0) NO-LOCK
    BY   IF torder = "l":U THEN location
    ELSE IF torder = "1":U THEN sort-code1
    ELSE IF torder = "2":U THEN sort-code2
    ELSE IF torder = "g":U THEN gl-code
    ELSE asset-code:


  {pt/newpage main-loop string(fa-mast.asset-code)}
     for each fa-tags where fa-tags.asset-code eq fa-mast.asset-code
			and fa-tags.fa-entity eq fa-mast.fa-entity:
			/* rpm 01/95 */
       i = fa-tags.tag-nof.
     /*  find location where location.location eq fa-mast.location. */
       repeat while i le fa-tags.tag-not:
	    display "Asset:" at 1 asset-code at 7   "Tag#:" at 16
	    i at 21
	    asset-desc at 1
	    skip
/*          location.description at 1
	    skip
	    "Room#:" at  1 room at 10
	    "Bldg:" at 16  building    at 22 skip
	    location.city at 1 location.state at 22 */
	    skip(4) with {&UGUI-RPT} no-box
	    no-label no-attr-space frame frame1.

	    i = i + 1.
       end.
     end.
end.
