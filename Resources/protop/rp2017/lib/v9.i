/* lib/v9.i
 *
 */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 10.0 &THEN
&global-define	CPYLOB	"no"
&global-define	NOW	substitute( "&1 &2", today, string( time, "hh:mm:ss" ))
&global-define	LNGCR	character
&global-define	DTZ	integer
&ELSE
&global-define	OE10	"yes"
&global-define	NOW	now
&global-define	LNGCR	longchar
&global-define	DTZ	datetime-tz
&ENDIF
