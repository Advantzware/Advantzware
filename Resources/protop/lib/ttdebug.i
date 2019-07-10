/* ttdebug.i
 * 
 * turn temp-table data collection capabilities on and off
 * 
 * 0) temp-table info requires OpenEdge 11 or higher
 *
 * 1) Modify lib/ttdebug.i to define DEBUGTT -- **** OBSOLETE **** no longer necessary
 *
 * 2) add TTDEBUG to bin/localenv:
 *
 *	export TTDEBUG=yes
 *
 * 3) You must also uncomment the -tt* parameters in etc/protop.pf
 *
 *	...
 *	# these define the temp-table stats collection for oe11 clients
 *	# older clients should ignore these parameters (but we comment them out anyway).
 *	
 *	-ttbaseindex 1
 *	-ttbasetable 1
 *	-ttindexrangesize 1000
 *	-tttablerangesize 1000
 *
 * of interest:
 *
 *	http://knowledgebase.progress.com/articles/Article/P95826
 *
 *	-tmpbsize 1 =  32 rows per block
 *	-tmpbsize 4 = 256 rows per block
 *	-tmpbsize 8 = 256 rows per block
 */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.0 &THEN

&global-define	xDEBUGTT	/*  **** OBSOLETE **** no longer necessary */

&ENDIF
