/* addon start program with additional propath */
/* Addon development propath - GUI first and addon */

assign /*PROPATH = "..\," + PROPATH.*/
    PROPATH = "c:/asi_gui9/pco500,z:/asi_gui9/pco500,z:/asi_gui9/pco500/addon," + PROPATH.
RUN ./nosweat.p.
