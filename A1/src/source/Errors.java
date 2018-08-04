package source;

import java_cup.runtime.ComplexSymbolFactory.Location;

/**
 * interface Errors - interface to allow reporting of compilation
 *      errors and other messages. Use flush() to cause output.
 */
public interface Errors {

    /** Signal an error at the given location */
    public void error( String m, Location loc );

    /** Signal a fatal error at the given location */
    public void fatal( String m, Location loc );
    
    /** Output debugging message or error */
    public void debugPrint( String msg );

    /** Output debugging message if debug turned on */
    public void debugMessage( String msg );

    /** Increment debug level for indenting messages */
    public void incDebug();
    
    /** Decrement debug level for indenting messages */
    public void decDebug();
    
    /** Report error is assert condition fails */
    public void checkAssert( boolean condition, String m, Location loc );
    
    /** Print immediately a summary of all errors reported */
    public void errorSummary();

    /** List impending error messages, and clear accumulated errors. */
    public void flush();

    /** Return whether any errors have been reported at all */
    public boolean hadErrors();
    
    /** Print line to output stream */
    public void println( String msg );
    
}
