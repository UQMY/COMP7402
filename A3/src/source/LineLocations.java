package source;

import java_cup.runtime.ComplexSymbolFactory.Location;

import java.util.ArrayList;
import java.util.List;

/** 
 * class LineLocations - tracks the locations of lines within text file.
 */

public class LineLocations {

    private List<Integer> lineEnds;
    
    LineLocations() {
        this.lineEnds = new ArrayList<Integer>();
        this.lineEnds.add( -1 );
    }
    /** Add an end-of-line location.
     * @requires the new location greater than or equal to previous last location.
     */
    void add( int p ) {
        assert endLast().compareTo( p ) <= 0;
        // Add line only if nonempty
        if( endLast().compareTo( p ) != 0 ) {
            lineEnds.add( p );
        }
    }
    /** Retrieve the line number on which the given location occurs.
     * @requires the location is not greater than the end of the last line.
     */
    int getLineNumber( Location loc ) {
        if (lineEnds.size() == 1) {
            if (lineEnds.get(0) != -1) {
                assert loc.getColumn() <= lineEnds.get(loc.getLine());
            }
        } else {
            assert loc.getColumn() <= (lineEnds.get(loc.getLine()) - lineEnds.get(loc.getLine() - 1));
        }
        return loc.getLine();
    }
    /** Get the location of the start of the line that contains location p.
     */
    Integer getLineStart( Location p ) {
        int endPrevious = lineEnds.get( p.getLine() );
        return endPrevious + 1;
    }
    /** Get the offset of location p from the start of the line on which
     * it occurs.
     */
    int offset( Location p ) {
        return p.getColumn();
    }
    /** Get the location of the end of the last line. */
    Integer endLast() {
        return lineEnds.get( lineEnds.size() - 1 );
    }
}
