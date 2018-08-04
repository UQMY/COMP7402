package source;

/** 
 * enumeration Severity - Error message severity types 
 */
public enum Severity {
    FATAL( "Fatal" ),
    RESTRICTION( "Restriction" ),
    ERROR( " Error" ),
    WARNING( "Warning" ),
    REPAIR( "Repair" ),
    NOTE( "Note" ),
    INFORMATION( "Information" );
   
    String message;

    private Severity( String message ) {
        this.message = message;
    }
    public String toString() {
        return message;
    }
}
