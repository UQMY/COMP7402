package parser;

import java_cup.runtime.ComplexSymbolFactory.Location;

/**
 * class IdentifierToken - Identifier token needs an identifier name
 */ 
public class IdentifierToken extends LexicalToken {

    private String name;

    /** Construct a token with the given type, location and string value. 
     * @param type should normally be IDENTIFIER.
     */
    public IdentifierToken( Token type, Location loc, String name ) {
        super(type,loc);
        this.name = name;
    }
    /** Extract name of IDENTIFIER token */
    @Override
    public String getName( ) {
        return name;
    }
    @Override
    public String toString() {
        return "identifier(\"" + name + "\")";
    }
}
