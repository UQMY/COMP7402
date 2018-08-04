package tree;

/**
 * enumeration Operator - operators in abstract syntax tree.
 */
public enum Operator {
    /* Binary operators */
    ADD_OP( "_+_" ),
    SUB_OP( "_-_" ),
    MUL_OP( "_*_" ),
    DIV_OP( "_/_" ),
    EQUALS_OP( "_=_" ),
    NEQUALS_OP( "_!=_" ),
    GREATER_OP( "_>_" ),
    LESS_OP( "_<_" ),
    LEQUALS_OP( "_<=_" ),
    GEQUALS_OP( "_>=_" ),
    /* unary operators */
    NEG_OP( "-_" ),

    INVALID_OP( "INVALID" );
    
    /** The name of the binary operator */
    String name;
    
    private Operator( String name ) {
        this.name = name;
    }
    public String getName() {
        return name;
    }
}
