package tree;

import java.util.LinkedList;
import java.util.List;
import syms.SymEntry;
import tree.StatementNode.BlockNode;

/**
 * class DeclNode - Handles Declarations lists and procedures.
 * DeclNode is an abstract class. 
 * The classes defined within DeclNode extend it.
 */
public abstract class DeclNode {
    
    /** Constructor */
    protected DeclNode() {
        super();
    }
    /** Simple visitor pattern implemented in subclasses */
    public abstract void accept( DeclVisitor visitor );
    /** Debugging output at level 0 */
    @Override
    public String toString() {
        return toString(0);
    }
    /** Debugging output of declarations */
    public abstract String toString( int level );
    /** Tree node representing a list of (procedure) declarations */
    public static class DeclListNode extends DeclNode {
        List<DeclNode> declarations;
        
        public DeclListNode() {
            declarations = new LinkedList<DeclNode>();
        }
        public List<DeclNode> getDeclarations() {
            return declarations;
        }
        public void addDeclaration( DeclNode declaration ) {
            declarations.add( declaration );
        }
        @Override
        public void accept(DeclVisitor visitor) {
            visitor.visitDeclListNode( this );
        }
        public String toString( int level ) {
            String s = "";
            for( DeclNode decl : declarations ) {
                s += StatementNode.newLine(level) + decl.toString(level);
            }
            return s;
        }
    }
    
    /** Tree node representing a single procedure. */
    public static class ProcedureNode extends DeclNode {
        protected SymEntry.ProcedureEntry procEntry;
        protected BlockNode block;

        public ProcedureNode( SymEntry.ProcedureEntry entry, 
                StatementNode.BlockNode block ) {
            super();
            this.procEntry = entry;
            this.block = block;
        }
        @Override
        public void accept( DeclVisitor visitor ) {
            visitor.visitProcedureNode( this );
        }
        public SymEntry.ProcedureEntry getProcEntry() {
            return procEntry;
        }
        public StatementNode.BlockNode getBlock() {
            return block;
        }
        public String toString( int level ) {
            List<SymEntry.ParamEntry> params =
                    procEntry.getType().getFormalParams();
            List<SymEntry.ParamEntry> resultParams =
                    procEntry.getType().getFormalResultParams();
            return "PROCEDURE " + procEntry.getIdent() +
                (params == null ? "" : params) +
                (params == null ? "" : resultParams) +
                " = " + block.toString( level+1 );
        }
    }

}
