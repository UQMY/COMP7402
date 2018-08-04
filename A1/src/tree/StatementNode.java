package tree;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import syms.Scope;
import java_cup.runtime.ComplexSymbolFactory.Location;
import syms.SymEntry;

/** 
 * class StatementNode - Abstract syntax tree representation of statements. 
 * Classes defined within StatementNode extend it.
 */
public abstract class StatementNode {
    /** Location in the input source program (line and column number effectively).
     * All statements have a location within the original source code.
     */
    private Location loc;

    /** Constructor */
    protected StatementNode( Location loc ) {
        this.loc = loc;
    }
    public Location getLocation() {
        return loc;
    }
    
    /** All statement nodes provide an accept method to implement the visitor
     * pattern to traverse the tree.
     * @param visitor class implementing the details of the particular
     *  traversal.
     */
    public abstract void accept( StatementVisitor visitor );
    
    /** All statement nodes provide a genCode method to implement the visitor
     * pattern to traverse the tree for code generation.
     * @param visitor class implementing the code generation
     */
    public abstract Code genCode( StatementTransform<Code> visitor );
    
    /** Debugging output of a statement at an indent level */
    public abstract String toString( int level );
    
    /** Debugging output at level 0 */
    @Override
    public String toString() {
        return this.toString(0);
    }
    
    /** Returns a string with a newline followed by spaces of length 2n. */
    public static String newLine( int n ) {
       String indent = "\n";
       while( n > 0) {
           indent += "  ";
           n--;
       }
       return indent;
    }
    
    /** Statement node representing an erroneous statement. */
    public static class ErrorNode extends StatementNode {
        public ErrorNode( Location loc ) {
            super( loc );
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitStatementErrorNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitStatementErrorNode( this );
        }
        @Override
        public String toString( int level) {
            return "ERROR";
        }
    }

    /** Node representing a Block consisting of declarations and
     * body of a procedure, function, or the main program. */
    public static class BlockNode extends StatementNode {
        private DeclNode.DeclListNode procedures; // declared within block
        private StatementNode body; // compound statement body
        private Scope blockLocals;  // scope of locals within block

        /** Constructor for a block within a procedure */
        public BlockNode( Location loc, DeclNode.DeclListNode procedures, 
                StatementNode body, Scope blockLocals) {
            super( loc );
            this.procedures = procedures;
            this.body = body;
            this.blockLocals = blockLocals;
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitBlockNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitBlockNode( this );
        }

        public DeclNode.DeclListNode getProcedures() {
            return procedures;
        }
        public StatementNode getBody() {
            return body;
        }
        public Scope getBlockLocals() {
            return blockLocals;
        }
        @Override
        public String toString( int level ) {
            return getProcedures().toString(level+1) + 
                    newLine(level) + "BEGIN" + 
                    newLine(level+1) + body.toString(level+1) +
                    newLine(level) + "END";
        }
    }

    /** Tree node representing an assignment statement. */
    public static class AssignmentNode extends StatementNode {
    	 /** Tree node for expression on left hand side of an assignment. */
        private ExpNode lValue;
        /** Tree node for the expression to be assigned. */
        private ExpNode exp;
        /** Tree node for expression on left hand side of an assignment. */
        private List<ExpNode> lValues;
        /** Tree node for the expression to be assigned. */
        private List<ExpNode> exps;

        public AssignmentNode( Location loc, ExpNode lValue, ExpNode exp ) {
            super( loc );
            this.lValue = lValue;
            this.exp = exp;
        }
        public AssignmentNode( Location loc, List<ExpNode> lValues, List<ExpNode> exps ) {
            super( loc );
            this.lValues = lValues;
            this.exps = exps;
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitAssignmentNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitAssignmentNode( this );
        }
        public ExpNode getVariable() {
            return lValue;
        }
        public void setVariable( ExpNode variable ) {
            this.lValue = variable;
        }
        public ExpNode getExp() {
            return exp;
        }
        public void setExp(ExpNode exp) {
            this.exp = exp;
        }
        public String getVariableName() {
            if( lValue instanceof ExpNode.VariableNode ) {
                return 
                    ((ExpNode.VariableNode)lValue).getVariable().getIdent();
            } else {
                return "<noname>";
            }
        }
        
        public List<ExpNode> getLValues() {
			return lValues;
		}
		public void setlValues(List<ExpNode> lValues) {
			this.lValues = lValues;
		}
		public List<ExpNode> getExps() {
			return exps;
		}
		public void setExps(List<ExpNode> exps) {
			this.exps = exps;
		}
		@Override
        public String toString( int level ) {
            return lValue.toString() + " := " + exp.toString();
        }
    }
    /** Tree node representing a list of assignment statement. */
    public static class AssignmentListNode extends StatementNode {
        /** Tree node for expression on left hand side of an assignment. */
        private List<ExpNode> lValues;
        private List<ExpNode> exps;

        public AssignmentListNode( Location loc ) {
            super( loc );
            this.lValues = new ArrayList<>();
            this.exps = new ArrayList<>();
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitAssignmentListNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitAssignmentListNode( this );
        }
        public void addLValue( ExpNode lValue ) {
        	lValues.add( lValue );
        }
        public void addExp( ExpNode exp ) {
        	exps.add( exp );
        }
        
        public List<ExpNode> getLValues() {
			return lValues;
		}
		public void setlValues(List<ExpNode> lValues) {
			this.lValues = lValues;
		}
		public List<ExpNode> getExps() {
			return exps;
		}
		public void setExps(List<ExpNode> exps) {
			this.exps = exps;
		}
		public ExpNode getVariable(int i) {
            return lValues.get(i);
        }
        public void setVariable( ExpNode variable, int i) {
        	lValues.set(i, variable);
        }
        public ExpNode getExp(int i) {
            return exps.get(i);
        }
        public void setExp(ExpNode exp, int i) {
            this.exps.set(i, exp);
        }
        public String getVariableName(int i) {
        	ExpNode lValue = lValues.get(i);
            if( lValue instanceof ExpNode.VariableNode ) {
                return 
                    ((ExpNode.VariableNode)lValue).getVariable().getIdent();
            } else {
                return "<noname>";
            }
        }
		@Override
        public String toString( int level ) {
        	
            return "";
        }
    }
    /** Tree node representing a "write" statement. */
    public static class WriteNode extends StatementNode {
        private ExpNode exp;

        public WriteNode( Location loc, ExpNode exp ) {
            super( loc );
            this.exp = exp;
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitWriteNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitWriteNode( this );
        }
        public ExpNode getExp() {
            return exp;
        }
        public void setExp( ExpNode exp ) {
            this.exp = exp;
        }
        @Override
        public String toString( int level ) {
            return "WRITE " + exp.toString();
        }
    }
    /** Tree node representing a "call" statement. */
    public static class CallNode extends StatementNode {
        private String id;
        private SymEntry.ProcedureEntry procEntry;
        public CallNode( Location loc, String id ) {
            super( loc );
            this.id = id;
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitCallNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitCallNode( this );
        }
        public String getId() {
            return id;
        }
        public SymEntry.ProcedureEntry getEntry() {
            return procEntry;
        }
        public void setEntry(SymEntry.ProcedureEntry entry) {
            this.procEntry = entry;
        }
        @Override
        public String toString( int level ) {
            String s = "CALL " + id;
            return s + ")";
        }
    }
    /** Tree node representing a statement list. */
    public static class ListNode extends StatementNode {
        private List<StatementNode> statements;
        
        public ListNode( Location loc ) {
            super( loc );
            this.statements = new ArrayList<StatementNode>();
        }
        public void addStatement( StatementNode s ) {
            statements.add( s );
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitStatementListNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitStatementListNode( this );
        }
        public List<StatementNode> getStatements() {
            return statements;
        }
        @Override
        public String toString( int level) {
            String result = "";
            String sep = "";
            for( StatementNode s : statements ) {
                result += sep + s.toString( level );
                sep = ";" + newLine(level);
            }
            return result;
        }
    }
    /** Tree node representing an "if" statement. */
    public static class IfNode extends StatementNode {
        private ExpNode condition;
        private StatementNode thenStmt;
        private StatementNode elseStmt;

        public IfNode( Location loc, ExpNode condition, 
                StatementNode thenStmt, StatementNode elseStmt ) {
            super( loc );
            this.condition = condition;
            this.thenStmt = thenStmt;
            this.elseStmt = elseStmt;
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitIfNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitIfNode( this );
        }
        public ExpNode getCondition() {
            return condition;
        }
        public void setCondition( ExpNode cond ) {
            this.condition = cond;
        }
        public StatementNode getThenStmt() {
            return thenStmt;
        }
        public StatementNode getElseStmt() {
            return elseStmt;
        }
        @Override
        public String toString( int level ) {
            return "IF " + condition.toString() + " THEN" + 
                        newLine(level+1) + thenStmt.toString( level+1 ) + 
                    newLine( level ) + "ELSE" + 
                        newLine(level+1) + elseStmt.toString( level+1 );
        }
    }

    /** Tree node representing a "while" statement. */
    public static class WhileNode extends StatementNode {
        private ExpNode condition;
        private StatementNode loopStmt;

        public WhileNode( Location loc, ExpNode condition, 
              StatementNode loopStmt ) {
            super( loc );
            this.condition = condition;
            this.loopStmt = loopStmt;
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitWhileNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitWhileNode( this );
        }
        public ExpNode getCondition() {
            return condition;
        }
        public void setCondition( ExpNode cond ) {
            this.condition = cond;
        }
        public StatementNode getLoopStmt() {
            return loopStmt;
        }
        @Override
        public String toString( int level ) {
            return "WHILE " + condition.toString() + " DO" +
                newLine(level+1) + loopStmt.toString( level+1 );
        }
    }
    
    /** Tree node representing an "if" statement. */
    public static class SkipNode extends StatementNode {

        public SkipNode( Location loc ) {
            super( loc );  
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitSkipNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitSkipNode( this );
        }
        @Override
        public String toString( int level ) {
            return "SKIP";
        }
    }
    
    /** Tree node representing a "do" statement. */
    public static class DoBranchNode extends StatementNode {
        private ExpNode condition;
        private StatementNode stmt;
        private int flag;	
       
        public DoBranchNode( Location loc, ExpNode condition, 
              StatementNode stmt, int flag ) {
            super( loc );
            this.condition = condition;
            this.stmt = stmt;
            this.flag = flag;
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitDoBranchNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitDoBranchNode( this );
        }
        public ExpNode getCondition() {
            return condition;
        }
        public void setCondition( ExpNode cond ) {
            this.condition = cond;
        }
        public int getFlag() {
        	return flag;
        }
        public StatementNode getStmt() {
            return stmt;
        }
        @Override
        public String toString( int level ) {
            return condition.toString() + " THEN " + 
            		stmt.toString() + " BREAK";
        }
    }
    
    /** Tree node representing a "do" statement. */
    public static class DoStatementNode extends StatementNode {
    	private List<DoBranchNode> branches;

        public DoStatementNode( Location loc ) {
            super( loc );
            this.branches = new ArrayList<>();;
        }
        public void addBranch( DoBranchNode b ) {
        	branches.add( b );
        }
        @Override
        public void accept( StatementVisitor visitor ) {
            visitor.visitDoStatementNode( this );
        }
        @Override
        public Code genCode( StatementTransform<Code> visitor ) {
            return visitor.visitDoStatementNode( this );
        }
        public List<DoBranchNode> getBranches() {
            return branches;
        }
        @Override
        public String toString( int level) {
            String result = "";
            String sep = "DO ";
            for( DoBranchNode b : branches ) {
                result += sep + b.toString( level ) + newLine(level);
                sep = "[] " ;
            }
            result += "OD";
            return result;
        }
    }
}

