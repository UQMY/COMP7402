package tree;

/** 
 * interface StatementVisitor - Provides the interface for the visitor pattern 
 * to be applied to an abstract syntax tree node for a statement. 
 * A class implementing this interface (such as the code generator) must 
 * provide implementations for visit methods for each of the statement 
 * node type. 
 * For example, the visit methods provided by the code generator statement
 * visitor implement the code generation for each type of statement node. 
 */
public interface  StatementTransform<ResultType> {

    ResultType visitBlockNode( StatementNode.BlockNode node );
    
    ResultType visitStatementErrorNode( StatementNode.ErrorNode node );

    ResultType visitStatementListNode( StatementNode.ListNode node );

    ResultType visitAssignmentNode( StatementNode.AssignmentNode node);

    ResultType visitWriteNode( StatementNode.WriteNode node);

    ResultType visitCallNode( StatementNode.CallNode node);
    ResultType visitIfNode( StatementNode.IfNode node);

    ResultType visitWhileNode( StatementNode.WhileNode node);
    
    ResultType visitSkipNode( StatementNode.SkipNode node);
    
    ResultType visitDoStatementNode( StatementNode.DoStatementNode node);
    
    ResultType visitDoBranchNode( StatementNode.DoBranchNode node);
    
    ResultType visitAssignmentListNode( StatementNode.AssignmentListNode node);
}
