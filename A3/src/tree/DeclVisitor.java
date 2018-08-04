package tree;


/** 
 * interface DeclVisitor - Visitor pattern for declarations and procedures.
 * Provides the interface for the visitor pattern to be applied to an
 * abstract syntax tree. A class implementing this interface (such as the
 * static checker) must provide implementations for visit methods for
 * each of the tree node type. 
 * For example, the visit methods provided by the static checker tree
 * visitor implement the type checks for each type of tree node. 
 */
public interface DeclVisitor {

    void visitDeclListNode(DeclNode.DeclListNode node);

    void visitProcedureNode(DeclNode.ProcedureNode node);
}
