package tree;
/**
 * interface ExpTransform - Handles visitor pattern for transforming expressions.
 */
public interface ExpTransform<ResultType> {
    ResultType visitErrorExpNode(ExpNode.ErrorNode node);
    ResultType visitConstNode(ExpNode.ConstNode node);
    ResultType visitIdentifierNode(ExpNode.IdentifierNode node);
    ResultType visitVariableNode(ExpNode.VariableNode node);
    ResultType visitReadNode(ExpNode.ReadNode node);
    ResultType visitOperatorNode(ExpNode.OperatorNode node);
    ResultType visitArgumentsNode(ExpNode.ArgumentsNode node);
    ResultType visitDereferenceNode(ExpNode.DereferenceNode node);
    ResultType visitNarrowSubrangeNode(ExpNode.NarrowSubrangeNode node);
    ResultType visitWidenSubrangeNode(ExpNode.WidenSubrangeNode node);
}
