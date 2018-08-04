package tree;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import source.Errors;
import java_cup.runtime.ComplexSymbolFactory.Location;
import syms.Predefined;
import syms.SymEntry;
import syms.SymEntry.OperatorEntry;
import syms.SymEntry.VarEntry;
import syms.Scope;
import syms.Type;
import syms.Type.FunctionType;
import syms.Type.IncompatibleTypes;
import syms.Type.ProductType;
import syms.Type.ReferenceType;
import syms.Type.ScalarType;
import tree.DeclNode.DeclListNode;
import tree.ExpNode.ArgumentsNode;
import tree.ExpNode.ArrayElementNode;
import tree.ExpNode.IdentifierNode;
import tree.ExpNode.OperatorNode;
import tree.StatementNode.*;

/**
 * class StaticSemantics - Performs the static semantic checks on the abstract
 * syntax tree using a visitor pattern to traverse the tree. See the notes on
 * the static semantics of PL0 to understand the PL0 type system in detail.
 */
public class StaticChecker implements DeclVisitor, StatementVisitor, ExpTransform<ExpNode> {

	/**
	 * The static checker maintains a reference to the current symbol table scope
	 * for the procedure currently being processed.
	 */
	private Scope currentScope;
	/** Errors are reported through the error handler. */
	private Errors errors;
	/** Track the tree node currently being checked (for debugging) */
	private Stack<String> nodeStack;

	/**
	 * Construct a static checker for PL0.
	 * 
	 * @param errors
	 *            is the error message handler.
	 */
	public StaticChecker(Errors errors) {
		super();
		this.errors = errors;
		nodeStack = new Stack<String>();
	}

	/**
	 * The tree traversal starts with a call to visitProgramNode. Then its
	 * descendants are visited using visit methods for each node type, which are
	 * called using the visitor pattern "accept" method (or "transform" for
	 * expression nodes) of the abstract syntax tree node.
	 */
	public void visitProgramNode(DeclNode.ProcedureNode node) {
		beginCheck("Program");
		// The main program is a special case of a procedure
		visitProcedureNode(node);
		endCheck("Program");
	}

	/** Procedure, function or main program node */
	public void visitProcedureNode(DeclNode.ProcedureNode node) {
		beginCheck("Procedure");
		SymEntry.ProcedureEntry procEntry = node.getProcEntry();
		// Set the current symbol table scope to that for the procedure.
		Scope localScope = procEntry.getLocalScope();
		// resolve all references to identifiers with the declarations
		localScope.resolveScope();
		// Enter the local scope
		currentScope = localScope;
		// Check the block of the procedure.
		visitBlockNode(node.getBlock());
		// Restore the symbol table to the parent scope (not really necessary)
		currentScope = currentScope.getParent();
		endCheck("Procedure");
	}

	/** Block node */
	public void visitBlockNode(BlockNode node) {
		beginCheck("Block");
		node.getProcedures().accept(this); // Check the procedures, if any
		node.getBody().accept(this); // Check the body of the block
		endCheck("Block");
	}

	/** Process the list of procedure declarations */
	public void visitDeclListNode(DeclListNode node) {
		beginCheck("DeclList");
		for (DeclNode declaration : node.getDeclarations()) {
			declaration.accept(this);
		}
		endCheck("DeclList");
	}

	/*************************************************
	 * Statement node static checker visit methods
	 *************************************************/
	public void visitStatementErrorNode(StatementNode.ErrorNode node) {
		beginCheck("StatementError");
		// Nothing to check - already invalid.
		endCheck("StatementError");
	}

	/** Assignment statement node */
	public void visitAssignmentNode(StatementNode.AssignmentNode node) {
		beginCheck("Assignment");
		ExpNode left = node.getVariable();
		// Check whether the left value is readOnly
		if (left instanceof IdentifierNode) {
			SymEntry entry = currentScope.lookup(((ExpNode.IdentifierNode) left).getId());
			if (entry instanceof SymEntry.VarEntry) {
				if (((VarEntry) entry).isReadOnly()) {
					staticError("can't assign to for loop control variable", 
							node.getVariable().getLocation());
				}
			}
		}
		// Check the left side left value.
		left = node.getVariable().transform(this);
		node.setVariable(left);
		// Check the right side expression.
		ExpNode exp = node.getExp().transform(this);
		node.setExp(exp);

		// Validate that it is a true left value and not a constant.
		Type lvalType = left.getType();
		if (!(lvalType instanceof Type.ReferenceType)) {
			if (lvalType != Type.ERROR_TYPE) {
				staticError("variable expected", left.getLocation());
			}
		} else {
			/*
			 * Validate that the right expression is assignment compatible with the left
			 * value. This may require that the right side expression is coerced to the
			 * dereferenced type of the left side LValue.
			 */
			Type baseType = ((Type.ReferenceType) lvalType).getBaseType();
			node.setExp(baseType.coerceExp(exp));
		}
		endCheck("Assignment");
	}

	/** Write statement node */
	public void visitWriteNode(StatementNode.WriteNode node) {
		beginCheck("Write");
		// Check the expression being written.
		ExpNode exp = node.getExp().transform(this);
		// coerce expression to be of type integer,
		// or complain if not possible.
		node.setExp(Predefined.INTEGER_TYPE.coerceExp(exp));
		endCheck("Write");
	}

	/** Call statement node */
	public void visitCallNode(StatementNode.CallNode node) {
		beginCheck("Call");
		SymEntry.ProcedureEntry procEntry = null;
		// Look up the symbol table entry for the procedure.
		SymEntry entry = currentScope.lookup(node.getId());
		if (entry instanceof SymEntry.ProcedureEntry) {
			procEntry = (SymEntry.ProcedureEntry) entry;
			node.setEntry(procEntry);
		} else {
			staticError("Procedure identifier required", node.getLocation());
			endCheck("Call");
			return;
		}
		endCheck("Call");
	}

	/** Statement list node */
	public void visitStatementListNode(StatementNode.ListNode node) {
		beginCheck("StatementList");
		for (StatementNode s : node.getStatements()) {
			s.accept(this);
		}
		endCheck("StatementList");
	}

	/** Check that the expression node can be coerced to boolean */
	private ExpNode checkCondition(ExpNode cond) {
		// Check and transform the condition
		cond = cond.transform(this);
		/*
		 * Validate that the condition is boolean, which may require coercing the
		 * condition to be of type boolean.
		 */
		return Predefined.BOOLEAN_TYPE.coerceExp(cond);
	}

	/** If statement node */
	public void visitIfNode(StatementNode.IfNode node) {
		beginCheck("If");
		// Check the condition.
		node.setCondition(checkCondition(node.getCondition()));
		node.getThenStmt().accept(this); // Check the 'then' part
		node.getElseStmt().accept(this); // Check the 'else' part.
		endCheck("If");
	}

	/** While statement node */
	public void visitWhileNode(StatementNode.WhileNode node) {
		beginCheck("While");
		// Check the condition
		node.setCondition(checkCondition(node.getCondition()));
		node.getLoopStmt().accept(this); // Check the body of the loop
		endCheck("While");
	}

	/** For statement node */
	public void visitForNode(StatementNode.ForNode node) {
		beginCheck("For");
		ExpNode exp = node.getExp();
		ExpNode cond1 = node.getCondition1().transform(this);
		ExpNode cond2 = node.getCondition2().transform(this);
		/* Get the base type of condition1. */
		Type coType = cond1.getType().optDereferenceType();
		coType = coType.optWidenSubrange();
		/* Coerce cond1 and cond2 to coType. */
		cond1 = coType.coerceExp(cond1);
		cond2 = coType.coerceExp(cond2);
		/* Check whether cond1 and cond2 have the same type. */
		if (!cond1.getType().equals(cond2.getType())) {
			staticError("types of upper and lower bounds must match", cond1.getLocation());
		}
		if (exp instanceof ExpNode.IdentifierNode) {
			/* Extend the current scope. */
			currentScope = currentScope.extendCurrentScope();
			/* Add control variable to the symbol table. */
			Type.ReferenceType refType = new Type.ReferenceType(cond1.getType());
			currentScope.addVariable(
					((ExpNode.IdentifierNode) exp).getId(), node.getLocation(), refType);
			/* Set control variable ReadOnly. */
			SymEntry.VarEntry entry = 
					(VarEntry) currentScope.lookup(((ExpNode.IdentifierNode) exp).getId());
			entry.setReadOnly(true);
			node.getLoopStmt().accept(this); // Check the body of the loop
			/* Set control variable not ReadOnly which enables it to increase. */
			entry.setReadOnly(false);
			node.setExp(exp.transform(this));
			node.setCondition1(cond1);
			node.setCondition2(cond2);
			/* Get out of the current scope. */
			currentScope = currentScope.getParent();
		}
		endCheck("For");
	}

	/*************************************************
	 * Expression node static checker visit methods. The static checking visitor
	 * methods for expressions transform the expression to include resolved
	 * identifier nodes, and add nodes like dereference nodes, and narrow and widen
	 * subrange nodes. These ensure that the transformed tree is type consistent.
	 *************************************************/
	public ExpNode visitErrorExpNode(ExpNode.ErrorNode node) {
		beginCheck("ErrorExp");
		// Nothing to do - already invalid.
		endCheck("ErrorExp");
		return node;
	}

	/** Constant expression node */
	public ExpNode visitConstNode(ExpNode.ConstNode node) {
		beginCheck("Const");
		// type already set up
		endCheck("Const");
		return node;
	}

	/** Reads an integer value from input */
	public ExpNode visitReadNode(ExpNode.ReadNode node) {
		beginCheck("Read");
		// type already set up
		endCheck("Read");
		return node;
	}

	/**
	 * Handles binary and unary operators, allowing the types of operators to be
	 * overloaded.
	 */
	public ExpNode visitOperatorNode(ExpNode.OperatorNode node) {
		beginCheck("Operator");
		/* Check the arguments to the operator */
		ExpNode arg = node.getArg().transform(this);
		OperatorEntry oe = currentScope.lookupOperator(node.getOp().getName());
		/* Add function type for SUCC_OP and PRED_OP. */
		if (node.getOp().compareTo(Operator.SUCC_OP) == 0 || 
				node.getOp().compareTo(Operator.PRED_OP) == 0) {
			Type argType = arg.getType();
			if (argType instanceof ReferenceType) {
				argType = ((ReferenceType) argType).getBaseType();
				oe.extendType(new FunctionType(argType, argType));
			}
		}
		/* Lookup the operator in the symbol table to get its type */
		Type opType = oe.getType();
		if (opType instanceof Type.FunctionType) {
			/*
			 * The operator is not overloaded. Its type is represented by a FunctionType
			 * from its argument's type to its result type.
			 */
			Type.FunctionType fType = (Type.FunctionType) opType;
			node.setArg(fType.getArgType().coerceExp(arg));
			node.setType(fType.getResultType());
		} else if (opType instanceof Type.IntersectionType) {
			/*
			 * The operator is overloaded. Its type is represented by an IntersectionType
			 * containing a set of possible types for the operator, each of which is a
			 * FunctionType. Each possible type is tried until one succeeds.
			 */
			errors.debugMessage("Coercing " + arg + " to " + opType);
			errors.incDebug();
			for (Type t : ((Type.IntersectionType) opType).getTypes()) {
				Type.FunctionType fType = (Type.FunctionType) t;
				Type opArgType = fType.getArgType();
				try {
					/*
					 * Coerce the argument to the argument type for this operator type. If the
					 * coercion fails an exception will be trapped and an alternative function 
					 * type within the intersection tried.
					 */
					ExpNode newArg = opArgType.coerceToType(arg);
					/* The coercion succeeded if we get here */
					node.setArg(newArg);
					node.setType(fType.getResultType());
					errors.decDebug();
					endCheck("Operator");
					return node;
				} catch (IncompatibleTypes ex) {
					// Allow "for" loop to try an alternative
					if (node.getArg() instanceof ArgumentsNode) {
						ExpNode argLeft = ((ArgumentsNode) node.getArg()).getLeft();
						ExpNode argright = ((ArgumentsNode) node.getArg()).getRight();
						/* Check base type of left argument and right argument. */
						if (argLeft != null && argright != null) {
							Type arglType = argLeft.getType();
							if (arglType instanceof ReferenceType) {
								arglType = ((ReferenceType) arglType).getBaseType();
							}
							Type argrType = argright.getType();
							if (argrType instanceof ReferenceType) {
								argrType = ((ReferenceType) argrType).getBaseType();
							}
							/* If left argument and right argument have same base type, 
							 * generate a new function type for the operator and coerce 
							 * the arguments. */
							if (arglType.equals(argrType)) {
								FunctionType extendType = new FunctionType(
										new ProductType(arglType, arglType), 
										Predefined.BOOLEAN_TYPE);
								Type extendArgType = extendType.getArgType();
								try {
									/*
									 * Coerce the argument to the argument type for this 
									 * operator type. If the coercion fails an exception 
									 * will be trapped and an alternative function type
									 * within the intersection tried.
									 */
									ExpNode newArg = extendArgType.coerceToType(arg);
									/* The coercion succeeded if we get here */
									node.setArg(newArg);
									node.setType(extendType.getResultType());
									errors.decDebug();
									endCheck("Operator");
									return node;
								} catch (IncompatibleTypes e) {
									// No alternatives.
								}
							}
						}
					}
				}
			}
			errors.decDebug();
			errors.debugMessage("Failed to coerce " + arg + " to " + opType);
			// no match in intersection type
			staticError("Type of argument " + arg.getType().getName() + 
					" does not match " + opType.getName(), node.getLocation());
			node.setType(Type.ERROR_TYPE);
		} else {
			errors.fatal("Invalid operator type", node.getLocation());
		}
		endCheck("Operator");
		return node;
	}

	/**
	 * An ArgumentsNode is used to represent a list of arguments, each of which is
	 * an expression. The arguments for a binary operator are represented by list
	 * with two elements.
	 */
	public ExpNode visitArgumentsNode(ExpNode.ArgumentsNode node) {
		beginCheck("Arguments");
		List<ExpNode> newExps = new LinkedList<ExpNode>();
		List<Type> types = new LinkedList<Type>();
		for (ExpNode exp : node.getArgs()) {
			ExpNode newExp = exp.transform(this);
			newExps.add(newExp);
			types.add(newExp.getType());
		}
		node.setArgs(newExps);
		node.setType(new Type.ProductType(types));
		endCheck("Arguments");
		return node;
	}

	/**
	 * A DereferenceNode allows a variable (of type ref(int) say) to be dereferenced
	 * to get its value (of type int).
	 */
	public ExpNode visitDereferenceNode(ExpNode.DereferenceNode node) {
		beginCheck("Dereference");
		// Check the left value referred to by this dereference node
		ExpNode lVal = node.getLeftValue().transform(this);
		node.setLeftValue(lVal);
		/*
		 * The type of the dereference node is the base type of its left value.
		 */
		Type lValueType = lVal.getType();
		if (lValueType instanceof Type.ReferenceType) {
			node.setType(lValueType.optDereferenceType()); // not optional here
		} else if (lValueType != Type.ERROR_TYPE) { // avoid cascading errors
			staticError("cannot dereference an expression which isn't a reference", 
					node.getLocation());
		}
		endCheck("Dereference");
		return node;
	}

	/**
	 * When parsing an identifier within an expression one can't tell whether it has
	 * been declared as a constant or an identifier. Here we check which it is and
	 * return either a constant or a variable node.
	 */
	public ExpNode visitIdentifierNode(ExpNode.IdentifierNode node) {
		beginCheck("Identifier");
		// First we look up the identifier in the symbol table.
		ExpNode newNode;
		SymEntry entry = currentScope.lookup(node.getId());
		if (entry instanceof SymEntry.ConstantEntry) {
			// Set up a new node which is a constant.
			debugMessage("Transformed " + node.getId() + " to Constant");
			SymEntry.ConstantEntry constEntry = (SymEntry.ConstantEntry) entry;
			newNode = new ExpNode.ConstNode(node.getLocation(), 
					constEntry.getType(), constEntry.getValue());
		} else if (entry instanceof SymEntry.VarEntry) {
			debugMessage("Transformed " + node.getId() + " to Variable");
			// Set up a new node which is a variable.
			SymEntry.VarEntry varEntry = (SymEntry.VarEntry) entry;
			newNode = new ExpNode.VariableNode(node.getLocation(), varEntry);
		} else {
			// Undefined identifier or a type or procedure identifier.
			// Set up new node to be an error node.
			newNode = new ExpNode.ErrorNode(node.getLocation());
			// System.out.println("Entry = " + entry);
			staticError("Constant or variable identifier required", node.getLocation());
		}
		endCheck("Identifier");
		return newNode;
	}

	/** Variable node is set up by Identifier node - no checks needed */
	public ExpNode visitVariableNode(ExpNode.VariableNode node) {
		beginCheck("Variable");
		// Type already set up
		endCheck("Variable");
		return node;
	}

	/** Narrow subrange node constructed during coerce - no checking needed */
	public ExpNode visitNarrowSubrangeNode(ExpNode.NarrowSubrangeNode node) {
		beginCheck("NarrowSubrange");
		// Nothing to do.
		endCheck("NarrowSubrange");
		return node;
	}

	/** Widen subrange node constructed during coerce - no checking needed */
	public ExpNode visitWidenSubrangeNode(ExpNode.WidenSubrangeNode node) {
		beginCheck("WidenSubrange");
		// Nothing to do.
		endCheck("WidenSubrange");
		return node;
	}

	/** Array element node used to represent an element in an array. */
	public ExpNode visitArrayElementNode(ArrayElementNode node) {
		beginCheck("ArrayElement");
		ExpNode lval = node.getlValue().transform(this);
		ExpNode exp = node.getExp().transform(this);
		Type lvalType = lval.getType();
		/* Check whether the left value is referenceType. */
		if (!(lvalType instanceof Type.ReferenceType)) {
			if (lvalType != Type.ERROR_TYPE) {
				staticError("variable expected", lval.getLocation());
			}
		} else {
			/* Set node type as the reference to the resultType of left value 
			 * if the left value is functionType. */
			Type baseType = ((Type.ReferenceType) lvalType).getBaseType();
			if (baseType instanceof FunctionType) {
				node.setType(new Type.ReferenceType(((FunctionType) baseType).getResultType()));
				node.setExp(((FunctionType) baseType).getArgType().coerceExp(exp));
			}else {
				staticError("Array expected", lval.getLocation());
			}
		}
		node.setlValue(lval);
		endCheck("ArrayElement");
		return node;
	}

	/**************************** Support Methods ***************************/
	/** Push current node onto debug rule stack and increase debug level */
	private void beginCheck(String nodeName) {
		nodeStack.push(nodeName);
		errors.debugMessage("Checking " + nodeName);
		errors.incDebug();
	}

	/** Pop current node from debug rule stack and decrease debug level */
	private void endCheck(String node) {
		errors.decDebug();
		errors.debugMessage("End check of " + node);
		if (nodeStack.isEmpty()) {
			errors.debugPrint("*** End of node " + node + " has no matching start");
		} else {
			String popped = nodeStack.pop();
			if (node != popped) {
				/** This indicates an error in the static checker - always prints */
				errors.debugPrint("*** End node " + node + " does not match start node " + popped);
			}
		}
	}

	/** Debugging message output */
	private void debugMessage(String msg) {
		errors.debugMessage(msg);
	}

	/** Error message handle for parsing errors */
	private void staticError(String msg, Location loc) {
		errors.debugMessage(msg);
		errors.error(msg, loc);
	}
}
