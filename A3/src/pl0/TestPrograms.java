package pl0;

import machine.StackMachine;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java_cup.runtime.ComplexSymbolFactory;
import parser.CUPParser;
import source.ErrorHandler;
import source.Source;
import tree.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Test that all programs in test-pgm produce the same output as in test-pgm/results.
 */
@RunWith(Parameterized.class)
public class TestPrograms {
    /* Work around the global error handler */
    private static final ErrorHandler errorHandler = new ErrorHandler(System.out,
            new Source(new ByteArrayInputStream(new byte[] {}), "no such file"), false);

    private final File program;

    public TestPrograms(File program) {
        this.program = program;
    }

    @Test
    /* Bizarrely, IntelliJ's JUnit4 runner appears to require the test method to be named "suite" ! */
    public void suite() throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos, true, "utf-8");
        Source src = new Source(program.getCanonicalPath());
        errorHandler.resetErrorHandler(ps, src, false);

        DeclNode.ProcedureNode tree = null;
        Procedures code = null;

        ps.println("Compiling " + program.getName());

        ComplexSymbolFactory csf = new ComplexSymbolFactory();
        /* Set up the lexical analyzer using the source program stream */
        parser.Lexer lex = new parser.Lexer(src, csf);
        /** Generated parser.
         * Set up the parser with the lexical analyzer. */
        Object parseResult;
        CUPParser parser = new CUPParser(lex, csf);
        parseResult = parser.parse().value;
        /* Flush any error messages from the parse */
        errorHandler.flush();
        ps.println("Parsing complete");
        if (parseResult instanceof DeclNode.ProcedureNode) {
                tree = (DeclNode.ProcedureNode) parseResult;
        
                StaticChecker staticSemantics = new StaticChecker(errorHandler);
                staticSemantics.visitProgramNode(tree);
                errorHandler.flush();
                ps.println("Static semantic analysis complete");

                if (!errorHandler.hadErrors()) {
                    CodeGenerator codeGen = new CodeGenerator(errorHandler);
                    code = codeGen.generateCode(tree);
                    errorHandler.flush();
                    ps.println("Code generation complete");
                }
        }

        errorHandler.errorSummary();

        if (code != null && !errorHandler.hadErrors()) {
            StackMachine machine;
            machine = new StackMachine(errorHandler, ps, false, code);
            ps.println("Running ...");
            machine.run();
            errorHandler.flush();
        }
            /* Compare the accumulated output */
        String output = new String(baos.toByteArray(), StandardCharsets.UTF_8);
        String result = slurp(resultFile(program));
        assertEquals("The outputs do not match", result, output);
    }

    @Parameterized.Parameters(name="{0}")
    public static List<?> testPrograms() throws IOException {
        File[] tests = new File("test-pgm")
                .listFiles(f -> f.isFile() && f.getName().endsWith(".pl0"));

        if (tests == null) {
            tests = new File[] {};
        }
        return Arrays.asList(tests);
    }

    private static File resultFile(File src) throws IOException {
        File parent = src.getParentFile();
        String path = parent == null ? "" : parent.getCanonicalPath() + "/";
        return new File(path + "results/r-" + src.getName());
    }

    private String slurp(File src) throws IOException {
        return new String(Files.readAllBytes(src.toPath()), StandardCharsets.UTF_8);
    }
}
