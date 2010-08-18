package com.redhat.ceylon.compiler.codegen;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.lang.model.element.TypeElement;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;

import com.redhat.ceylon.compiler.parser.CeylonParser;
import com.sun.tools.javac.api.JavacTaskImpl;
import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.comp.Resolve;
import com.sun.tools.javac.file.JavacFileManager;
import com.sun.tools.javac.jvm.ClassReader;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.JCTree.JCAnnotation;
import com.sun.tools.javac.tree.JCTree.JCBlock;
import com.sun.tools.javac.tree.JCTree.JCClassDecl;
import com.sun.tools.javac.tree.JCTree.JCCompilationUnit;
import com.sun.tools.javac.tree.JCTree.JCExpression;
import com.sun.tools.javac.tree.JCTree.JCFieldAccess;
import com.sun.tools.javac.tree.JCTree.JCLiteral;
import com.sun.tools.javac.tree.JCTree.JCMethodDecl;
import com.sun.tools.javac.tree.JCTree.JCStatement;
import com.sun.tools.javac.tree.JCTree.JCTypeParameter;
import com.sun.tools.javac.tree.JCTree.JCVariableDecl;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javac.util.Name;
import com.sun.tools.javac.util.Names;
import com.sun.tools.javac.util.Options;
import com.sun.tools.javac.util.Position;

import static com.sun.tools.javac.code.Flags.*;
import static com.sun.tools.javac.code.TypeTags.*;
import com.redhat.ceylon.compiler.tree.CeylonTree;
import com.redhat.ceylon.compiler.tree.CeylonTree.*;

public class Gen {
    Context context;
    TreeMaker make;
    Names names;
    ClassReader reader;
    Resolve resolve;
    JavaCompiler compiler;
    DiagnosticCollector<JavaFileObject> diagnostics;
    JavacFileManager fileManager;
    JavacTaskImpl task;
    Options options;
    
    JCCompilationUnit jcCompilationUnit;

    public Gen() throws Exception {
        compiler = ToolProvider.getSystemJavaCompiler();
        diagnostics
        = new DiagnosticCollector<JavaFileObject>();
        fileManager
        = (JavacFileManager)compiler.getStandardFileManager(diagnostics, null, null);
        fileManager.setLocation(StandardLocation.CLASS_OUTPUT,
                Arrays.asList(new File("/tmp")));

        fileManager.setLocation(StandardLocation.CLASS_PATH,
                Arrays.asList(new File("/tmp"), new File(System.getProperty("user.dir") + "/runtime")));
        Iterable<? extends JavaFileObject> compilationUnits
        = fileManager.getJavaFileObjectsFromStrings(new ArrayList<String>());

        JavaCompiler.CompilationTask aTask
        = compiler.getTask(null, fileManager,
                diagnostics,
                Arrays.asList("-g", /* "-verbose", */
                        "-source", "7", "-XDallowFunctionTypes"),
                        null, compilationUnits);
        setup((JavacTaskImpl)aTask);
    }

    void setup (JavacTaskImpl task) {
        this.task = task;

        context = task.getContext();
        options = Options.instance(context);
        // It's a bit weird to see "invokedynamic" set here,
        // but it has to be done before Resolve.instance().
        options.put("invokedynamic", "invokedynamic");
        make = TreeMaker.instance(context);
        names = Names.instance(context);
        reader = ClassReader.instance(context);
        resolve = Resolve.instance(context);
    }

    class Singleton<T> implements Iterable<T>{
        private T thing;
        Singleton() { }
        List<T> asList() { return List.of(thing); }
        void append(T t) {
            if (thing != null)
                throw new RuntimeException();
            thing = t;
        }
        public Iterator<T> iterator() {
            return asList().iterator();
        }
        public T thing() { return this.thing; }
    }
    
    JCFieldAccess makeSelect(JCExpression s1, String s2) {
        return make.Select(s1, names.fromString(s2));
    }

    JCFieldAccess makeSelect(String s1, String s2) {
        return makeSelect(make.Ident(names.fromString(s1)), s2);
    }

    JCFieldAccess makeSelect(String s1, String s2, String... rest) {
        return makeSelect(makeSelect(s1, s2), rest);
    }

    JCFieldAccess makeSelect(JCFieldAccess s1, String[] rest) {
        JCFieldAccess acc = s1;

        for (String s : rest)
            acc = makeSelect(acc, s);

        return acc;
    }
    
    // Make a name from a list of strings, using only the first component. 
    Name makeName(Iterable<String> components) {
        Iterator<String> iterator = components.iterator();
        String s = iterator.next();
        assert(!iterator.hasNext());
        return names.fromString(s);
    }
    
    String toFlatName(Iterable<String> components) {
        StringBuffer buf = new StringBuffer();
        Iterator<String> iterator;
        String s;
        
        for (iterator = components.iterator();
            iterator.hasNext();) {
            buf.append(iterator.next());
            if (iterator.hasNext())
                buf.append('.');
        }
        
        return buf.toString();
    }
    
    public JCExpression makeIdent(Iterable<String> components) {

        JCExpression type = null;
        for (String component : components) {
            if (type == null)
                type = make.Ident(names.fromString(component));
            else
                type = make.Select(type, names.fromString(component));
        }
        
        return type;
    }
    
    public void run(CeylonTree.CompilationUnit t) throws IOException {        
        JCCompilationUnit tree = convert(t);
        tree.sourcefile = new CeylonFileObject(fileManager.getFileForInput(t.source.path));

        Iterable<? extends TypeElement> result =
            task.enter(List.of(tree));
        /*Iterable<? extends JavaFileObject> files =*/ task.generate(result);

        System.out.println(diagnostics.getDiagnostics());
    }
    
    public JCCompilationUnit convert(CeylonTree.CompilationUnit t) {
        final ListBuffer<JCTree> defs = new ListBuffer<JCTree>();
        
        defs.append(make.Import(makeIdent(Arrays.asList("ceylon", "*")), false));
                
        t.visitChildren(new CeylonTree.Visitor () {
            public void visit(CeylonTree.ClassDeclaration decl) {
                defs.append(convert(decl));
            }
            public void visit(CeylonTree.MethodDeclaration decl) {
                // This is a top-level method.  Generate a class with the
                // name of the method and a corresponding run() method.
                
                final ListBuffer<JCVariableDecl> params = 
                    new ListBuffer<JCVariableDecl>();
                final ListBuffer<JCStatement> annotations = 
                    new ListBuffer<JCStatement>();
                final Singleton<JCBlock> body = 
                    new Singleton<JCBlock>();
                Singleton<JCExpression> restype =
                    new Singleton<JCExpression>();
                
                processMethodDeclaration(decl, params, body, restype, (ListBuffer<JCTypeParameter>)null,
                        annotations);
                
                JCMethodDecl meth = make.MethodDef(make.Modifiers(PUBLIC|STATIC),
                        names.fromString("run"),
                        make.TypeIdent(VOID),
                        List.<JCTypeParameter>nil(),
                        params.toList(),
                        List.<JCExpression>nil(), body.thing(), null);
                
                meth.setPos(Position.encodePosition(decl.source.line, decl.source.column));
                
                List<JCTree> innerDefs = List.<JCTree>of(meth);
                
                // FIXME: This is wrong because the annotation registration is done
                // within the scope of the class, but the annotations are lexically
                // outside it.
                if (annotations.length() > 0) {
                    innerDefs = innerDefs.append(registerAnnotations(annotations.toList()));
                }
                
                JCClassDecl classDef = 
                    make.ClassDef(make.Modifiers(PUBLIC, List.<JCAnnotation>nil()),
                            names.fromString(decl.nameAsString()),
                            List.<JCTypeParameter>nil(), null,
                            List.<JCExpression>nil(),
                            innerDefs);
                
                defs.append(classDef);
            }
        });

        JCCompilationUnit topLev =
            make.TopLevel(List.<JCTree.JCAnnotation>nil(),
                    /* package id*/ null, defs.toList());

        System.out.println(topLev);
        return topLev;
    }

    // FIXME: There must be a better way to do this.
    void processMethodDeclaration(CeylonTree.MethodDeclaration decl, 
            final ListBuffer<JCVariableDecl> params, 
            final Singleton<JCBlock> block,
            final Singleton<JCExpression> restype,
            final ListBuffer<JCTypeParameter> typarams,
            final ListBuffer<JCStatement> annotations) {

        System.err.println(decl);
        
        for (FormalParameter param: decl.params) {
            params.append(convert(param));
        }
        
        for (CeylonTree stmt: decl.stmts)
            stmt.accept(new CeylonTree.Visitor () {
                public void visit(CeylonTree.Block b) {
                    block.thing = convert(b);
                }
            });
        
        for (CeylonTree.Annotation a: decl.annotations) {
            a.accept(new CeylonTree.Visitor () {
                public void visit(CeylonTree.UserAnnotation userAnn) {
                    annotations.append(make.Exec(convert(userAnn)));
                }
                public void visit(CeylonTree.LanguageAnnotation langAnn) {
                    // FIXME
                }
            });
        }
    }                

    JCBlock registerAnnotations(List<JCStatement> annos) {
        JCBlock block = make.Block(Flags.STATIC, annos);
        return block;        
    }
    
   class ExpressionVisitor extends CeylonTree.Visitor {
        public JCExpression result;
    }
    class ListVisitor<T> extends CeylonTree.Visitor {
        public List<T> result = List.<T>nil();
    }
    
    JCExpression convert(CeylonTree.UserAnnotation userAnn) {
       List<JCExpression> values = List.<JCExpression>nil();
        for (CeylonTree expr: userAnn.values()) {
            values = values.append(convertExpression(expr));
        }
        return make.Apply(null, makeSelect(userAnn.name, "run"),
                values);
    }
    
    JCExpression convert(CeylonTree.ReflectedLiteral value) {
        
        ListVisitor<String> v = new ListVisitor<String>() {
            public void visit(CeylonTree.Type type) {
                TypeName name = type.name();
                for (String component: name.components) {
                    result = result.append(component);
                }
            }
            public void visit(CeylonTree.MemberName name) {
                result = result.append(name.name);
            }
        };
        
        for (CeylonTree op: value.operands())
            op.accept(v);

        JCExpression result;

        if (Character.isUpperCase(v.result.last().charAt(0))) {
            // This looks like something of a kludge, but I think
            // it's a legitimate way to determine if this is the
            // name of a class.

            v.result = v.result.append("class");
            result = makeIdent(v.result);
       } else {
           // In the case of method literals, we're going to do this lazily.
           // To do otherwise would be very expensive
           
            result = make.Apply (null, makeSelect("ceylon", "Method", "instance"),
                    List.<JCExpression>of(
                            make.Literal(toFlatName(v.result))));
        }

        result.setPos(Position.encodePosition(value.source.line, value.source.column));
        return result;
    }
    
    JCVariableDecl convert(CeylonTree.FormalParameter param) {
        Name name = names.fromString(param.name());
        
        JCVariableDecl v = make.VarDef(make.Modifiers(0), name,
                variableType(param.type()), null);
        
        return v;
    }

    JCExpression variableType(CeylonTree.Type t) {
        final Singleton<JCExpression>result =
            new Singleton<JCExpression>();
        t.visitChildren(new CeylonTree.Visitor () {
            public void visit(CeylonTree.Void v) {
                result.thing = make.TypeIdent(VOID);
            }
            public void visit(CeylonTree.TypeName name) {
                result.thing = makeIdent(name.components());
            }
        });
        return result.thing();
    }
    
    public JCClassDecl convert(CeylonTree.ClassDeclaration cdecl) {
        final ListBuffer<JCVariableDecl> params = 
            new ListBuffer<JCVariableDecl>();
        
        cdecl.visitChildren(new CeylonTree.Visitor () {
            public void visit(CeylonTree.FormalParameter param) {
                JCExpression vartype = makeIdent(param.type().name().components());
                JCVariableDecl var = make.VarDef(make.Modifiers(PUBLIC), makeName(param.names), vartype, null);
                System.out.println(var);
                params.append(var);
            }
            
            public void visit(CeylonTree.Block b) {
                
            }
        });
        
        JCClassDecl classDef = 
            make.ClassDef(make.Modifiers(PUBLIC, List.<JCTree.JCAnnotation>nil()),
                    names.fromString(cdecl.nameAsString()),
                    List.<JCTypeParameter>nil(), null,
                    List.<JCExpression>nil(),
                    List.<JCTree>nil());

        System.out.println(classDef);

        return classDef;
    }

    public JCBlock convert(CeylonTree.Block block) {
        final ListBuffer<JCStatement> stmts =
            new ListBuffer<JCStatement>();
        
        for (CeylonTree stmt: block.getStmts()) 
            stmt.accept(new CeylonTree.Visitor () {
                public void visit(CeylonTree.CallExpression expr) {
                    stmts.append(make.Exec(convert(expr)));
            }});
        
        return make.Block(0, stmts.toList());
    }

    JCExpression convert(CeylonTree.CallExpression ce) {
        final Singleton<JCExpression> expr =
            new Singleton<JCExpression>();
        final ListBuffer<JCExpression> args =
            new ListBuffer<JCExpression>();
        
        ce.getMethod().accept (new CeylonTree.Visitor () {
            public void visit(CeylonTree.OperatorDot access) {
                expr.append(convert(access));
            }});
        
        for (CeylonTree arg: ce.args())
            args.append(convertArg(arg));
          
        JCExpression call = make.Apply(null, expr.thing(), args.toList());
        call.setPos(Position.encodePosition(ce.source.line, ce.source.column));
        return call;
    }
    
    JCExpression convertArg(CeylonTree arg) {
        return convertExpression(arg);
    }
    
    JCExpression convert(CeylonTree.SimpleStringLiteral string) {
        String s = string.value;
        JCLiteral lit = make.Literal (s);
        return make.Apply (null, makeSelect("ceylon", "String", "instance"),
                List.<JCExpression>of(lit));
    }
    
    JCExpression convert(CeylonTree.OperatorDot access)
    {
        final CeylonTree.Name memberName = access.memberName();
        final CeylonTree operand = access.operand();
        
        class V extends CeylonTree.Visitor {
            public JCExpression result;
            public void visit(CeylonTree.MemberName op) {
                result = makeIdent(Arrays.asList(op.name, memberName.name));
            }
            public void visit(CeylonTree.OperatorDot op) {
                result = make.Select(convert(op), names.fromString(memberName.name));
            }
            public void visit(CeylonTree.Operator op) {
                result = make.Select(convertExpression(op), names.fromString(memberName.name));
            }
        }

        V v = new V();
        operand.accept(v);
        return v.result;
    }   

    JCExpression convertExpression(CeylonTree expr) {
        class V extends CeylonTree.Visitor {
            public JCExpression result;

            public void visit(OperatorDot access) {
                result = convert(access);
            }
            public void visit(Operator op) {
                result = convert(op);
            }
            public void visit(NaturalLiteral lit) {
                JCExpression n = make.Literal(lit.value.longValue());
                result = make.Apply (null, makeSelect("ceylon", "Integer", "instance"),
                        List.of(n));
            }
            public void visit(CeylonTree.SimpleStringLiteral string) {
                result = convert(string);
            }
            public void visit(CeylonTree.CallExpression call) {
                result = convert(call);
            }
            public void visit(CeylonTree.ReflectedLiteral value) {
                result = convert(value);
            }
        }

        V v = new V();
        expr.accept(v);
        return v.result;
    }

    private static Map<Integer, String> operatorImplementors;

    static {
        operatorImplementors = new HashMap<Integer, String>();

        operatorImplementors.put(CeylonParser.PLUS,       "operatorAdd");
        operatorImplementors.put(CeylonParser.MINUS,      "operatorSubtract");
        operatorImplementors.put(CeylonParser.TIMES,      "operatorMultiply");
        operatorImplementors.put(CeylonParser.POWER,      "operatorPower");
        operatorImplementors.put(CeylonParser.DIVIDED,    "operatorDivide");
        operatorImplementors.put(CeylonParser.REMAINDER,  "operatorModulo");
        operatorImplementors.put(CeylonParser.BITWISEAND, "operatorBitwiseAnd");
        operatorImplementors.put(CeylonParser.BITWISEOR,  "operatorBitwiseOr");
        operatorImplementors.put(CeylonParser.BITWISEXOR, "operatorBitwiseXor");
        operatorImplementors.put(CeylonParser.EQEQ,       "operatorEqual");
        operatorImplementors.put(CeylonParser.IDENTICAL,  "operatorIdentical");
        operatorImplementors.put(CeylonParser.NOTEQ,      "operatorNotEqual");
        operatorImplementors.put(CeylonParser.LT,         "operatorLessThan");
        operatorImplementors.put(CeylonParser.GT,         "operatorGreaterThan");
        operatorImplementors.put(CeylonParser.LTEQ,       "operatorLessEqual");
        operatorImplementors.put(CeylonParser.GTEQ,       "operatorGreaterEqual");
        operatorImplementors.put(CeylonParser.COMPARE,    "operatorCompare");
    }

    JCExpression convert(CeylonTree.Operator op) {
        JCExpression result;
        CeylonTree[] operands = op.toArray();

        switch (op.operatorKind) {
        case CeylonParser.PLUS:
        case CeylonParser.MINUS:
        case CeylonParser.TIMES:
        case CeylonParser.POWER:
        case CeylonParser.DIVIDED:
        case CeylonParser.REMAINDER:
        case CeylonParser.BITWISEAND:
        case CeylonParser.BITWISEOR:
        case CeylonParser.BITWISEXOR:
        case CeylonParser.EQEQ:
        case CeylonParser.IDENTICAL:
        case CeylonParser.NOTEQ:
        case CeylonParser.LT:
        case CeylonParser.GT:
        case CeylonParser.LTEQ:
        case CeylonParser.GTEQ:
        case CeylonParser.COMPARE:
            result = make.Apply(null,
                                make.Select(convertExpression(operands[0]),
                                            names.fromString(operatorImplementors.get(op.operatorKind))),
                                List.of(convertExpression(operands[1])));
            break;

        default:
            throw new RuntimeException(CeylonParser.tokenNames[op.operatorKind]);
        }
        return result;
    }

}

