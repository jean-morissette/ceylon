package com.redhat.ceylon.compiler.codegen;

import static com.sun.tools.javac.code.TypeTags.VOID;

import com.redhat.ceylon.compiler.util.Util;
import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.TypeTags;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javac.util.Name;

/**
 * Builds a class for global variables. See {@link GlobalTransformer} for an overview.
 *
 * The generated class can be customized by calling methods of this class.
 */
public class AttributeDefinitionBuilder {
    private final Name fieldName;

    private final JCTree.JCExpression variableType;
    private final String variableName;

    private long classFlags;

    private boolean readable = true;
    private long getterFlags;
    private JCTree.JCBlock getterBlock;

    private JCTree.JCExpression variableInit;

    private boolean writable = true;
    private long setterFlags;
    private JCTree.JCBlock setterBlock;
    
    private List<JCTree.JCAnnotation> valueAnnotations = List.nil();
    private List<JCTree.JCAnnotation> classAnnotations = List.nil();

    private boolean skipConstructor;

    private AbstractTransformer owner;

    public AttributeDefinitionBuilder(AbstractTransformer owner, JCTree.JCExpression variableType, String variableName) {
        this.owner = owner;
        this.variableType = variableType;
        this.variableName = variableName;
        this.fieldName = owner.names().fromString("value");
    }

    /**
     * Generates the class and returns the generated tree.
     * @return the generated class tree, to be added to the appropriate {@link JCTree.JCCompilationUnit}.
     */
    public JCTree build() {
        ListBuffer<JCTree> defs = ListBuffer.lb();
        appendDefinitionsTo(defs);
        return owner.make().ClassDef(
                owner.make().Modifiers(classFlags, classAnnotations.prependList(owner.gen.makeAtCeylon())),
                getClassName(variableName),
                List.<JCTree.JCTypeParameter>nil(),
                null,
                List.<JCTree.JCExpression>nil(),
                defs.toList());
    }

    /**
     * Appends to <tt>defs</tt> the definitions that would go into the class generated by {@link #build()}
     * @param defs a {@link ListBuffer} to which the definitions will be appended.
     */
    public void appendDefinitionsTo(ListBuffer<JCTree> defs) {
        if (getterBlock == null) {
            defs.append(generateField());
        }

        if (readable) {
            defs.append(generateGetter());
        }

        if (writable) {
            defs.append(generateSetter());
        }
        
        if(!skipConstructor){
            // make a private constructor
            defs.append(owner.make().MethodDef(owner.make().Modifiers(Flags.PRIVATE),
                    owner.names().init,
                    owner.make().TypeIdent(VOID),
                    List.<JCTree.JCTypeParameter>nil(),
                    List.<JCTree.JCVariableDecl>nil(),
                    List.<JCTree.JCExpression>nil(),
                    owner.make().Block(0, List.<JCTree.JCStatement>nil()),
                    null));
        }
    }

    private JCTree generateField() {
        int flags = Flags.PRIVATE | Flags.STATIC;
        if (!writable) {
            flags |= Flags.FINAL;
        }

        return owner.make().VarDef(
                owner.make().Modifiers(flags),
                fieldName,
                variableType,
                variableInit
        );
    }

    private JCTree generateGetter() {
        JCTree.JCBlock body = getterBlock != null
                ? getterBlock
                : generateDefaultGetterBlock();

        return owner.make().MethodDef(
                owner.make().Modifiers(getterFlags, valueAnnotations),
                getGetterName(variableName),
                variableType,
                List.<JCTree.JCTypeParameter>nil(),
                List.<JCTree.JCVariableDecl>nil(),
                List.<JCTree.JCExpression>nil(),
                body,
                null
        );
    }

    private JCTree.JCBlock generateDefaultGetterBlock() {
        return owner.make().Block(0L, List.<JCTree.JCStatement>of(owner.make().Return(owner.make().Ident(fieldName))));
    }

    private JCTree generateSetter() {
        Name paramName = owner.names().fromString("newValue");

        JCTree.JCBlock body;
        if (getterBlock != null) {
            body = setterBlock != null ? setterBlock : createEmptyBlock();
        } else {
            body = generateDefaultSetterBlock(paramName);
        }
        return owner.make().MethodDef(
                owner.make().Modifiers(setterFlags),
                getSetterName(variableName),
                owner.make().TypeIdent(TypeTags.VOID),
                List.<JCTree.JCTypeParameter>nil(),
                List.<JCTree.JCVariableDecl>of(
                        owner.make().VarDef(owner.make().Modifiers(0, valueAnnotations), paramName, variableType, null)
                ),
                List.<JCTree.JCExpression>nil(),
                body,
                null
        );
    }

    private JCTree.JCBlock createEmptyBlock() {
        return owner.make().Block(0L, List.<JCTree.JCStatement>nil());
    }

    private JCTree.JCBlock generateDefaultSetterBlock(Name paramName) {
        return owner.make().Block(0L, List.<JCTree.JCStatement>of(
                owner.make().Exec(
                        owner.make().Assign(
                                owner.make().Ident(fieldName),
                                owner.make().Ident(paramName)))));
    }

    /**
     * Adds to the modifier flags of the generated class.
     * @param classFlags the modifier flags (see {@link Flags})
     * @return this instance for method chaining
     */
    public AttributeDefinitionBuilder classModifiers(long classFlags) {
        this.classFlags |= classFlags;
        return this;
    }
    
    private AttributeDefinitionBuilder classIs(boolean is, long mod) {
        if (is) {
            classModifiers(mod);
        } else {
            this.classFlags &= ~mod;
        }
        return this;
    }
    
    /**
     * Sets the class STATIC flag.
     * @param isStatic true to set the flag, false to clear it
     * @return this instance for method chaining
     * 
     * @see #classModifiers(long)
     */
    public AttributeDefinitionBuilder classIsStatic(boolean isStatic) {
        return classIs(isStatic, Flags.STATIC);
    }
    
    /**
     * Sets the class FINAL flag.
     * @param isFinal true to set the flag, false to clear it
     * @return this instance for method chaining
     * 
     * @see #classModifiers(long)
     */
    public AttributeDefinitionBuilder classIsFinal(boolean isFinal) {
        return classIs(isFinal, Flags.FINAL);
    }
    
    /**
     * Sets the class PUBLIC flag.
     * @param isPublic true to set the flag, false to clear it
     * @return this instance for method chaining
     * 
     * @see #classModifiers(long)
     */
    public AttributeDefinitionBuilder classIsPublic(boolean isPublic) {
        return classIs(isPublic, Flags.PUBLIC);
    }

    /**
     * Adds to the modifier flags of the generated getter. 
     * If no getter is generated the modifier flags will be silently
     * ignored.
     * @param getterFlags the modifier flags (see {@link Flags})
     * @return this instance for method chaining
     */
    public AttributeDefinitionBuilder getterModifiers(long getterFlags) {
        this.getterFlags |= getterFlags;
        return this;
    }

    private AttributeDefinitionBuilder getterIs(boolean is, long mod) {
        if (is) {
            getterModifiers(mod);
        } else {
            this.getterFlags &= ~mod;
        }
        return this;
    }
    
    /**
     * Sets the setter STATIC flag.
     * @param isStatic true to set the flag, false to clear it
     * @return this instance for method chaining
     * 
     * @see #getterModifiers(long)
     */
    public AttributeDefinitionBuilder getterIsStatic(boolean isStatic) {
        return getterIs(isStatic, Flags.STATIC);
    }
    
    /**
     * Sets the getter FINAL flag.
     * @param isFinal true to set the flag, false to clear it
     * @return this instance for method chaining
     * 
     * @see #getterModifiers(long)
     */
    public AttributeDefinitionBuilder getterIsFinal(boolean isFinal) {
        return getterIs(isFinal, Flags.FINAL);
    }
    
    /**
     * Sets the getter PUBLIC flag.
     * @param isPublic true to set the flag, false to clear it
     * @return this instance for method chaining
     * 
     * @see #getterModifiers(long)
     */
    public AttributeDefinitionBuilder getterIsPublic(boolean isPublic) {
        return getterIs(isPublic, Flags.PUBLIC);
    }

    /**
     * Sets the modifier flags of the generated getter. If no getter is generated the modifier flags will be silently
     * ignored.
     * @param getterFlags the modifier flags (see {@link Flags})
     * @return this instance for method chaining
     */
    public AttributeDefinitionBuilder getterFlags(long getterFlags) {
        this.getterFlags = getterFlags;
        return this;
    }
    
    public AttributeDefinitionBuilder addGetterFlags(long getterFlags) {
        this.getterFlags = this.getterFlags | getterFlags;
        return this;
    }

    /**
     * Sets the code block to use for the generated getter. If no getter is generated the code block will be
     * silently ignored.
     * @param getterBlock a code block
     * @return this instance for method chaining
     */
    public AttributeDefinitionBuilder getterBlock(JCTree.JCBlock getterBlock) {
        this.getterBlock = getterBlock;
        return this;
    }

    /**
     * Adds to the modifier flags of the generated setter. 
     * If no setter is generated the modifier flags will be silently
     * ignored.
     * @param setterFlags the modifier flags (see {@link Flags})
     * @return this instance for method chaining
     */
    public AttributeDefinitionBuilder setterModifiers(long setterFlags) {
        this.setterFlags |= setterFlags;
        return this;
    }

    private AttributeDefinitionBuilder setterIs(boolean is, long mod) {
        if (is) {
            setterModifiers(mod);
        } else {
            this.setterFlags &= ~mod;
        }
        return this;
    }
    
    /**
     * Sets the setter STATIC flag.
     * @param isStatic true to set the flag, false to clear it
     * @return this instance for method chaining
     * 
     * @see #setterModifiers(long)
     */
    public AttributeDefinitionBuilder setterIsStatic(boolean isStatic) {
        return setterIs(isStatic, Flags.STATIC);
    }
    
    /**
     * Sets the setter FINAL flag.
     * @param isFinal true to set the flag, false to clear it
     * @return this instance for method chaining
     * 
     * @see #setterModifiers(long)
     */
    public AttributeDefinitionBuilder setterIsFinal(boolean isFinal) {
        return setterIs(isFinal, Flags.FINAL);
    }
    
    /**
     * Sets the setter PUBLIC flag.
     * @param isPublic true to set the flag, false to clear it
     * @return this instance for method chaining
     * 
     * @see #setterModifiers(long)
     */
    public AttributeDefinitionBuilder setterIsPublic(boolean isPublic) {
        return setterIs(isPublic, Flags.PUBLIC);
    }

    /**
     * Sets the code block to use for the generated setter. If no setter is generated the code block will be
     * silently ignored.
     * @param setterBlock a code block
     * @return this instance for method chaining
     */
    public AttributeDefinitionBuilder setterBlock(JCTree.JCBlock setterBlock) {
        this.setterBlock = setterBlock;
        return this;
    }

    /**
     * Causes the generated global to be immutable. The <tt>value</tt> field is declared <tt>final</tt> and no
     * setter is generated.
     * @return this instance for method chaining
     */
    public AttributeDefinitionBuilder immutable() {
        this.writable = false;
        return this;
    }

    /**
     * The <tt>value</tt> field will be declared with the initial value given by the parameter
     * @param initialValue the initial value of the global.
     * @return this instance for method chaining
     */
    public AttributeDefinitionBuilder initialValue(JCTree.JCExpression initialValue) {
        this.variableInit = initialValue;
        return this;
    }

    /**
     * Applies the given <tt>valueAnnotations</tt> to the getter method and to the parameter of the setter method.
     * @param valueAnnotations the annotations to apply.
     * @return this instance for method chaining
     */
    public AttributeDefinitionBuilder valueAnnotations(List<JCTree.JCAnnotation> valueAnnotations) {
        this.valueAnnotations = valueAnnotations;
        return this;
    }

    public AttributeDefinitionBuilder classAnnotations(List<JCTree.JCAnnotation> classAnnotations) {
        this.classAnnotations = classAnnotations;
        return this;
    }

    public AttributeDefinitionBuilder skipConstructor() {
        this.skipConstructor = true;
        return this;
    }
    
    private Name getClassName(String variableName) {
        return owner.gen.quoteName(variableName);
    }

    private Name getGetterName(String variableName) {
        return owner.names().fromString(Util.getGetterName(variableName));
    }

    private Name getSetterName(String variableName) {
        return owner.names().fromString(Util.getSetterName(variableName));
    }
}